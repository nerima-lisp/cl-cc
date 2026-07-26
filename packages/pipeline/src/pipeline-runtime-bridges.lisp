;;;; pipeline-runtime-bridges.lisp - Language runtime bridge registration
(in-package :cl-cc/pipeline)

(defun %register-backend-protocol-bridges ()
  "Register every registered backend's bridge symbols with the VM.

The pipeline used to name cl-cc/php and cl-cc/javascript directly and scan them
for their %PHP-/%JS- helpers, which made the orchestrator depend on each
backend's package name and internal naming convention -- and made either
backend impossible to move to its own repository, since a dependent cannot name
an external package's internal symbols. Each backend now answers for itself
through cl-cc/backend-protocol, and the only thing that stays here is the part
that needs the VM: calling VM-REGISTER-HOST-BRIDGE.

Returns the symbols registered, which is what the migration was checked
against: the set has to be identical to what the old package scan produced."
  (let ((registered '()))
    (dolist (entry cl-cc/backend-protocol:*registered-backends* (nreverse registered))
      (dolist (sym (cl-cc/backend-protocol:backend-bridge-symbols (cdr entry)))
        (when (and (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym)))
          (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym))
          (push sym registered))))))

(defun %register-php-runtime-bridges ()
  "Register PHP runtime helpers as VM host bridge functions.

Kept as a name because the pipeline calls it at a specific point in start-up;
the work is now the backend protocol's, and this registers whatever backends
have registered themselves, PHP among them."
  (%register-backend-protocol-bridges))

(defvar *js-runtime-bridge-symbols-cache* nil)
(defvar *js-runtime-global-symbols-cache* nil)

(defun %js-runtime-bridge-symbols ()
  (or *js-runtime-bridge-symbols-cache*
      (let ((pkg (find-package :cl-cc/javascript)))
        (when pkg
          (setf *js-runtime-bridge-symbols-cache*
                (let (symbols)
                  (do-symbols (sym pkg)
                    (when (and (eq (symbol-package sym) pkg)
                               (fboundp sym)
                               (not (macro-function sym))
                               (not (special-operator-p sym))
                               (let ((name (symbol-name sym)))
                                 (and (>= (length name) 4)
                                      (string= "%JS-" name :end2 4))))
                      (push sym symbols)))
                  (nreverse symbols)))))))

(defun %js-runtime-global-symbols ()
  (or *js-runtime-global-symbols-cache*
      (let ((pkg (find-package :cl-cc/javascript)))
        (when pkg
          (setf *js-runtime-global-symbols-cache*
                (let (symbols)
                  (do-symbols (sym pkg)
                    (when (and (eq (symbol-package sym) pkg)
                               (boundp sym)
                               (let ((name (symbol-name sym)))
                                 (and (>= (length name) 4)
                                      ;; *JS-* specials (Symbol/Infinity/error classes/...) and
                                      ;; %JS-* special vars like %js-this (so `this' at top
                                      ;; level resolves to undefined rather than erroring; a
                                      ;; method call overrides it with the receiver).
                                      (or (string= "*JS-" name :end2 4)
                                          (string= "%JS-" name :end2 4)))))
                      (push sym symbols)))
                  (nreverse symbols)))))))

(defun %register-js-runtime-bridges ()
  "Register JavaScript runtime helpers as VM host bridge functions.

The JS frontend lowers operators, member access, and builtins to calls on
cl-cc/javascript::%js-* helpers (%js-get-prop, %js-make-array, %js-console-log,
%js-make-console, ...). Register every fbound, non-macro %JS-* function whose
home package is :cl-cc/javascript so compiled JS can call them through the VM
host bridge - the same package-derived whitelist used for PHP."
  (when (find-package :cl-cc/javascript)
    (dolist (sym (%js-runtime-bridge-symbols))
      (when (and (fboundp sym)
                 (not (macro-function sym))
                 (not (special-operator-p sym)))
        (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym))))
      ;; Route JS callbacks (Array.map/filter/reduce/sort) back through the VM
      ;; when the callback is a compiled-JS closure; host functions still go via
      ;; APPLY. Host array methods call (%js-funcall fn ...) -> *js-apply-fn*; this
      ;; is the controlled inverse bridge (host runtime -> VM closure) using the
      ;; *vm-state* dynamically bound around VM execution.
      (setf cl-cc/javascript::*js-apply-fn*
            (labels ((invoke (fn args)
                       (cond
                         ((cl-cc/vm::%vm-closure-object-p fn)
                          (cl-cc/vm::%vm-call-closure-sync fn cl-cc/vm:*vm-state* args))
                         ;; A callable JS object (e.g. `super', Intl/Symbol stubs)
                         ;; carries its implementation under __call__.
                         ((and (hash-table-p fn) (gethash "__call__" fn))
                          (invoke (gethash "__call__" fn) args))
                         (t (apply fn args)))))
              #'invoke))
      ;; Teach prototype method lookup to recognize a compiled-JS method (a
      ;; vm-closure) as callable, so obj.method resolves to a bound method.
      (setf cl-cc/javascript::*js-callable-p*
            (lambda (x) (or (functionp x) (cl-cc/vm::%vm-closure-object-p x))))
      ;; Bind `this' for a method/constructor call in BOTH the host special and
      ;; the VM-global %js-this: a compiled-JS method body reads `this' via
      ;; vm-get-global, so the host dynamic binding alone is invisible to it.
      ;; Save/restore around the call so nested method calls (a.m() calling b.n())
      ;; restore the outer receiver.
      (setf cl-cc/javascript::*js-apply-with-this-fn*
            (lambda (this fn args)
              (let ((state cl-cc/vm:*vm-state*))
                (if state
                    (let* ((gv   (cl-cc/vm:vm-global-vars state))
                           (had  (nth-value 1 (gethash 'cl-cc/javascript::%js-this gv)))
                           (prev (gethash 'cl-cc/javascript::%js-this gv)))
                      (setf (gethash 'cl-cc/javascript::%js-this gv) this)
                      (unwind-protect
                           (let ((cl-cc/javascript::%js-this this))
                             (funcall cl-cc/javascript::*js-apply-fn* fn args))
                        (if had
                            (setf (gethash 'cl-cc/javascript::%js-this gv) prev)
                            (remhash 'cl-cc/javascript::%js-this gv))))
                    (let ((cl-cc/javascript::%js-this this))
                      (funcall cl-cc/javascript::*js-apply-fn* fn args))))))))

(defun seed-js-runtime-globals (state)
  "Seed JavaScript runtime special variables into STATE's VM globals.

The JS prelude (js-program-forms) binds standard globals to the VALUES of host
specials - e.g. Symbol to *js-symbol-global*, Infinity to *js-inf-float*, the
error classes to *js-error-class* etc. Each such reference compiles to a
vm-get-global, so without seeding EVERY JS program fails at runtime with
'Unbound global variable: *JS-...*'. Mirror the package-derived function-bridge
whitelist: copy every bound *JS-...* special in :cl-cc/javascript into STATE's
globals so the prelude resolves."
  (when (and (find-package :cl-cc/javascript) state)
    (dolist (sym (%js-runtime-global-symbols))
      (when (boundp sym)
        (setf (gethash sym (cl-cc/vm:vm-global-vars state))
              (symbol-value sym))))))
