;;;; pipeline-runtime-bridges.lisp - Language runtime bridge registration
(in-package :cl-cc/pipeline)

(defun %register-backend-runtime-bridges ()
  "Install every registered backend's runtime helpers as VM host bridges.

The VM host bridge is a whitelist. Backends (php/js/…) own the knowledge of
which of their functions are callable from compiled code and register a
provider thunk with cl-cc/bootstrap; here we install whatever the registry
yields. The pipeline no longer references any backend package for this — see
each backend's runtime-bridge-provider.lisp. Called for every language so a
program of any backend sees its bridges (registration is idempotent)."
  (dolist (provider (cl-cc/bootstrap:backend-bridge-providers))
    (dolist (entry (funcall provider))
      (cl-cc/vm:vm-register-host-bridge (car entry) (cdr entry)))))

(defvar *js-runtime-global-symbols-cache* nil)

;; JS function bridges (%JS-*) are now registered via the backend bridge
;; registry (see cl-cc/javascript's runtime-bridge-provider.lisp and
;; %register-backend-runtime-bridges, which installs every provider). Only the
;; VM-closure integration and *JS-* global seeding remain pipeline-side.

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
  "Install the JavaScript runtime's VM-closure integration.

The %JS-* function bridges are registered via the backend bridge registry (see
runtime-bridge-provider.lisp). What remains here is the two-way VM integration
the pipeline installs into the JS runtime: routing JS callbacks back through the
VM, recognizing compiled-JS closures as callable, and binding `this'. This
coupling is the next step of the §5-1 decoupling."
  (when (find-package :cl-cc/javascript)
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
