;;;; backend.lisp — self-registration with cl-cc/backend-protocol
;;;;
;;;; JavaScript's coupling to the pipeline ran both ways. Outbound, the pipeline
;;;; scanned this package for %JS-* helpers to register as VM host bridges and
;;;; for *JS-* specials to seed into VM globals. Inbound, it SETF'd this
;;;; package's *JS-APPLY-FN*, *JS-CALLABLE-P* and *JS-APPLY-WITH-THIS-FN* with
;;;; closures built out of cl-cc/vm internals, so that a host array method
;;;; handed a compiled-JS callback could re-enter the VM.
;;;;
;;;; Both directions are answered here now. The defaults in runtime-call.lisp
;;;; already anticipated this -- they document themselves as "the pipeline
;;;; rebinds it" -- so the mechanism did not change, only which side reaches
;;;; across. The pipeline supplies capability (recognise a VM closure, call one,
;;;; read/write a VM global) with no VM types in the signature; the policy stays
;;;; here, where knowing what a callable JS value is and how `this' nests is
;;;; ordinary local knowledge.

(in-package :cl-cc/javascript)

(defclass javascript-backend () ()
  (:documentation "The JavaScript language backend, as seen by cl-cc/backend-protocol."))

(defun %js-bridge-name-p (symbol)
  "Return T when SYMBOL is one of this package's %JS-* runtime helpers."
  (let ((name (symbol-name symbol)))
    (and (>= (length name) 4)
         (string= "%JS-" name :end2 4))))

(defun %js-global-seed-name-p (symbol)
  "Return T when SYMBOL is a special this package needs seeded into VM globals.

*JS-* covers the prelude's bindings -- Symbol, Infinity, the error classes --
each of which compiles to a VM-GET-GLOBAL, so without seeding every JS program
fails at run time with an unbound global. %JS-* covers specials like %JS-THIS,
so `this' at top level reads as undefined rather than erroring."
  (let ((name (symbol-name symbol)))
    (and (>= (length name) 4)
         (or (string= "*JS-" name :end2 4)
             (string= "%JS-" name :end2 4)))))

(defmethod cl-cc/backend-protocol:backend-bridge-symbols ((backend javascript-backend))
  "Every fbound, non-macro %JS-* function whose home package is :cl-cc/javascript."
  (let ((pkg (find-package :cl-cc/javascript))
        (symbols '()))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym))
                   (%js-bridge-name-p sym))
          (push sym symbols))))
    (nreverse symbols)))

(defmethod cl-cc/backend-protocol:backend-global-symbols ((backend javascript-backend))
  "Every bound *JS-* / %JS-* special whose home package is :cl-cc/javascript."
  (let ((pkg (find-package :cl-cc/javascript))
        (symbols '()))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (boundp sym)
                   (%js-global-seed-name-p sym))
          (push sym symbols))))
    (nreverse symbols)))

(defmethod cl-cc/backend-protocol:install-backend-vm-integration
    ((backend javascript-backend) integration)
  "Install VM-aware invokers over the host-only defaults in runtime-call.lisp."
  (let ((closure-p (cl-cc/backend-protocol:vm-integration-closure-p integration))
        (call-closure (cl-cc/backend-protocol:vm-integration-call-closure integration))
        (global-bound-p (cl-cc/backend-protocol:vm-integration-global-bound-p integration))
        (global-value (cl-cc/backend-protocol:vm-integration-global-value integration))
        (set-global (cl-cc/backend-protocol:vm-integration-set-global integration))
        (remove-global (cl-cc/backend-protocol:vm-integration-remove-global integration)))
    ;; Route a callback back through the VM when it is a compiled-JS closure;
    ;; host functions still go via APPLY. A callable JS object (`super', the
    ;; Intl/Symbol stubs) carries its implementation under __call__.
    (setf *js-apply-fn*
          (labels ((invoke (fn args)
                     (cond
                       ((funcall closure-p fn) (funcall call-closure fn args))
                       ((and (hash-table-p fn) (gethash "__call__" fn))
                        (invoke (gethash "__call__" fn) args))
                       (t (apply fn args)))))
            #'invoke))
    ;; Prototype method lookup has to see a compiled-JS method as callable, or
    ;; obj.method resolves to a data value instead of a bound method.
    (setf *js-callable-p*
          (lambda (x) (or (functionp x) (funcall closure-p x))))
    ;; Bind `this' in BOTH the host special and the VM global: a compiled-JS
    ;; method body reads `this' through VM-GET-GLOBAL and cannot see a host
    ;; dynamic binding. Save and restore so a nested call (a.m() calling b.n())
    ;; leaves the outer receiver in place.
    (setf *js-apply-with-this-fn*
          (lambda (this fn args)
            (let ((had (funcall global-bound-p '%js-this))
                  (prev (funcall global-value '%js-this)))
              (funcall set-global '%js-this this)
              (unwind-protect
                   (let ((%js-this this))
                     (funcall *js-apply-fn* fn args))
                (if had
                    (funcall set-global '%js-this prev)
                    (funcall remove-global '%js-this))))))
    integration))

(cl-cc/backend-protocol:register-backend :javascript (make-instance 'javascript-backend))
