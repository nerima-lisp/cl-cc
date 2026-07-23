;;;; vm-integration.lisp — wire the JS runtime into the cl-cc VM.
;;;;
;;;; Compiled JS runs on the VM, but the JS host runtime (Array.map callbacks,
;;;; method dispatch, `this' binding, prelude globals) must know how to invoke a
;;;; VM closure. That glue was historically installed by the pipeline, coupling
;;;; the pipeline to :cl-cc/javascript internals. It now lives here: JS owns it
;;;; and registers itself with cl-cc/bootstrap; the pipeline runs every
;;;; registered backend's installer/seeder without naming any backend.
;;;;
;;;; This is why :cl-cc/javascript depends on :cl-cc-vm.

(in-package :cl-cc/javascript)

(defun %install-js-vm-integration ()
  "Install the JS↔VM closure integration into the JS runtime specials."
  ;; Route JS callbacks (Array.map/filter/reduce/sort) back through the VM when
  ;; the callback is a compiled-JS closure; host functions still go via APPLY.
  (setf *js-apply-fn*
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
  ;; Recognize a compiled-JS method (a vm-closure) as callable.
  (setf *js-callable-p*
        (lambda (x) (or (functionp x) (cl-cc/vm::%vm-closure-object-p x))))
  ;; Bind `this' for a method/constructor call in BOTH the host special and the
  ;; VM-global %js-this (a compiled-JS body reads `this' via vm-get-global).
  ;; Save/restore around the call so nested method calls restore the receiver.
  (setf *js-apply-with-this-fn*
        (lambda (this fn args)
          (let ((state cl-cc/vm:*vm-state*))
            (if state
                (let* ((gv   (cl-cc/vm:vm-global-vars state))
                       (had  (nth-value 1 (gethash '%js-this gv)))
                       (prev (gethash '%js-this gv)))
                  (setf (gethash '%js-this gv) this)
                  (unwind-protect
                       (let ((%js-this this))
                         (funcall *js-apply-fn* fn args))
                    (if had
                        (setf (gethash '%js-this gv) prev)
                        (remhash '%js-this gv))))
                (let ((%js-this this))
                  (funcall *js-apply-fn* fn args)))))))

(defun %js-runtime-global-symbols ()
  "The *JS-* / %JS-* special variables the JS prelude reads as VM globals."
  (let ((pkg (find-package :cl-cc/javascript))
        (symbols '()))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (boundp sym)
                   (let ((name (symbol-name sym)))
                     (and (>= (length name) 4)
                          (or (string= "*JS-" name :end2 4)
                              (string= "%JS-" name :end2 4)))))
          (push sym symbols))))
    (nreverse symbols)))

(defun %seed-js-globals (state)
  "Seed the JS prelude's *JS-* globals into STATE's VM globals."
  (when state
    (dolist (sym (%js-runtime-global-symbols))
      (when (boundp sym)
        (setf (gethash sym (cl-cc/vm:vm-global-vars state))
              (symbol-value sym))))))

(cl-cc/bootstrap:register-backend-vm-integration-installer #'%install-js-vm-integration)
(cl-cc/bootstrap:register-backend-global-seeder #'%seed-js-globals)
