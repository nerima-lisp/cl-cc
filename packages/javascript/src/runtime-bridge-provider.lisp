;;;; runtime-bridge-provider.lisp — register JS runtime helpers as a backend
;;;; bridge provider.
;;;;
;;;; The JS frontend lowers operators, member access, and builtins to calls on
;;;; %JS-* helpers; the VM host bridge is a whitelist. Previously the pipeline
;;;; scanned :cl-cc/javascript for these; now JS owns that knowledge and
;;;; registers a provider thunk (see cl-cc/bootstrap:backend-bridge-providers).
;;;;
;;;; This covers only the function bridges. The VM-closure integration
;;;; (*js-apply-fn*, *js-callable-p*, *js-apply-with-this-fn*) and the *JS-*
;;;; global seeding remain installed by the pipeline for now — that two-way
;;;; coupling is a separate step (§5-1).

(in-package :cl-cc/javascript)

(defun %js-host-bridge-entries ()
  "Return (symbol . function) pairs for every fbound, non-macro %JS-* function
homed in :cl-cc/javascript."
  (let ((pkg (find-package :cl-cc/javascript))
        (entries '()))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym))
                   (let ((name (symbol-name sym)))
                     (and (>= (length name) 4)
                          (string= "%JS-" name :end2 4))))
          (push (cons sym (fdefinition sym)) entries))))
    (nreverse entries)))

(register-backend-bridge-provider #'%js-host-bridge-entries)
