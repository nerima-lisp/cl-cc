;;;; runtime-bridge-provider.lisp — register PHP runtime helpers as a backend
;;;; bridge provider.
;;;;
;;;; The PHP frontend lowers builtins to calls on %PHP-* helpers; the VM host
;;;; bridge is a whitelist. Previously the pipeline scanned :cl-cc/php for these
;;;; symbols, coupling it to this package. Now PHP owns that knowledge and
;;;; registers a provider thunk; the pipeline installs whatever the registry
;;;; yields (see cl-cc/bootstrap:backend-bridge-providers).
;;;;
;;;; The set is identical to the old pipeline scan: the previous hand-maintained
;;;; alias alist was entirely %PHP-* identity mappings, so it is subsumed here.

(in-package :cl-cc/php)

(defun %php-host-bridge-entries ()
  "Return (symbol . function) pairs for every fbound, non-macro %PHP-* function
homed in :cl-cc/php."
  (let ((pkg (find-package :cl-cc/php))
        (entries '()))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym))
                   (let ((name (symbol-name sym)))
                     (and (>= (length name) 5)
                          (string= "%PHP-" name :end2 5))))
          (push (cons sym (fdefinition sym)) entries))))
    (nreverse entries)))

(register-backend-bridge-provider #'%php-host-bridge-entries)
