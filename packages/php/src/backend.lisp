;;;; backend.lisp — self-registration with cl-cc/backend-protocol
;;;;
;;;; The pipeline used to scan this package itself, which meant the pipeline
;;;; knew both the package name :cl-cc/php and the %PHP- prefix its lowering
;;;; uses. The scan lives here now: knowing its own naming convention is the one
;;;; thing this system is certain to know, and moving it removes the last
;;;; compile-time reference from cl-cc/pipeline into cl-cc/php.
;;;;
;;;; The registered symbol set is unchanged by the move -- same predicate, same
;;;; package, just evaluated from the other side of the boundary.

(in-package :cl-cc/php)

(defclass php-backend () ()
  (:documentation "The PHP language backend, as seen by cl-cc/backend-protocol."))

(defun %php-bridge-name-p (symbol)
  "Return T when SYMBOL is one of this package's %PHP-* runtime helpers.

Home-package identity, not just the prefix: a symbol inherited or imported from
elsewhere that happens to start with %PHP- is not ours to bridge."
  (let ((name (symbol-name symbol)))
    (and (>= (length name) 5)
         (string= "%PHP-" name :end2 5))))

(defmethod cl-cc/backend-protocol:backend-bridge-symbols ((backend php-backend))
  "Every fbound, non-macro %PHP-* function whose home package is :cl-cc/php.

Derived from the package rather than hand-listed: an explicit alist had already
drifted behind the lowering once, which is the failure this scan exists to
prevent."
  (let ((pkg (find-package :cl-cc/php))
        (symbols '()))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym))
                   (%php-bridge-name-p sym))
          (push sym symbols))))
    (nreverse symbols)))

(cl-cc/backend-protocol:register-backend :php (make-instance 'php-backend))
