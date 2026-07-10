(in-package :cl-cc/prolog)

;;; Built-in Predicate Dispatch Table (data layer)
;;;
;;; Each handler is (lambda (args env k)) — continuation-passing style:
;;; args = predicate arguments, env = current bindings, k = success continuation.

(defvar *builtin-predicates* (make-hash-table :test 'eq))

(defmacro define-prolog-builtin (name lambda-list &body body)
  "Define a CPS Prolog builtin handler with a destructured ARGS list."
  (let ((docstring (when (stringp (first body))
                     (pop body))))
    `(defun ,name (args env k)
       ,@(when docstring (list docstring))
       (destructuring-bind ,lambda-list args
         ,@body))))

(declaim (ftype function solve-conjunction solve-goal register-builtin-specs))

(defun register-builtin-specs (table specs)
  "Populate TABLE from SPECS entries of the form (NAME HANDLER)."
  (dolist (spec specs)
    (destructuring-bind (name handler) spec
      (setf (gethash name table)
            (symbol-function handler)))))

(define-prolog-builtin prolog-or-handler (alt1 &rest alts)
  "Try each alternative goal in sequence."
  (dolist (alt (cons alt1 alts))
    (solve-goal alt env k)))

(define-prolog-builtin prolog-unify-handler (left right)
  "Unify LEFT and RIGHT, then continue with the resulting environment."
  (when-unify-succeeds (new-env left right env)
    (funcall k new-env)))

(define-prolog-builtin prolog-not-unify-handler (left right)
  "Succeed only when LEFT and RIGHT do not unify."
  (when-unify-fails (left right env)
    (funcall k env)))

(define-prolog-builtin prolog-when-handler (condition)
  "Evaluate CONDITION as Lisp and continue only on truthy results."
  (when (eval-lisp-condition condition env)
    (funcall k env)))
