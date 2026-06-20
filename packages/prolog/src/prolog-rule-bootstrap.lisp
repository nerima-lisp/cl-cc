(in-package :cl-cc/prolog)

;;; Load-time bootstrap for declarative Prolog rules

(eval-when (:load-toplevel :execute)
  (dolist (spec *prolog-declarative-rule-specs*)
    (destructuring-bind (head &optional body) spec
      (register-prolog-rule head body)))

  (dolist (spec *prolog-type-rule-specs*)
    (destructuring-bind (result-type expr-kind operators) spec
      (dolist (op operators)
        (register-prolog-rule
         `(type-of (,expr-kind ,op ?a ?b) ?env (,result-type))
         '((type-of ?a ?env (integer-type))
           (type-of ?b ?env (integer-type))))))))
