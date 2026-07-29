;;;; tests/unit/parse/cl/lower-tests.lisp — CL lowering unit tests

(in-package :cl-cc/test)



(defun lower (sexp)
  "Lower an s-expression to an AST node."
  (cl-cc::lower-sexp-to-ast sexp))

(it-sequential "lower-integer-produces-ast-int"
  (let ((node (lower 42)))
    (expect (cl-cc::ast-int-p node) :to-be-truthy)
    (expect (= 42 (cl-cc::ast-int-value node)) :to-be-truthy)))

(it-sequential "lower-unary-minus-becomes-negation"
  (let ((node (lower '(- 7))))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be '-)
    (expect (= 0 (cl-cc::ast-int-value (cl-cc::ast-binop-lhs node))) :to-be-truthy)
    (expect (= 7 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs node))) :to-be-truthy)))

(it-sequential "lower-if-form"
  (let ((node (lower '(if x 1 2))))
    (expect (cl-cc::ast-if-p node) :to-be-truthy)
    (expect (cl-cc::ast-var-name (cl-cc::ast-if-cond node)) :to-be 'x)))

(it-sequential "lower-setq-multi-var"
  (let ((node (lower '(setq a 1 b 2))))
    (expect (cl-cc::ast-progn-p node) :to-be-truthy)
    (expect (= 2 (length (cl-cc::ast-progn-forms node))) :to-be-truthy)))
