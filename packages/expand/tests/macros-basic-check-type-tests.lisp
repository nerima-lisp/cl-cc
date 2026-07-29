;;;; tests/unit/expand/macros-basic-check-type-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)



(it-sequential "check-type-expansion"
  (let* ((result (our-macroexpand-1 '(check-type x integer)))
         (guard-form (third result))
         (restart-form (third guard-form))
         (restart-clause (third restart-form)))
    (expect (car result) :to-be 'tagbody)
    (expect (car guard-form) :to-be 'unless)
    (expect (second guard-form) :to-equal '(typep x 'integer))
    (expect (car restart-form) :to-be 'restart-case)
    (expect (car restart-clause) :to-be 'store-value)))
