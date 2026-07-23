;;;; tests/unit/expand/array-predicate-expansion-tests.lisp

(in-package :cl-cc/test)



(it-sequential "array-predicate-remains-direct-call adjustable-array-p"
  (destructuring-bind (form) (list '(adjustable-array-p arr))
    (assert-no-expansion form)))

(it-sequential "array-predicate-remains-direct-call array-has-fill-pointer-p"
  (destructuring-bind (form) (list '(array-has-fill-pointer-p arr))
    (assert-no-expansion form)))
