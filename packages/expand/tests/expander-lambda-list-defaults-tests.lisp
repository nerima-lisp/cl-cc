;;;; tests/unit/expand/expander-lambda-list-defaults-tests.lisp — Lambda-list default expander tests
;;;;
;;;; Tests for expand-lambda-list-defaults.

(in-package :cl-cc/test)



(it-sequential "lambda-list-no-defaults"
  (expect (cl-cc/expand::expand-lambda-list-defaults '(x y z)) :to-equal '(x y z)))

(it-sequential "lambda-list-optional-with-default"
  (let ((result (cl-cc/expand::expand-lambda-list-defaults '(x &optional (y 42)))))
    (expect (first result) :to-equal 'x)
    (expect (second result) :to-equal '&optional)
    ;; y's default should be expanded (42 stays as 42)
    (expect (consp (third result)) :to-be-truthy)))
