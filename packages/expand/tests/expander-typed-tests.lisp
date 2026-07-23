;;;; tests/unit/expand/expander-typed-tests.lisp — Typed-form expander tests

(in-package :cl-cc/test)



(it-sequential "expand-typed-defun-plain-params-unchanged"
  (let* ((result (cl-cc/expand::expand-typed-defun-or-lambda
                  'defun 'my-typed-fn '(a b) '((+ a b))))
         (result-str (format nil "~S" result)))
    (expect (search "MY-TYPED-FN" result-str) :to-be-truthy)))

(it-sequential "expand-typed-defun-typed-params-stripped"
  (let* ((result (cl-cc/expand::expand-typed-defun-or-lambda
                  'defun 'typed-adder '((x integer) (y integer)) '((+ x y))))
         (result-str (format nil "~S" result)))
    (expect (search "TYPEP" result-str) :to-be-truthy)))

(it-sequential "expand-typed-lambda-produces-lambda"
  (let* ((result (cl-cc/expand::expand-typed-defun-or-lambda
                  'lambda nil '(x) '(x)))
         (result-str (format nil "~S" result)))
    (expect (search "LAMBDA" result-str) :to-be-truthy)))
