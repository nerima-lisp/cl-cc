;;;; tests/unit/expand/expander-definitions-function-tests.lisp — Definition-form function tests

(in-package :cl-cc/test)



(it-sequential "expander-defun-lambda-preserve-structure defun"
  (destructuring-bind (form expected-head params-pos expected-params) (list '(defun triple (x) (* 3 x)) 'defun 2 '(x))
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be expected-head)
    (expect (nth params-pos result) :to-equal expected-params))))

(it-sequential "expander-defun-lambda-preserve-structure lambda"
  (destructuring-bind (form expected-head params-pos expected-params) (list '(lambda (x y) (+ x y)) 'lambda 1 '(x y))
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be expected-head)
    (expect (nth params-pos result) :to-equal expected-params))))

(it-sequential "expander-lambda-optional-default-expanded"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(lambda (x &optional (y (+ 1 1))) (+ x y)))))
    (expect (car result) :to-be 'lambda)
    (let* ((params (second result))
           (opt-param (third params)))
      (expect (first opt-param) :to-equal 'y)
      (expect (second opt-param) :to-equal '(+ 1 1)))))
