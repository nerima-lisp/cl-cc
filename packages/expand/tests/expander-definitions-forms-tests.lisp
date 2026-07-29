;;;; tests/unit/expand/expander-definitions-forms-tests.lisp — Definition-form expander tests

(in-package :cl-cc/test)



(it-sequential "expander-defun-expands-body"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(defun foo (x) (1+ x)))))
    (expect (car result) :to-be 'defun)
    (expect (second result) :to-be 'foo)))

(it-sequential "expander-lambda-expands-typed-params"
  (let ((result (format nil "~S"
                        (cl-cc/expand:compiler-macroexpand-all
                         '(lambda ((x integer)) (1+ x))))))
    (expect (search "LAMBDA" result) :to-be-truthy)
    (expect (search "TYPEP" result) :to-be-truthy)))

(it-sequential "expander-defclass-and-deftype-expands"
  (let ((class-result (cl-cc/expand:compiler-macroexpand-all
                       '(defclass sample () ((slot :initform (1+ 2))))))
        (type-result (cl-cc/expand:compiler-macroexpand-all
                      '(deftype sample-type () 'integer))))
    (expect (car class-result) :to-be 'defclass)
    (expect (car type-result) :to-be 'quote)
    (expect (second type-result) :to-be 'sample-type)))
