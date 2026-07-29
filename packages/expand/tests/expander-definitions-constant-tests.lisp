;;;; tests/unit/expand/expander-definitions-constant-tests.lisp — Definition-form constant tests

(in-package :cl-cc/test)



(it-sequential "expander-defconstant-to-defparameter"
  (let ((basic   (cl-cc/expand:compiler-macroexpand-all '(defconstant +my-const+ 42)))
        (with-doc (cl-cc/expand:compiler-macroexpand-all '(defconstant +pi+ 3.14159 "Pi constant"))))
    (expect (car basic) :to-be 'defparameter)
    (expect (third basic) :to-equal 42)
    (expect (car with-doc) :to-be 'defparameter)
    (expect (second with-doc) :to-be '+pi+)))
