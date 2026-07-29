;;;; tests/unit/expand/expander-definitions-type-tests.lisp — Definition-form type tests

(in-package :cl-cc/test)



(it-sequential "expander-deftype-registers-alias"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(deftype my-index-type fixnum))))
    (expect (car result) :to-be 'quote)
    (expect (second result) :to-be 'my-index-type)))
