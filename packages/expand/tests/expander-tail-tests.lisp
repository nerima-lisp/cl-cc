;;;; tests/unit/expand/expander-tail-tests.lisp — Tail-form expander tests

(in-package :cl-cc/test)



(it-sequential "expander-tail-round-normalizes-arity"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(round x))))
    (expect (car result) :to-be 'round)
    (expect (third result) :to-equal 1)))

(it-sequential "expander-tail-error-and-warn-expand-format-strings"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(error "oops" 1))))
    (expect (car result) :to-be 'error)
    (expect (search "FORMAT" (format nil "~S" (second result))) :to-be-truthy)))
