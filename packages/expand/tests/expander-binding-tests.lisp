;;;; tests/unit/expand/expander-binding-tests.lisp — Binding helper tests

(in-package :cl-cc/test)



(it-sequential "let-binding-symbol-value"
  (let ((b (cl-cc/expand::expand-let-binding '(x 42))))
    (expect (first b) :to-equal 'x)
    (expect (second b) :to-equal 42)))

(it-sequential "let-binding-bare-symbol"
  (expect (cl-cc/expand::expand-let-binding 'x) :to-equal 'x))

(it-sequential "expand-flet-labels-binding"
  (let ((full  (cl-cc/expand::expand-flet-labels-binding '(foo (x) (+ x 1))))
        (short (cl-cc/expand::expand-flet-labels-binding '(foo (x)))))
    (expect (first full) :to-equal 'foo)
    (expect (second full) :to-equal '(x))
    (expect (consp  (third full)) :to-be-truthy)
    (expect short :to-equal '(foo (x)))))
