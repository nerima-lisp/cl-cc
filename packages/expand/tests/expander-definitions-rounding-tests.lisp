;;;; tests/unit/expand/expander-definitions-rounding-tests.lisp — Definition-form rounding tests

(in-package :cl-cc/test)



(it-sequential "expander-rounding-one-arg-normalization"
  (let ((cases '((floor . (floor n))
                 (ceiling . (ceiling n))
                 (truncate . (truncate n))
                 (round . (round n)))))
    (dolist (case cases)
      (let ((result (cl-cc/expand:compiler-macroexpand-all (cdr case))))
        (expect (car result) :to-be (car case))
        (expect (second result) :to-be 'n)
        (expect (third result) :to-equal 1)))))

(it-sequential "expander-floor-two-arg-unchanged"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(floor n 3))))
    (expect (car result) :to-be 'floor)
    (expect (second result) :to-be 'n)
    (expect (third result) :to-equal 3)))
