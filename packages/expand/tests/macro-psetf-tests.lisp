;;;; tests/unit/expand/macro-psetf-tests.lisp — PSETF macro tests

(in-package :cl-cc/test)



(it-sequential "psetf-empty"
  (let ((result (our-macroexpand-1 '(psetf))))
    (expect 'let :to-be (car result))
    (expect (cadr result) :to-be-null)
    (expect (car (last result)) :to-be-null)))

(it-sequential "psetf-one-pair-has-single-temp-binding"
  (let ((result (our-macroexpand-1 '(psetf x 10))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 1) :to-be-truthy)
    (expect (= (cadr (caadr result)) 10) :to-be-truthy)
    (expect 'setf :to-be (car (caddr result)))
    (expect 'x :to-be (cadr (caddr result)))
    (expect (car (last result)) :to-be-null)))

(it-sequential "psetf-two-pairs-captures-both-before-assigning"
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 2) :to-be-truthy)
    (expect (= (cadr (first (cadr result))) 1) :to-be-truthy)
    (expect (= (cadr (second (cadr result))) 2) :to-be-truthy)
    (expect 'setf :to-be (car (caddr result)))
    (expect 'a :to-be (cadr (caddr result)))
    (expect 'setf :to-be (car (cadddr result)))
    (expect 'b :to-be (cadr (cadddr result)))
    (expect (car (last result)) :to-be-null)))

(it-sequential "psetf-odd-args-signals-error"
  (signals error (our-macroexpand-1 '(psetf x 1 y))))

(it-sequential "psetf-three-pairs"
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2 c 3))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 3) :to-be-truthy)
    (expect 'setf :to-be (car (caddr result)))))

(it-sequential "integration-psetf-full-expansion"
  (let ((result (our-macroexpand '(psetf x 1 y 2))))
    (expect 'let :to-be (car result))))
