;;;; tests/unit/expand/macro-shiftf-tests.lisp — SHIFTF macro tests

(in-package :cl-cc/test)



(it-sequential "shiftf-two-place-structure"
  (let ((result (our-macroexpand-1 '(shiftf x 99))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 1) :to-be-truthy)
    (expect 'x :to-be (cadr (caadr result)))
    (expect 'setf :to-be (car (caddr result)))
    (expect 'x :to-be (cadr (caddr result)))
    (let ((tmp-var (first (caadr result))))
      (expect tmp-var :to-be (car (last result))))))

(it-sequential "shiftf-returns-first-old-value"
  (let ((result (our-macroexpand-1 '(shiftf a b 0))))
    (let ((first-temp (first (caadr result))))
      (expect first-temp :to-be (car (last result))))))

(it-sequential "shiftf-three-place-chain"
  (let ((result (our-macroexpand-1 '(shiftf a b c 0))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 3) :to-be-truthy)
    (expect 'setf :to-be (car (caddr result)))
    (expect 'setf :to-be (car (cadddr result)))
    (let ((first-temp (first (caadr result))))
      (expect first-temp :to-be (car (last result))))))

(it-sequential "shiftf-insufficient-args-signals-error no-args"
  (destructuring-bind (form) (list '(shiftf))
    (signals error (our-macroexpand-1 form))))

(it-sequential "shiftf-insufficient-args-signals-error one-arg"
  (destructuring-bind (form) (list '(shiftf x))
    (signals error (our-macroexpand-1 form))))

(it-sequential "integration-shiftf-full-expansion"
  (let ((result (our-macroexpand '(shiftf a b 0))))
    (expect (search "shiftf" (string-downcase (format nil "~S" result))) :to-be-falsy)))
