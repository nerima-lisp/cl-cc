;;;; tests/unit/expand/macros-stdlib-bind-error-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)



(it-sequential "with-slots-expansion"
  (let* ((result     (our-macroexpand-1 '(with-slots (x) obj body)))
         (inner-form (caddr result))
         (binding    (caadr inner-form)))
    (expect 'let :to-be (car result))
    (expect 'symbol-macrolet :to-be (car inner-form))
    (expect 'x :to-be (car binding))
    (expect 'slot-value :to-be (caadr binding))))

(it-sequential "nth-value-expansion"
  (let* ((result (our-macroexpand-1 '(nth-value 1 form))))
    (expect 'let :to-be (car result))
    (expect (caadar (second result)) :to-be 'multiple-value-list)))

(it-sequential "destructuring-bind-expansion"
  (let* ((result (our-macroexpand-1 '(destructuring-bind (a b) expr body)))
         (inner  (caddr result)))
    (expect 'let :to-be (car result))
    (expect 'let* :to-be (car inner))))

(it-sequential "assert-expansion"
  (let* ((basic  (our-macroexpand-1 '(assert test)))
         (custom (our-macroexpand-1 '(assert test () "bad input"))))
    (expect 'unless :to-be (car  basic))
    (expect 'test :to-equal (cadr basic))
    (expect 'cerror :to-be (car (caddr basic)))
    (expect 'cerror :to-be (car (caddr custom)))
    (expect "bad input" :to-equal (caddr (caddr custom)))))

(it-sequential "ignore-errors-expansion"
  (let* ((result (our-macroexpand-1 '(ignore-errors expr)))
         (clause (caddr result)))
    (expect 'handler-case :to-be (car result))
    (expect 'error :to-be (car clause))))
