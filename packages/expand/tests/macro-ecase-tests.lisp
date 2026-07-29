;;;; tests/unit/expand/macro-ecase-tests.lisp — ECASE macro tests

(in-package :cl-cc/test)



(it-sequential "ecase-expands-to-let-with-case"
  (let ((result (our-macroexpand-1 '(ecase x (1 'one) (2 'two)))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 1) :to-be-truthy)
    (expect 'x :to-be (cadr (caadr result)))
    (let ((case-form (caddr result)))
      (expect 'case :to-be (car case-form))
      (let ((tmp-var (first (caadr result))))
        (expect tmp-var :to-be (cadr case-form))))))

(it-sequential "ecase-has-otherwise-error-clause"
  (let* ((result (our-macroexpand-1 '(ecase x (a 1) (b 2))))
         (case-form (caddr result))
         (clauses (cddr case-form))
         (otherwise-clause (find 'otherwise clauses :key #'car)))
    (expect otherwise-clause :to-be-truthy)
    (expect 'error :to-be (caadr otherwise-clause))))

(it-sequential "ecase-preserves-user-clauses"
  (let* ((result (our-macroexpand-1 '(ecase val (:foo 'foo-result) (:bar 'bar-result))))
         (case-form (caddr result))
         (user-clauses (butlast (cddr case-form))))
    (expect (= (length user-clauses) 2) :to-be-truthy)
    (expect :foo :to-be (car (first user-clauses)))
    (expect :bar :to-be (car (second user-clauses)))))

(it-sequential "ecase-empty-input-still-has-otherwise-clause"
  (let* ((result (our-macroexpand-1 '(ecase x)))
         (case-form (caddr result))
         (clauses (cddr case-form)))
    (expect (= (length clauses) 1) :to-be-truthy)
    (expect 'otherwise :to-be (car (first clauses)))))

(it-sequential "integration-ecase-full-expansion"
  (let ((result (our-macroexpand '(ecase x (1 'one) (2 'two)))))
    (expect 'let :to-be (car result))))
