;;;; tests/unit/expand/macro-etypecase-tests.lisp — ETYPECASE macro tests

(in-package :cl-cc/test)



(it-sequential "etypecase-expands-to-let-with-typecase"
  (let ((result (our-macroexpand-1 '(etypecase x (string 'str) (integer 'int)))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 1) :to-be-truthy)
    (expect 'x :to-be (cadr (caadr result)))
    (let ((typecase-form (caddr result)))
      (expect 'typecase :to-be (car typecase-form)))))

(it-sequential "etypecase-has-otherwise-error-clause"
  (let* ((result (our-macroexpand-1 '(etypecase x (string 'str))))
         (typecase-form (caddr result))
         (clauses (cddr typecase-form))
         (otherwise-clause (find 'otherwise clauses :key #'car)))
    (expect otherwise-clause :to-be-truthy)
    (expect 'error :to-be (caadr otherwise-clause))))

(it-sequential "etypecase-preserves-user-type-clauses"
  (let* ((result (our-macroexpand-1 '(etypecase v (string "s") (integer "i") (symbol "sym"))))
         (typecase-form (caddr result))
         (user-clauses (butlast (cddr typecase-form))))
    (expect (= (length user-clauses) 3) :to-be-truthy)
    (expect 'string :to-be (car (first user-clauses)))
    (expect 'integer :to-be (car (second user-clauses)))
    (expect 'symbol :to-be (car (third user-clauses)))))

(it-sequential "etypecase-key-var-used-in-typecase"
  (let* ((result (our-macroexpand-1 '(etypecase (foo-call) (integer 0))))
         (tmp-var (first (caadr result)))
         (typecase-form (caddr result)))
    (expect tmp-var :to-be (cadr typecase-form))))

(it-sequential "integration-etypecase-full-expansion"
  (let ((result (our-macroexpand '(etypecase v (string "s") (integer "i")))))
    (expect 'let :to-be (car result))))
