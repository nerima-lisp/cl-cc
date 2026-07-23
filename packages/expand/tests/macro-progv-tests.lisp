;;;; tests/unit/expand/macro-progv-tests.lisp — PROGV macro tests

(in-package :cl-cc/test)



(it-sequential "progv-basic-structure"
  (let ((result (our-macroexpand-1 '(progv '(x y) '(1 2) body))))
    (expect 'let* :to-be (car result))
    (expect (= (length (cadr result)) 3) :to-be-truthy)))

(it-sequential "progv-binds-symbols-and-values"
  (let* ((result (our-macroexpand-1 '(progv sym-list val-list body-form)))
         (bindings (cadr result)))
    (expect 'sym-list :to-be (cadr (first bindings)))
    (expect 'val-list :to-be (cadr (second bindings)))))

(it-sequential "progv-calls-progv-enter"
  (let* ((result (our-macroexpand-1 '(progv '(x) '(1) (print x))))
         (bindings (cadr result))
         (saved-binding (third bindings))
         (enter-call (second saved-binding)))
    (expect 'cl-cc/expand::%progv-enter :to-be (car enter-call))))

(it-sequential "progv-body-unwind-structure"
  (let* ((result      (our-macroexpand-1 '(progv '(x) '(1) form1 form2)))
         (bindings    (cadr result))
         (saved-var   (first (third bindings)))
         (unwind-form (caddr result))
         (cleanup     (caddr unwind-form)))
    (expect (car unwind-form) :to-be 'unwind-protect)
    (expect (car (cadr unwind-form)) :to-be 'progn)
    (expect (car cleanup) :to-be 'cl-cc/expand::%progv-exit)
    (expect (cadr cleanup) :to-be saved-var)))
