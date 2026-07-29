;;;; tests/unit/expand/macro-multiple-value-tests.lisp — Macro multiple-value tests

(in-package :cl-cc/test)



(it-sequential "multiple-value-list-expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-list (values 1 2 3)))))
    (expect (member (car result) '(list let)) :to-be-truthy))
  (let ((result (our-macroexpand-1 '(multiple-value-list (foo)))))
    (expect (car result) :to-be 'let)
    (expect (consp (second result)) :to-be-truthy))
  (let ((result (our-macroexpand '(multiple-value-list (values 1 2 3)))))
    (expect (search "multiple-value-list" (string-downcase (format nil "~S" result))) :to-be-falsy)))

(it-sequential "multiple-value-bind-macro-expansion"
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b c) (values 1 2 3) body1 body2))))
    (expect (member (car result) '(let let*)) :to-be-truthy))
  (let ((result (our-macroexpand-1 '(multiple-value-bind (x) (foo) body))))
    (expect (car result) :to-be 'let)
    (expect (caadar (second result)) :to-be 'multiple-value-list))
  (let ((result (our-macroexpand-1 '(multiple-value-bind (a b) (values 1 2)))))
    (expect (member (car result) '(let let*)) :to-be-truthy)))

(it-sequential "multiple-value-setq-macro-expansion"
  (let* ((result   (our-macroexpand-1 '(multiple-value-setq (a b c) (values 1 2 3))))
         (bindings (cadr result)))
    (expect (car result) :to-be 'let)
    (expect (consp bindings) :to-be-truthy)
    (expect (caadar bindings) :to-be 'multiple-value-list))
  (let ((result (our-macroexpand-1 '(multiple-value-setq (a b) (values 1 2) extra-body))))
    (expect (car result) :to-be 'let)
    (expect (= 5 (length result)) :to-be-truthy))
  (let* ((result   (our-macroexpand-1 '(multiple-value-setq (x) (foo))))
         (bindings (cadr result)))
    (expect (car result) :to-be 'let)
    (expect (caadar bindings) :to-be 'multiple-value-list)))
