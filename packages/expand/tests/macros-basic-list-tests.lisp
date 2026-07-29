;;;; tests/unit/expand/macros-basic-list-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)



(it-sequential "list-empty-is-nil"
  (expect nil :to-equal (our-macroexpand-1 '(list))))

(it-sequential "list-expands-to-nested-cons one-element"
  (destructuring-bind (form expected) (list '(list x) '(cons x nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "list-expands-to-nested-cons two-elements"
  (destructuring-bind (form expected) (list '(list a b) '(cons a (cons b nil)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "list-expands-to-nested-cons three-elements"
  (destructuring-bind (form expected) (list '(list a b c) '(cons a (cons b (cons c nil))))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "list-expands-to-nested-cons literals"
  (destructuring-bind (form expected) (list '(list 1 2 3) '(cons 1 (cons 2 (cons 3 nil))))
    (expect expected :to-equal (our-macroexpand-1 form))))
