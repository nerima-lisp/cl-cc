;;;; tests/unit/expand/macros-introspection-tests.lisp — Introspection helper tests

(in-package :cl-cc/test)



(it-sequential "macros-introspection-equalp-expands"
  (expect (car (our-macroexpand-1 '(equalp 1 1))) :to-be 'labels))
