;;;; tests/unit/expand/macros-list-utils-tests.lisp — List utility macro tests

(in-package :cl-cc/test)



(it-sequential "macros-list-utils-sort-expands"
  (expect (car (our-macroexpand-1 '(sort lst #'<))) :to-be 'let*))

(it-sequential "macros-list-utils-list*-and-pushnew"
  (expect (our-macroexpand-1 '(list* x)) :to-equal 'x)
  (expect (car (our-macroexpand-1 '(pushnew x xs))) :to-be 'let))

(it-sequential "macros-list-utils-stable-sort-and-nreconc"
  (expect (our-macroexpand-1 '(stable-sort xs #'<)) :to-equal '(sort xs #'<))
  (expect (car (our-macroexpand-1 '(nreconc xs tail))) :to-be 'nconc))
