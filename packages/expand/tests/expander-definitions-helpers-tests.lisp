;;;; tests/unit/expand/expander-definitions-helpers-tests.lisp — Definition helper tests

(in-package :cl-cc/test)



(it-sequential "expand-lambda-list-defaults-expands-defaults"
  (expect (cl-cc/expand::expand-lambda-list-defaults
                 '(x &optional (y (1+ 2) y-p) &key ((:z z) (1+ 3) z-p))) :to-equal '(x &optional (y (+ 2 1) y-p) &key ((:z z) (+ 3 1) z-p))))

(it-sequential "expand-lambda-list-defaults-keeps-plain-params"
  (expect (cl-cc/expand::expand-lambda-list-defaults '(a b &rest args)) :to-equal '(a b &rest args)))
