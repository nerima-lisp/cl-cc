;;;; tests/unit/expand/expander-control-tests.lisp — Control-form expander tests

(in-package :cl-cc/test)



(it-sequential "expand-eval-when-keeps-body execute"
  (destructuring-bind (situations) (list '(:execute))
    (expect (consp (cl-cc/expand::expand-eval-when-form situations '((+ 1 2)))) :to-be-truthy)))

(it-sequential "expand-eval-when-keeps-body load-toplevel"
  (destructuring-bind (situations) (list '(:load-toplevel))
    (expect (consp (cl-cc/expand::expand-eval-when-form situations '((+ 1 2)))) :to-be-truthy)))

(it-sequential "expand-eval-when-compile-only-returns-nil"
  (handler-bind ((warning #'muffle-warning))
    (let ((result (cl-cc/expand::expand-eval-when-form '(:compile-toplevel) '((+ 1 2)))))
      (expect result :to-be nil))))

(it-sequential "expand-macrolet-form-expands-local-macro"
  (let* ((result (cl-cc/expand::expand-macrolet-form
                  '((my-one () 1))
                  '((my-one))))
         (str (format nil "~S" result)))
    (expect (search "1" str) :to-be-truthy)))

(it-sequential "expand-macrolet-form-binds-environment"
  (let* ((result (cl-cc/expand::expand-macrolet-form
                  '((probe (&environment env) (if env :has-env :no-env)))
                  '((probe))))
         (str (format nil "~S" result)))
    (expect (search ":HAS-ENV" str) :to-be-truthy)))

(it-sequential "expand-macrolet-form-body-is-progn"
  (let ((result (cl-cc/expand::expand-macrolet-form
                 '((add1 (x) (+ x 1)))
                 '((add1 2) (add1 3)))))
    (expect (consp result) :to-be-truthy)))
