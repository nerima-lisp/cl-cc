(in-package :cl-cc/test)



(it-sequential "macros-restarts-control-expansion find-restart"
  (destructuring-bind (form expected-car) (list '(find-restart 'my-restart) 'let)
    (expect (car (our-macroexpand-1 form)) :to-be expected-car)))

(it-sequential "macros-restarts-control-expansion invoke-restart"
  (destructuring-bind (form expected-car) (list '(invoke-restart 'my-restart) 'let*)
    (expect (car (our-macroexpand-1 form)) :to-be expected-car)))

(it-sequential "macros-restarts-control-expansion abort"
  (destructuring-bind (form expected-car) (list '(abort) 'let)
    (expect (car (our-macroexpand-1 form)) :to-be expected-car)))

(it-sequential "macros-restarts-control-expansion restart-name"
  (destructuring-bind (form expected-car) (list '(restart-name r) 'if)
    (expect (car (our-macroexpand-1 form)) :to-be expected-car)))

(it-sequential "macros-restarts-compute-restarts-expands"
  (expect (our-macroexpand-1 '(compute-restarts)) :to-be 'cl-cc/expand::*%active-restarts*))
