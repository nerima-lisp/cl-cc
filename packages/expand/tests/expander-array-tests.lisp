;;;; tests/unit/expand/expander-array-tests.lisp — Array expander tests

(in-package :cl-cc/test)



(it-sequential "expand-make-array-adjustable-promotes adjustable"
  (destructuring-bind (kwargs expected-keyword) (list '(:adjustable t) "ADJUSTABLE")
    (let ((*print-circle* t))
    (let ((result (cl-cc/expand::expand-make-array-form 10 kwargs)))
      (expect (car result) :to-be 'make-array)
      (expect (search expected-keyword (format nil "~S" result)) :to-be-truthy)))))

(it-sequential "expand-make-array-adjustable-promotes fill-pointer"
  (destructuring-bind (kwargs expected-keyword) (list '(:fill-pointer t) "FILL-POINTER")
    (let ((*print-circle* t))
    (let ((result (cl-cc/expand::expand-make-array-form 10 kwargs)))
      (expect (car result) :to-be 'make-array)
      (expect (search expected-keyword (format nil "~S" result)) :to-be-truthy)))))

(it-sequential "expand-make-array-simple"
  (let ((result (cl-cc/expand::expand-make-array-form 10 nil)))
    (expect result :to-be-truthy)))
