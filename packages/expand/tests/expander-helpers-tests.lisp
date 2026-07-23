;;;; tests/unit/expand/expander-helpers-tests.lisp — Expander helper tests

(in-package :cl-cc/test)



(it-sequential "expand-make-array-form-initial-element"
  (let ((result (cl-cc/expand::expand-make-array-form 3 '(:initial-element 9))))
    (expect (car result) :to-be 'make-array)
    (expect (search "INITIAL-ELEMENT" (format nil "~S" result)) :to-be-truthy)))

(it-sequential "expand-make-array-form-fill-pointer"
  (let ((result (cl-cc/expand::expand-make-array-form 3 '(:adjustable t))))
    (expect (car result) :to-be 'make-array)
    (expect (search "ADJUSTABLE" (format nil "~S" result)) :to-be-truthy)))

(it-sequential "expand-setf-accessor-falls-back-to-slot-value"
  (let ((result (cl-cc/expand::expand-setf-accessor '(foo obj) 'value)))
    (expect (car result) :to-be 'cl-cc/bootstrap:rt-slot-set)
    (expect (second result) :to-be 'obj)))
