;;;; tests/unit/expand/expander-setf-places-helpers-tests.lisp — Setf-place helper tests

(in-package :cl-cc/test)



(it-sequential "expand-setf-cons-place-car-and-cdr"
  (let ((car-result (cl-cc/expand::expand-setf-cons-place '(car x) 'v))
        (cdr-result (cl-cc/expand::expand-setf-cons-place '(cdr x) 'v)))
    (assert-form-string-contains car-result "RPLACA")
    (assert-form-string-contains cdr-result "RPLACD")))

(it-sequential "expand-setf-cons-place-nth"
  (let ((result (cl-cc/expand::expand-setf-cons-place '(nth 2 x) 'v)))
    (assert-form-string-contains result "NTHCDR")))

(it-sequential "expand-setf-cons-place-deep-cxr"
  (let ((result (cl-cc/expand::expand-setf-cons-place '(cdddr method-entry) 'v)))
    (assert-form-string-contains result "RPLACD")
    (assert-form-string-contains result "CDR")))
