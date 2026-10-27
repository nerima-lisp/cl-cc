;;; runtime-stdlib-3-numeric-tests.lisp — FR-952/955/956 native VM number tower

(in-package :cl-cc/test)



(it-sequential "fr-952-bignum-expt-2-1000"
  (let ((value (cl-cc/vm/numbers::vm-bignum-expt 2 1000)))
    (expect (cl-cc/vm/numbers::vm-bignum-p value) :to-be-truthy)
    (expect (write-to-string value :base 10) :to-equal (write-to-string (expt 2 1000)))))

(it-sequential "fr-956-complex-euler-identity"
  (let ((result (cl-cc/vm::vm-complex-exp (cl-cc/vm::vm-complex-make 0 pi))))
    (expect (cl-cc/vm::vm-complex-p result) :to-be-truthy)
    (expect (< (abs (+ 1 (cl-cc/vm::vm-realpart result))) 1.0d-12) :to-be-truthy)
    (expect (< (abs (cl-cc/vm::vm-imagpart result)) 1.0d-12) :to-be-truthy)))

