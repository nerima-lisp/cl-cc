;;; runtime-stdlib-3-numeric-tests.lisp — FR-952/955/956 native VM number tower

(in-package :cl-cc/test)



(it-sequential "fr-952-bignum-expt-2-1000"
  (let ((value (cl-cc/vm::vm-bignum-expt 2 1000)))
    (expect (cl-cc/vm::vm-bignum-p value) :to-be-truthy)
    (expect (cl-cc/vm::vm-bignum-to-string value 10) :to-equal (write-to-string (expt 2 1000)))))

(it-sequential "fr-952-bignum-large-multiplication"
  (let* ((lhs (1- (expt 2 257)))
         (rhs (+ (expt 2 193) 12345))
         (product (cl-cc/vm::vm-bignum-mul lhs rhs)))
    (expect (cl-cc/vm::vm-bignum-p product) :to-be-truthy)
    (expect (= (* lhs rhs) (cl-cc/vm::vm-bignum-to-integer product)) :to-be-truthy)))

(it-sequential "fr-952-bignum-gcd"
  (let* ((a (* (expt 2 180) 45))
         (b (* (expt 2 120) 75))
         (g (cl-cc/vm::vm-bignum-gcd a b)))
    (expect (cl-cc/vm::vm-bignum-p g) :to-be-truthy)
    (expect (= (gcd a b) (cl-cc/vm::vm-bignum-to-integer g)) :to-be-truthy)))

(it-sequential "fr-955-rational-addition-normalizes"
  (let ((result (cl-cc/vm::vm-rational-add
                 (cl-cc/vm::vm-make-ratio 1 3)
                 (cl-cc/vm::vm-make-ratio 1 6))))
    (expect (cl-cc/vm::vm-ratio-p result) :to-be-truthy)
    (expect (= 1 (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy)))

(it-sequential "fr-955-rationalize-decimal"
  (let ((result (cl-cc/vm::vm-rationalize 0.1d0)))
    (expect (cl-cc/vm::vm-ratio-p result) :to-be-truthy)
    (expect (= 1 (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= 10 (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy)))

(it-sequential "fr-955-vm-floor-rational-remainder"
  (multiple-value-bind (q r)
      (cl-cc/vm::vm-floor (cl-cc/vm::vm-make-ratio 7 3))
    (expect (= 2 q) :to-be-truthy)
    (expect (cl-cc/vm::vm-ratio-p r) :to-be-truthy)
    (expect (= 1 (cl-cc/vm::vm-ratio-numerator r)) :to-be-truthy)
    (expect (= 3 (cl-cc/vm::vm-ratio-denominator r)) :to-be-truthy)))

(it-sequential "fr-956-complex-sqrt-minus-one"
  (let ((result (cl-cc/vm::vm-complex-sqrt -1)))
    (expect (cl-cc/vm::vm-complex-p result) :to-be-truthy)
    (expect (= 0 (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= 1 (cl-cc/vm::vm-imagpart result)) :to-be-truthy)))

(it-sequential "fr-956-complex-euler-identity"
  (let ((result (cl-cc/vm::vm-complex-exp (cl-cc/vm::vm-complex-make 0 pi))))
    (expect (cl-cc/vm::vm-complex-p result) :to-be-truthy)
    (expect (< (abs (+ 1 (cl-cc/vm::vm-realpart result))) 1.0d-12) :to-be-truthy)
    (expect (< (abs (cl-cc/vm::vm-imagpart result)) 1.0d-12) :to-be-truthy)))

(it-sequential "fr-956-complex-print-format"
  (expect (write-to-string (cl-cc/vm::vm-complex-make 3 4)) :to-equal "#C(3 4)"))
