;;; vm-bignum-tests.lisp — Unit tests for the VM native bignum/rational/complex tower
;;;
;;; Covers: vm-bignum-{add,sub,negate,div,mul}, vm-bignum-to-string with non-decimal
;;; radix, vm-integer->bignum roundtrip, vm-rational-{sub,mul,div}, vm-complex-
;;; {add,sub,mul,div,conjugate,abs,phase,log}, and
;;; vm-bignum-burnikel-ziegler-divide rounding modes.

(in-package :cl-cc/test)



;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: vm-integer->bignum roundtrip
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "bignum-integer-roundtrip zero"
  (destructuring-bind (n) (list 0)
    (let ((bn (cl-cc/vm::vm-integer->bignum n)))
    (expect (cl-cc/vm::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm::vm-bignum-to-integer bn)) :to-be-truthy))))

(it-sequential "bignum-integer-roundtrip positive"
  (destructuring-bind (n) (list 12345)
    (let ((bn (cl-cc/vm::vm-integer->bignum n)))
    (expect (cl-cc/vm::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm::vm-bignum-to-integer bn)) :to-be-truthy))))

(it-sequential "bignum-integer-roundtrip negative"
  (destructuring-bind (n) (list -99999)
    (let ((bn (cl-cc/vm::vm-integer->bignum n)))
    (expect (cl-cc/vm::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm::vm-bignum-to-integer bn)) :to-be-truthy))))

(it-sequential "bignum-integer-roundtrip power-of-2"
  (destructuring-bind (n) (list (expt 2 128))
    (let ((bn (cl-cc/vm::vm-integer->bignum n)))
    (expect (cl-cc/vm::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm::vm-bignum-to-integer bn)) :to-be-truthy))))

(it-sequential "bignum-integer-roundtrip large-neg"
  (destructuring-bind (n) (list (- (expt 2 200)))
    (let ((bn (cl-cc/vm::vm-integer->bignum n)))
    (expect (cl-cc/vm::vm-bignum-p bn) :to-be-truthy)
    (expect (= n (cl-cc/vm::vm-bignum-to-integer bn)) :to-be-truthy))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-bignum-add
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "bignum-add pos+pos"
  (destructuring-bind (a b expected) (list (expt 2 128) (expt 2 64) (+ (expt 2 128) (expt 2 64)))
    (let ((result (cl-cc/vm::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer result)) :to-be-truthy))))

(it-sequential "bignum-add pos+neg"
  (destructuring-bind (a b expected) (list (expt 2 64) (- (expt 2 32)) (- (expt 2 64) (expt 2 32)))
    (let ((result (cl-cc/vm::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer result)) :to-be-truthy))))

(it-sequential "bignum-add neg+neg"
  (destructuring-bind (a b expected) (list (- (expt 2 64)) (- (expt 2 64)) (- (expt 2 65)))
    (let ((result (cl-cc/vm::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer result)) :to-be-truthy))))

(it-sequential "bignum-add sum-zero"
  (destructuring-bind (a b expected) (list (expt 2 64) (- (expt 2 64)) 0)
    (let ((result (cl-cc/vm::vm-bignum-add a b)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer result)) :to-be-truthy))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: vm-bignum-sub and vm-bignum-negate
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "bignum-sub large-sub"
  (destructuring-bind (a b expected) (list (expt 2 200) (expt 2 100) (- (expt 2 200) (expt 2 100)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-sub a b))) :to-be-truthy)))

(it-sequential "bignum-sub neg-result"
  (destructuring-bind (a b expected) (list (expt 2 64) (expt 2 128) (- (expt 2 64) (expt 2 128)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-sub a b))) :to-be-truthy)))

(it-sequential "bignum-sub sub-self"
  (destructuring-bind (a b expected) (list (expt 2 64) (expt 2 64) 0)
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-sub a b))) :to-be-truthy)))

(it-sequential "bignum-negate negate-positive"
  (destructuring-bind (n expected) (list (expt 2 100) (- (expt 2 100)))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-negate n))) :to-be-truthy)))

(it-sequential "bignum-negate negate-negative"
  (destructuring-bind (n expected) (list (- (expt 2 100)) (expt 2 100))
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-negate n))) :to-be-truthy)))

(it-sequential "bignum-negate negate-zero"
  (destructuring-bind (n expected) (list 0 0)
    (expect (= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-negate n))) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: vm-bignum-div
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "bignum-div pos-div-pos"
  (destructuring-bind (dividend divisor expected-q expected-r) (list (expt 2 128) (expt 2 64) (expt 2 64) 0)
    (multiple-value-bind (q r) (cl-cc/vm::vm-bignum-div dividend divisor)
    (expect (= expected-q (cl-cc/vm::vm-bignum-to-integer q)) :to-be-truthy)
    (expect (= expected-r (cl-cc/vm::vm-bignum-to-integer r)) :to-be-truthy))))

(it-sequential "bignum-div large-modulo"
  (destructuring-bind (dividend divisor expected-q expected-r) (list (* (expt 2 100) 7) 7 (expt 2 100) 0)
    (multiple-value-bind (q r) (cl-cc/vm::vm-bignum-div dividend divisor)
    (expect (= expected-q (cl-cc/vm::vm-bignum-to-integer q)) :to-be-truthy)
    (expect (= expected-r (cl-cc/vm::vm-bignum-to-integer r)) :to-be-truthy))))

(it-sequential "bignum-div non-exact"
  (destructuring-bind (dividend divisor expected-q expected-r) (list (+ (* (expt 2 64) 5) 3) 5 (expt 2 64) 3)
    (multiple-value-bind (q r) (cl-cc/vm::vm-bignum-div dividend divisor)
    (expect (= expected-q (cl-cc/vm::vm-bignum-to-integer q)) :to-be-truthy)
    (expect (= expected-r (cl-cc/vm::vm-bignum-to-integer r)) :to-be-truthy))))

(it-sequential "bignum-div-by-zero-signals-error"
  (signals error (cl-cc/vm::vm-bignum-div (expt 2 64) 0)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: vm-bignum-to-string with non-decimal radix
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "bignum-to-string-radix hex-255"
  (destructuring-bind (n radix expected) (list 255 16 "FF")
    (expect (cl-cc/vm::vm-bignum-to-string n radix) :to-equal expected)))

(it-sequential "bignum-to-string-radix binary-10"
  (destructuring-bind (n radix expected) (list 10 2 "1010")
    (expect (cl-cc/vm::vm-bignum-to-string n radix) :to-equal expected)))

(it-sequential "bignum-to-string-radix octal-8"
  (destructuring-bind (n radix expected) (list 8 8 "10")
    (expect (cl-cc/vm::vm-bignum-to-string n radix) :to-equal expected)))

(it-sequential "bignum-to-string-radix zero"
  (destructuring-bind (n radix expected) (list 0 16 "0")
    (expect (cl-cc/vm::vm-bignum-to-string n radix) :to-equal expected)))

(it-sequential "bignum-to-string-radix negative-hex"
  (destructuring-bind (n radix expected) (list -255 16 "-FF")
    (expect (cl-cc/vm::vm-bignum-to-string n radix) :to-equal expected)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: vm-bignum-burnikel-ziegler-divide rounding modes
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "bignum-bz-divide-rounding-modes truncate-pos"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 7 2 :truncate 3 1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes truncate-neg"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list -7 2 :truncate -3 -1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes floor-pos"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 7 2 :floor 3 1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes floor-neg"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list -7 2 :floor -4 1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes ceiling-pos"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 7 2 :ceiling 4 -1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes ceiling-neg"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list -7 2 :ceiling -3 -1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes round-tie-up"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 5 2 :round 2 1)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

(it-sequential "bignum-bz-divide-rounding-modes round-exact"
  (destructuring-bind (dividend divisor rounding expected-q expected-r) (list 6 2 :round 3 0)
    (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (expect (= expected-q q) :to-be-truthy)
    (expect (= expected-r r) :to-be-truthy))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: vm-rational-sub, vm-rational-mul, vm-rational-div
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "rational-sub half-minus-third"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 2 1 3 1 6)
    (let ((result (cl-cc/vm::vm-rational-sub (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-sub third-minus-half"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 3 1 2 -1 6)
    (let ((result (cl-cc/vm::vm-rational-sub (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-sub same-value"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 3 4 3 4 0 1)
    (let ((result (cl-cc/vm::vm-rational-sub (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-mul half-times-third"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 2 1 3 1 6)
    (let ((result (cl-cc/vm::vm-rational-mul (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-mul two-thirds-times-3"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 2 3 3 1 2 1)
    (let ((result (cl-cc/vm::vm-rational-mul (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-mul neg-product"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list -1 2 1 3 -1 6)
    (let ((result (cl-cc/vm::vm-rational-mul (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-div half-div-third"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 2 1 3 3 2)
    (let ((result (cl-cc/vm::vm-rational-div (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-div third-div-neg"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 1 3 -1 2 -2 3)
    (let ((result (cl-cc/vm::vm-rational-div (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-div integer-result"
  (destructuring-bind (an ad bn bd expected-n expected-d) (list 2 3 2 9 3 1)
    (let ((result (cl-cc/vm::vm-rational-div (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (expect (= expected-n (cl-cc/vm::vm-ratio-numerator result)) :to-be-truthy)
    (expect (= expected-d (cl-cc/vm::vm-ratio-denominator result)) :to-be-truthy))))

(it-sequential "rational-div-by-zero-signals-error"
  (signals error (cl-cc/vm::vm-rational-div (cl-cc/vm::vm-make-ratio 1 2)
                                (cl-cc/vm::vm-make-ratio 0 1))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: vm-complex-add, vm-complex-sub, vm-complex-mul, vm-complex-div
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "complex-add pos-plus-pos"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 1 2 3 4 4 6)
    (let ((result (cl-cc/vm::vm-complex-add (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-add cancel-imag"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 2 5 1 -5 3 0)
    (let ((result (cl-cc/vm::vm-complex-add (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-add zero-plus-z"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 0 0 3 4 3 4)
    (let ((result (cl-cc/vm::vm-complex-add (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-sub simple-sub"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 5 7 2 3 3 4)
    (let ((result (cl-cc/vm::vm-complex-sub (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-sub neg-result"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 1 1 3 4 -2 -3)
    (let ((result (cl-cc/vm::vm-complex-sub (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-sub sub-self"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 3 4 3 4 0 0)
    (let ((result (cl-cc/vm::vm-complex-sub (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-mul i-squared"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 0 1 0 1 -1 0)
    (let ((result (cl-cc/vm::vm-complex-mul (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-mul real-times-z"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 3 0 2 5 6 15)
    (let ((result (cl-cc/vm::vm-complex-mul (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-mul standard"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 1 2 3 4 -5 10)
    (let ((result (cl-cc/vm::vm-complex-mul (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-div div-by-real"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 6 4 2 0 3 2)
    (let ((result (cl-cc/vm::vm-complex-div (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-div div-by-i"
  (destructuring-bind (ar ai br bi expected-r expected-i) (list 0 4 0 2 2 0)
    (let ((result (cl-cc/vm::vm-complex-div (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (expect (= expected-r (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= expected-i (cl-cc/vm::vm-imagpart result)) :to-be-truthy))))

(it-sequential "complex-div-by-zero-signals-error"
  (signals error (cl-cc/vm::vm-complex-div (cl-cc/vm::vm-complex-make 1 2)
                               (cl-cc/vm::vm-complex-make 0 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 9: vm-complex-conjugate, vm-complex-abs, vm-complex-phase,
;;;            vm-complex-log
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "complex-conjugate-negates-imaginary"
  (let ((result (cl-cc/vm::vm-complex-conjugate (cl-cc/vm::vm-complex-make 3 4))))
    (expect (= 3 (cl-cc/vm::vm-realpart result)) :to-be-truthy)
    (expect (= -4 (cl-cc/vm::vm-imagpart result)) :to-be-truthy)))

(it-sequential "complex-abs-3-4-is-5"
  (expect (= 5.0 (cl-cc/vm::vm-complex-abs (cl-cc/vm::vm-complex-make 3 4))) :to-be-truthy))

(it-sequential "complex-phase-pure-imaginary-is-half-pi"
  (let ((phase (cl-cc/vm::vm-complex-phase (cl-cc/vm::vm-complex-make 0 1))))
    (expect (< (abs (- phase (/ pi 2))) 1.0d-12) :to-be-truthy)))

(it-sequential "complex-log-of-e-to-the-i-pi"
  (let* ((z (cl-cc/vm::vm-complex-exp (cl-cc/vm::vm-complex-make 0 pi)))
         (log-z (cl-cc/vm::vm-complex-log z)))
    (expect (< (abs (cl-cc/vm::vm-realpart log-z)) 1.0d-12) :to-be-truthy)
    (expect (< (abs (- (cl-cc/vm::vm-imagpart log-z) pi)) 1.0d-12) :to-be-truthy)))
