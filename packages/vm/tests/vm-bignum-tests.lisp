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

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-bignum-add
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: vm-bignum-sub and vm-bignum-negate
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: vm-bignum-div
;;; ═══════════════════════════════════════════════════════════════════════════

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

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: vm-rational-sub, vm-rational-mul, vm-rational-div
;;; ═══════════════════════════════════════════════════════════════════════════

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: vm-complex-add, vm-complex-sub, vm-complex-mul, vm-complex-div
;;; ═══════════════════════════════════════════════════════════════════════════

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

(it-sequential "complex-log-of-e-to-the-i-pi"
  (let* ((z (cl-cc/vm::vm-complex-exp (cl-cc/vm::vm-complex-make 0 pi)))
         (log-z (cl-cc/vm::vm-complex-log z)))
    (expect (< (abs (cl-cc/vm::vm-realpart log-z)) 1.0d-12) :to-be-truthy)
    (expect (< (abs (- (cl-cc/vm::vm-imagpart log-z) pi)) 1.0d-12) :to-be-truthy)))
