;;;; tests/unit/vm/vm-numeric-tests.lisp — VM numeric tower instruction tests

(in-package :cl-cc/test)



(defun %vm-num-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-num-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(it-sequential "vm-type-of nil"
  (destructuring-bind (src expected) (list nil 'null)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of fixnum"
  (destructuring-bind (src expected) (list 42 'fixnum)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of bignum"
  (destructuring-bind (src expected) (list (+ most-positive-fixnum 1) 'bignum)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of ratio"
  (destructuring-bind (src expected) (list 1/2 'ratio)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of float"
  (destructuring-bind (src expected) (list 1.0f0 'single-float)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of str"
  (destructuring-bind (src expected) (list "hello" 'string)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of char"
  (destructuring-bind (src expected) (list #\a 'character)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of sym"
  (destructuring-bind (src expected) (list 'foo 'symbol)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of cons"
  (destructuring-bind (src expected) (list '(a) 'cons)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of pathname"
  (destructuring-bind (src expected) (list #p"/tmp" 'pathname)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-type-of random-state"
  (destructuring-bind (src expected) (list (cl-cc:make-random-state t) 'cl-cc/vm:random-state)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-float-convert-and-scale convert"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-float-inst 42 42.0)
    (if (eq ctor #'cl-cc:make-vm-scale-float)
      (expect (= expected (%vm-num-binary ctor src 3)) :to-be-truthy)
      (expect (= expected (%vm-num-unary ctor src)) :to-be-truthy))))

(it-sequential "vm-float-convert-and-scale scale"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-scale-float 1.0 8.0)
    (if (eq ctor #'cl-cc:make-vm-scale-float)
      (expect (= expected (%vm-num-binary ctor src 3)) :to-be-truthy)
      (expect (= expected (%vm-num-unary ctor src)) :to-be-truthy))))

(it-sequential "vm-float-introspection float-precision"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-float-precision 1.0 (float-precision 1.0))
    (expect (%vm-num-unary ctor src) :to-equal expected)))

(it-sequential "vm-float-introspection float-radix"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-float-radix 1.0 (float-radix 1.0))
    (expect (%vm-num-unary ctor src) :to-equal expected)))

(it-sequential "vm-float-introspection float-sign"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-float-sign -2.5 (float-sign -2.5))
    (expect (%vm-num-unary ctor src) :to-equal expected)))

(it-sequential "vm-float-introspection float-digits"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-float-digits 1.0 (float-digits 1.0))
    (expect (%vm-num-unary ctor src) :to-equal expected)))

(it-sequential "vm-float-decode-values decode-float"
  (destructuring-bind (ctor) (list #'cl-cc:make-vm-decode-float)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (= 3 (length (cl-cc:vm-values-list s))) :to-be-truthy))))

(it-sequential "vm-float-decode-values integer-decode-float"
  (destructuring-bind (ctor) (list #'cl-cc:make-vm-integer-decode-float)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (= 3 (length (cl-cc:vm-values-list s))) :to-be-truthy))))

(it-sequential "vm-bignum-digit-vector-splits-little-endian-digits"
  (let ((base cl-cc/vm::+vm-bignum-digit-base+))
    (multiple-value-bind (digits sign)
        (cl-cc/vm::vm-bignum-digit-vector (+ base 42))
      (expect (= 1 sign) :to-be-truthy)
      (expect (= 2 (length digits)) :to-be-truthy)
      (expect (= 42 (aref digits 0)) :to-be-truthy)
      (expect (= 1 (aref digits 1)) :to-be-truthy))))

(it-sequential "vm-bignum-schoolbook-multiply-digits-computes-product-digits"
  (let ((digits (cl-cc/vm::vm-bignum-schoolbook-multiply-digits
                 #(3 2 1) #(6 5 4) 10)))
    (expect (coerce digits 'list) :to-equal '(8 8 0 6 5))))

(it-sequential "vm-bignum-karatsuba-multiply-digits-matches-schoolbook"
  (let* ((lhs #(9 8 7 6 5 4 3 2))
         (rhs #(1 2 3 4 5 6 7 8))
         (schoolbook (cl-cc/vm::vm-bignum-schoolbook-multiply-digits lhs rhs 10))
         (karatsuba  (cl-cc/vm::vm-bignum-karatsuba-multiply-digits lhs rhs 10 2)))
    (expect (coerce karatsuba 'list) :to-equal (coerce schoolbook 'list))))

(it-sequential "vm-bignum-multiply-digits-selects-karatsuba-by-threshold"
  (let* ((lhs #(9 8 7 6 5 4 3 2))
         (rhs #(1 2 3 4 5 6 7 8))
         (result (cl-cc/vm::vm-bignum-multiply-digits
                  lhs rhs :base 10 :threshold 4))
         (expected (cl-cc/vm::vm-bignum-karatsuba-multiply-digits lhs rhs 10 2)))
    (expect (coerce result 'list) :to-equal (coerce expected 'list))))

(it-sequential "vm-bignum-integer-from-digits-reconstructs-value"
  (expect (= 123456789 (cl-cc/vm::vm-bignum-integer-from-digits #(789 456 123) 1 1000)) :to-be-truthy)
  (expect (= -123456789 (cl-cc/vm::vm-bignum-integer-from-digits #(789 456 123) -1 1000)) :to-be-truthy))

(it-sequential "vm-bignum-multiply-integers-matches-host positive-large"
  (destructuring-bind (lhs rhs) (list (expt cl-cc/vm::+vm-bignum-digit-base+ 6) (+ (expt cl-cc/vm::+vm-bignum-digit-base+ 4) 12345))
    (expect (cl-cc/vm::vm-bignum-multiply-integers lhs rhs :threshold 4) :to-be (* lhs rhs))))

(it-sequential "vm-bignum-multiply-integers-matches-host mixed-sign-large"
  (destructuring-bind (lhs rhs) (list (- (expt cl-cc/vm::+vm-bignum-digit-base+ 5)) (+ (expt cl-cc/vm::+vm-bignum-digit-base+ 3) 6789))
    (expect (cl-cc/vm::vm-bignum-multiply-integers lhs rhs :threshold 4) :to-be (* lhs rhs))))

(it-sequential "vm-bignum-multiplication-strategy-selects-thresholded-plan fixnum"
  (destructuring-bind (lhs rhs threshold expected) (list 21 2 4 :fixnum)
    (expect (cl-cc/vm::vm-bignum-multiplication-strategy
              lhs rhs :threshold threshold) :to-be expected)))

(it-sequential "vm-bignum-multiplication-strategy-selects-thresholded-plan schoolbook"
  (destructuring-bind (lhs rhs threshold expected) (list most-positive-fixnum 2 64 :schoolbook)
    (expect (cl-cc/vm::vm-bignum-multiplication-strategy
              lhs rhs :threshold threshold) :to-be expected)))

(it-sequential "vm-bignum-multiplication-strategy-selects-thresholded-plan karatsuba"
  (destructuring-bind (lhs rhs threshold expected) (list (expt cl-cc/vm::+vm-bignum-digit-base+ 5) (expt cl-cc/vm::+vm-bignum-digit-base+ 5) 4 :karatsuba)
    (expect (cl-cc/vm::vm-bignum-multiplication-strategy
              lhs rhs :threshold threshold) :to-be expected)))

(it-sequential "vm-bignum-multiply-plan-records-digits-sign-and-strategy"
  (let ((plan (cl-cc/vm::vm-bignum-multiply-plan (- most-positive-fixnum) 2 :threshold 64)))
    (expect (getf plan :strategy) :to-be :schoolbook)
    (expect (= -1 (getf plan :sign)) :to-be-truthy)
    (expect (vectorp (getf plan :lhs-digits)) :to-be-truthy)
    (expect (vectorp (getf plan :rhs-digits)) :to-be-truthy)))

(it-sequential "vm-bignum-burnikel-ziegler-divide-plan-records-chunk-metadata"
  (let ((plan (cl-cc/vm::vm-bignum-burnikel-ziegler-divide-plan
               (+ (expt cl-cc/vm::+vm-bignum-digit-base+ 6) 123)
               (+ (expt cl-cc/vm::+vm-bignum-digit-base+ 3) 7)
               :block-size 8)))
    (expect (getf plan :algorithm) :to-be :burnikel-ziegler)
    (expect (= 8 (getf plan :chunk-size)) :to-be-truthy)
    (expect (plusp (getf plan :chunk-count)) :to-be-truthy)
    (expect (vectorp (getf plan :lhs-digits)) :to-be-truthy)
    (expect (vectorp (getf plan :rhs-digits)) :to-be-truthy)))

(it-sequential "vm-bignum-burnikel-ziegler-divide-matches-truncate"
  (let* ((lhs (+ (expt cl-cc/vm::+vm-bignum-digit-base+ 7) 99))
         (rhs (+ (expt cl-cc/vm::+vm-bignum-digit-base+ 3) 5)))
    (multiple-value-bind (q r)
        (cl-cc/vm::vm-bignum-burnikel-ziegler-divide lhs rhs)
      (multiple-value-bind (eq er) (truncate lhs rhs)
        (expect (= eq q) :to-be-truthy)
        (expect (= er r) :to-be-truthy)))))

(it-sequential "vm-float-rounding ffloor"
  (destructuring-bind (ctor) (list #'cl-cc:make-vm-ffloor)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (expect (numberp (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 2 (length (cl-cc:vm-values-list s))) :to-be-truthy))))

(it-sequential "vm-float-rounding fceiling"
  (destructuring-bind (ctor) (list #'cl-cc:make-vm-fceiling)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (expect (numberp (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 2 (length (cl-cc:vm-values-list s))) :to-be-truthy))))

(it-sequential "vm-float-rounding ftruncate"
  (destructuring-bind (ctor) (list #'cl-cc:make-vm-ftruncate)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (expect (numberp (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 2 (length (cl-cc:vm-values-list s))) :to-be-truthy))))

(it-sequential "vm-float-rounding fround"
  (destructuring-bind (ctor) (list #'cl-cc:make-vm-fround)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (expect (numberp (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 2 (length (cl-cc:vm-values-list s))) :to-be-truthy))))

(it-sequential "vm-rational"
  (expect (%vm-num-unary #'cl-cc:make-vm-rational 0.5) :to-equal 1/2))

(it-sequential "vm-rational-parts numerator"
  (destructuring-bind (make-inst expected) (list #'cl-cc:make-vm-numerator 3)
    (expect (= expected (%vm-num-unary make-inst 3/4)) :to-be-truthy)))

(it-sequential "vm-rational-parts denominator"
  (destructuring-bind (make-inst expected) (list #'cl-cc:make-vm-denominator 4)
    (expect (= expected (%vm-num-unary make-inst 3/4)) :to-be-truthy)))

(it-sequential "vm-gcd-lcm gcd"
  (destructuring-bind (make-inst a b expected) (list #'cl-cc:make-vm-gcd 12 8 4)
    (expect (= expected (%vm-num-binary make-inst a b)) :to-be-truthy)))

(it-sequential "vm-gcd-lcm lcm"
  (destructuring-bind (make-inst a b expected) (list #'cl-cc:make-vm-lcm 4 6 12)
    (expect (= expected (%vm-num-binary make-inst a b)) :to-be-truthy)))

(it-sequential "vm-complex-construct"
  (expect (%vm-num-binary #'cl-cc:make-vm-complex 3 4) :to-equal #C(3 4)))

(it-sequential "vm-complex-parts realpart"
  (destructuring-bind (make-inst expected) (list #'cl-cc:make-vm-realpart 3)
    (expect (= expected (%vm-num-unary make-inst #C(3 4))) :to-be-truthy)))

(it-sequential "vm-complex-parts imagpart"
  (destructuring-bind (make-inst expected) (list #'cl-cc:make-vm-imagpart 4)
    (expect (= expected (%vm-num-unary make-inst #C(3 4))) :to-be-truthy)))

(it-sequential "vm-conjugate"
  (expect (%vm-num-unary #'cl-cc:make-vm-conjugate #C(3 4)) :to-equal #C(3 -4)))

(it-sequential "vm-complex-unbox-plan-splits-local-complex"
  (let ((plan (cl-cc/vm::vm-complex-unbox-plan #C(3 4) :local-p t)))
    (expect (getf plan :representation) :to-be :split-registers)
    (expect (= 3 (getf plan :real)) :to-be-truthy)
    (expect (= 4 (getf plan :imag)) :to-be-truthy)))

(it-sequential "vm-complex-unbox-plan-keeps-escaping-complex-boxed"
  (let ((plan (cl-cc/vm::vm-complex-unbox-plan #C(3 4) :local-p nil)))
    (expect (getf plan :representation) :to-be :boxed)
    (expect (getf plan :value) :to-equal #C(3 4))))

(it-sequential "vm-complex-unboxed-add-plan-adds-components"
  (let ((plan (cl-cc/vm::vm-complex-unboxed-add-plan #C(1 2) #C(3 4) :local-p t)))
    (expect (getf plan :representation) :to-be :split-registers)
    (expect (= 4 (getf plan :real)) :to-be-truthy)
    (expect (= 6 (getf plan :imag)) :to-be-truthy)))

(it-sequential "vm-complex-add-with-unboxing-uses-split-plan-for-local-values"
  (expect (cl-cc/vm::vm-complex-add-with-unboxing #C(1 2) #C(3 4) :local-p t) :to-equal #C(4 6)))

(it-sequential "vm-complex-add-with-unboxing-falls-back-to-boxed-add-for-escaping-values"
  (expect (cl-cc/vm::vm-complex-add-with-unboxing #C(1 2) #C(3 4) :local-p nil) :to-equal #C(4 6)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-842: Kahan Summation
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "kahan-accumulator-basics"
  (let ((acc (cl-cc/vm::make-kahan-accumulator)))
    (expect (cl-cc/vm::kahan-accumulator-p acc) :to-be-truthy)
    (expect (= 0.0d0 (cl-cc/vm::kahan-result acc)) :to-be-truthy)))

(it-sequential "kahan-accumulator-seeded"
  (let ((acc (cl-cc/vm::make-kahan-accumulator 3.5d0)))
    (expect (= 3.5d0 (cl-cc/vm::kahan-result acc)) :to-be-truthy)))

(it-sequential "kahan-sum-empty"
  (expect (= 0.0d0 (cl-cc/vm::kahan-sum '())) :to-be-truthy))

(it-sequential "kahan-sum-single"
  (expect (= 3.14d0 (cl-cc/vm::kahan-sum '(3.14d0))) :to-be-truthy))

(it-sequential "kahan-sum-basic"
  (expect (= 6.0d0 (cl-cc/vm::kahan-sum '(1.0d0 2.0d0 3.0d0))) :to-be-truthy))

(it-sequential "kahan-sum-vector"
  (expect (= 6.0d0 (cl-cc/vm::kahan-sum #(1.0d0 2.0d0 3.0d0))) :to-be-truthy))

(it-sequential "kahan-sum-integers"
  (expect (= 6.0d0 (cl-cc/vm::kahan-sum '(1 2 3))) :to-be-truthy))

(it-sequential "kahan-sum-precision-advantage"
  (let* ((large 1.0d0)
         (small 1.0d-12)
         (c -1.0d0)
         (naive (+ (+ large small) c))
         (kahan (cl-cc/vm::kahan-sum (list large small c))))
    (expect (>= (abs kahan) (abs naive)) :to-be-truthy)))

(it-sequential "kahan-add-accumulates"
  (let ((acc (cl-cc/vm::make-kahan-accumulator 10.0d0)))
    (cl-cc/vm::kahan-add! acc 20.0d0)
    (cl-cc/vm::kahan-add! acc 30.0d0)
    (expect (= 60.0d0 (cl-cc/vm::kahan-result acc)) :to-be-truthy)))

(it-sequential "pairwise-sum-empty"
  (expect (= 0.0d0 (cl-cc/vm::pairwise-sum '())) :to-be-truthy))

(it-sequential "pairwise-sum-basic"
  (expect (= 10.0d0 (cl-cc/vm::pairwise-sum '(1.0d0 2.0d0 3.0d0 4.0d0))) :to-be-truthy))

(it-sequential "pairwise-sum-vector"
  (expect (= 10.0d0 (cl-cc/vm::pairwise-sum #(1.0d0 2.0d0 3.0d0 4.0d0))) :to-be-truthy))

(it-sequential "pairwise-sum-integers"
  (expect (= 10.0d0 (cl-cc/vm::pairwise-sum '(1 2 3 4))) :to-be-truthy))

(it-sequential "pairwise-sum-single"
  (expect (= 3.14d0 (cl-cc/vm::pairwise-sum '(3.14d0))) :to-be-truthy))

(it-sequential "pairwise-sum-matches-kahan"
  (let* ((rng (make-random-state t))
         (data (loop repeat 200 collect (random 1.0d0 rng)))
         (kahan (cl-cc/vm::kahan-sum data))
         (pair (cl-cc/vm::pairwise-sum data)))
    (expect (< (abs (- kahan pair)) 1.0d-10) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-843: Float Exception Control
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "get-float-traps-returns-list"
  (let ((traps (cl-cc/vm::get-float-traps)))
    (expect (listp traps) :to-be-truthy)
    (dolist (trap traps)
      (expect (keywordp trap) :to-be-truthy))))

(it-sequential "with-float-traps-masked-suppresses-divide-by-zero"
  (let ((result :unset))
    (cl-cc/vm::with-float-traps-masked (:divide-by-zero)
      (setf result (/ 1.0d0 0.0d0)))
    (expect (sb-ext:float-infinity-p result) :to-be-truthy)))

(it-sequential "with-float-traps-masked-multiple"
  (let ((result :unset))
    (cl-cc/vm::with-float-traps-masked (:divide-by-zero :overflow :underflow :inexact)
      (setf result (/ 1.0d0 0.0d0)))
    (expect (sb-ext:float-infinity-p result) :to-be-truthy)))

(it-sequential "floating-point-modes-variable"
  (expect (listp cl-cc/vm::*floating-point-modes*) :to-be-truthy)
  (expect (getf cl-cc/vm::*floating-point-modes* :rounding-mode) :to-be-truthy))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-844 / FR-860 / FR-861 / FR-829: Numeric runtime extensions
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "double-double-add-preserves-low-word"
  (let ((sum (cl-cc/vm:dd+ (cl-cc/vm:make-double-double 10000000000000000.0d0)
                           1.0d0)))
    (expect (cl-cc/vm:double-double-p sum) :to-be-truthy)
    (expect (cl-cc/vm:dd-to-string sum 0) :to-equal "10000000000000001")))

(it-sequential "double-double-multiply-normalizes-result"
  (let ((product (cl-cc/vm:dd* (cl-cc/vm:make-double-double 1.5d0) 2.0d0)))
    (expect (cl-cc/vm:double-double-p product) :to-be-truthy)
    (expect (cl-cc/vm:dd-to-string product 3) :to-equal "3.000")))

(it-sequential "with-precision-binds-dynamic-precision"
  (let ((outer cl-cc/vm:*numeric-precision*))
    (expect (= 113 (cl-cc/vm:with-precision 113 cl-cc/vm:*numeric-precision*)) :to-be-truthy)
    (expect (= outer cl-cc/vm:*numeric-precision*) :to-be-truthy)))

(it-sequential "numeric-contagion-table-follows-ansi-hierarchy"
  (expect (cl-cc/vm:infer-numeric-result-type 'integer 'rational) :to-be 'rational)
  (expect (cl-cc/vm:infer-numeric-result-type 'single-float 'double-float) :to-be 'double-float)
  (expect (cl-cc/vm:infer-numeric-result-type 'double-float 'complex) :to-be 'complex))

(it-sequential "inline-arithmetic-dispatch-selects-specialized-entry"
  (let ((entry (cl-cc/vm:arithmetic-dispatch-entry '+ 1 2.0d0)))
    (expect entry :to-be-truthy)
    (expect (car entry) :to-equal '(fixnum float))
    (expect (= 3.0d0 (cl-cc/vm:inline-arithmetic-dispatch '+ 1 2.0d0)) :to-be-truthy)))

(it-sequential "vm-arith-dispatch-instruction-executes-through-table"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 6)
    (cl-cc:vm-reg-set s 2 7)
    (exec1 (cl-cc/vm:make-vm-arith-dispatch :dst 0 :lhs 1 :rhs 2 :op '*) s)
    (expect (= 42 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

(it-sequential "fixnum-overflow-detection-promotes-past-host-fixnum"
  (let ((result (cl-cc/vm:vm-fixnum-add-with-overflow-detection
                 most-positive-fixnum 1)))
    (expect (integerp result) :to-be-truthy)
    (expect (typep result 'bignum) :to-be-truthy)
    (expect (= (+ most-positive-fixnum 1) result) :to-be-truthy)))

(it-sequential "fixnum-overflow-detection-can-skip-explicit-check"
  (expect (= (+ most-positive-fixnum 1) (cl-cc/vm:vm-fixnum-add-with-overflow-detection
             most-positive-fixnum 1 :safety 0)) :to-be-truthy))
