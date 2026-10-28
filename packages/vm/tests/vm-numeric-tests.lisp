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
(it-sequential "vm-bignum-multiply-plan-records-digits-sign-and-strategy"
  (let ((plan (cl-cc/vm::vm-bignum-multiply-plan (- most-positive-fixnum) 2 :threshold 64)))
    (expect (getf plan :strategy) :to-be :schoolbook)
    (expect (= -1 (getf plan :sign)) :to-be-truthy)
    (expect (vectorp (getf plan :lhs-digits)) :to-be-truthy)
    (expect (vectorp (getf plan :rhs-digits)) :to-be-truthy)))

(it-sequential "vm-complex-add-with-unboxing-uses-split-plan-for-local-values"
  (expect (cl-cc/vm::vm-complex-add-with-unboxing #C(1 2) #C(3 4) :local-p t) :to-equal #C(4 6)))

(it-sequential "vm-complex-add-with-unboxing-falls-back-to-boxed-add-for-escaping-values"
  (expect (cl-cc/vm::vm-complex-add-with-unboxing #C(1 2) #C(3 4) :local-p nil) :to-equal #C(4 6)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-842: Kahan Summation
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "kahan-sum-single"
  (expect (= 3.14d0 (cl-cc/vm::kahan-sum '(3.14d0))) :to-be-truthy))

(it-sequential "kahan-sum-basic"
  (expect (= 6.0d0 (cl-cc/vm::kahan-sum '(1.0d0 2.0d0 3.0d0))) :to-be-truthy))
(it-sequential "kahan-sum-empty"
  (expect (= 0.0d0 (cl-cc/vm::kahan-sum (quote ()))) :to-be-truthy))

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

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-844 / FR-860 / FR-861 / FR-829: Numeric runtime extensions
;;; ═══════════════════════════════════════════════════════════════════════════
