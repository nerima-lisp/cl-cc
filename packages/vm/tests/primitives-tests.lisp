;;;; tests/unit/vm/primitives-tests.lisp — VM Primitive Instruction Tests
;;;
;;; Tests for execute-instruction on type predicates, comparisons,
;;; arithmetic extensions, boolean ops, and vm-typep.
;;;
;;; Relies on make-test-vm / exec1 helpers from list-tests.lisp.

(in-package :cl-cc/test)

;;; Tests that call with-replaced-function mutate the global symbol-function
;;; binding and are therefore not safe to run concurrently with other workers.

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Helpers: construct and execute unary/binary VM instructions
;;; ═══════════════════════════════════════════════════════════════════════════

(defun %with-unary-vm-state (value thunk)
  "Run THUNK with a fresh VM state whose unary source register is preloaded."
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 value)
    (funcall thunk state)))

(defun %with-binary-vm-state (lhs rhs thunk)
  "Run THUNK with a fresh VM state whose binary operand registers are preloaded.
This centralizes the repeated :R1/:R2 setup used by binary instruction tests."
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 lhs)
    (cl-cc:vm-reg-set state 2 rhs)
    (funcall thunk state)))

(defun %run-unary-inst-with (instruction-thunk src-val)
  "Run INSTRUCTION-THUNK against SRC-VAL and return the destination register.
INSTRUCTION-THUNK receives the source register index and must build the VM instruction."
  (%with-unary-vm-state
   src-val
   (lambda (state)
     (exec1 (funcall instruction-thunk 1) state)
     (cl-cc:vm-reg-get state 0))))

(defun %run-unary-inst (ctor-fn src-val)
  "Run a unary VM instruction constructor against SRC-VAL and return DST."
  (%run-unary-inst-with
   (lambda (src)
     (funcall ctor-fn :dst 0 :src src))
   src-val))

(defun %run-binary-inst (ctor-fn lhs rhs)
  "Run a binary VM instruction constructor against LHS/RHS and return DST."
  (%with-binary-vm-state
   lhs rhs
   (lambda (state)
     (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) state)
     (cl-cc:vm-reg-get state 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: Type Predicates (pred1 pattern — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-type-pred cons-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-cons-p '(a . b) 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred cons-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-cons-p 42 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred null-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-null-p nil 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred null-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-null-p 'x 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred symbol-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-symbol-p 'hello 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred symbol-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-symbol-p 42 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred number-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-number-p 42 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred number-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-number-p "hello" 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred integer-p-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-integer-p 42 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred integer-p-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-integer-p 3.14 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred evenp-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-evenp 4 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred evenp-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-evenp 3 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred oddp-true"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-oddp 3 1)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

(it-sequential "prim-type-pred oddp-false"
  (destructuring-bind (ctor src-val expected) (list #'cl-cc:make-vm-oddp 4 0)
    (expect (= expected (%run-unary-inst ctor src-val)) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: EQL comparison (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-eq equal"
  (destructuring-bind (lhs rhs expected) (list 42 42 1)
    (expect (= expected (%run-binary-inst #'cl-cc:make-vm-eq lhs rhs)) :to-be-truthy)))

(it-sequential "prim-eq not-equal"
  (destructuring-bind (lhs rhs expected) (list 42 99 0)
    (expect (= expected (%run-binary-inst #'cl-cc:make-vm-eq lhs rhs)) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: Numeric Comparisons (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-comparison lt-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-lt 3 5 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison lt-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-lt 5 3 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison lt-equal"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-lt 3 3 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison gt-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-gt 5 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison gt-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-gt 3 5 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison le-true-lt"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-le 3 5 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison le-true-eq"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-le 3 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison le-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-le 5 3 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison ge-true-gt"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-ge 5 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison ge-true-eq"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-ge 3 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison ge-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-ge 3 5 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison num-eq-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-num-eq 7 7 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-comparison num-eq-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-num-eq 7 8 0)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: Unary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-unary-arith neg-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-neg 5 -5)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith neg-neg"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-neg -3 3)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith neg-zero"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-neg 0 0)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith abs-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-abs 5 5)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith abs-neg"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-abs -7 7)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith abs-zero"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-abs 0 0)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith inc-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-inc 5 6)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith inc-neg"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-inc -1 0)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith dec-pos"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-dec 5 4)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

(it-sequential "prim-unary-arith dec-zero"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-dec 0 -1)
    (expect (= expected (%run-unary-inst ctor src)) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: Binary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-binary-arith div-10/3"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-div 10 3 3)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith div-neg"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-div -7 2 -4)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith cl-div-rational"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-cl-div 3 4 3/4)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith cl-div-ratio-by-ratio"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-cl-div 1/3 4/5 5/12)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith mod-10/3"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-mod 10 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith mod-neg"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-mod -7 2 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith min-lhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-min 3 5 3)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith min-rhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-min 5 3 3)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith max-lhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-max 5 3 5)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith max-rhs"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-max 3 5 5)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith rem-10/3"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-rem 10 3 1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))

(it-sequential "prim-binary-arith rem-neg"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-rem -7 2 -1)
    (expect (= expected (%run-binary-inst ctor lhs rhs)) :to-be-truthy)))


(it-sequential "prim-add-fixnum51-overflow-uses-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-add-integers
                             (lambda (lhs rhs)
                               (setf called t)
                               (+ lhs rhs)))
      (expect (= (ash 1 50) (%run-binary-inst #'cl-cc:make-vm-add cl-cc/vm::+vm-max-fixnum51+ 1)) :to-be-truthy)
      (expect called :to-be-truthy))))

(it-sequential "prim-add-fixnum51-fast-path-skips-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-add-integers
                             (lambda (lhs rhs)
                               (declare (ignore lhs rhs))
                               (setf called t)
                               -1))
      (expect (= 42 (%run-binary-inst #'cl-cc:make-vm-add 40 2)) :to-be-truthy)
      (expect called :to-be-null))))

(it-sequential "prim-sub-fixnum51-overflow-uses-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-subtract-integers
                             (lambda (lhs rhs)
                               (setf called t)
                               (- lhs rhs)))
      (expect (= (1- cl-cc/vm::+vm-min-fixnum51+) (%run-binary-inst #'cl-cc:make-vm-sub cl-cc/vm::+vm-min-fixnum51+ 1)) :to-be-truthy)
      (expect called :to-be-truthy))))

(it-sequential "prim-mul-fixnum51-overflow-uses-bignum-fallback"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-multiply-integers
                             (lambda (lhs rhs &key base threshold)
                               (declare (ignore base threshold))
                               (setf called t)
                               (* lhs rhs)))
      (expect (= (* cl-cc/vm::+vm-max-fixnum51+ 2) (%run-binary-inst #'cl-cc:make-vm-mul cl-cc/vm::+vm-max-fixnum51+ 2)) :to-be-truthy)
      (expect called :to-be-truthy))))


(it-sequential "prim-trampoline-instruction-produces-forceable-thunk"
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 20)
    (cl-cc:vm-reg-set state 2 22)
    (cl-cc:vm-reg-set state 9 #'+)
    (exec1 (cl-cc:make-vm-trampoline :dst 0 :func 9 :args '(1 2)) state)
    (let ((thunk (cl-cc:vm-reg-get state 0)))
      (expect (cl-cc/vm::vm-trampoline-thunk-p thunk) :to-be-truthy)
      (expect (= 42 (cl-cc/vm::vm-force-trampoline-result thunk)) :to-be-truthy))))

(it-sequential "prim-cl-div-fast-path-selection fixnum"
  (destructuring-bind (lhs rhs expected expected-path) (list 3 4 3/4 :fixnum)
    (multiple-value-bind (result path)
      (cl-cc/vm::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))

(it-sequential "prim-cl-div-fast-path-selection ratio-ratio"
  (destructuring-bind (lhs rhs expected expected-path) (list 1/3 4/5 5/12 :fixnum-rational)
    (multiple-value-bind (result path)
      (cl-cc/vm::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))

(it-sequential "prim-cl-div-fast-path-selection fixnum-ratio"
  (destructuring-bind (lhs rhs expected expected-path) (list 2 3/5 10/3 :fixnum-rational)
    (multiple-value-bind (result path)
      (cl-cc/vm::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))

(it-sequential "prim-cl-div-fast-path-selection ratio-fixnum"
  (destructuring-bind (lhs rhs expected expected-path) (list 2/3 5 2/15 :fixnum-rational)
    (multiple-value-bind (result path)
      (cl-cc/vm::%vm-cl-div-fast-path lhs rhs)
    (expect (= expected result) :to-be-truthy)
    (expect path :to-be expected-path))))

;;; Division by zero errors

(it-sequential "prim-div-by-zero div"
  (destructuring-bind (make-inst) (list #'cl-cc:make-vm-div)
    (%with-binary-vm-state
   10 0
   (lambda (state)
     (signals error (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state))))))

(it-sequential "prim-div-by-zero mod"
  (destructuring-bind (make-inst) (list #'cl-cc:make-vm-mod)
    (%with-binary-vm-state
   10 0
   (lambda (state)
     (signals error (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: Multiple-Value Division (truncate/floor/ceiling/round)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-rounding-behavior truncate"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-truncate 3 '(3  1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc:vm-values-list state) :to-equal expected-vals))))))

(it-sequential "prim-rounding-behavior floor"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-floor-inst 3 '(3  1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc:vm-values-list state) :to-equal expected-vals))))))

(it-sequential "prim-rounding-behavior ceiling"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-ceiling-inst 4 '(4 -1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc:vm-values-list state) :to-equal expected-vals))))))

(it-sequential "prim-rounding-behavior round"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-round-inst 4 '(4 -1))
    (%with-binary-vm-state
   7 2
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
      (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
      (when expected-vals
        (expect (cl-cc:vm-values-list state) :to-equal expected-vals))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; FR-885: Float Rounding (ffloor/fceiling/ftruncate/fround)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-float-rounding-behavior ffloor"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-ffloor 2.0 '(2.0 1))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-behavior fceiling"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-fceiling 3.0 '(3.0 -2))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-behavior ftruncate"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-ftruncate 2.0 '(2.0 1))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-behavior fround"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-fround 2.0 '(2.0 1))
    (%with-binary-vm-state
   7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-negative ffloor-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-ffloor -3.0 '(-3.0  2))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-negative fceiling-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-fceiling -2.0 '(-2.0 -1))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-negative ftruncate-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-ftruncate -2.0 '(-2.0 -1))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))

(it-sequential "prim-float-rounding-negative fround-neg-denom"
  (destructuring-bind (make-inst expected-q expected-vals) (list #'cl-cc:make-vm-fround -2.0 '(-2.0 -1))
    (%with-binary-vm-state
   -7 3
   (lambda (state)
     (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) state)
     (expect (= expected-q (cl-cc:vm-reg-get state 0)) :to-be-truthy)
     (expect (cl-cc:vm-values-list state) :to-equal expected-vals)))))


(it-sequential "prim-truncate-bignum-uses-vm-bignum-division-path"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-burnikel-ziegler-divide
                             (lambda (lhs rhs &key base block-size rounding)
                               (declare (ignore lhs rhs base block-size rounding))
                               (setf called t)
                               (values 9 1)))
      (%with-binary-vm-state
       (+ most-positive-fixnum 10)
       3
       (lambda (state)
         (exec1 (cl-cc:make-vm-truncate :dst 0 :lhs 1 :rhs 2) state)
         (expect called :to-be-truthy)
         (expect (= 9 (cl-cc:vm-reg-get state 0)) :to-be-truthy)
         (expect (cl-cc:vm-values-list state) :to-equal '(9 1)))))))

(it-sequential "prim-div-bignum-uses-vm-bignum-division-path"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-burnikel-ziegler-divide
                             (lambda (lhs rhs &key base block-size rounding)
                               (declare (ignore lhs rhs base block-size rounding))
                               (setf called t)
                               (values 8 2)))
      (%with-binary-vm-state
       (+ most-positive-fixnum 10)
       3
       (lambda (state)
         (exec1 (cl-cc:make-vm-div :dst 0 :lhs 1 :rhs 2) state)
         (expect called :to-be-truthy)
         (expect (= 8 (cl-cc:vm-reg-get state 0)) :to-be-truthy))))))

(it-sequential "prim-div-bignum-matches-floor-for-negative-operands"
  (let* ((lhs (- (+ most-positive-fixnum 10)))
         (rhs 3))
    (%with-binary-vm-state
     lhs rhs
     (lambda (state)
       (exec1 (cl-cc:make-vm-div :dst 0 :lhs 1 :rhs 2) state)
       (expect (= (floor lhs rhs) (cl-cc:vm-reg-get state 0)) :to-be-truthy)))))

(it-sequential "prim-mul-bignum-uses-vm-bignum-multiply-path"
  (let ((called nil))
    (with-replaced-function (cl-cc/vm::vm-bignum-multiply-integers
                             (lambda (lhs rhs &key base threshold)
                               (declare (ignore lhs rhs base threshold))
                               (setf called t)
                               77))
      (%with-binary-vm-state
       (+ most-positive-fixnum 10)
       3
       (lambda (state)
         (exec1 (cl-cc:make-vm-mul :dst 0 :lhs 1 :rhs 2) state)
         (expect called :to-be-truthy)
         (expect (= 77 (cl-cc:vm-reg-get state 0)) :to-be-truthy))))))


;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: Boolean Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-not-nil-returns-true"
  (expect (%run-unary-inst #'cl-cc:make-vm-not nil) :to-equal t))

(it-sequential "prim-not-zero-returns-true"
  (expect (%run-unary-inst #'cl-cc:make-vm-not 0) :to-equal t))

(it-sequential "prim-not-integer-returns-nil"
  (expect (%run-unary-inst #'cl-cc:make-vm-not 42) :to-be-null))

(it-sequential "prim-and-cases both-true"
  (destructuring-bind (lhs rhs expected) (list 1 2 t)
    (expect (%run-binary-inst #'cl-cc:make-vm-and lhs rhs) :to-equal expected)))

(it-sequential "prim-and-cases zero-false"
  (destructuring-bind (lhs rhs expected) (list 0 2 nil)
    (expect (%run-binary-inst #'cl-cc:make-vm-and lhs rhs) :to-equal expected)))

(it-sequential "prim-and-cases one-false"
  (destructuring-bind (lhs rhs expected) (list 1 nil nil)
    (expect (%run-binary-inst #'cl-cc:make-vm-and lhs rhs) :to-equal expected)))

(it-sequential "prim-or-cases both-false"
  (destructuring-bind (lhs rhs expected) (list nil nil nil)
    (expect (%run-binary-inst #'cl-cc:make-vm-or lhs rhs) :to-equal expected)))

(it-sequential "prim-or-cases zero-false"
  (destructuring-bind (lhs rhs expected) (list nil 0 nil)
    (expect (%run-binary-inst #'cl-cc:make-vm-or lhs rhs) :to-equal expected)))

(it-sequential "prim-or-cases one-true"
  (destructuring-bind (lhs rhs expected) (list nil 42 t)
    (expect (%run-binary-inst #'cl-cc:make-vm-or lhs rhs) :to-equal expected)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: Dynamic / TypeRep Runtime API
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "vm-type-rep-equality-and-inference"
  (let ((integer-rep (cl-cc/vm:vm-type-rep 'integer))
        (string-rep (cl-cc/vm:vm-type-rep 'string)))
    (expect (cl-cc/vm:vm-type-rep-equal integer-rep (cl-cc/vm:make-vm-type-rep 'integer)) :to-be-truthy)
    (expect (cl-cc/vm:vm-type-rep-equal integer-rep string-rep) :to-be-falsy)
    (expect (cl-cc/vm:vm-type-rep-specifier (cl-cc/vm:vm-type-rep-of 42)) :to-equal 'fixnum)
    (expect (cl-cc/vm:vm-type-rep-specifier (cl-cc/vm:vm-type-rep-of "hello")) :to-equal 'string)))

(it-sequential "vm-wrap-and-unwrap-dynamic-success"
  (let ((dynamic-value (cl-cc/vm:vm-wrap-dynamic 42 'integer)))
    (expect (cl-cc/vm:vm-dynamic-p dynamic-value) :to-be-truthy)
    (multiple-value-bind (value ok)
        (cl-cc/vm:vm-unwrap-dynamic dynamic-value 'integer)
      (expect ok :to-be-truthy)
      (expect (= 42 value) :to-be-truthy)))
  (let ((dynamic-value (cl-cc/vm:vm-wrap-dynamic 42)))
    (multiple-value-bind (value ok)
        (cl-cc/vm:vm-unwrap-dynamic dynamic-value 'integer)
      (expect ok :to-be-truthy)
      (expect (= 42 value) :to-be-truthy))))

(it-sequential "vm-unwrap-dynamic-mismatch-fails-safely"
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-unwrap-dynamic (cl-cc/vm:vm-wrap-dynamic 42 'integer) 'string)
    (expect value :to-be-null)
    (expect ok :to-be-falsy)))

(it-sequential "vm-cast-with-type-rep-works-for-plain-and-dynamic-values"
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep 42 'integer)
    (expect ok :to-be-truthy)
    (expect (= 42 value) :to-be-truthy))
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep 42 'fixnum)
    (expect ok :to-be-truthy)
    (expect (= 42 value) :to-be-truthy))
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep (cl-cc/vm:vm-wrap-dynamic "hello" 'string) 'string)
    (expect ok :to-be-truthy)
    (expect value :to-equal "hello"))
  (multiple-value-bind (value ok)
      (cl-cc/vm:vm-cast-with-type-rep 42 'string)
    (expect value :to-be-null)
    (expect ok :to-be-falsy)))
