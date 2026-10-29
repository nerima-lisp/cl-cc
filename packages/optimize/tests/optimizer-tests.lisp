;;;; tests/optimizer-tests.lisp — Optimizer Tests
;;;
;;; Tests verifying that the multi-pass optimizer:
;;;   - Correctly folds constants at compile time (semantics preserved)
;;;   - Eliminates dead code (pure instructions with unused results)
;;;   - Removes dead jumps and unreachable code
;;;   - Preserves all observable semantics (evaluated values)

(in-package :cl-cc/test)


;;; ── Helpers ──────────────────────────────────────────────────────────────

(defun opt-count-instr (expr type-sym)
  "Compile EXPR, run optimizer, count instructions of TYPE-SYM in the result."
  (let* ((result   (compile-string expr :target :vm))
         (instrs   (optimize-instructions (%get-instructions result)))
         (cl-type  (find-symbol (symbol-name type-sym) :cl-cc)))
    (count-if (lambda (i) (typep i cl-type)) instrs)))

(defun opt-has-p (expr type-sym)
  "T if compiling EXPR leaves at least one instruction of TYPE-SYM after optimization."
  (> (opt-count-instr expr type-sym) 0))

(defun make-bswap-tree-instructions ()
  "Construct the canonical masked-shift tree for a 32-bit byte swap."
  (list (make-vm-const :dst :r1 :value #xFF)
        (make-vm-logand :dst :r2 :lhs :r0 :rhs :r1)
        (make-vm-const :dst :r3 :value 24)
        (make-vm-ash   :dst :r4 :lhs :r2 :rhs :r3)
        (make-vm-const :dst :r5 :value #xFF00)
        (make-vm-logand :dst :r6 :lhs :r0 :rhs :r5)
        (make-vm-const :dst :r7 :value 8)
        (make-vm-ash   :dst :r8 :lhs :r6 :rhs :r7)
        (make-vm-const :dst :r9 :value #xFF0000)
        (make-vm-logand :dst :r10 :lhs :r0 :rhs :r9)
        (make-vm-const :dst :r11 :value -8)
        (make-vm-ash   :dst :r12 :lhs :r10 :rhs :r11)
        (make-vm-const :dst :r13 :value #xFF000000)
        (make-vm-logand :dst :r14 :lhs :r0 :rhs :r13)
        (make-vm-const :dst :r15 :value -24)
        (make-vm-ash   :dst :r16 :lhs :r14 :rhs :r15)
        (make-vm-logior :dst :r17 :lhs :r4 :rhs :r8)
        (make-vm-logior :dst :r18 :lhs :r12 :rhs :r16)
        (make-vm-logior :dst :r19 :lhs :r17 :rhs :r18)))

;;; ── Constant Folding ─────────────────────────────────────────────────────
;;; Each case verifies: (1) correct runtime value, (2) binary-op eliminated.

(it-sequential "optimizer-fold add"
  (destructuring-bind (expected expr instr) (list 7 "(+ 3 4)" 'vm-add)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold sub"
  (destructuring-bind (expected expr instr) (list 6 "(- 10 4)" 'vm-sub)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold mul"
  (destructuring-bind (expected expr instr) (list 12 "(* 3 4)" 'vm-mul)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold chained"
  (destructuring-bind (expected expr instr) (list 12 "(+ (* 2 3) (- 10 4))" 'vm-add)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold deeper-chain"
  (destructuring-bind (expected expr instr) (list 16 "(- (* 5 5) (* 3 3))" 'vm-sub)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

;;; ── Algebraic Identity Simplification ───────────────────────────────────
;;; Each case verifies: (1) correct runtime value, (2) instruction eliminated.

(it-sequential "optimizer-algebraic-identity add-zero"
  (destructuring-bind (expected expr instr) (list 5 "(let ((x 5))  (+ x 0))" 'vm-add)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-identity mul-one"
  (destructuring-bind (expected expr instr) (list 7 "(let ((x 7))  (* x 1))" 'vm-mul)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-identity mul-zero"
  (destructuring-bind (expected expr instr) (list 0 "(let ((x 99)) (* x 0))" 'vm-mul)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-identity sub-zero"
  (destructuring-bind (expected expr instr) (list 42 "(let ((x 42)) (- x 0))" 'vm-sub)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

;;; ── Dead Code Elimination: Semantics ─────────────────────────────────────

(it-sequential "optimizer-dce progn-dead-exprs"
  (destructuring-bind (expected expr) (list 99 "(progn (+ 1 2) (+ 3 4) 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-dce let-nested"
  (destructuring-bind (expected expr) (list 3 "(let ((x 1)) (let ((y 2)) (+ x y)))")
    (assert-run= expected expr)))

(it-sequential "optimizer-dce mixed-dead-subform"
  (destructuring-bind (expected expr) (list 42 "(let ((x 42)) (+ 0 x))")
    (assert-run= expected expr)))

;;; ── Constant Branch Elimination: Semantics ───────────────────────────────

(it-sequential "optimizer-branch-fold const-true"
  (destructuring-bind (expected expr) (list 42 "(if 1 42 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-branch-fold zero-true"
  (destructuring-bind (expected expr) (list 42 "(if 0 42 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-branch-fold t-true"
  (destructuring-bind (expected expr) (list 42 "(if t 42 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-branch-nil-false"
  (assert-run-false "(if nil 42 nil)"))

;;; ── Conditional Truthiness Preservation ──────────────────────────────────

(it-sequential "optimizer-branch-lowering preserves-truthiness const-true" (assert-run= 42 "(if 1 42 99)"))

(it-sequential "optimizer-branch-lowering preserves-truthiness zero-true" (assert-run= 42 "(if 0 42 99)"))

;;; ── Dominated Predicate Elimination ─────────────────────────────────────

;;; ── Data-Table Coverage Tests ──────────────────────────────────────────────

;;; End-to-end, bitwise, unary folding, inlining, and prolog peephole tests are in
;;; optimizer-e2e-tests.lisp. Low-level pass tests are in optimizer-tests-lowlevel2.lisp.
