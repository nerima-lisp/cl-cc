;;;; tests/unit/optimize/optimizer-loop-transforms-tests.lisp
;;;; Unit tests for loop optimization passes: rotation, peeling, unrolling.
;;;;
;;;; Covers: opt-pass-loop-rotation, opt-pass-loop-peel, opt-pass-loop-unrolling.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-loop-transforms-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-loop-transforms-tests-suite))

;;; ─── Helper: build instruction sequences ─────────────────────────────────

(defun %loop-transforms-make-while-loop-insts ()
  "Build a simple while-loop instruction sequence used by loop optimization tests.
Pattern: loop on :r0 while (integer-p :r0), adding :r2 each iteration."
  (list (make-vm-label :name "Lh")
        (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
        (make-vm-jump-zero :reg :r1 :label "Lexit")
        (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
        (make-vm-jump :label "Lh")
        (make-vm-label :name "Lexit")
        (make-vm-ret :reg :r0)))

(defun %loop-transforms-make-counted-loop-insts (&key (lim 3))
  "Build a counted-loop instruction sequence for loop unrolling tests.
Pattern: count :i from 0 to LIM by 1, accumulating in :sum."
  (list (make-vm-const :dst :i   :value 0)
        (make-vm-const :dst :lim :value lim)
        (make-vm-const :dst :one :value 1)
        (make-vm-label :name "Lh")
        (make-vm-lt :dst :c :lhs :i :rhs :lim)
        (make-vm-jump-zero :reg :c :label "Lexit")
        (make-vm-add :dst :sum :lhs :sum :rhs :i)
        (make-vm-add :dst :i   :lhs :i   :rhs :one)
        (make-vm-jump :label "Lh")
        (make-vm-label :name "Lexit")
        (make-vm-ret :reg :sum)))

;;; ─── Loop rotation ────────────────────────────────────────────────────────

;;; ─── Loop peeling ─────────────────────────────────────────────────────────

;;; ─── Loop unrolling ───────────────────────────────────────────────────────

