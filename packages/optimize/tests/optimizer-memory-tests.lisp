;;;; tests/unit/optimize/optimizer-memory-tests.lisp
;;;; Unit tests for optimizer-memory-alias.lisp — heap alias and interval analysis
;;;;
;;;; Covers: opt-heap-root-inst-p, opt-heap-root-kind,
;;;;   opt-compute-heap-aliases, opt-compute-points-to,
;;;;   opt-interval-* arithmetic, opt-compute-value-ranges,
;;;;   cfg-value-ranges, simple-induction-variable detection.

(in-package :cl-cc/test)

;;; ─── opt-heap-root-inst-p ───────────────────────────────────────────────

;;; ─── opt-heap-root-kind ─────────────────────────────────────────────────

;;; ─── %opt-build-root-map / opt-compute-heap-aliases ────────────────────

;;; ─── Memory SSA snapshot (FR-217 partial) ───────────────────────────────

;;; ─── opt-interval-* arithmetic ───────────────────────────────────────────

;;; ─── opt-compute-value-ranges ──────────────────────────────────────────

(defun %test-path-sensitive-branch-cfg ()
  (cl-cc/optimize:cfg-build
   (list (make-vm-get-global :dst :raw :name 'raw)
         (make-vm-const :dst :mask :value #xFF)
         (make-vm-logand :dst :idx :lhs :raw :rhs :mask)
         (make-vm-const :dst :len :value 10)
         (make-vm-lt :dst :cmp :lhs :idx :rhs :len)
         (make-vm-jump-zero :reg :cmp :label "then")
         (make-vm-jump :label "join")
         (make-vm-label :name "then")
         (make-vm-jump :label "join")
         (make-vm-label :name "join")
         (make-vm-ret :reg :idx))))

(defun %test-path-sensitive-counted-loop-cfg ()
  (cl-cc/optimize:cfg-build
   (list (make-vm-const :dst :i :value 0)
         (make-vm-const :dst :one :value 1)
         (make-vm-const :dst :n :value 4)
         (make-vm-label :name "loop")
         (make-vm-lt :dst :cmp :lhs :i :rhs :n)
         (make-vm-jump-zero :reg :cmp :label "exit")
         (make-vm-add :dst :j :lhs :i :rhs :one)
         (make-vm-move :dst :i :src :j)
         (make-vm-jump :label "loop")
         (make-vm-label :name "exit")
         (make-vm-ret :reg :i))))

