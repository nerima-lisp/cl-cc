;;;; tests/unit/optimize/ssa-tests.lisp — SSA Construction/Destruction Tests
;;;
;;; Tests for Phase 1: ssa-construct, ssa-round-trip, ssa-place-phis,
;;; and SSA destruction (phi elimination → parallel copies).

(in-package :cl-cc/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun count-type (insts type-sym)
  "Count instructions of TYPE-SYM in INSTS."
  (count-if (lambda (i) (typep i (find-symbol (symbol-name type-sym) :cl-cc)))
            insts))

(defun make-ssa-test-block (id)
  (cl-cc/optimize:make-basic-block :id id))

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

;;; ─── SSA Rename State ────────────────────────────────────────────────────

;;; ─── SSA Round-Trip ──────────────────────────────────────────────────────

;;; ─── Phi Placement ───────────────────────────────────────────────────────

;;; ─── Targeted Trivial Phi Elimination Tests ─────────────────────────────

;;; ─── Parallel Copy Sequentialization ─────────────────────────────────────

;;; ─── %ssa-resolve-reg ────────────────────────────────────────────────────

;;; ─── %ssa-rewrite-tree ───────────────────────────────────────────────────

;;; ─── ssa-rewrite-dst ─────────────────────────────────────────────────────

;;; ─── %ssa-collect-uses ───────────────────────────────────────────────────

