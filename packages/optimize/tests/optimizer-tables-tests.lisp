;;;; tests/unit/optimize/optimizer-tables-tests.lisp
;;;; Coverage tests for src/optimize/optimizer-tables.lisp
;;;;
;;;; Covers the data tables:
;;;;   *opt-binary-fold-table*, *opt-binary-cmp-fold-table*,
;;;;   *opt-unary-fold-table*, *opt-type-pred-fold-table*,
;;;;   *opt-binary-zero-guard-types*, *opt-binary-no-fold-types*,
;;;;   *opt-commutative-inst-types*, *opt-binary-lhs-rhs-types*,
;;;;   *opt-unary-src-types*, *opt-read-regs-table*
;;;; and the dispatchers:
;;;;   opt-inst-dst, opt-inst-read-regs.
;;;;
;;;; Note: opt-falsep, opt-register-keyword-p, opt-binary-lhs-rhs-p,
;;;; opt-unary-src-p, opt-foldable-unary-arith-p, opt-foldable-type-pred-p
;;;; are already covered in optimizer-tests.lisp.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-integration-suite))

;;; ─── *opt-binary-fold-table* ─────────────────────────────────────────────

;;; ─── *opt-binary-cmp-fold-table* ─────────────────────────────────────────

;;; ─── *opt-unary-fold-table* ──────────────────────────────────────────────

;;; ─── *opt-type-pred-fold-table* ──────────────────────────────────────────

;;; ─── Table coverage and derived type lists ─────────────────────────────────

;;; ─── opt-inst-dst ─────────────────────────────────────────────────────────

;;; ─── opt-inst-read-regs ───────────────────────────────────────────────────

;;; ─── %opt-branch-target-labels ───────────────────────────────────────────

;;; ─── %fold-vm-jump-zero ──────────────────────────────────────────────────

;;; ─── %opt-known-constant-p ───────────────────────────────────────────────

;;; ─── %opt-apply-algebraic-action ─────────────────────────────────────────

