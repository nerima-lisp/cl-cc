;;;; tests/unit/optimize/optimizer-dataflow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-dataflow.lisp
;;;;
;;;; Covers: %sccp-env-copy, %sccp-env-equal-p, %sccp-env-merge,
;;;;   %sccp-fold-inst (const, move, jump-zero, binary, unary),
;;;;   %sccp-redirect-successors, %sccp-process-block,
;;;;   opt-pass-sccp (empty, straight-line const fold).

(in-package :cl-cc/test)

;;; ─── %sccp-env-copy ───────────────────────────────────────────────────────

;;; ─── %sccp-env-equal-p ───────────────────────────────────────────────────

;;; ─── %sccp-env-merge ─────────────────────────────────────────────────────

;;; ─── %sccp-fold-inst ─────────────────────────────────────────────────────

;;; ─── %sccp-redirect-successors ──────────────────────────────────────────

;;; ─── %sccp-process-block ─────────────────────────────────────────────────

;;; ─── opt-pass-sccp ───────────────────────────────────────────────────────

;;; ─── %sccp-update-env-for-inst ───────────────────────────────────────────

;;; ─── %sccp-fold-inst jump-zero cases ─────────────────────────────────────

;;; ─── Generic dataflow worklist ───────────────────────────────────────────

;;; ─── Available expressions ───────────────────────────────────────────────

;;; ─── Reaching definitions ────────────────────────────────────────────────

;;; ─── Abstract interpretation framework (FR-251 partial) ──────────────────

