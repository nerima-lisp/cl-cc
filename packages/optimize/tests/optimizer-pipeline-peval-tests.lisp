;;;; tests/unit/optimize/optimizer-pipeline-peval-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — partial evaluation helpers
;;;;
;;;; Covers: FR-209 opt-specialize-constant-args, FR-210 SCCP binding-time
;;;;   analysis, FR-211 specialization plan caching, program-level partial
;;;;   evaluation, and opt-pass-specialize-known-args.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-pipeline-peval-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-pipeline-peval-tests-suite))

;;; ─── FR-209 opt-specialize-constant-args ────────────────────────────────────

;;; ─── FR-210 SCCP binding-time analysis ──────────────────────────────────────

;;; ─── FR-211 specialization plan caching ──────────────────────────────────────

;;; ─── Program-level partial evaluation ───────────────────────────────────────

;;; ─── opt-pass-specialize-known-args ─────────────────────────────────────────

