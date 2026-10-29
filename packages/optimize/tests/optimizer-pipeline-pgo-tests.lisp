;;;; tests/unit/optimize/optimizer-pipeline-pgo-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — PGO, ThinLTO, tiered JIT, deopt/OSR
;;;;
;;;; Covers: PGO counter plans, profile templates, ThinLTO import, tiered JIT
;;;;   compilation thresholds, deoptimization materialization, and OSR triggers.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-pipeline-pgo-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-pipeline-pgo-tests-suite))

;;; ─── PGO: hot chain / loop rotation ─────────────────────────────────────────

;;; ─── ThinLTO module summaries ────────────────────────────────────────────────

;;; ─── Tiered JIT compilation thresholds ──────────────────────────────────────

;;; ─── Deoptimization ──────────────────────────────────────────────────────────

;;; ─── On-Stack Replacement (OSR) ──────────────────────────────────────────────

;;; ─── PGO counter plan and profile template (FR-295) ─────────────────────────

