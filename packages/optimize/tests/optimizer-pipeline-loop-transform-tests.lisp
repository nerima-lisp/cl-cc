;;;; tests/unit/optimize/optimizer-pipeline-loop-transform-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — polyhedral and loop transform passes
;;;;
;;;; Covers: FR-523 affine loop analysis, FR-524 loop interchange,
;;;;   FR-525 polyhedral scheduling, FR-526 loop fusion/fission,
;;;;   FR-527 ML inline scoring, FR-528 learned codegen costs.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-pipeline-loop-transform-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-pipeline-loop-transform-tests-suite))

;;; ─── FR-523 Affine loop analysis ────────────────────────────────────────────

;;; ─── FR-524 Loop interchange ─────────────────────────────────────────────────

;;; ─── FR-525 Polyhedral scheduling ───────────────────────────────────────────

;;; ─── FR-526 Loop fusion / fission ───────────────────────────────────────────

;;; ─── FR-527 ML inline scoring ────────────────────────────────────────────────

;;; ─── FR-528 Learned codegen cost ─────────────────────────────────────────────

