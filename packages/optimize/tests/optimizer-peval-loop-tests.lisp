;;;; tests/unit/optimize/optimizer-peval-loop-tests.lisp
;;;; Unit tests for partial evaluation and loop-level optimization helpers.
;;;;
;;;; Covers: FR-209 constant specialization, FR-210 SCCP binding-times,
;;;;   FR-211 specialization plan cache, FR-295 PGO counter plans,
;;;;   program-level partial evaluation, offline BTA, specialize-known-args pass,
;;;;   FR-523 affine loop analysis, FR-524 loop interchange, FR-525 polyhedral
;;;;   scheduling, FR-526 loop fusion/fission, FR-527 MLGO scoring,
;;;;   FR-528 learned codegen cost.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-peval-loop-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-peval-loop-tests-suite))

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

;;; ─── FR-523/524/525/526/527/528 Affine/Polyhedral/Loop/ML passes ──────────

