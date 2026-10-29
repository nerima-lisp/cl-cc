;;;; tests/unit/optimize/optimizer-flow-block-tests.lisp
;;;; Unit tests for optimizer-flow-core.lisp — block/CFG helpers and correlation passes
;;;;
;;;; Covers: %block-mergeable-successor-p, %block-strip-merge-jump,
;;;;   cfg-block-temperature, opt-pass-branch-correlation,
;;;;   opt-pass-tail-merge (tail-duplication).

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-integration-suite))

;;; ─── Straight-line preservation / empty-input ───────────────────────────

;;; ─── %type-check-elim helpers ───────────────────────────────────────────────

;;; ─── %block-mergeable-successor-p / %block-strip-merge-jump / cfg-block-temperature ──

