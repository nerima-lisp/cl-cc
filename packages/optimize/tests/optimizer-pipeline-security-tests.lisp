;;;; tests/unit/optimize/optimizer-pipeline-security-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — security hardening passes
;;;;
;;;; Covers: CFI, retpoline, stack canary, shadow stack, HTM.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-pipeline-security-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-pipeline-security-tests-suite))

;;; ─── Control Flow Integrity (CFI) ───────────────────────────────────────────

;;; ─── Retpoline ───────────────────────────────────────────────────────────────

;;; ─── Stack canary ────────────────────────────────────────────────────────────

;;; ─── Shadow stack (CET) ──────────────────────────────────────────────────────

;;; ─── Hardware Transactional Memory (HTM) ─────────────────────────────────────

