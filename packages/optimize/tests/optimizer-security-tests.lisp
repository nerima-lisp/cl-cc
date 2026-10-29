;;;; tests/unit/optimize/optimizer-security-tests.lisp
;;;; Unit tests for security hardening helpers in the optimizer:
;;;; CFI (control-flow integrity), retpoline, stack canary, shadow stack.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-security-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-security-tests-suite))

;;; ─── CFI ─────────────────────────────────────────────────────────────────────

;;; ─── Retpoline ───────────────────────────────────────────────────────────────

;;; ─── Stack canary ────────────────────────────────────────────────────────────

;;; ─── Shadow stack ────────────────────────────────────────────────────────────

