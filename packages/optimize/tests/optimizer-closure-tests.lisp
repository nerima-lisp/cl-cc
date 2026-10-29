;;;; tests/optimizer-closure-tests.lisp — Closure optimization pass tests
;;; FR-330 closure capture dedup and FR-079 closure thunk sharing.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-integration-suite))


;;; ── FR-330 closure-capture-dedup ──────────────────────────────────────────

;;; ── FR-079 closure-thunk-sharing ──────────────────────────────────────────

