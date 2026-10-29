;;;; tests/unit/optimize/optimizer-concurrency-tests.lisp
;;;; Unit tests for concurrency/async helpers in the optimizer:
;;;; async state machine, coroutine strategy, channel select, STM, lock-free reclamation.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-concurrency-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-concurrency-tests-suite))

;;; ─── Async state machine ─────────────────────────────────────────────────────

;;; ─── Coroutine strategy ──────────────────────────────────────────────────────

;;; ─── Channel select ──────────────────────────────────────────────────────────

;;; ─── STM plan ────────────────────────────────────────────────────────────────

;;; ─── Lock-free reclamation ───────────────────────────────────────────────────

progn
