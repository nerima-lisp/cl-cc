;;;; tests/unit/optimize/optimizer-strength-ext-tests.lisp
;;;; Unit tests for src/optimize/optimizer-strength-ext.lisp
;;;;
;;;; Covers: opt-reassociate-commutative-p, opt-copy-commutative-binop,
;;;;   opt-pass-reassociate (constant-drift, label-env-clear, passthrough),
;;;;   opt-pass-batch-concatenate (chain packing, non-chain passthrough,
;;;;   empty input).

(in-package :cl-cc/test)

;;; ─── opt-reassociate-commutative-p ──────────────────────────────────────

;;; ─── opt-copy-commutative-binop ──────────────────────────────────────────

;;; ─── opt-pass-reassociate ────────────────────────────────────────────────

;;; ─── opt-pass-batch-concatenate ──────────────────────────────────────────

;;; ─── *opt-commutative-binop-table* / opt-reassociate-commutative-p ───────

