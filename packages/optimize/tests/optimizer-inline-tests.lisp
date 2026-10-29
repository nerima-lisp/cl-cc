;;;; tests/unit/optimize/optimizer-inline-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline.lisp
;;;;
;;;; Covers: opt-max-reg-index (empty, single, multi-register programs),
;;;;   opt-make-renaming (register discovery + counter assignment),
;;;;   opt-collect-function-defs (linear body detection, jump rejection),
;;;;   opt-build-function-name-map (symbol→label tracking),
;;;;   opt-build-call-graph (direct call edge extraction),
;;;;   opt-call-graph-recursive-labels (SCC detection),
;;;;   opt-known-callee-labels (register tracking through closures/moves).

(in-package :cl-cc/test)

(defmacro assert-inline-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── opt-max-reg-index ───────────────────────────────────────────────────────

;;; ─── opt-make-renaming ───────────────────────────────────────────────────────

;;; ─── opt-collect-function-defs ───────────────────────────────────────────────

;;; ─── opt-build-function-name-map ─────────────────────────────────────────────

;;; ─── opt-build-call-graph ────────────────────────────────────────────────────

;;; ─── opt-call-graph-recursive-labels ────────────────────────────────────────

;;; ─── opt-pass-inline ─────────────────────────────────────────────────────────

;;; ─── opt-known-callee-labels ─────────────────────────────────────────────────

;;; ─── opt-can-safely-rename-p ─────────────────────────────────────────────────

;;; ─── opt-rename-regs-in-inst ─────────────────────────────────────────────────

;;; ─── %opt-collect-sexp-regs-into-cell ───────────────────────────────────────

;;; ─── opt-can-safely-rename-p ─────────────────────────────────────────────────

;;; ─── opt-body-has-global-refs-p ──────────────────────────────────────────────

