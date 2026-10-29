;;;; tests/unit/optimize/egraph-extraction-tests.lisp — E-Graph Extraction/Saturation Tests
;;;
;;; Tests for saturation, extraction, cost model, enode internals,
;;; optimize-with-egraph, and the iteration-budget thresholds data table.
;;;
;;; Helper make-test-egraph and egraph-find-dst-inst are defined in
;;; egraph-tests.lisp (loaded first).

(in-package :cl-cc/test)

(defmacro assert-egraph-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── Saturation ──────────────────────────────────────────────────────────

;;; ─── Extraction ──────────────────────────────────────────────────────────

;;; ─── Optimize-With-Egraph ────────────────────────────────────────────────

;;; ─── Cost Model ──────────────────────────────────────────────────────────

;;; ─── enode-memo-key ──────────────────────────────────────────────────────

;;; ─── egraph-canonical-enode ──────────────────────────────────────────────

;;; ─── egraph-build-rhs ────────────────────────────────────────────────────

;;; ─── egraph-class-has-op-p / egraph-class-const-value ───────────────────

;;; ─── vm-inst-to-enode-op ─────────────────────────────────────────────────

;;; ─── egraph-add-instructions ─────────────────────────────────────────────

;;; ─── Compound pattern matching ───────────────────────────────────────────

;;; ─── *opt-iteration-budget-thresholds* data coverage ────────────────────

;;; ─── %egraph-rewrite-inst ────────────────────────────────────────────────

