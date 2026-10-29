;;;; tests/unit/optimize/egraph-tests.lisp — E-Graph Equality Saturation Tests
;;;
;;; Tests for Phase 2: e-graph data structures, union-find, saturation,
;;; extraction, and rewrite rules.

(in-package :cl-cc/test)




;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun make-test-egraph ()
  "Create a fresh empty e-graph."
  (cl-cc/optimize:make-e-graph))

(defun egraph-find-dst-inst (instructions dst)
  "Return the first instruction in INSTRUCTIONS whose :dst register equals DST."
  (find dst instructions :key #'cl-cc/vm:vm-dst))

;;; ─── Union-Find ──────────────────────────────────────────────────────────

;;; ─── Merge ───────────────────────────────────────────────────────────────

;;; ─── E-Graph Statistics ──────────────────────────────────────────────────

;;; ─── Pattern Matching ────────────────────────────────────────────────────

;;; ─── Rule Application ────────────────────────────────────────────────────

