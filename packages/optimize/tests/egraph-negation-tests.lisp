;;;; tests/unit/optimize/egraph-negation-tests.lisp — E-Graph Negation Rule Tests
;;;
;;; Continuation of egraph-rules-tests.lisp covering negation rules:
;;;   - Negation: mul-neg1
;;;   - Negation: double-neg / not-not
;;;   - Negation: add-neg / sub-neg
;;;
;;; Helpers (make-eg-const, eg-merged-p, eg-all-nodes, eg-class-contains-op-p,
;;; eg-any-class-data-eql-p, eg-any-class-has-op-p, eg-saturate,
;;; eg-rule-registered-p) are defined in egraph-rules-tests.lisp which loads
;;; first; both files share the :cl-cc/test package.

(in-package :cl-cc/test)

;;; ─── Negation: mul-neg1 ──────────────────────────────────────────────────
;;; mul-neg1-r: (mul ?x (const -1)) → (neg ?x)
;;; RHS (neg ?x) is an actual neg-node, not a const.  Pre-add (neg x) so
;;; the rule can merge mul-id with the pre-existing neg class.

;;; ─── Negation: double-neg / not-not ─────────────────────────────────────

;;; ─── Negation: add-neg / sub-neg ─────────────────────────────────────────
;;; add-neg: (add ?x (neg ?y)) → (sub ?x ?y)
;;; sub-neg: (sub ?x (neg ?y)) → (add ?x ?y)
;;; Use var for x and a const for y so x ≠ y (different memo keys).

