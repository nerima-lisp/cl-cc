;;;; tests/unit/optimize/egraph-rules-bitwise-tests.lisp — E-Graph Bitwise & Advanced Rule Tests
;;;
;;; Continuation of egraph-rules-tests.lisp.  Covers bitwise rules, strength
;;; reduction, type predicate rules, advanced/composed rules, and the full
;;; rule registry check.

(in-package :cl-cc/test)

;;; ─── Bitwise: self-identity (logand-self, logior-self) ──────────────────

;;; ─── Bitwise: logior-zero / logxor-zero (bidirectional zero-identity) ───

;;; ─── Bitwise: logxor-self / ash-zero-base ───────────────────────────────

;;; ─── Strength Reduction Rules — Registration Tests ───────────────────────
;;; mul-pow2/mul-pow2-l/div-pow2 use (const ?n) guards that cannot bind ?n
;;; via the current pattern-matcher.  Test registration only.

;;; ─── Type Predicate Rules — Registration Tests ───────────────────────────

;;; ─── Advanced: mul-neg-neg ───────────────────────────────────────────────
;;; mul-neg-neg: (mul (neg ?x) (neg ?y)) → (mul ?x ?y).
;;; Use var for x and a const for y so they're in different classes.

;;; ─── Advanced: neg-sub ───────────────────────────────────────────────────
;;; neg-sub: (neg (sub ?x ?y)) → (sub ?y ?x).

;;; ─── Rule Registry: All 51 Rules Present ────────────────────────────────

;;; ─── Idempotency ─────────────────────────────────────────────────────────

;;; ─── Composition ─────────────────────────────────────────────────────────

