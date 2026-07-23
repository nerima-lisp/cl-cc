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

(it-sequential "egraph-rule-mul-neg1-fires rhs-neg1"
  (destructuring-bind (side) (list :rhs)
    (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (cn1 (make-eg-const eg -1))
         (mul (if (eq side :rhs)
                  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x cn1)
                  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul cn1 x)))
         (neg (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg mul neg) :to-be-truthy))))

(it-sequential "egraph-rule-mul-neg1-fires lhs-neg1"
  (destructuring-bind (side) (list :lhs)
    (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (cn1 (make-eg-const eg -1))
         (mul (if (eq side :rhs)
                  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x cn1)
                  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul cn1 x)))
         (neg (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg mul neg) :to-be-truthy))))

;;; ─── Negation: double-neg / not-not ─────────────────────────────────────

(it-sequential "egraph-rule-double-negation-fires double-neg"
  (destructuring-bind (op) (list 'cl-cc/optimize::neg)
    (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (op1  (cl-cc/optimize:egraph-add eg op x))
         (op2  (cl-cc/optimize:egraph-add eg op op1)))
    (eg-saturate eg)
    (expect (eg-merged-p eg op2 x) :to-be-truthy))))

(it-sequential "egraph-rule-double-negation-fires not-not"
  (destructuring-bind (op) (list 'cl-cc/optimize::not)
    (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (op1  (cl-cc/optimize:egraph-add eg op x))
         (op2  (cl-cc/optimize:egraph-add eg op op1)))
    (eg-saturate eg)
    (expect (eg-merged-p eg op2 x) :to-be-truthy))))

(it-sequential "egraph-rule-negated-comparison-fires not-lt"
  (destructuring-bind (not-op cmp-op dual-op) (list 'cl-cc/optimize::not 'cl-cc/optimize::lt 'cl-cc/optimize::ge)
    (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc/optimize:egraph-add eg cmp-op x y))
         (not (cl-cc/optimize:egraph-add eg not-op cmp))
         (dual (cl-cc/optimize:egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg not dual) :to-be-truthy))))

(it-sequential "egraph-rule-negated-comparison-fires not-gt"
  (destructuring-bind (not-op cmp-op dual-op) (list 'cl-cc/optimize::not 'cl-cc/optimize::gt 'cl-cc/optimize::le)
    (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc/optimize:egraph-add eg cmp-op x y))
         (not (cl-cc/optimize:egraph-add eg not-op cmp))
         (dual (cl-cc/optimize:egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg not dual) :to-be-truthy))))

(it-sequential "egraph-rule-negated-comparison-fires not-le"
  (destructuring-bind (not-op cmp-op dual-op) (list 'cl-cc/optimize::not 'cl-cc/optimize::le 'cl-cc/optimize::gt)
    (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc/optimize:egraph-add eg cmp-op x y))
         (not (cl-cc/optimize:egraph-add eg not-op cmp))
         (dual (cl-cc/optimize:egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg not dual) :to-be-truthy))))

(it-sequential "egraph-rule-negated-comparison-fires not-ge"
  (destructuring-bind (not-op cmp-op dual-op) (list 'cl-cc/optimize::not 'cl-cc/optimize::ge 'cl-cc/optimize::lt)
    (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc/optimize:egraph-add eg cmp-op x y))
         (not (cl-cc/optimize:egraph-add eg not-op cmp))
         (dual (cl-cc/optimize:egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg not dual) :to-be-truthy))))

;;; ─── Negation: add-neg / sub-neg ─────────────────────────────────────────
;;; add-neg: (add ?x (neg ?y)) → (sub ?x ?y)
;;; sub-neg: (sub ?x (neg ?y)) → (add ?x ?y)
;;; Use var for x and a const for y so x ≠ y (different memo keys).

(it-sequential "egraph-rule-neg-rewrites add-neg"
  (destructuring-bind (const-val outer-op inverse-op) (list 7 'cl-cc/optimize::add 'cl-cc/optimize::sub)
    (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg const-val))
         (ny   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg y))
         (expr (cl-cc/optimize:egraph-add eg outer-op x ny))
         (dual (cl-cc/optimize:egraph-add eg inverse-op x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg expr dual) :to-be-truthy))))

(it-sequential "egraph-rule-neg-rewrites sub-neg"
  (destructuring-bind (const-val outer-op inverse-op) (list 5 'cl-cc/optimize::sub 'cl-cc/optimize::add)
    (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg const-val))
         (ny   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg y))
         (expr (cl-cc/optimize:egraph-add eg outer-op x ny))
         (dual (cl-cc/optimize:egraph-add eg inverse-op x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg expr dual) :to-be-truthy))))
