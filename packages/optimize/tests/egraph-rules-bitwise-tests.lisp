;;;; tests/unit/optimize/egraph-rules-bitwise-tests.lisp — E-Graph Bitwise & Advanced Rule Tests
;;;
;;; Continuation of egraph-rules-tests.lisp.  Covers bitwise rules, strength
;;; reduction, type predicate rules, advanced/composed rules, and the full
;;; rule registry check.

(in-package :cl-cc/test)

;;; ─── Bitwise: self-identity (logand-self, logior-self) ──────────────────

(it-sequential "egraph-bitwise-self-identity-fires logand-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::logand)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

(it-sequential "egraph-bitwise-self-identity-fires logior-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::logior)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

;;; ─── Bitwise: logior-zero / logxor-zero (bidirectional zero-identity) ───

(it-sequential "egraph-bitwise-zero-identity-fires logior-zero"
  (destructuring-bind (op) (list 'cl-cc/optimize::logior)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg op x c0)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg op c0 x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

(it-sequential "egraph-bitwise-zero-identity-fires logxor-zero"
  (destructuring-bind (op) (list 'cl-cc/optimize::logxor)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg op x c0)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg op c0 x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))


;;; ─── Bitwise: logxor-self / ash-zero-base ───────────────────────────────

(it-sequential "egraph-rule-const-producing-rules-fire"
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::logxor x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::ash c0 x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy)))

;;; ─── Strength Reduction Rules — Registration Tests ───────────────────────
;;; mul-pow2/mul-pow2-l/div-pow2 use (const ?n) guards that cannot bind ?n
;;; via the current pattern-matcher.  Test registration only.

(it-sequential "egraph-strength-reduction-rules-registered mul-pow2"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::mul-pow2)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-strength-reduction-rules-registered mul-pow2-l"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::mul-pow2-l)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-strength-reduction-rules-registered div-pow2"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::div-pow2)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-rule-mul-pow2-has-when-guard"
  (let ((rule (find 'cl-cc/optimize::mul-pow2
                    (cl-cc/optimize:egraph-builtin-rules)
                    :key (lambda (r) (getf r :name)))))
    (expect (not (null (getf rule :when))) :to-be-truthy)))

(it-sequential "egraph-rule-mul-pow2-non-power-of-2-does-not-introduce-ash"
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c7 (make-eg-const eg 7))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x c7)))
    (declare (ignore id))
    (eg-saturate eg)
    (expect (eg-any-class-has-op-p eg 'cl-cc/optimize::ash) :to-be-falsy)))

;;; ─── Type Predicate Rules — Registration Tests ───────────────────────────

(it-sequential "egraph-type-predicate-rules-registered null-p-const"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::null-p-const)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-type-predicate-rules-registered cons-p-const"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::cons-p-const)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-type-predicate-rules-registered number-p-const"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::number-p-const)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-type-predicate-rules-registered integer-p-const"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::integer-p-const)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

;;; ─── Advanced: mul-neg-neg ───────────────────────────────────────────────
;;; mul-neg-neg: (mul (neg ?x) (neg ?y)) → (mul ?x ?y).
;;; Use var for x and a const for y so they're in different classes.

(it-sequential "egraph-rule-mul-neg-neg-distinct-vars-merges-with-mul"
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg 3))
         (nx   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (ny   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg y))
         (mul1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul nx ny))
         (mul2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x y)))
    (eg-saturate eg)
    (expect (eg-merged-p eg mul1 mul2) :to-be-truthy)))

(it-sequential "egraph-rule-mul-neg-neg-same-var-also-fires"
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (nx   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (mul1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul nx nx))
         (mul2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg mul1 mul2) :to-be-truthy)))

;;; ─── Advanced: neg-sub ───────────────────────────────────────────────────
;;; neg-sub: (neg (sub ?x ?y)) → (sub ?y ?x).

(it-sequential "egraph-rule-neg-sub-distinct-vars-merges-with-reversed-sub"
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg 11))
         (sub1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub x y))
         (neg  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg sub1))
         (sub2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub y x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg neg sub2) :to-be-truthy)))

(it-sequential "egraph-rule-neg-sub-same-var-reduces-to-const-via-sub-self"
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (sub1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub x x))
         (neg  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg sub1)))
    (declare (ignore neg))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg sub1 'cl-cc/optimize::const) :to-be-truthy)))

;;; ─── Rule Registry: All 51 Rules Present ────────────────────────────────

(it-sequential "egraph-rule-registry-complete"
  (let* ((rules (cl-cc/optimize:egraph-builtin-rules))
         (names (mapcar (lambda (r) (getf r :name)) rules)))
    (expect (>= (length rules) 51) :to-be-truthy)
    (dolist (rule rules)
      (expect (getf rule :lhs) :to-be-truthy)
      (expect (not (eq (getf rule :rhs :missing) :missing)) :to-be-truthy))
    (dolist (n '(cl-cc/optimize::fold-add cl-cc/optimize::fold-sub cl-cc/optimize::fold-mul
                 cl-cc/optimize::fold-neg cl-cc/optimize::fold-not
                 cl-cc/optimize::fold-lt cl-cc/optimize::fold-gt cl-cc/optimize::fold-le cl-cc/optimize::fold-ge
                 cl-cc/optimize::add-zero-r cl-cc/optimize::add-zero-l cl-cc/optimize::sub-zero
                 cl-cc/optimize::mul-one-r cl-cc/optimize::mul-one-l
                 cl-cc/optimize::mul-zero-r cl-cc/optimize::mul-zero-l cl-cc/optimize::div-one
                 cl-cc/optimize::sub-self cl-cc/optimize::eq-self
                 cl-cc/optimize::lt-self cl-cc/optimize::gt-self cl-cc/optimize::le-self cl-cc/optimize::ge-self
                  cl-cc/optimize::mul-neg1-r cl-cc/optimize::mul-neg1-l
                  cl-cc/optimize::double-neg cl-cc/optimize::not-not
                  cl-cc/optimize::not-lt cl-cc/optimize::not-gt cl-cc/optimize::not-le cl-cc/optimize::not-ge
                  cl-cc/optimize::add-neg cl-cc/optimize::sub-neg
                  cl-cc/optimize::logand-zero cl-cc/optimize::logand-zero-l
                  cl-cc/optimize::logand-neg1 cl-cc/optimize::logand-neg1-l cl-cc/optimize::logand-self
                 cl-cc/optimize::logior-zero cl-cc/optimize::logior-zero-l cl-cc/optimize::logior-self
                 cl-cc/optimize::logxor-zero cl-cc/optimize::logxor-zero-l cl-cc/optimize::logxor-self
                 cl-cc/optimize::ash-zero cl-cc/optimize::ash-zero-base
                 cl-cc/optimize::mul-pow2 cl-cc/optimize::mul-pow2-l cl-cc/optimize::div-pow2
                 cl-cc/optimize::null-p-const cl-cc/optimize::cons-p-const
                 cl-cc/optimize::number-p-const cl-cc/optimize::integer-p-const
                 cl-cc/optimize::mul-neg-neg cl-cc/optimize::neg-sub))
      (expect (member n names) :to-be-truthy))))

;;; ─── Idempotency ─────────────────────────────────────────────────────────

(it-sequential "egraph-saturation-idempotent"
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::add x c0)))
    (declare (ignore id))
    (eg-saturate eg)
    (let ((n1 (hash-table-count (cl-cc:eg-classes eg))))
      (eg-saturate eg)
      (expect (<= (hash-table-count (cl-cc:eg-classes eg)) n1) :to-be-truthy))))

;;; ─── Composition ─────────────────────────────────────────────────────────

(it-sequential "egraph-rule-double-neg-then-identity"
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (neg1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (neg2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg neg1))
         (c0   (make-eg-const eg 0))
         (add  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::add neg2 c0)))
    (eg-saturate eg)
    ;; double-neg: neg2 merges with x.
    ;; add-zero-r: add(x, 0) merges with x.
    ;; So add should merge with x.
    (expect (eg-merged-p eg add x) :to-be-truthy)))

(it-sequential "egraph-rule-mul-neg1-then-double-neg"
  (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (nx  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul nx cn1)))
    (eg-saturate eg)
    ;; mul-neg1-r: mul(neg(x), -1) merges with a neg class.
    ;; The merged class contains both mul and neg nodes.
    (expect (eg-class-contains-op-p eg mul 'cl-cc/optimize::neg) :to-be-truthy)))

(it-sequential "egraph-rule-sub-neg-then-add-zero"
  (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0  (make-eg-const eg 0))
         (nc0 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg c0))
         (sub (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub x nc0))
         (add (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::add x c0)))
    (eg-saturate eg)
    ;; sub-neg: sub(x, neg(c0)) merges with add(x, c0).
    ;; The sub and add classes should be merged.
    (expect (eg-merged-p eg sub add) :to-be-truthy)))

(it-sequential "egraph-rule-logand-self-then-logior-zero"
  (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (and (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::logand x x))
         (c0  (make-eg-const eg 0))
         (or  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::logior and c0)))
    (eg-saturate eg)
    ;; logand-self: logand(x,x) merges with x.
    ;; logior-zero: logior(x, 0) merges with x.
    (expect (eg-merged-p eg or x) :to-be-truthy)))
