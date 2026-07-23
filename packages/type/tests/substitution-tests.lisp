;;;; tests/unit/type/substitution-tests.lisp — Substitution & Zonking Tests
;;;
;;; Tests for substitution data structure, zonk on various type constructors,
;;; composition, occurs check, generalize/instantiate, and normalization.

(in-package :cl-cc/test)

;;; ─── Substitution Structure ─────────────────────────────────────────────

(it-sequential "subst-lookup-empty-cases empty-subst"
  (destructuring-bind (subst) (list (make-substitution))
    (let ((v (cl-cc/type:fresh-type-var 'a)))
    (multiple-value-bind (val found-p) (subst-lookup v subst)
      (expect val :to-be-null)
      (expect found-p :to-be-falsy)))))

(it-sequential "subst-lookup-empty-cases nil-subst"
  (destructuring-bind (subst) (list nil)
    (let ((v (cl-cc/type:fresh-type-var 'a)))
    (multiple-value-bind (val found-p) (subst-lookup v subst)
      (expect val :to-be-null)
      (expect found-p :to-be-falsy)))))

(it-sequential "subst-make-substitution-starts-at-generation-0"
  (let ((s (make-substitution)))
    (expect (= 0 (substitution-generation s)) :to-be-truthy)))

(it-sequential "subst-extend-leaves-original-unchanged"
  (let* ((v  (cl-cc/type:fresh-type-var 'a))
         (s1 (make-substitution))
         (s2 (subst-extend v cl-cc/type:type-int s1)))
    (multiple-value-bind (val found) (subst-lookup v s1)
      (declare (ignore val))
      (expect found :to-be-falsy))
    (multiple-value-bind (val found) (subst-lookup v s2)
      (expect found :to-be-truthy)
      (expect val :to-be cl-cc/type:type-int))))

(it-sequential "subst-extend-to-nil-creates-generation-1"
  (let* ((v (cl-cc/type:fresh-type-var 'a))
         (s (subst-extend v cl-cc/type:type-int nil)))
    (multiple-value-bind (val found) (subst-lookup v s)
      (expect found :to-be-truthy)
      (expect val :to-be cl-cc/type:type-int))
    (expect (= 1 (substitution-generation s)) :to-be-truthy)))

(it-sequential "subst-extend!-mutates-and-increments-generation"
  (let* ((v1 (cl-cc/type:fresh-type-var 'a))
         (v2 (cl-cc/type:fresh-type-var 'b))
         (s  (make-substitution)))
    (subst-extend! v1 cl-cc/type:type-int s)
    (multiple-value-bind (val found) (subst-lookup v1 s)
      (expect found :to-be-truthy)
      (expect val :to-be cl-cc/type:type-int))
    (expect (= 1 (substitution-generation s)) :to-be-truthy)
    (subst-extend! v2 cl-cc/type:type-string s)
    (expect (= 2 (substitution-generation s)) :to-be-truthy)))

;;; ─── Composition ────────────────────────────────────────────────────────

(it-sequential "subst-compose-nil-nil-returns-valid-substitution"
  (let ((s (subst-compose nil nil)))
    (expect (cl-cc/type:substitution-p s) :to-be-truthy)))

(it-sequential "subst-compose-nil-left-returns-s2"
  (let* ((v  (cl-cc/type:fresh-type-var 'a))
         (s2 (subst-extend v cl-cc/type:type-int nil)))
    (expect (subst-compose nil s2) :to-be s2)))

(it-sequential "subst-compose-nil-right-preserves-bindings"
  (let* ((v  (cl-cc/type:fresh-type-var 'a))
         (s1 (subst-extend v cl-cc/type:type-int nil)))
    (multiple-value-bind (val found) (subst-lookup v (subst-compose s1 nil))
      (expect found :to-be-truthy)
      (expect val :to-be cl-cc/type:type-int))))

(it-sequential "subst-compose-resolves-chains-through-s2-range"
  (let* ((a      (cl-cc/type:fresh-type-var 'a))
         (b      (cl-cc/type:fresh-type-var 'b))
         (s2     (subst-extend a b nil))
         (s1     (subst-extend b cl-cc/type:type-int nil))
         (result (subst-compose s1 s2)))
    (multiple-value-bind (val found) (subst-lookup a result)
      (expect found :to-be-truthy)
      (expect (cl-cc/type:type-primitive-p val) :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name val) :to-be 'fixnum))))

;;; ─── Zonk: Various Type Constructors ────────────────────────────────────

(it-sequential "zonk-nil-primitive-and-unbound-are-unchanged"
  (let ((s (make-substitution)))
    (expect (zonk nil s) :to-be-null)
    (expect (zonk cl-cc/type:type-int s) :to-be cl-cc/type:type-int)
    (let ((v (cl-cc/type:fresh-type-var 'a)))
      (expect (zonk v s) :to-be v))))

(it-sequential "zonk-bound-var-resolves-to-bound-type"
  (let* ((v (cl-cc/type:fresh-type-var 'a))
         (s (subst-extend v cl-cc/type:type-int nil)))
    (expect (zonk v s) :to-be cl-cc/type:type-int)))

(it-sequential "zonk-chain-resolves-to-terminal"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (s (make-substitution)))
    (subst-extend! a b s)
    (subst-extend! b cl-cc/type:type-int s)
    (let ((result (zonk a s)))
      (expect (cl-cc/type:type-primitive-p result) :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name result) :to-be 'fixnum))))

(it-sequential "zonk-arrow-type-substitutes-params-and-return"
  (let* ((a     (cl-cc/type:fresh-type-var 'a))
         (b     (cl-cc/type:fresh-type-var 'b))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return b))
         (s     (make-substitution)))
    (subst-extend! a cl-cc/type:type-int s)
    (subst-extend! b cl-cc/type:type-string s)
    (let ((result (zonk fn-ty s)))
      (expect (cl-cc/type:type-arrow-p result) :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name (car (cl-cc/type:type-arrow-params result))) :to-be 'fixnum)
      (expect (cl-cc/type:type-primitive-name (cl-cc/type:type-arrow-return result)) :to-be 'string))))

(it-sequential "zonk-product-type-substitutes-elements"
  (let* ((a    (cl-cc/type:fresh-type-var 'a))
         (prod (cl-cc/type:make-type-product :elems (list a cl-cc/type:type-string)))
         (s    (subst-extend a cl-cc/type:type-int nil))
         (result (zonk prod s)))
    (expect (cl-cc/type:type-product-p result) :to-be-truthy)
    (expect (= 2 (length (cl-cc/type:type-product-elems result))) :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name
                         (first (cl-cc/type:type-product-elems result))) :to-be 'fixnum)))

(it-sequential "zonk-forall-type-substitutes-body"
  (let* ((a        (cl-cc/type:fresh-type-var 'a))
         (b        (cl-cc/type:fresh-type-var 'b))
         (forall-ty (cl-cc/type:make-type-forall :var a :body b))
         (s        (subst-extend b cl-cc/type:type-int nil))
         (result   (zonk forall-ty s)))
    (expect (cl-cc/type:type-forall-p result) :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name
                         (cl-cc/type:type-forall-body result)) :to-be 'fixnum)))

(it-sequential "zonk-type-app-substitutes-fun-and-arg"
  (let* ((a      (cl-cc/type:fresh-type-var 'a))
         (b      (cl-cc/type:fresh-type-var 'b))
         (app-ty (cl-cc/type:make-type-app :fun a :arg b))
         (s      (make-substitution)))
    (subst-extend! a cl-cc/type:type-int s)
    (subst-extend! b cl-cc/type:type-string s)
    (let ((result (zonk app-ty s)))
      (expect (cl-cc/type:type-app-p result) :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name (cl-cc/type:type-app-fun result)) :to-be 'fixnum)
      (expect (cl-cc/type:type-primitive-name (cl-cc/type:type-app-arg result)) :to-be 'string))))

(it-sequential "zonk-union-type-substitutes-members"
  (let* ((a        (cl-cc/type:fresh-type-var 'a))
         (union-ty (cl-cc/type:make-type-union-raw :types (list a cl-cc/type:type-string)))
         (s        (subst-extend a cl-cc/type:type-int nil))
         (result   (zonk union-ty s)))
    (expect (cl-cc/type:type-union-p result) :to-be-truthy)
    (expect (= 2 (length (cl-cc/type:type-union-types result))) :to-be-truthy)))

(it-sequential "zonk-effect-row-var-is-resolved"
  (let* ((rv  (cl-cc/type:fresh-type-var 'r))
         (eff (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list eff) :row-var rv))
         (s   (subst-extend rv (cl-cc/type:make-type-effect-row :effects nil :row-var nil) nil)))
    (expect (cl-cc/type:type-effect-row-p (zonk row s)) :to-be-truthy)))

(it-sequential "zonk-effect-rows-are-merged-when-var-resolves-to-row"
  (let* ((rv   (cl-cc/type:fresh-type-var 'r))
         (eff1 (cl-cc/type:make-type-effect-op :name 'io  :args nil))
         (eff2 (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list eff1) :row-var rv))
         (row2 (cl-cc/type:make-type-effect-row :effects (list eff2) :row-var nil))
         (s    (subst-extend rv row2 nil))
         (result (zonk row1 s)))
    (expect (cl-cc/type:type-effect-row-p result) :to-be-truthy)
    (expect (= 2 (length (cl-cc/type:type-effect-row-effects result))) :to-be-truthy)))

;;; ─── Occurs Check ───────────────────────────────────────────────────────

(it-sequential "type-occurs-check"
  (let ((s (make-substitution)))
    (let ((v (cl-cc/type:fresh-type-var 'a)))
      (expect (type-occurs-p v v s) :to-be-truthy)
      (expect (type-occurs-p v (cl-cc/type:make-type-arrow-raw :params (list v) :return cl-cc/type:type-int) s) :to-be-truthy)
      (expect (type-occurs-p v cl-cc/type:type-int s) :to-be-falsy)))
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return cl-cc/type:type-int))
         (s (subst-extend b fn-ty nil)))
    (expect (type-occurs-p a b s) :to-be-truthy)))

;;; ─── Generalize / Instantiate ───────────────────────────────────────────

(it-sequential "generalize-quantification-cases outside-env"
  (destructuring-bind (expected-count var-in-env-p) (list 1 nil)
    (let* ((a (cl-cc/type:fresh-type-var 'a))
         (env (if var-in-env-p
                  (cl-cc/type:type-env-extend 'x (cl-cc/type:type-to-scheme a) (cl-cc/type:type-env-empty))
                  nil))
         (ret (if var-in-env-p cl-cc/type:type-int a))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return ret))
         (scheme (generalize env fn-ty)))
    (expect (= expected-count (length (cl-cc/type:type-scheme-quantified-vars scheme))) :to-be-truthy))))

(it-sequential "generalize-quantification-cases in-env"
  (destructuring-bind (expected-count var-in-env-p) (list 0 t)
    (let* ((a (cl-cc/type:fresh-type-var 'a))
         (env (if var-in-env-p
                  (cl-cc/type:type-env-extend 'x (cl-cc/type:type-to-scheme a) (cl-cc/type:type-env-empty))
                  nil))
         (ret (if var-in-env-p cl-cc/type:type-int a))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return ret))
         (scheme (generalize env fn-ty)))
    (expect (= expected-count (length (cl-cc/type:type-scheme-quantified-vars scheme))) :to-be-truthy))))

(it-sequential "instantiate-produces-fresh"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return a))
         (scheme (generalize nil fn-ty))
         (inst1 (instantiate scheme)))
    ;; Instantiation produces an arrow with fresh vars
    (expect (cl-cc/type:type-arrow-p inst1) :to-be-truthy)
    (let ((p1 (car (cl-cc/type:type-arrow-params inst1)))
          (r1 (cl-cc/type:type-arrow-return inst1)))
      ;; Param and return should be the same fresh var (since both were 'a')
      (expect (cl-cc/type:type-var-p p1) :to-be-truthy)
      (expect (cl-cc/type:type-var-equal-p p1 r1) :to-be-truthy)
      ;; Fresh var should be different from the original quantified var
      (expect (cl-cc/type:type-var-equal-p p1 a) :to-be-falsy))))

;;; ─── Normalize ──────────────────────────────────────────────────────────

(it-sequential "normalize-type-variables-renames-distinct-vars"
  (let* ((fn    (cl-cc/type:make-type-arrow-raw
                 :params (list (cl-cc/type:fresh-type-var 'xyz))
                 :return (cl-cc/type:fresh-type-var 'qqq)))
         (normed (cl-cc/type:normalize-type-variables fn)))
    (let ((p (car (cl-cc/type:type-arrow-params normed)))
          (r (cl-cc/type:type-arrow-return normed)))
      (expect (cl-cc/type:type-var-p p) :to-be-truthy)
      (expect (cl-cc/type:type-var-p r) :to-be-truthy)
      (expect (cl-cc/type:type-var-equal-p p r) :to-be-falsy))))

(it-sequential "normalize-type-variables-preserves-shared-variable"
  (let* ((v     (cl-cc/type:fresh-type-var 'x))
         (fn    (cl-cc/type:make-type-arrow-raw :params (list v) :return v))
         (normed (cl-cc/type:normalize-type-variables fn)))
    (expect (cl-cc/type:type-var-equal-p
                  (car (cl-cc/type:type-arrow-params normed))
                  (cl-cc/type:type-arrow-return normed)) :to-be-truthy)))

(it-sequential "zonk-env-substitutes-all-bindings"
  (let* ((a        (cl-cc/type:fresh-type-var 'a))
         (env      (cl-cc/type:make-type-env
                    :bindings (list (cons 'x a)
                                    (cons 'y cl-cc/type:type-string))))
         (s        (subst-extend a cl-cc/type:type-int nil))
         (result   (cl-cc/type:zonk-env env s))
         (bindings (cl-cc/type:type-env-bindings result)))
    (expect (cl-cc/type:type-primitive-name (cdr (first bindings))) :to-be 'fixnum)
    (expect (cl-cc/type:type-primitive-name (cdr (second bindings))) :to-be 'string)))

;;; ─── %nv-canonical / %nv-norm (extracted helpers) ────────────────────────────

(it-sequential "nv-canonical-creates-fresh-var"
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0))
        (v            (cl-cc/type:fresh-type-var 'x)))
    (let ((nv1 (cl-cc/type::%nv-canonical v mapping counter-cell)))
      (expect (cl-cc/type:type-var-p nv1) :to-be-truthy)
      (expect (= 1 (car counter-cell)) :to-be-truthy)
      (let ((nv2 (cl-cc/type::%nv-canonical v mapping counter-cell)))
        (expect nv2 :to-be nv1)
        (expect (= 1 (car counter-cell)) :to-be-truthy)))))

(it-sequential "nv-norm-passes-through-non-var"
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0)))
    (expect (cl-cc/type::%nv-norm cl-cc/type:type-int mapping counter-cell) :to-be cl-cc/type:type-int)
    (expect (= 0 (car counter-cell)) :to-be-truthy)))

(it-sequential "nv-norm-renames-type-var"
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0))
        (v            (cl-cc/type:fresh-type-var 'z)))
    (let ((result (cl-cc/type::%nv-norm v mapping counter-cell)))
      (expect (cl-cc/type:type-var-p result) :to-be-truthy)
      (expect (eq result v) :to-be-falsy))))

;;; ─── apply-unification ───────────────────────────────────────────────────────

(it-sequential "apply-unification-nil-subst-returns-nil"
  (let ((ty (cl-cc/type:parse-type-specifier 'fixnum)))
    (expect (cl-cc/type:apply-unification ty nil) :to-be-falsy)))

(it-sequential "apply-unification-empty-subst-returns-ty"
  (let ((ty    (cl-cc/type:parse-type-specifier 'fixnum))
        (subst (make-hash-table :test #'eql)))
    (expect (cl-cc/type:apply-unification ty subst) :to-be ty)))

(it-sequential "apply-unification-resolves-mapped-var"
  (let* ((v     (cl-cc/type:fresh-type-var 'x))
          (target (cl-cc/type:parse-type-specifier 'fixnum))
         (subst  (cl-cc/type:subst-extend v target nil)))
    (let ((result (cl-cc/type:apply-unification v subst)))
      (expect result :to-be-truthy)
      (expect (cl-cc/type:type-var-p result) :to-be-falsy))))
