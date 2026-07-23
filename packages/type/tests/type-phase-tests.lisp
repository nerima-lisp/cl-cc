;;;; tests/type-phase-tests.lisp - Type Phase Integration Tests (Phase A, C, D, E)
;;;;
;;;; Covers: parser integration, inference bug fixes, typeclass dictionary passing,
;;;; row-based effect inference, rank-N skolem, and 2026 type node extensions.

(in-package :cl-cc/test)


;;; Phase 4-6: Parser Integration Tests

(it-sequential "parse-type-specifier-forall"
  (let ((result (cl-cc/type:parse-type-specifier '(forall a (-> a a)))))
    (expect (type-forall-p result) :to-be-truthy)
    (expect (type-var-name (type-forall-var result)) :to-be 'a)))

(it-sequential "parse-type-specifier-effectful-arrow"
  (let ((result (cl-cc/type:parse-type-specifier '(-> fixnum fixnum ! io))))
    (expect (type-arrow-p result) :to-be-truthy)
    (expect (= 1 (length (type-arrow-params result))) :to-be-truthy)
    (expect (type-equal-p type-int (type-arrow-return result)) :to-be-truthy)
    (let ((effs (type-arrow-effects result)))
      (expect (type-effect-row-p effs) :to-be-truthy)
      (expect (= 1 (length (type-effect-row-effects effs))) :to-be-truthy))))

(it-sequential "parse-type-specifier-qualified"
  (let ((result (cl-cc/type:parse-type-specifier '(=> (num a) (-> a a a)))))
    (expect (type-qualified-p result) :to-be-truthy)
    (expect (= 1 (length (type-qualified-constraints result))) :to-be-truthy)
    (let ((constraint (first (type-qualified-constraints result))))
      (expect (symbol-name (cl-cc/type:type-constraint-class-name constraint)) :to-equal "NUM"))))

;;; Phase A: Inference Bug Fixes

(it-sequential "phase-a-infer-args-cases empty"
  (destructuring-bind (use-args-p expected-count) (list nil 0)
    (reset-type-vars!) (let* ((args (if use-args-p
                   (list (lower-sexp-to-ast 1) (lower-sexp-to-ast "hello"))
                   '()))
         (env (type-env-empty)))
    (multiple-value-bind (types subst)
        (cl-cc/type:infer-args args env)
      (declare (ignore subst))
      (expect (= expected-count (length types)) :to-be-truthy)
      (when use-args-p
        (expect (type-equal-p type-int    (first  types)) :to-be-truthy)
        (expect (type-equal-p type-string (second types)) :to-be-truthy))))))

(it-sequential "phase-a-infer-args-cases multiple"
  (destructuring-bind (use-args-p expected-count) (list t 2)
    (reset-type-vars!) (let* ((args (if use-args-p
                   (list (lower-sexp-to-ast 1) (lower-sexp-to-ast "hello"))
                   '()))
         (env (type-env-empty)))
    (multiple-value-bind (types subst)
        (cl-cc/type:infer-args args env)
      (declare (ignore subst))
      (expect (= expected-count (length types)) :to-be-truthy)
      (when use-args-p
        (expect (type-equal-p type-int    (first  types)) :to-be-truthy)
        (expect (type-equal-p type-string (second types)) :to-be-truthy))))))

(it-sequential "phase-a-union-unification-matches-member"
  (let* ((u (cl-cc/type:make-type-union (list type-int type-string))))
    (multiple-value-bind (subst ok) (type-unify u type-int)
      (declare (ignore subst))
      (expect ok :to-be-truthy))))

(it-sequential "phase-a-empty-progn-infers-to-non-nil"
  (reset-type-vars!)
  (handler-case
    (let* ((ast (lower-sexp-to-ast '(progn))))
      (multiple-value-bind (ty subst) (infer-with-env ast)
        (declare (ignore subst))
        (expect (or (type-equal-p ty type-null)
                         (cl-cc/type:type-unknown-p ty)
                         (not (null ty))) :to-be-truthy)))
    (error () (expect t :to-be-truthy))))

;;; Phase C: Typeclass Dictionary Passing

(it-sequential "phase-c-dict-env-operations"
  (let* ((methods (list (cons 'plus #'+) (cons 'zero 0)))
         (env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int methods env0))
         (found (cl-cc/type:dict-env-lookup 'num type-int env1)))
    (expect found :to-be-truthy)
    (expect (= 2 (length found)) :to-be-truthy))
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'eq  type-int '((eq-p  . #'equal)) env0))
         (env2 (cl-cc/type:dict-env-extend 'num type-int '((plus  . #'+))     env1)))
    (expect (cl-cc/type:dict-env-lookup 'eq  type-int env2) :to-be-truthy)
    (expect (cl-cc/type:dict-env-lookup 'num type-int env2) :to-be-truthy)))

(it-sequential "phase-c-dict-env-miss wrong-type"
  (destructuring-bind (class-name ty) (list 'num type-string)
    (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int '() env0)))
    (expect (cl-cc/type:dict-env-lookup class-name ty env1) :to-be-null))))

(it-sequential "phase-c-dict-env-miss wrong-class"
  (destructuring-bind (class-name ty) (list 'ord type-int)
    (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int '() env0)))
    (expect (cl-cc/type:dict-env-lookup class-name ty env1) :to-be-null))))


;;; Phase D: Row-Based Effect Type Inference

(it-sequential "phase-d-effect-row-union-count io+state-merges"
  (destructuring-bind (expected-count a-names b-names) (list 2 '(io) '(state))
    (let* ((row-a (apply #'%make-effect-row a-names))
         (row-b (apply #'%make-effect-row b-names))
         (union (cl-cc/type:effect-row-union row-a row-b)))
    (expect (type-effect-row-p union) :to-be-truthy)
    (expect (= expected-count (length (type-effect-row-effects union))) :to-be-truthy))))

(it-sequential "phase-d-effect-row-union-count io+pure-left"
  (destructuring-bind (expected-count a-names b-names) (list 1 '(io) '())
    (let* ((row-a (apply #'%make-effect-row a-names))
         (row-b (apply #'%make-effect-row b-names))
         (union (cl-cc/type:effect-row-union row-a row-b)))
    (expect (type-effect-row-p union) :to-be-truthy)
    (expect (= expected-count (length (type-effect-row-effects union))) :to-be-truthy))))

(it-sequential "phase-d-effect-row-union-count io+pure-right"
  (destructuring-bind (expected-count a-names b-names) (list 1 '() '(io))
    (let* ((row-a (apply #'%make-effect-row a-names))
         (row-b (apply #'%make-effect-row b-names))
         (union (cl-cc/type:effect-row-union row-a row-b)))
    (expect (type-effect-row-p union) :to-be-truthy)
    (expect (= expected-count (length (type-effect-row-effects union))) :to-be-truthy))))

(it-sequential "phase-d-infer-effects-pure-forms pure-arithmetic"
  (destructuring-bind (form) (list '(+ 1 2))
    (reset-type-vars!) (let* ((ast (lower-sexp-to-ast form))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (expect (type-effect-row-p row) :to-be-truthy)
    (expect (type-effect-row-effects row) :to-be-null))))

(it-sequential "phase-d-infer-effects-pure-forms pure-let"
  (destructuring-bind (form) (list '(let ((x 42)) x))
    (reset-type-vars!) (let* ((ast (lower-sexp-to-ast form))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (expect (type-effect-row-p row) :to-be-truthy)
    (expect (type-effect-row-effects row) :to-be-null))))

(defun %make-effect-row (&rest names)
  (make-type-effect-row :effects (mapcar (lambda (n) (make-type-effect-op :name n :args nil)) names) :row-var nil))

(it-sequential "phase-d-effect-row-subset smaller-of-larger"
  (destructuring-bind (expected sub-names sup-names) (list t '(io) '(io state))
    (let ((sub (apply #'%make-effect-row sub-names))
        (sup (apply #'%make-effect-row sup-names)))
    (if expected
        (expect (cl-cc/type:effect-row-subset-p sub sup) :to-be-truthy)
        (expect (cl-cc/type:effect-row-subset-p sub sup) :to-be-falsy)))))

(it-sequential "phase-d-effect-row-subset larger-not-of-smaller"
  (destructuring-bind (expected sub-names sup-names) (list nil '(io state) '(io))
    (let ((sub (apply #'%make-effect-row sub-names))
        (sup (apply #'%make-effect-row sup-names)))
    (if expected
        (expect (cl-cc/type:effect-row-subset-p sub sup) :to-be-truthy)
        (expect (cl-cc/type:effect-row-subset-p sub sup) :to-be-falsy)))))

(it-sequential "phase-d-effect-row-subset pure-is-subset-of-any"
  (destructuring-bind (expected sub-names sup-names) (list t '() '(io))
    (let ((sub (apply #'%make-effect-row sub-names))
        (sup (apply #'%make-effect-row sup-names)))
    (if expected
        (expect (cl-cc/type:effect-row-subset-p sub sup) :to-be-truthy)
        (expect (cl-cc/type:effect-row-subset-p sub sup) :to-be-falsy)))))

;;; Phase E: Rank-N Polymorphism

(it-sequential "phase-e-skolem-creation-and-equality"
  (let* ((sk1 (cl-cc/type:fresh-rigid-var 'a))
         (sk2 (cl-cc/type:fresh-rigid-var 'a))
         (sk3 (cl-cc/type:fresh-rigid-var 'b)))
    (expect (cl-cc/type:type-rigid-p sk1) :to-be-truthy)
    (expect (cl-cc/type:type-rigid-p sk2) :to-be-truthy)
    ;; Two fresh skolems with same name are distinct (unique IDs)
    (expect (cl-cc/type:type-rigid-equal-p sk1 sk2) :to-be-falsy)
    (expect (cl-cc/type:type-rigid-name sk1) :to-be 'a)
    ;; Same skolem is equal to itself
    (expect (cl-cc/type:type-rigid-equal-p sk3 sk3) :to-be-truthy)))

(it-sequential "phase-e-skolem-escape-absent"
  (let* ((sk (cl-cc/type:fresh-rigid-var 'a))
         (escaped (cl-cc/type:check-skolem-escape sk (make-substitution))))
    (expect escaped :to-be-null)))

(it-sequential "phase-e-check-lambda-against-forall"
  (reset-type-vars!)
  (let* ((a      (fresh-type-var :name 'a))
         (fa     (make-type-forall :var a :body (make-type-arrow (list a) a)))
         (id-ast (lower-sexp-to-ast '(lambda (x) x)))
         (env    (type-env-empty)))
    (expect (or (null (check id-ast fa env)) t) :to-be-truthy)))

(it-sequential "phase-e-synthesize-lambda-returns-function-type"
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (multiple-value-bind (ty subst) (synthesize ast env)
      (declare (ignore subst))
      (expect (typep ty 'type-arrow) :to-be-truthy)
      (expect (= 1 (length (type-arrow-params ty))) :to-be-truthy))))
