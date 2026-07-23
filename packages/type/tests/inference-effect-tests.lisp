;;;; packages/type/tests/inference-effect-tests.lisp — Effect Inference Tests
;;;
;;; Covers effect inference, effect signatures, skolem helpers, bidirectional check,
;;; annotation, condition classes, value restriction, and polymorphic recursion helpers.

(in-package :cl-cc/test)

;;; ─── Effect Inference ─────────────────────────────────────────────────────

(defmacro assert-inference-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

(it-sequential "infer-effects-single-forms setq-state"
  (destructuring-bind (sexp expected-effect) (list '(setq x 42) "STATE")
    (let* ((ast (cl-cc:lower-sexp-to-ast sexp))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (assert-inference-boolean-case
        expected-effect
        (let* ((effects (cl-cc/type:type-effect-row-effects row))
               (names (mapcar #'cl-cc/type:type-effect-op-name effects)))
          (expect (member expected-effect names :key #'symbol-name :test #'string=) :to-be-truthy))
      (expect (cl-cc/type:type-effect-row-effects row) :to-be-null)))))

(it-sequential "infer-effects-single-forms lambda-pure"
  (destructuring-bind (sexp expected-effect) (list '(lambda (x) x) nil)
    (let* ((ast (cl-cc:lower-sexp-to-ast sexp))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (assert-inference-boolean-case
        expected-effect
        (let* ((effects (cl-cc/type:type-effect-row-effects row))
               (names (mapcar #'cl-cc/type:type-effect-op-name effects)))
          (expect (member expected-effect names :key #'symbol-name :test #'string=) :to-be-truthy))
      (expect (cl-cc/type:type-effect-row-effects row) :to-be-null)))))

(it-sequential "infer-effects-multi-form-unions if"
  (destructuring-bind (sexp) (list '(if (print 1) (setq x 2) 3))
    (let* ((ast (cl-cc:lower-sexp-to-ast sexp))
         (row (cl-cc/type:infer-effects ast (type-env-empty)))
         (names (mapcar #'cl-cc/type:type-effect-op-name (cl-cc/type:type-effect-row-effects row))))
    (expect (member "IO"    names :key #'symbol-name :test #'string=) :to-be-truthy)
    (expect (member "STATE" names :key #'symbol-name :test #'string=) :to-be-truthy))))

(it-sequential "infer-effects-multi-form-unions progn"
  (destructuring-bind (sexp) (list '(progn (print 1) (setq x 2)))
    (let* ((ast (cl-cc:lower-sexp-to-ast sexp))
         (row (cl-cc/type:infer-effects ast (type-env-empty)))
         (names (mapcar #'cl-cc/type:type-effect-op-name (cl-cc/type:type-effect-row-effects row))))
    (expect (member "IO"    names :key #'symbol-name :test #'string=) :to-be-truthy)
    (expect (member "STATE" names :key #'symbol-name :test #'string=) :to-be-truthy))))

;;; ─── Effect Signature Registry ────────────────────────────────────────────

(it-sequential "infer-effect-signature-dispatch print"
  (destructuring-bind (fn-name expected-effect-name) (list 'print "IO")
    (let ((row (cl-cc/type:lookup-effect-signature fn-name)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (assert-inference-boolean-case
        expected-effect-name
        (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                             (cl-cc/type:type-effect-row-effects row))))
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-truthy))
      (expect (cl-cc/type:type-effect-row-effects row) :to-be-null)))))

(it-sequential "infer-effect-signature-dispatch error"
  (destructuring-bind (fn-name expected-effect-name) (list 'error "ERROR")
    (let ((row (cl-cc/type:lookup-effect-signature fn-name)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (assert-inference-boolean-case
        expected-effect-name
        (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                             (cl-cc/type:type-effect-row-effects row))))
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-truthy))
      (expect (cl-cc/type:type-effect-row-effects row) :to-be-null)))))

(it-sequential "infer-effect-signature-dispatch unknown"
  (destructuring-bind (fn-name expected-effect-name) (list 'completely-unknown-fn-xyz nil)
    (let ((row (cl-cc/type:lookup-effect-signature fn-name)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (assert-inference-boolean-case
        expected-effect-name
        (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                             (cl-cc/type:type-effect-row-effects row))))
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-truthy))
      (expect (cl-cc/type:type-effect-row-effects row) :to-be-null)))))

(it-sequential "infer-custom-effect-signature-register-and-lookup"
  (let ((custom-row (cl-cc/type:make-type-effect-row
                     :effects (list (cl-cc/type:make-type-effect-op :name 'network :args nil))
                     :row-var nil)))
    (cl-cc/type:register-effect-signature 'http-get-xyz custom-row)
    (let ((result (cl-cc/type:lookup-effect-signature 'http-get-xyz)))
      (expect result :to-be custom-row))))

(it-sequential "infer-dict-env-constraint-satisfaction"
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'local-num type-int '((plus . #'+)) env0))
         (constraint (cl-cc/type:make-type-constraint
                      :class-name 'local-num
                      :type-arg type-int))
         (q (cl-cc/type:make-type-qualified :constraints (list constraint)
                                            :body type-int)))
    (cl-cc/type:check-qualified-constraints q nil env1)
    (signals cl-cc/type:type-inference-error (cl-cc/type:check-qualified-constraints q nil env0))))

;;; ─── infer-with-effects ───────────────────────────────────────────────────

;;; ─── check-body-effects ───────────────────────────────────────────────────

(it-sequential "infer-with-effects-returns-triple"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(print 42))))
    (multiple-value-bind (ty subst effects)
        (cl-cc/type:infer-with-effects ast (type-env-empty))
      (declare (ignore subst))
      (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum)
      (expect (cl-cc/type:type-effect-row-p effects) :to-be-truthy)
      (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                           (cl-cc/type:type-effect-row-effects effects))))
        (expect (member "IO" names :key #'symbol-name :test #'string=) :to-be-truthy)))))

(it-sequential "infer-check-body-effects-io-row-succeeds"
  (let ((asts (list (cl-cc:lower-sexp-to-ast '(print 42)))))
    (cl-cc/type:check-body-effects asts cl-cc/type:+io-effect-row+ (type-env-empty))
    (expect t :to-be-truthy)))

(it-sequential "infer-check-body-effects-pure-row-signals-error"
  (let ((asts (list (cl-cc:lower-sexp-to-ast '(print 42)))))
    (signals cl-cc/type:type-inference-error (cl-cc/type:check-body-effects asts cl-cc/type:+pure-effect-row+ (type-env-empty)))))

;;; ─── Skolem Helpers ───────────────────────────────────────────────────────

(it-sequential "infer-skolem-appears-in-type-p"
  (let* ((sk (cl-cc/type:fresh-rigid-var "a"))
         (fn-with (cl-cc/type:make-type-arrow-raw
                    :params (list sk) :return cl-cc/type:type-int))
         (fn-without (cl-cc/type:make-type-arrow-raw
                      :params (list cl-cc/type:type-int) :return cl-cc/type:type-int)))
    (expect (cl-cc/type:skolem-appears-in-type-p sk fn-with) :to-be-truthy)
    (expect (cl-cc/type:skolem-appears-in-type-p sk fn-without) :to-be-falsy)))

(it-sequential "infer-skolem-appears-in-subst-p"
  (let* ((sk (cl-cc/type:fresh-rigid-var "a"))
         (v  (cl-cc/type:fresh-type-var 'x)))
    (expect (cl-cc/type::skolem-appears-in-subst-p sk (subst-extend v sk (make-substitution))) :to-be-truthy)
    (expect (cl-cc/type::skolem-appears-in-subst-p sk (subst-extend v cl-cc/type:type-int (make-substitution))) :to-be-falsy)))

(it-sequential "infer-check-skolem-escape-signals-error"
  (let* ((sk (cl-cc/type:fresh-rigid-var "a"))
         (v (cl-cc/type:fresh-type-var 'x))
         (s (subst-extend v sk (make-substitution))))
    (signals cl-cc/type:type-inference-error (cl-cc/type:check-skolem-escape sk s))))

;;; ─── Bidirectional check: forall ──────────────────────────────────────────

(it-sequential "infer-check-forall-type-skolemizes-and-succeeds"
  (reset-type-vars!)
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (fn-body (cl-cc/type:make-type-arrow-raw :params (list a) :return a))
         (forall-ty (cl-cc/type:make-type-forall :var a :body fn-body))
         (ast (cl-cc:lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (let ((subst (cl-cc/type:check ast forall-ty env)))
      (declare (ignore subst))
      (expect t :to-be-truthy))))

(it-sequential "infer-check-type-unknown-returns-nil-subst"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '42))
        (env (type-env-empty)))
    (let ((subst (cl-cc/type:check ast cl-cc/type:+type-unknown+ env)))
      (expect subst :to-be-null))))

;;; ─── annotate-type / defun-without-annotation ───────────────────────────

(it-sequential "infer-annotate-type-returns-int-and-original-ast"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '42)))
    (multiple-value-bind (ty result-ast)
        (cl-cc/type:annotate-type ast (type-env-empty))
      (assert-type-equal ty cl-cc/type:type-int)
      (expect result-ast :to-be ast))))

(it-sequential "infer-defun-return-type-is-fixnum"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(defun f (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (cl-cc/type:type-arrow-p ty) :to-be-truthy)
      (assert-type-equal (cl-cc/type:type-arrow-return ty) cl-cc/type:type-int))))

;;; ─── Condition Classes ────────────────────────────────────────────────────

(it-sequential "infer-condition-base-error-has-message"
  (let ((c (make-condition 'cl-cc/type:type-inference-error :message "test")))
    (expect (typep c 'error) :to-be-truthy)
    (expect (cl-cc/type:type-inference-error-message c) :to-equal "test")))

(it-sequential "infer-condition-unbound-variable-carries-name"
  (let ((c (make-condition 'cl-cc/type:unbound-variable-error :name 'x)))
    (expect (typep c 'cl-cc/type:type-inference-error) :to-be-truthy)
    (expect (cl-cc/type:unbound-variable-error-name c) :to-be 'x)))

(it-sequential "infer-condition-type-mismatch-carries-expected-and-actual"
  (let ((c (make-condition 'cl-cc/type:type-mismatch-error
                           :expected cl-cc/type:type-int
                           :actual cl-cc/type:type-string)))
    (expect (typep c 'cl-cc/type:type-inference-error) :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name
                        (cl-cc/type:type-mismatch-error-expected c)) :to-be 'fixnum)
    (expect (cl-cc/type:type-primitive-name
                        (cl-cc/type:type-mismatch-error-actual c)) :to-be 'string)))

;;; ─── FR-1604: Value Restriction ──────────────────────────────────────────

(it-sequential "infer-syntactic-value-p integer"
  (destructuring-bind (sexp expected) (list '42 t)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p quote"
  (destructuring-bind (sexp expected) (list ''hello t)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p lambda"
  (destructuring-bind (sexp expected) (list '(lambda (x) x) t)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p function"
  (destructuring-bind (sexp expected) (list '#'car t)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p call"
  (destructuring-bind (sexp expected) (list '(foo 1) nil)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p binop"
  (destructuring-bind (sexp expected) (list '(+ x y) nil)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p if"
  (destructuring-bind (sexp expected) (list '(if t 1 2) nil)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-syntactic-value-p progn"
  (destructuring-bind (sexp expected) (list '(progn 1 2) nil)
    (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (assert-inference-boolean-case
        expected
        (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)
      (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))))

(it-sequential "infer-value-restriction-lambda-binding-generalizes"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(let ((id (lambda (x) x))) (id 42)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

(it-sequential "infer-value-restriction-call-binding-stays-monomorphic"
  (reset-type-vars!)
  (let* ((fn-ty (cl-cc/type:make-type-arrow-raw
                 :params (list cl-cc/type:type-int)
                 :return cl-cc/type:type-int))
         (env (type-env-extend 'identity (type-to-scheme fn-ty) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(let ((x (identity 42))) x))))
    (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

;;; ─── FR-004: Polymorphic Recursion ──────────────────────────────────────

(it-sequential "infer-poly-recursion-helper-cases found"
  (destructuring-bind (decls name expected) (list '((type (-> fixnum fixnum) length) (optimize (speed 3))) 'length '(-> fixnum fixnum))
    (let ((spec (cl-cc/type::%find-fn-type-declaration name decls)))
    (assert-inference-boolean-case
        expected
        (expect spec :to-equal expected)
      (expect spec :to-be-null)))))

(it-sequential "infer-poly-recursion-helper-cases miss-name"
  (destructuring-bind (decls name expected) (list '((type fixnum x) (type string y)) 'length nil)
    (let ((spec (cl-cc/type::%find-fn-type-declaration name decls)))
    (assert-inference-boolean-case
        expected
        (expect spec :to-equal expected)
      (expect spec :to-be-null)))))

(it-sequential "infer-poly-recursion-helper-cases nil-name"
  (destructuring-bind (decls name expected) (list '((type fixnum x)) nil nil)
    (let ((spec (cl-cc/type::%find-fn-type-declaration name decls)))
    (assert-inference-boolean-case
        expected
        (expect spec :to-equal expected)
      (expect spec :to-be-null)))))

;;; ─── %infer-effects-union ────────────────────────────────────────────────

(it-sequential "infer-effects-union-empty-is-pure"
  (reset-type-vars!)
  (let ((result (cl-cc/type::%infer-effects-union nil nil)))
    (expect (cl-cc/type:effect-row-subset-p result cl-cc/type:+pure-effect-row+) :to-be-truthy)
    (expect (cl-cc/type:effect-row-subset-p cl-cc/type:+pure-effect-row+ result) :to-be-truthy)))

(it-sequential "infer-effects-union-pure-forms-stay-pure"
  (reset-type-vars!)
  (let* ((asts   (list (cl-cc:lower-sexp-to-ast '1) (cl-cc:lower-sexp-to-ast '2)))
         (result (cl-cc/type::%infer-effects-union asts nil)))
    (expect (cl-cc/type:effect-row-subset-p result cl-cc/type:+pure-effect-row+) :to-be-truthy)))
