;;;; tests/unit/type/inference-forms-tests.lisp — Type Inference AST Form Tests
;;;
;;; Tests for infer() on concrete AST node types: ast-quote, ast-the, typed holes,
;;; ast-setq, ast-block, ast-defun/defvar, ast-function, lambda-param-env,
;;; ast-flet/ast-labels.
;;; Depends on helpers defined in inference-tests.lisp (same package, loads first).

(in-package :cl-cc/test)

;;; ─── infer: ast-quote edge cases ──────────────────────────────────────────

(it-sequential "infer-quote-literal-types string"
  (destructuring-bind (sexp expected-type) (list '(quote "hello") cl-cc/type:type-string)
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type)))))

(it-sequential "infer-quote-literal-types cons"
  (destructuring-bind (sexp expected-type) (list '(quote (1 2 3)) cl-cc/type:type-cons)
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type)))))

;;; ─── infer: ast-the ───────────────────────────────────────────────────────

(it-sequential "infer-the-matching-type-is-fixnum"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the fixnum 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum))))

(it-sequential "infer-the-refinement-base-is-fixnum"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the (refine fixnum plusp) 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (let ((prim (if (cl-cc/type:type-refinement-p ty)
                      (cl-cc/type:type-refinement-base ty)
                      ty)))
        (expect (cl-cc/type:type-primitive-name prim) :to-be 'fixnum)))))

(it-sequential "infer-the-type-mismatch-signals-error"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the string 42))))
    (signals cl-cc/type:type-mismatch-error (infer-with-env ast))))

(it-sequential "infer-typed-hole-signals-error"
  (reset-type-vars!)
  (let* ((env (type-env-extend 'x (type-to-scheme cl-cc/type:type-int) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(+ x _))))
    (signals cl-cc/type::typed-hole-error (cl-cc/type:infer ast env))))

(it-sequential "infer-typed-hole-message-names-in-scope-vars"
  (reset-type-vars!)
  (let* ((env (type-env-extend 'x (type-to-scheme cl-cc/type:type-int) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(+ x _))))
    (handler-case
        (cl-cc/type:infer ast env)
      (cl-cc/type::typed-hole-error (e)
        (let ((message (cl-cc/type:type-inference-error-message e)))
          (expect (search "Available: X ::" message) :to-be-truthy))))))

;;; ─── infer: ast-setq / ast-block / infer-with-constraints ───────────────

(it-sequential "infer-setq-returns-fixnum"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(setq x 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum))))

(it-sequential "infer-block-returns-last-form"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(block b 1 2 3))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

(it-sequential "infer-application-resolves-constraint"
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '((lambda (x) x) 42))))
    (multiple-value-bind (ty subst residual)
        (cl-cc/type::infer-with-constraints ast (type-env-empty))
      (declare (ignore subst))
      (expect residual :to-be-null)
      (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum))))

;;; ─── infer: ast-defun / ast-defvar ────────────────────────────────────────

(it-sequential "infer-top-level-form-types defun"
  (destructuring-bind (sexp expected-kind) (list '(defun f (x) (+ x 1)) :function)
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (if (eq expected-kind :function)
          (expect (cl-cc/type:type-arrow-p ty) :to-be-truthy)
          (assert-type-equal ty cl-cc/type:type-symbol))))))

(it-sequential "infer-top-level-form-types defvar-init"
  (destructuring-bind (sexp expected-kind) (list '(defvar *x* 42) :symbol)
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (if (eq expected-kind :function)
          (expect (cl-cc/type:type-arrow-p ty) :to-be-truthy)
          (assert-type-equal ty cl-cc/type:type-symbol))))))

(it-sequential "infer-top-level-form-types defvar-no-val"
  (destructuring-bind (sexp expected-kind) (list '(defvar *x*) :symbol)
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (if (eq expected-kind :function)
          (expect (cl-cc/type:type-arrow-p ty) :to-be-truthy)
          (assert-type-equal ty cl-cc/type:type-symbol))))))

;;; ─── infer: ast-function ──────────────────────────────────────────────────

(it-sequential "infer-function-ref-cases known"
  (destructuring-bind (bound-p) (list t)
    (reset-type-vars!) (if bound-p
      (let* ((fn-ty (cl-cc/type:make-type-arrow-raw
                     :params (list cl-cc/type:type-int) :return cl-cc/type:type-int))
             (env (type-env-extend 'f (type-to-scheme fn-ty) (type-env-empty)))
             (ast (cl-cc:lower-sexp-to-ast '(function f))))
        (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
          (declare (ignore subst))
          (expect (cl-cc/type:type-arrow-p ty) :to-be-truthy)))
      (let ((ast (cl-cc:lower-sexp-to-ast '(function unknown-fn-xyz))))
        (multiple-value-bind (ty subst) (infer-with-env ast)
          (declare (ignore subst))
          (expect (cl-cc/type:type-unknown-p ty) :to-be-truthy))))))

(it-sequential "infer-function-ref-cases unknown"
  (destructuring-bind (bound-p) (list nil)
    (reset-type-vars!) (if bound-p
      (let* ((fn-ty (cl-cc/type:make-type-arrow-raw
                     :params (list cl-cc/type:type-int) :return cl-cc/type:type-int))
             (env (type-env-extend 'f (type-to-scheme fn-ty) (type-env-empty)))
             (ast (cl-cc:lower-sexp-to-ast '(function f))))
        (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
          (declare (ignore subst))
          (expect (cl-cc/type:type-arrow-p ty) :to-be-truthy)))
      (let ((ast (cl-cc:lower-sexp-to-ast '(function unknown-fn-xyz))))
        (multiple-value-bind (ty subst) (infer-with-env ast)
          (declare (ignore subst))
          (expect (cl-cc/type:type-unknown-p ty) :to-be-truthy))))))

;;; ─── %make-lambda-param-env ────────────────────────────────────────────────

(it-sequential "infer-make-lambda-param-env-empty-params"
  (reset-type-vars!)
  (let ((empty-env (cl-cc/type:type-env-empty)))
    (multiple-value-bind (types body-env)
        (cl-cc/type::%make-lambda-param-env nil empty-env)
      (expect types :to-be-null)
      (expect body-env :to-equal empty-env))))

(it-sequential "infer-make-lambda-param-env-two-params"
  (reset-type-vars!)
  (multiple-value-bind (types body-env)
      (cl-cc/type::%make-lambda-param-env '(x y) (cl-cc/type:type-env-empty))
    (expect (= 2 (length types)) :to-be-truthy)
    (expect (cl-cc/type:type-var-p (first types)) :to-be-truthy)
    (expect (cl-cc/type:type-var-p (second types)) :to-be-truthy)
    (expect (eq (first types) (second types)) :to-be-falsy)
    (expect (cl-cc/type:type-env-p body-env) :to-be-truthy)
    (multiple-value-bind (scheme-x found-x) (cl-cc/type:type-env-lookup 'x body-env)
      (expect found-x :to-be-truthy)
      (expect (cl-cc/type:type-scheme-quantified-vars scheme-x) :to-be-null))))

;;; ─── infer: ast-flet / ast-labels ─────────────────────────────────────────

(it-sequential "infer-local-function-forms flet"
  (destructuring-bind (sexp) (list '(flet   ((f (x) (+ x 1))) (f 5)))
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum)))))

(it-sequential "infer-local-function-forms labels"
  (destructuring-bind (sexp) (list '(labels ((f (x) (+ x 1))) (f 5)))
    (reset-type-vars!) (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum)))))

;;; ─── Advanced FR inference integration ────────────────────────────────────

(it-sequential "advanced-infer-spawn-enforces-send-and-returns-future"
  (reset-type-vars!)
  (let ((valid (cl-cc:lower-sexp-to-ast '(spawn 1)))
        (invalid (cl-cc:lower-sexp-to-ast '(spawn '(1 2)))))
    (multiple-value-bind (ty subst) (infer-with-env valid)
      (declare (ignore subst))
      (expect (cl-cc/type:type-advanced-p ty) :to-be-truthy)
      (expect (cl-cc/type:type-advanced-feature-id ty) :to-equal "FR-2201"))
    (signals cl-cc/type:type-inference-error (infer-with-env invalid))))

(it-sequential "advanced-infer-shared-ref-enforces-sync"
  (reset-type-vars!)
  (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(shared-ref (lambda (x) x))))))

(it-sequential "advanced-infer-ffi-interface-smt-plugin-and-synthesis-validate-descriptors"
  (flet ((infer-ok (form)
            (multiple-value-bind (ty subst) (infer-with-env (cl-cc:lower-sexp-to-ast form))
              (declare (ignore subst))
              (expect ty :to-be-truthy))))
    (reset-type-vars!)
    (multiple-value-bind (ty subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(foreign-call '(foreign strlen (c-string) c-int) "abc")))
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(foreign-call '(foreign strlen (c-string) c-int) 1))))

    (infer-ok '(load-type-interface 'user-module '((lookup (function (fixnum) fixnum)) save) '"sha256:abc"))
    (multiple-value-bind (imported-ty imported-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast 'lookup))
      (declare (ignore imported-subst))
      (expect (cl-cc/type:type-arrow-p imported-ty) :to-be-truthy))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(load-type-interface 'user-module '(lookup lookup) '"sha256:abc"))))

    (infer-ok '(smt-assert '(< x 3) 'z3 'lia))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(smt-assert '(< x 3) 'unknown 'lia))))

    (infer-ok '(run-type-plugin 'nat-normalise 'solve))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(run-type-plugin 'nat-normalise 'emit))))

    (multiple-value-bind (synth-ty synth-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(synthesize-program '(-> integer integer) 'enumerative 8)))
      (declare (ignore synth-subst))
      (expect (cl-cc/type:type-arrow-p synth-ty) :to-be-truthy))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(synthesize-program '(-> integer integer) 'enumerative 0))))))

(it-sequential "advanced-infer-mapped-and-conditional-type-calls-run-contracts"
  (flet ((infer-ok (form)
            (multiple-value-bind (ty subst) (infer-with-env (cl-cc:lower-sexp-to-ast form))
              (declare (ignore subst))
              (expect ty :to-be-truthy))))
    (reset-type-vars!)
    (multiple-value-bind (mapped-ty mapped-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-mapped-type 'fixnum 'optional)))
      (declare (ignore mapped-subst))
      (expect (cl-cc/type:type-union-p mapped-ty) :to-be-truthy)
      (expect (some (lambda (member) (cl-cc/type:type-equal-p member cl-cc/type:type-null))
                         (cl-cc/type:type-union-types mapped-ty)) :to-be-truthy))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-mapped-type '(list fixnum) 'mysterious))))

    (multiple-value-bind (conditional-ty conditional-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-conditional-type '(list fixnum) 'list 'item 'item 'null)))
      (declare (ignore conditional-subst))
      (assert-type-equal conditional-ty cl-cc/type:type-int))
    (multiple-value-bind (else-ty else-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-conditional-type 'fixnum 'list 'item 'item 'null)))
      (declare (ignore else-subst))
      (assert-type-equal else-ty cl-cc/type:type-null))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-conditional-type '(list fixnum) 'list 'item 'item 'item))))))

(it-sequential "advanced-infer-registries-dispatch-custom-hooks"
  (let ((smt-called nil)
        (plugin-called nil)
        (synthesis-called nil))
    (cl-cc/type:register-smt-solver 'unit-solver
                                    (lambda (constraint theory)
                                      (setf smt-called (list constraint theory))
                                      (list :status :sat :counterexample :none)))
    (cl-cc/type:register-type-checker-plugin 'unit-plugin 'solve
                                             (lambda (ast arg-types env)
                                               (declare (ignore ast arg-types env))
                                               (setf plugin-called t)
                                               (list :status :ok :type cl-cc/type:type-string)))
    (cl-cc/type:register-type-synthesis-strategy 'unit-search
                                                 (lambda (signature fuel)
                                                   (setf synthesis-called (list signature fuel))
                                                   (list :status :candidate :signature signature)))
    (multiple-value-bind (smt-ty smt-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(smt-assert '(< x 3) 'unit-solver 'lia)))
      (declare (ignore smt-subst))
      (assert-type-equal smt-ty cl-cc/type:type-bool))
    (multiple-value-bind (plugin-ty plugin-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(run-type-plugin 'unit-plugin 'solve)))
      (declare (ignore plugin-subst))
      (assert-type-equal plugin-ty cl-cc/type:type-string))
    (multiple-value-bind (synth-ty synth-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(synthesize-program 'fixnum 'unit-search 3)))
      (declare (ignore synth-subst))
      (assert-type-equal synth-ty cl-cc/type:type-int))
    (expect smt-called :to-be-truthy)
    (expect plugin-called :to-be-truthy)
    (expect synthesis-called :to-be-truthy)))

(it-sequential "advanced-infer-constructor-policies-cover-channels-actors-stm-coroutines-simd-and-routing"
  (reset-type-vars!)
  (flet ((infer-type (form)
           (multiple-value-bind (ty subst) (infer-with-env (cl-cc:lower-sexp-to-ast form))
             (declare (ignore subst))
             ty)))
    (let ((channel-ty (infer-type '(make-typed-channel 'fixnum)))
          (buffered-ty (infer-type '(make-buffered-channel 'fixnum 4)))
          (actor-ty (infer-type '(make-actor-ref 'fixnum)))
          (stm-ty (infer-type '(make-tvar 'fixnum 1)))
          (generator-ty (infer-type '(make-generator-type 'fixnum 'string)))
          (coroutine-ty (infer-type '(make-coroutine-type 'fixnum 'string 'integer)))
          (simd-ty (infer-type '(make-simd-type 'fixnum 4))))
      (expect (cl-cc/type:type-advanced-p channel-ty) :to-be-truthy)
      (expect (cl-cc/type:type-advanced-p buffered-ty) :to-be-truthy)
      (expect (cl-cc/type:type-advanced-feature-id channel-ty) :to-equal "FR-2202")
      (expect (cl-cc/type:type-advanced-feature-id buffered-ty) :to-equal "FR-2202")
      (expect (cl-cc/type:type-advanced-feature-id actor-ty) :to-equal "FR-2203")
      (expect (cl-cc/type:type-advanced-feature-id stm-ty) :to-equal "FR-2204")
      (expect (cl-cc/type:type-advanced-feature-id generator-ty) :to-equal "FR-2205")
      (expect (cl-cc/type:type-advanced-feature-id coroutine-ty) :to-equal "FR-2205")
      (expect (cl-cc/type:type-advanced-feature-id simd-ty) :to-equal "FR-2206"))
    (multiple-value-bind (api-ty api-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(make-api-type 'get '"/users/{id}" '((id integer)) 'user)))
      (declare (ignore api-subst))
      (expect (cl-cc/type:type-advanced-p api-ty) :to-be-truthy)
      (expect (cl-cc/type:type-advanced-feature-id api-ty) :to-equal "FR-3305"))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-buffered-channel 'fixnum))))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-typed-channel))))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-actor-ref))))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-tvar 'fixnum))))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-generator-type 'fixnum))))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-coroutine-type 'fixnum 'string))))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-simd-type 'fixnum))))
    (multiple-value-bind (api2-ty api2-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(make-api-type 'get '"/users/{id}" '((id fixnum)) 'fixnum)))
      (declare (ignore api2-subst))
      (expect (cl-cc/type:type-advanced-p api2-ty) :to-be-truthy)
      (expect (cl-cc/type:type-advanced-feature-id api2-ty) :to-equal "FR-3305"))
    (signals cl-cc/type:type-inference-error (infer-with-env (cl-cc:lower-sexp-to-ast '(make-api-type 'get '"/users" '()))))))
