;;;; tests/unit/compile/codegen-core-helper-tests.lisp — Codegen helper and CPS routing tests

(in-package :cl-cc/test)

;;; ─── %case-of-case-collapse-node (extracted IF-collapse helper) ─────────────

(it-sequential "case-of-case-collapse-node-non-if-passthrough"
  (let* ((outer-cond (make-ast-var :name 'x))
         (leaf       (make-ast-int :value 42))
         (result     (cl-cc/compile::%case-of-case-collapse-node leaf outer-cond t)))
    (expect result :to-be leaf)))

(it-sequential "case-of-case-collapse-node-different-cond-passthrough"
  (let* ((outer-cond (make-ast-var :name 'x))
         (inner-cond (make-ast-var :name 'y))
         (inner-if   (make-ast-if :cond inner-cond
                                  :then (make-ast-int :value 1)
                                  :else (make-ast-int :value 2)))
         (result     (cl-cc/compile::%case-of-case-collapse-node inner-if outer-cond t)))
    (expect result :to-be inner-if)))

(it-sequential "case-of-case-collapse-node-same-cond-extracts-then"
  (let* ((cond       (make-ast-var :name 'p))
         (then-node  (make-ast-int :value 10))
         (else-node  (make-ast-int :value 20))
         (inner-if   (make-ast-if :cond cond :then then-node :else else-node))
         (result     (cl-cc/compile::%case-of-case-collapse-node inner-if cond t)))
    (expect result :to-be then-node)))

(it-sequential "case-of-case-collapse-node-same-cond-extracts-else"
  (let* ((cond      (make-ast-var :name 'p))
         (then-node (make-ast-int :value 10))
         (else-node (make-ast-int :value 20))
         (inner-if  (make-ast-if :cond cond :then then-node :else else-node))
         (result    (cl-cc/compile::%case-of-case-collapse-node inner-if cond nil)))
    (expect result :to-be else-node)))

(deftest-compile codegen-toplevel-cps-semantic-preservation
  "Top-level CPS routing preserves common supported forms."
  :cases (("two-safe-forms" 7 "(+ 1 2) (+ 3 4)")
          ("defvar-then-use" 3 "(defvar *ulw-cps* 1) (+ *ulw-cps* 2)")
          ("call-bearing-form" 6 "(defun add1 (x) (+ x 1)) (add1 5)")
          ("apply" 6 "(apply #'+ (list 1 2 3))")
          ("values-primary" 1 "(values 1 2 3)")
          ("multiple-value-bind" 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))"))
  :stdlib nil)

;;; ─── Numeric constructor helpers ────────────────────────────────────────────

(it-sequential "lookup-numeric-binop-ctor-symbol-known-pair"
  (let ((sym (cl-cc/compile::%lookup-numeric-binop-ctor-symbol '+ :generic)))
    (expect (symbolp sym) :to-be-truthy)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "lookup-numeric-binop-ctor-symbol-unknown-op"
  (expect (cl-cc/compile::%lookup-numeric-binop-ctor-symbol 'nonexistent-op :generic) :to-be-falsy))

(it-sequential "lookup-numeric-binop-ctor-symbol-missing-kind"
  (expect (cl-cc/compile::%lookup-numeric-binop-ctor-symbol '/ :fixnum) :to-be-falsy))

(it-sequential "numeric-binop-ctor-function-known-pair-returns-function"
  (let ((fn (cl-cc/compile::%numeric-binop-ctor-function '+ :generic)))
    (expect (functionp fn) :to-be-truthy)))

(it-sequential "numeric-binop-ctor-function-unknown-op-returns-nil"
  (expect (cl-cc/compile::%numeric-binop-ctor-function 'bogus-op :generic) :to-be-falsy))

(it-sequential "binop-ctor-returns-callable-function"
  (let ((fn (cl-cc/compile::binop-ctor '+)))
    (expect (functionp fn) :to-be-truthy)
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (expect (typep inst 'cl-cc/vm::vm-add) :to-be-truthy))))

(it-sequential "binop-ctor-signals-error-for-unknown-op"
  (signals error (cl-cc/compile::binop-ctor 'no-such-op)))

(it-sequential "ast-proven-type-fixnum-int-literal"
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-int :value 42))
         (ty   (cl-cc/compile::%ast-proven-type ctx node)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/compile::%proven-fixnum-type-p ty) :to-be-truthy)))

(it-sequential "ast-proven-type-nil-for-bignum"
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-int :value (1+ most-positive-fixnum))))
    (expect (cl-cc/compile::%ast-proven-type ctx node) :to-be-falsy)))

(it-sequential "ast-proven-type-ast-the-returns-declared"
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-the :type 'fixnum :value (make-ast-int :value 10)))
         (ty   (cl-cc/compile::%ast-proven-type ctx node)))
    (expect (cl-cc/compile::%proven-fixnum-type-p ty) :to-be-truthy)))

(it-sequential "ast-proven-type-nil-for-non-literal-node"
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-progn :forms (list (make-ast-int :value 1)))))
    (expect (cl-cc/compile::%ast-proven-type ctx node) :to-be-falsy)))

(it-sequential "numeric-binop-constructor-fixnum-path"
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-int :value 3))
         (rhs (make-ast-int :value 5))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (expect (functionp fn) :to-be-truthy)
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (expect (typep inst 'cl-cc/vm::vm-integer-add) :to-be-truthy))))

(it-sequential "numeric-binop-constructor-float-path"
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-quote :value 3.0))
         (rhs (make-ast-quote :value 2.0))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (expect (functionp fn) :to-be-truthy)
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (expect (typep inst 'cl-cc/vm::vm-float-add) :to-be-truthy))))

(it-sequential "numeric-binop-constructor-float-path-through-ast-the"
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-the :type 'float :value (make-ast-quote :value 3.0)))
         (rhs (make-ast-the :type 'float :value (make-ast-quote :value 2.0)))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (expect (functionp fn) :to-be-truthy)
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (expect (typep inst 'cl-cc/vm::vm-float-add) :to-be-truthy))))

(it-sequential "numeric-binop-constructor-generic-fallback"
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-var :name 'x))
         (rhs (make-ast-var :name 'y))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (expect (functionp fn) :to-be-truthy)
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (expect (typep inst 'cl-cc/vm::vm-add) :to-be-truthy))))

;;; ─── Branch typing / collapse helpers ──────────────────────────────────────

(it-sequential "branch-type-env-no-guard-var-returns-base"
  (let* ((ctx (make-codegen-ctx))
         (env (cl-cc/compile:ctx-type-env ctx))
         (result (cl-cc/compile::%branch-type-env ctx nil nil :then)))
    (expect result :to-be env)))

(it-sequential "branch-type-env-then-extends-with-guard-type"
  (let* ((ctx        (make-codegen-ctx))
         (guard-type (cl-cc/type:parse-type-specifier 'fixnum))
         (new-env    (cl-cc/compile::%branch-type-env ctx 'x guard-type :then)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type:type-env-lookup 'x new-env)
      (expect found-p :to-be-truthy)
      (expect (not (null scheme)) :to-be-truthy))))

(it-sequential "branch-type-env-else-no-prior-binding-returns-base"
  (let* ((ctx      (make-codegen-ctx))
         (base     (cl-cc/compile:ctx-type-env ctx))
         (guard-ty (cl-cc/type:parse-type-specifier 'fixnum))
         (result   (cl-cc/compile::%branch-type-env ctx 'z guard-ty :else)))
    (expect result :to-be base)))

(it-sequential "branch-type-env-unknown-branch-returns-base"
  (let* ((ctx        (make-codegen-ctx))
         (base       (cl-cc/compile:ctx-type-env ctx))
         (guard-type (cl-cc/type:parse-type-specifier 'fixnum))
         (result     (cl-cc/compile::%branch-type-env ctx 'x guard-type :bogus)))
    (expect result :to-be base)))

(it-sequential "case-of-case-collapse-branch-delegates-to-node"
  (let* ((cond  (make-ast-var :name 'p))
         (then  (make-ast-int :value 1))
         (else  (make-ast-int :value 2))
         (inner (make-ast-if :cond cond :then then :else else)))
    (expect (cl-cc/compile::%case-of-case-collapse-branch cond inner t) :to-be then)
    (expect (cl-cc/compile::%case-of-case-collapse-branch cond inner nil) :to-be else)))

;;; ─── %compile-if-branch ─────────────────────────────────────────────────────

(it-sequential "compile-if-branch-emits-move-to-dst"
  (let* ((ctx  (make-codegen-ctx))
         (dst  (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-if-branch
     (make-ast-int :value 99) ctx dst nil nil nil :then)
    (let ((insts (codegen-instructions ctx)))
      (expect (some (lambda (i) (and (typep i 'cl-cc/vm::vm-move)
                                          (eq dst (cl-cc/vm::vm-dst i))))
                         insts) :to-be-truthy))))

(it-sequential "compile-if-branch-emits-jump-when-label-supplied"
  (let* ((ctx  (make-codegen-ctx))
         (dst  (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-if-branch
     (make-ast-int :value 7) ctx dst nil nil nil :then "end_label_0")
    (let ((insts (codegen-instructions ctx)))
      (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) insts) :to-be-truthy))))

(it-sequential "compile-if-branch-no-jump-without-label"
  (let* ((ctx  (make-codegen-ctx))
         (dst  (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-if-branch
     (make-ast-int :value 5) ctx dst nil nil nil :else)
    (let ((insts (codegen-instructions ctx)))
      (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) insts) :to-be-falsy))))
