;;;; tests/unit/compile/codegen-tests.lisp — Codegen Unit Tests
;;;
;;; Direct tests for compile-ast methods and codegen helpers.
;;; Tests compile individual AST nodes to VM instructions without
;;; going through the full pipeline (parser + macro expander).

(in-package :cl-cc/test)




;;; ─── Helpers ────────────────────────────────────────────────────────────

(defun make-codegen-ctx ()
  "Create a fresh compiler context for codegen tests.

Codegen unit tests assert on exact label names such as `DEFUN_FOO_0`. Those
labels become order-dependent if a prior REPL-oriented test leaked
`*repl-label-counter*` or related REPL globals into `compiler-context`
initialization. Bind the REPL state hooks to NIL here so unit tests always get a
stable, isolated context."
  (let ((cl-cc/compile:*repl-label-counter* nil)
        (cl-cc/compile:*repl-global-variables* nil)
        (cl-cc/compile:*repl-capture-label-counter* nil))
    (make-instance 'cl-cc/compile:compiler-context)))

(defun codegen-instructions (ctx)
  "Return the emitted instructions from CTX (in order)."
  (nreverse (copy-list (cl-cc/compile:ctx-instructions ctx))))

(defun codegen-find-inst (ctx type)
  "Find the first instruction of TYPE in CTX's emitted instructions."
  (find-if (lambda (i) (typep i type)) (codegen-instructions ctx)))

(defun %compile-expression-vm-instructions (expression)
  "Compile EXPRESSION for the VM target and return its emitted instructions."
  (cl-cc/compile:compilation-result-vm-instructions
   (cl-cc:compile-expression expression :target :vm)))

(defun %vm-instruction-present-p (instructions instruction-type)
  "Return true when INSTRUCTIONS contains an instance of INSTRUCTION-TYPE."
  (find-if (lambda (inst) (typep inst instruction-type)) instructions))

;;; ─── compile-ast: ast-int ───────────────────────────────────────────────

(it-sequential "codegen-integer-literal-returns-keyword-register"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-int :value 42) ctx)))
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-local-var-returns-bound-register"
  (let* ((ctx (make-codegen-ctx))
         (reg :R99))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x reg)))
    (let ((result (compile-ast (make-ast-var :name 'x) ctx)))
      (expect result :to-be reg))))

(it-sequential "codegen-unbound-var-signals-error"
  (let ((ctx (make-codegen-ctx)))
    (signals error (compile-ast (make-ast-var :name 'nonexistent-var-xyz) ctx))))

(it-sequential "codegen-int-emits-const-value positive"
  (destructuring-bind (n) (list 42)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value n) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (= n (cl-cc::vm-const-value inst)) :to-be-truthy)))))

(it-sequential "codegen-int-emits-const-value zero"
  (destructuring-bind (n) (list 0)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value n) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (= n (cl-cc::vm-const-value inst)) :to-be-truthy)))))

(it-sequential "codegen-int-emits-const-value negative"
  (destructuring-bind (n) (list -1)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value n) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (= n (cl-cc::vm-const-value inst)) :to-be-truthy)))))

;;; ─── compile-ast: ast-var ───────────────────────────────────────────────

(it-sequential "codegen-var-constant-emits-const t"
  (destructuring-bind (name) (list t)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name name) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal name)))))

(it-sequential "codegen-var-constant-emits-const nil"
  (destructuring-bind (name) (list nil)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name name) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal name)))))

(it-sequential "codegen-var-constant-emits-const keyword"
  (destructuring-bind (name) (list :foo)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name name) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal name)))))


;;; ─── compile-ast: ast-quote ─────────────────────────────────────────────

(it-sequential "codegen-quote-forms-emit-const-value symbol"
  (destructuring-bind (datum) (list 'hello)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal datum)))))

(it-sequential "codegen-quote-forms-emit-const-value list"
  (destructuring-bind (datum) (list '(1 2 3))
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal datum)))))

(it-sequential "codegen-quote-forms-emit-const-value nil"
  (destructuring-bind (datum) (list nil)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal datum)))))

(it-sequential "codegen-quote-forms-emit-const-value string"
  (destructuring-bind (datum) (list "hello")
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal datum)))))

(it-sequential "codegen-quote-forms-emit-const-value empty-str"
  (destructuring-bind (datum) (list "")
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal datum)))))

(it-sequential "codegen-quote-forms-emit-const-value char"
  (destructuring-bind (datum) (list #\a)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal datum)))))

(it-sequential "codegen-string-literal-pool-deduplicates-string-equal-values"
  (let ((ctx (make-codegen-ctx))
        (first-literal (copy-seq "shared"))
        (second-literal (copy-seq "shared")))
    (let ((cl-cc/compile:*string-literal-pool* (make-hash-table :test #'equal)))
      (let ((first-reg (compile-ast (make-ast-quote :value first-literal) ctx))
            (second-reg (compile-ast (make-ast-quote :value second-literal) ctx)))
        (expect second-reg :to-be first-reg)
        (let ((consts (remove-if-not (lambda (inst)
                                       (typep inst 'cl-cc/vm::vm-const))
                                     (codegen-instructions ctx))))
          (expect (= 1 (length consts)) :to-be-truthy)
          (expect (cl-cc::vm-const-value (first consts)) :to-equal "shared"))))))

(it-sequential "codegen-string-literal-pool-does-not-deduplicate-non-strings"
  (let ((ctx (make-codegen-ctx)))
    (let ((cl-cc/compile:*string-literal-pool* (make-hash-table :test #'equal)))
      (compile-ast (make-ast-quote :value 'same-symbol) ctx)
      (compile-ast (make-ast-quote :value 'same-symbol) ctx)
      (let ((consts (remove-if-not (lambda (inst)
                                     (typep inst 'cl-cc/vm::vm-const))
                                   (codegen-instructions ctx))))
        (expect (= 1 (length consts)) :to-be-truthy)))))

;;; ─── compile-ast: ast-binop ─────────────────────────────────────────────

(it-sequential "codegen-binop-emits-correct-instruction add"
  (destructuring-bind (op inst-type) (list '+ 'cl-cc/vm::vm-add)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-binop-emits-correct-instruction sub"
  (destructuring-bind (op inst-type) (list '- 'cl-cc/vm::vm-sub)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-binop-emits-correct-instruction mul"
  (destructuring-bind (op inst-type) (list '* 'cl-cc/vm::vm-mul)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-binop-emits-correct-instruction lt"
  (destructuring-bind (op inst-type) (list '< 'cl-cc/vm::vm-lt)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-binop-emits-correct-instruction gt"
  (destructuring-bind (op inst-type) (list '> 'cl-cc/vm::vm-gt)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-binop-emits-correct-instruction eq"
  (destructuring-bind (op inst-type) (list '= 'cl-cc/vm::vm-num-eq)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

;;; ─── codegen helpers ─────────────────────────────────────────────────────

(it-sequential "codegen-make-register-returns-unique-keywords"
  (let* ((ctx (make-codegen-ctx))
         (r1 (cl-cc/compile:make-register ctx))
         (r2 (cl-cc/compile:make-register ctx)))
    (expect (keywordp r1) :to-be-truthy)
    (expect (keywordp r2) :to-be-truthy)
    (expect (eq r1 r2) :to-be-falsy)))

(it-sequential "codegen-emit-appends-one-instruction"
  (let ((ctx (make-codegen-ctx)))
    (cl-cc/compile:emit ctx (cl-cc::make-vm-const :dst :R0 :value 42))
    (expect (= 1 (length (codegen-instructions ctx))) :to-be-truthy)))

(it-sequential "codegen-make-label-returns-unique-strings"
  (let* ((ctx (make-codegen-ctx))
         (l1 (cl-cc/compile:make-label ctx "TEST"))
         (l2 (cl-cc/compile:make-label ctx "TEST")))
    (expect (string= l1 l2) :to-be-falsy)))


(it-sequential "codegen-make-compile-opts-uses-global-speed-policy-by-default"
  (let* ((old (gethash 'speed cl-cc/expand:*declaim-optimize-registry*))
         (_ignored old))
    (unwind-protect
         (progn
           (setf (gethash 'speed cl-cc/expand:*declaim-optimize-registry*) 3)
           (let* ((opts (cl-cc/compile::%make-compile-opts))
                  (speed (getf opts :speed)))
             (expect (= 3 speed) :to-be-truthy)))
      (setf (gethash 'speed cl-cc/expand:*declaim-optimize-registry*) old))))


(it-sequential "codegen-maybe-bump-opts-speed-from-ast-defun-declaration"
  (let* ((opts (list :speed nil))
         (ast (cl-cc/ast:make-ast-defun
               :name 'f
               :params '(x)
               :optional-params nil
               :rest-param nil
               :key-params nil
               :declarations '((optimize (speed 3)))
               :documentation nil
               :body (list (make-ast-var :name 'x)))))
    (cl-cc/compile::%maybe-bump-opts-speed-from-ast opts ast)
    (expect (= 3 (getf opts :speed)) :to-be-truthy)))

(it-sequential "codegen-maybe-bump-opts-speed-from-ast-does-not-lower-existing-speed"
  (let* ((opts (list :speed 3))
         (ast (cl-cc/ast:make-ast-defun
               :name 'f
               :params '(x)
               :optional-params nil
               :rest-param nil
               :key-params nil
               :declarations '((optimize (speed 1)))
               :documentation nil
               :body (list (make-ast-var :name 'x)))))
    (cl-cc/compile::%maybe-bump-opts-speed-from-ast opts ast)
    (expect (= 3 (getf opts :speed)) :to-be-truthy)))

;;; ─── compile-ast: ast-call fast paths ────────────────────────────────────

(it-sequential "codegen-call-higher-order-fast-path funcall"
  (destructuring-bind (fn-sym inst-type) (list 'funcall 'cl-cc/vm::vm-call)
    (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'fn fn-reg)))
    (compile-ast (make-ast-call :func fn-sym
                                :args (list (make-ast-var :name 'fn)
                                            (make-ast-int :value 1)))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call) :to-be-null))))

(it-sequential "codegen-call-higher-order-fast-path apply"
  (destructuring-bind (fn-sym inst-type) (list 'apply 'cl-cc/vm::vm-apply)
    (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'fn fn-reg)))
    (compile-ast (make-ast-call :func fn-sym
                                :args (list (make-ast-var :name 'fn)
                                            (make-ast-int :value 1)))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call) :to-be-null))))

(it-sequential "codegen-call-noescape-array-length-emits-const"
  (let* ((ctx (make-codegen-ctx))
         ;; Register a noescape array binding of size 3
         (arr-reg (cl-cc/compile:make-register ctx))
         (z-reg   (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 3 nil z-reg z-reg z-reg))))
    (compile-ast (make-ast-call :func 'array-length
                                :args (list (make-ast-var :name 'arr)))
                 ctx)
    (let ((const-inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect const-inst :to-be-truthy)
      (expect (= 3 (cl-cc::vm-const-value const-inst)) :to-be-truthy))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))

(it-sequential "codegen-continuation-forms-emit-dedicated-instructions call/cc"
  (destructuring-bind (expression expected-inst) (list '(call/cc (lambda (k) (funcall k 42))) 'cl-cc/vm::vm-call/cc)
    (let ((instructions (%compile-expression-vm-instructions expression)))
    (expect (%vm-instruction-present-p instructions expected-inst) :to-be-truthy))))

(it-sequential "codegen-continuation-forms-emit-dedicated-instructions call-with-continuation-prompt"
  (destructuring-bind (expression expected-inst) (list '(call-with-continuation-prompt (lambda () 1) 'reset) 'cl-cc/vm::vm-call-with-prompt)
    (let ((instructions (%compile-expression-vm-instructions expression)))
    (expect (%vm-instruction-present-p instructions expected-inst) :to-be-truthy))))

(it-sequential "codegen-continuation-forms-emit-dedicated-instructions abort-to-prompt"
  (destructuring-bind (expression expected-inst) (list '(abort-to-prompt 'reset 42) 'cl-cc/vm::vm-abort-to-prompt)
    (let ((instructions (%compile-expression-vm-instructions expression)))
    (expect (%vm-instruction-present-p instructions expected-inst) :to-be-truthy))))

(it-sequential "codegen-block-return-from-keeps-direct-jump"
  (let* ((ctx (make-codegen-ctx))
         (node (make-ast-block :name nil
                               :body (list (make-ast-return-from
                                            :name nil
                                            :value (make-ast-int :value 5))))))
    (compile-ast node ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call/cc) :to-be-null)))

;;; ─── optimize-ast / %loc macro ───────────────────────────────────────────

(it-sequential "optimize-ast-preserves-source-location"
  (let* ((src (make-ast-if :cond (make-ast-int :value 1 :source-line 5)
                            :then (make-ast-int :value 1)
                            :else (make-ast-int :value 0)
                            :source-file "test.lisp"
                            :source-line 5
                            :source-column 2))
         (result (cl-cc/compile:optimize-ast src)))
    (expect (cl-cc::ast-source-file result) :to-equal "test.lisp")
    (expect (= 5 (cl-cc::ast-source-line result)) :to-be-truthy)
    (expect (= 2 (cl-cc::ast-source-column result)) :to-be-truthy)))

(it-sequential "optimize-ast-recursively-folds-ast-progn"
  (let* ((node (make-ast-progn :forms (list (make-ast-binop :op '+
                                                             :lhs (make-ast-int :value 2)
                                                             :rhs (make-ast-int :value 3)))))
         (result (cl-cc/compile:optimize-ast node)))
    (expect (typep result 'cl-cc::ast-progn) :to-be-truthy)
    (expect (typep (first (cl-cc/ast:ast-progn-forms result)) 'cl-cc::ast-int) :to-be-truthy)
    (expect (= 5 (cl-cc/ast:ast-int-value (first (cl-cc/ast:ast-progn-forms result)))) :to-be-truthy)))

(it-sequential "optimize-ast-folds-defconstant-backed-binop"
  (cl-cc/expand::compiler-macroexpand-all '(defconstant +optimize-ast-inline-constant+ 41))
  (let* ((node (make-ast-binop :op '+
                               :lhs (make-ast-var :name '+optimize-ast-inline-constant+)
                               :rhs (make-ast-int :value 1)))
         (result (cl-cc/compile:optimize-ast node)))
    (expect (typep result 'cl-cc::ast-int) :to-be-truthy)
    (expect (= 42 (cl-cc/ast:ast-int-value result)) :to-be-truthy)))

(it-sequential "optimize-ast-keeps-shadowed-defconstant-as-variable let-body"
  (destructuring-bind (node body-reader) (list (make-ast-let :bindings (list (cons 'optimize-shadowed-constant
                                               (make-ast-int :value 2)))
                         :declarations nil
                         :body (list (make-ast-var :name 'optimize-shadowed-constant))) #'cl-cc/ast:ast-let-body)
    (cl-cc/expand::compiler-macroexpand-all '(defconstant optimize-shadowed-constant 99)) (let* ((result (cl-cc/compile:optimize-ast node))
         (body-form (first (funcall body-reader result))))
    (expect (typep body-form 'cl-cc::ast-var) :to-be-truthy)
    (expect (cl-cc/ast:ast-var-name body-form) :to-be 'optimize-shadowed-constant))))

(it-sequential "optimize-ast-keeps-shadowed-defconstant-as-variable lambda-key-param"
  (destructuring-bind (node body-reader) (list (make-ast-lambda :params nil
                            :optional-params nil
                            :rest-param nil
                            :key-params (list (list 'optimize-shadowed-constant nil nil nil))
                            :declarations nil
                            :body (list (make-ast-var :name 'optimize-shadowed-constant))) #'cl-cc/ast:ast-lambda-body)
    (cl-cc/expand::compiler-macroexpand-all '(defconstant optimize-shadowed-constant 99)) (let* ((result (cl-cc/compile:optimize-ast node))
         (body-form (first (funcall body-reader result))))
    (expect (typep body-form 'cl-cc::ast-var) :to-be-truthy)
    (expect (cl-cc/ast:ast-var-name body-form) :to-be 'optimize-shadowed-constant))))

(it-sequential "optimize-ast-keeps-shadowed-defconstant-as-variable defun-optional-param"
  (destructuring-bind (node body-reader) (list (cl-cc/ast:make-ast-defun :name 'optimize-shadowed-constant-user
                                     :params nil
                                     :optional-params (list (list 'optimize-shadowed-constant nil nil))
                                     :rest-param nil
                                     :key-params nil
                                     :declarations nil
                                     :documentation nil
                                     :body (list (make-ast-var :name 'optimize-shadowed-constant))) #'cl-cc/ast:ast-defun-body)
    (cl-cc/expand::compiler-macroexpand-all '(defconstant optimize-shadowed-constant 99)) (let* ((result (cl-cc/compile:optimize-ast node))
         (body-form (first (funcall body-reader result))))
    (expect (typep body-form 'cl-cc::ast-var) :to-be-truthy)
    (expect (cl-cc/ast:ast-var-name body-form) :to-be 'optimize-shadowed-constant))))

;;; ─── %let-binding-special-p ─────────────────────────────────────────────

(it-sequential "let-binding-special-p-dispatch earmuffs-and-global"
  (destructuring-bind (sym register-p expected) (list '*x* t t)
    (let ((ctx (make-codegen-ctx)))
    (when register-p
      (setf (gethash sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (expect (cl-cc/compile::%let-binding-special-p sym ctx) :to-equal expected))))

(it-sequential "let-binding-special-p-dispatch earmuffs-no-registration"
  (destructuring-bind (sym register-p expected) (list '*unregistered* nil nil)
    (let ((ctx (make-codegen-ctx)))
    (when register-p
      (setf (gethash sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (expect (cl-cc/compile::%let-binding-special-p sym ctx) :to-equal expected))))

(it-sequential "let-binding-special-p-dispatch no-earmuffs-global"
  (destructuring-bind (sym register-p expected) (list 'plain t nil)
    (let ((ctx (make-codegen-ctx)))
    (when register-p
      (setf (gethash sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (expect (cl-cc/compile::%let-binding-special-p sym ctx) :to-equal expected))))

;;; ─── %let-noescape-closure ──────────────────────────────────────────────

(it-sequential "let-noescape-closure-unmutated-lambda-is-eligible"
  (let* ((lam  (make-ast-lambda :params '(x)
                                :optional-params nil
                                :rest-param nil
                                :key-params nil
                                :body (list (make-ast-var :name 'x))))
         (body (list (make-ast-call :func (make-ast-var :name 'f)
                                    :args (list (make-ast-int :value 1)))))
         (result (cl-cc/compile::%let-noescape-closure 'f lam nil nil nil body)))
    (expect result :to-be lam)))

(it-sequential "let-noescape-closure-the-wrapped-lambda-is-eligible"
  (let* ((lam (make-ast-lambda :params '(x)
                               :optional-params nil
                               :rest-param nil
                               :key-params nil
                               :body (list (make-ast-var :name 'x))))
         (wrapped (make-ast-the :type 'function :value lam))
         (body (list (make-ast-call :func (make-ast-var :name 'f)
                                    :args (list (make-ast-int :value 1)))))
         (result (cl-cc/compile::%let-noescape-closure 'f wrapped nil nil nil body)))
    (expect result :to-be lam)))

(it-sequential "let-noescape-closure-mutated-binding-returns-nil"
  (let* ((lam (make-ast-lambda :params '(x)
                               :optional-params nil
                               :rest-param nil
                               :key-params nil
                               :body (list (make-ast-var :name 'x))))
         (body (list (make-ast-int :value 1))))
    (expect (cl-cc/compile::%let-noescape-closure 'f lam nil '(f) nil body) :to-be-null)))

;;; ─── %let-noescape-array-size ───────────────────────────────────────────

(it-sequential "let-noescape-array-size-non-make-array-returns-nil"
  (let ((expr (make-ast-int :value 5)))
    (expect (cl-cc/compile::%let-noescape-array-size 'arr expr nil nil nil nil) :to-be-null)))

(it-sequential "let-noescape-cons-p-non-cons-expr-returns-nil"
  (let ((expr (make-ast-int :value 5)))
    (expect (cl-cc/compile::%let-noescape-cons-p 'c expr nil nil nil nil) :to-be-null)))

;;; ─── FR-864/892 MV buffer and load-time-value codegen ─────────────────────

(it-sequential "codegen-nth-value-emits-o1-buffer-read"
  (let* ((ctx (make-codegen-ctx))
         (node (make-ast-call :func '%nth-value
                              :args (list (make-ast-int :value 1)
                                          (cl-cc/ast:make-ast-values
                                           :forms (list (make-ast-int :value 10)
                                                        (make-ast-int :value 20)))))))
    (compile-ast node ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-nth-value) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values-to-list) :to-be-falsy)))

(it-sequential "codegen-load-time-value-records-cell"
  (let ((ctx (make-codegen-ctx))
        (cl-cc/compile::*load-time-value-cells* nil)
        (cl-cc/compile::*next-load-time-value-cell-id* 0))
    (compile-ast (make-ast-call :func '%load-time-value
                                :args (list (make-ast-call :func '+
                                                          :args (list (make-ast-int :value 1)
                                                                      (make-ast-int :value 2)))
                                            (make-ast-var :name nil)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-load-time-value)))
      (expect inst :to-be-truthy)
      (expect (= 0 (cl-cc/vm::vm-load-time-value-cell-id inst)) :to-be-truthy)
      (expect (cl-cc/vm::vm-load-time-value-cell-form
                     (first cl-cc/compile::*load-time-value-cells*)) :to-equal '(+ 1 2)))))
