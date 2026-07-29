;;;; tests/unit/compile/codegen-functions-callsite-tests.lisp — Call-site codegen tests
;;;;
;;;; Tests for apply/lambda/let/call/flet/rest-param and function resolution.
;;;; Suite: cl-cc-codegen-functions-serial-suite (defined in codegen-functions-tests.lisp).

(in-package :cl-cc/test)


;;; ─── apply ────────────────────────────────────────────────────────────────

(it-sequential "codegen-apply-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                                :func (make-ast-function :name '+)
                               :args (list (make-ast-quote :value '(1 2 3))))
                             ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-apply)) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-quoted-nil-compilation"
  (let* ((ctx (make-codegen-ctx))
          (reg (compile-ast (cl-cc/ast:make-ast-apply
                               :func (make-ast-function :name 'list)
                               :args (list (make-ast-int :value 1)
                                           (make-ast-int :value 2)
                                           (make-ast-quote :value nil)))
                             ctx)))
     (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
     (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-apply)) :to-be-truthy)
     (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-list-call-spread-emits-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                             :func (make-ast-function :name '+)
                             :args (list (make-ast-int :value 1)
                                         (make-ast-call :func (make-ast-var :name 'list)
                                                        :args (list (make-ast-int :value 2)
                                                                    (make-ast-int :value 3)))))
                           ctx))
         (call-inst (codegen-find-inst ctx 'cl-cc/vm::vm-call)))
    (expect call-inst :to-be-truthy)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-apply)) :to-be-truthy)
    (expect (= 3 (length (cl-cc/vm::vm-args call-inst))) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-local-list-call-spread-keeps-dynamic-apply"
  (let* ((ctx (make-codegen-ctx))
         (list-reg (cl-cc/compile:make-register ctx))
         (reg (progn
                (setf (cl-cc/compile:ctx-env ctx) (list (cons 'list list-reg)))
                (compile-ast (cl-cc/ast:make-ast-apply
                              :func (make-ast-function :name '+)
                              :args (list (make-ast-int :value 1)
                                          (make-ast-call :func (make-ast-var :name 'list)
                                                         :args (list (make-ast-int :value 2)
                                                                     (make-ast-int :value 3)))))
                             ctx))))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-tail-list-call-spread-emits-tail-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (progn
                (setf (cl-cc/compile:ctx-tail-position ctx) t)
                (compile-ast (cl-cc/ast:make-ast-apply
                               :func (make-ast-function :name '+)
                               :args (list (make-ast-int :value 1)
                                           (make-ast-call :func (make-ast-var :name 'list)
                                                          :args (list (make-ast-int :value 2)
                                                                      (make-ast-int :value 3)))))
                             ctx)))
         (tail-call-inst (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call)))
    (expect tail-call-inst :to-be-truthy)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-apply)) :to-be-truthy)
    (expect (= 3 (length (cl-cc/vm::vm-args tail-call-inst))) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-improper-quoted-list-falls-back-to-vm-apply"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                               :func (make-ast-function :name '+)
                               :args (list (make-ast-quote :value '(1 . 2))))
                             ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-the-wrapped-function-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                             :func (make-ast-the
                                    :type 'function
                                    :value (make-ast-function :name '+))
                             :args (list (make-ast-quote :value '(1 2 3))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-quoted-function-designator-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                             :func (make-ast-quote :value '+)
                             :args (list (make-ast-quote :value '(1 2 3))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-funcall-the-wrapped-function-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-call
                             :func 'funcall
                             :args (list (make-ast-the
                                          :type 'function
                                          :value (make-ast-function :name '+))
                                         (make-ast-quote :value '(1 2 3))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-not-the-wrapped-predicate-tests-nil-only"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-call
                             :func 'not
                             :args (list (make-ast-the
                                          :type 'boolean
                                          :value (make-ast-call
                                                  :func (make-ast-var :name 'numberp)
                                                  :args (list (make-ast-quote :value 42))))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-not) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-null-p) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-the-wrapped-literal-spread-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                             :func (make-ast-function :name '+)
                             :args (list (make-ast-the
                                          :type 'list
                                          :value (make-ast-quote :value '(1 2 3)))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-the-wrapped-list-call-spread-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                             :func (make-ast-function :name '+)
                             :args (list (make-ast-the
                                          :type 'list
                                          :value (make-ast-call
                                                  :func (make-ast-var :name 'list)
                                                  :args (list (make-ast-int :value 2)
                                                              (make-ast-int :value 3))))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-the-wrapped-list-call-with-wrapped-function-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-apply
                             :func (make-ast-function :name '+)
                             :args (list (make-ast-the
                                          :type 'list
                                          :value (make-ast-call
                                                  :func (make-ast-the
                                                         :type 'function
                                                         :value (make-ast-var :name 'list))
                                                  :args (list (make-ast-int :value 2)
                                                              (make-ast-int :value 3))))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-the-wrapped-local-list-call-keeps-dynamic-apply"
  (let* ((ctx (make-codegen-ctx))
         (list-reg (cl-cc/compile:make-register ctx))
         (reg (progn
                (setf (cl-cc/compile:ctx-env ctx) (list (cons 'list list-reg)))
                (compile-ast (cl-cc/ast:make-ast-apply
                              :func (make-ast-function :name '+)
                              :args (list (make-ast-the
                                           :type 'list
                                           :value (make-ast-call
                                                   :func (make-ast-var :name 'list)
                                                   :args (list (make-ast-int :value 2)
                                                               (make-ast-int :value 3))))))
                             ctx))))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-tail-literal-spread-emits-tail-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (progn
                (setf (cl-cc/compile:ctx-tail-position ctx) t)
                (compile-ast (cl-cc/ast:make-ast-apply
                              :func (make-ast-function :name '+)
                              :args (list (make-ast-quote :value '(1 2 3))))
                             ctx))))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-truthy)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-call)) :to-be-truthy)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-apply)) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-tail-dynamic-spread-emits-tail-apply"
  (let* ((ctx (make-codegen-ctx))
         (xs-reg (cl-cc/compile:make-register ctx))
          (reg (progn
                 (setf (cl-cc/compile:ctx-env ctx) (list (cons 'xs xs-reg)))
                 (setf (cl-cc/compile:ctx-tail-position ctx) t)
                 (compile-ast (cl-cc/ast:make-ast-apply
                               :func (make-ast-function :name '+)
                              :args (list (make-ast-var :name 'xs)))
                             ctx)))
         (apply-inst (codegen-find-inst ctx 'cl-cc/vm::vm-apply)))
    (expect apply-inst :to-be-truthy)
    (expect (cl-cc/vm::vm-tail-p apply-inst) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-apply-run list-only"
  (destructuring-bind (expected code) (list 6 "(apply #'+ '(1 2 3))")
    (assert-run= expected code)))

(it-sequential "codegen-apply-run leading-args"
  (destructuring-bind (expected code) (list 10 "(apply #'+ 1 2 '(3 4))")
    (assert-run= expected code)))

(it-sequential "codegen-apply-run quoted-nil"
  (destructuring-bind (expected code) (list 3 "(apply #'+ 1 2 nil)")
    (assert-run= expected code)))

(it-sequential "codegen-apply-run list-call"
  (destructuring-bind (expected code) (list 10 "(let ((c 3) (d 4)) (apply #'+ 1 2 (list c d)))")
    (assert-run= expected code)))

(it-sequential "codegen-apply-run local-list-call"
  (destructuring-bind (expected code) (list 36 "(flet ((list (a b) (cons 12 (cons 23 nil)))) (apply #'+ 1 (list 2 3)))")
    (assert-run= expected code)))

;;; ─── lambda ───────────────────────────────────────────────────────────────

(it-sequential "codegen-lambda-returns-register"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-lambda :params '(x)
                                             :body (list (make-ast-int :value 1)))
                           ctx)))
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-let-lambda-escape-determines-closure noescape"
  (destructuring-bind (ast closure-p) (list (make-ast-let
            :bindings (list (cons 'f (make-ast-lambda
                                      :params '(x)
                                      :body (list (make-ast-var :name 'x)))))
            :body (list (make-ast-call :func (make-ast-var :name 'f)
                                       :args (list (make-ast-int :value 7))))) nil)
    (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast ast ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (if closure-p
          (progn (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)
                 (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy))
          (progn (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)
                 (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))))))

(it-sequential "codegen-let-lambda-escape-determines-closure escaped"
  (destructuring-bind (ast closure-p) (list (make-ast-let
            :bindings (list (cons 'f (make-ast-lambda
                                      :params '(x)
                                      :body (list (make-ast-var :name 'x)))))
            :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'f)))
                        (make-ast-call :func (make-ast-var :name 'f)
                                       :args (list (make-ast-int :value 9))))) t)
    (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast ast ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (if closure-p
          (progn (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)
                 (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy))
          (progn (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)
                 (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))))))

(it-sequential "codegen-let-dynamic-extent-closure-declaration-controls-noescape no-declaration"
  (destructuring-bind (declarations noescape-p) (list nil nil)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'f (make-ast-lambda
                                           :params '(x)
                                           :body (list (make-ast-binop
                                                        :op '+
                                                        :lhs (make-ast-var :name 'x)
                                                        :rhs (make-ast-int :value 1))))))
                :declarations declarations
                :body (list (make-ast-let
                             :bindings (list (cons 'caller
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func (make-ast-var :name 'f)
                                                                  :args (list (make-ast-int :value 7)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'caller)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-add) :to-be-truthy)
    (if noescape-p
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null))
        (progn
          (expect (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                           (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref)) :to-be-truthy)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy))))))

(it-sequential "codegen-let-dynamic-extent-closure-declaration-controls-noescape with-dynamic-extent"
  (destructuring-bind (declarations noescape-p) (list '((dynamic-extent f)) t)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'f (make-ast-lambda
                                           :params '(x)
                                           :body (list (make-ast-binop
                                                        :op '+
                                                        :lhs (make-ast-var :name 'x)
                                                        :rhs (make-ast-int :value 1))))))
                :declarations declarations
                :body (list (make-ast-let
                             :bindings (list (cons 'caller
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func (make-ast-var :name 'f)
                                                                  :args (list (make-ast-int :value 7)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'caller)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-add) :to-be-truthy)
    (if noescape-p
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null))
        (progn
          (expect (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                           (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref)) :to-be-truthy)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy))))))

;;; ─── call ─────────────────────────────────────────────────────────────────

(it-sequential "codegen-call-known-function"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'cons
                                 :args (list (make-ast-int :value 1)
                                             (make-ast-int :value 2)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)))

(it-sequential "codegen-call-list-accessor-emits-instruction car"
  (destructuring-bind (func inst-type) (list 'car 'cl-cc/vm::vm-car)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func func
                                 :args (list (make-ast-quote :value '(1 2))))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-call-list-accessor-emits-instruction cdr"
  (destructuring-bind (func inst-type) (list 'cdr 'cl-cc/vm::vm-cdr)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func func
                                 :args (list (make-ast-quote :value '(1 2))))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

;;; ─── function symbol resolution ───────────────────────────────────────────

(it-sequential "resolve-func-sym-dispatch global-no-env"
  (destructuring-bind (emits-const sym local-reg is-global expected-val) (list t 'foo nil nil 'foo)
    (let* ((ctx (make-codegen-ctx)))
    (when local-reg
      (push (cons sym local-reg) (cl-cc/compile:ctx-env ctx)))
    (when is-global
      (setf (gethash sym (cl-cc/compile:ctx-global-functions ctx)) t))
    (let ((result (cl-cc/compile::%resolve-func-sym-reg sym ctx))
          (inst   (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (if emits-const
          (progn (expect inst :to-be-truthy)
                 (expect (cl-cc::vm-const-value inst) :to-be expected-val))
          (progn (expect inst :to-be-null)
                 (expect result :to-be expected-val)))))))

(it-sequential "resolve-func-sym-dispatch global-shadows-env"
  (destructuring-bind (emits-const sym local-reg is-global expected-val) (list t 'gfn :r77 t 'gfn)
    (let* ((ctx (make-codegen-ctx)))
    (when local-reg
      (push (cons sym local-reg) (cl-cc/compile:ctx-env ctx)))
    (when is-global
      (setf (gethash sym (cl-cc/compile:ctx-global-functions ctx)) t))
    (let ((result (cl-cc/compile::%resolve-func-sym-reg sym ctx))
          (inst   (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (if emits-const
          (progn (expect inst :to-be-truthy)
                 (expect (cl-cc::vm-const-value inst) :to-be expected-val))
          (progn (expect inst :to-be-null)
                 (expect result :to-be expected-val)))))))

(it-sequential "resolve-func-sym-dispatch local-env"
  (destructuring-bind (emits-const sym local-reg is-global expected-val) (list nil 'my-fn :r42 nil :r42)
    (let* ((ctx (make-codegen-ctx)))
    (when local-reg
      (push (cons sym local-reg) (cl-cc/compile:ctx-env ctx)))
    (when is-global
      (setf (gethash sym (cl-cc/compile:ctx-global-functions ctx)) t))
    (let ((result (cl-cc/compile::%resolve-func-sym-reg sym ctx))
          (inst   (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (if emits-const
          (progn (expect inst :to-be-truthy)
                 (expect (cl-cc::vm-const-value inst) :to-be expected-val))
          (progn (expect inst :to-be-null)
                 (expect result :to-be expected-val)))))))

(it-sequential "compile-closure-body"
  (let* ((ctx       (make-codegen-ctx))
         (base-env  (list (cons 'outer :r0)))
         (param-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) base-env)
    (cl-cc/compile::%compile-closure-body ctx '(p) (list param-reg)
                                   (list (make-ast-int :value 1)) base-env)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-ret) :to-be-truthy)
    (expect (cl-cc/compile:ctx-env ctx) :to-equal base-env)
    (expect (assoc 'p (cl-cc/compile:ctx-env ctx)) :to-be-null)))

;;; ─── flet ─────────────────────────────────────────────────────────────────

(it-sequential "codegen-flet-closure-vs-func-ref noescape"
  (destructuring-bind (scenario) (list :noescape)
    (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (ecase scenario
                  (:noescape
                   (make-ast-flet
                    :bindings (list (list 'f '(x) (make-ast-var :name 'x)))
                    :body (list (make-ast-call :func 'f :args (list (make-ast-int :value 5))))))
                  (:capturing
                   (make-ast-let
                    :bindings (list (cons 'y (make-ast-int :value 9)))
                    :body (list (make-ast-flet
                                 :bindings (list (list 'f '(x) (make-ast-var :name 'y)))
                                 :body (list (make-ast-call :func 'f :args (list (make-ast-int :value 5)))))))))
                ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (ecase scenario
        (:noescape
         (expect (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref) :to-be-truthy)
         (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null))
        (:capturing
         (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)))))))

(it-sequential "codegen-flet-closure-vs-func-ref capturing"
  (destructuring-bind (scenario) (list :capturing)
    (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (ecase scenario
                  (:noescape
                   (make-ast-flet
                    :bindings (list (list 'f '(x) (make-ast-var :name 'x)))
                    :body (list (make-ast-call :func 'f :args (list (make-ast-int :value 5))))))
                  (:capturing
                   (make-ast-let
                    :bindings (list (cons 'y (make-ast-int :value 9)))
                    :body (list (make-ast-flet
                                 :bindings (list (list 'f '(x) (make-ast-var :name 'y)))
                                 :body (list (make-ast-call :func 'f :args (list (make-ast-int :value 5)))))))))
                ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (ecase scenario
        (:noescape
         (expect (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref) :to-be-truthy)
         (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null))
        (:capturing
         (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)))))))

;;; ─── rest params ──────────────────────────────────────────────────────────

(it-sequential "codegen-rest-params-stack-alloc-classification local-consumer"
  (destructuring-bind (expected declarations body) (list t nil (list (make-ast-call :func 'car :args (list (make-ast-var :name 'args)))))
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :declarations declarations
                :body body)))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (if expected
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-truthy)
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-null))))))

(it-sequential "codegen-rest-params-stack-alloc-classification direct-return"
  (destructuring-bind (expected declarations body) (list nil nil (list (make-ast-var :name 'args)))
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :declarations declarations
                :body body)))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (if expected
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-truthy)
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-null))))))

(it-sequential "codegen-rest-params-stack-alloc-classification dynamic-extent"
  (destructuring-bind (expected declarations body) (list t '((dynamic-extent args)) (list (make-ast-var :name 'args)))
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :declarations declarations
                :body body)))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (if expected
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-truthy)
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-null))))))

(it-sequential "codegen-rest-params-stack-alloc-classification inner-capture"
  (destructuring-bind (expected declarations body) (list nil nil (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'args)))))
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :declarations declarations
                :body body)))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (if expected
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-truthy)
          (expect (cl-cc/vm::vm-closure-rest-stack-alloc-p inst) :to-be-null))))))
