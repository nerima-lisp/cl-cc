;;;; tests/unit/compile/codegen-functions-tests.lisp — Codegen function/call unit tests

(in-package :cl-cc/test)

;; These tests exercise self-tail-loop lowering and closure/defun registration
;; paths that consult process-global compiler state during compilation. They
;; pass reliably in isolation but have shown parallel-only flakes in the full
;; suite, so keep this file on a dedicated serial child suite.

(defbefore :each (cl-cc-codegen-functions-serial-suite)
  (setf cl-cc/compile:*labels-boxed-fns* nil
         cl-cc/compile:*compiling-typed-fn* nil)
  (clrhash cl-cc/expand:*function-type-registry*)
  (clrhash cl-cc/expand:*declaim-inline-registry*))


(it-sequential "codegen-function-ref-returns-register"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-function :name 'car) ctx)))
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-closure-form-emits-vm-closure defun"
  (destructuring-bind (expected ast) (list :func-ref (cl-cc/ast:make-ast-defun
                                :name 'my-fn :params '(x)
                                :body (list (make-ast-var :name 'x))))
    (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'y (cl-cc/compile:make-register ctx))))
    (compile-ast ast ctx)
    (ecase expected
      (:closure
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy))
      (:func-ref
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref) :to-be-truthy)
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null))))))

(it-sequential "codegen-closure-form-emits-vm-closure lambda"
  (destructuring-bind (expected ast) (list :func-ref (make-ast-lambda
                                :params '(x)
                                :body (list (make-ast-var :name 'x))))
    (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'y (cl-cc/compile:make-register ctx))))
    (compile-ast ast ctx)
    (ecase expected
      (:closure
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy))
      (:func-ref
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref) :to-be-truthy)
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null))))))

(it-sequential "codegen-closure-form-emits-vm-closure capturing-lambda"
  (destructuring-bind (expected ast) (list :closure (make-ast-lambda
                                         :params '(x)
                                         :body (list (make-ast-var :name 'y))))
    (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'y (cl-cc/compile:make-register ctx))))
    (compile-ast ast ctx)
    (ecase expected
      (:closure
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy))
      (:func-ref
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref) :to-be-truthy)
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null))))))

(it-sequential "codegen-closure-form-emits-vm-closure labels"
  (destructuring-bind (expected ast) (list :closure (cl-cc/ast:make-ast-labels
                               :bindings (list (list 'g '(x) (make-ast-var :name 'x)))
                               :body (list (make-ast-call :func 'g
                                                           :args (list (make-ast-int :value 2))))))
    (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'y (cl-cc/compile:make-register ctx))))
    (compile-ast ast ctx)
    (ecase expected
      (:closure
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy))
      (:func-ref
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref) :to-be-truthy)
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null))))))

(it-sequential "codegen-labels-gensym-binding-name-compiles"
  (let* ((name (gensym "MAP"))
         (ctx (make-codegen-ctx))
         (ast (cl-cc/ast:make-ast-labels
               :bindings (list (list name '(x) (make-ast-var :name 'x)))
               :body (list (make-ast-call :func name
                                           :args (list (make-ast-int :value 2)))))))
    (compile-ast ast ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)))

(it-sequential "codegen-defun-registers-global"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defun :name 'my-fn
                                   :params '(x)
                                   :body (list (make-ast-var :name 'x)))
                  ctx)
    (expect (gethash 'my-fn (cl-cc/compile:ctx-global-functions ctx)) :to-be-truthy)))

(it-sequential "codegen-callable-inline-policy-merge-cases local-inline"
  (destructuring-bind (declarations global-policy pending-policy expected) (list '((inline f)) nil nil :inline)
    (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (when global-policy
      (setf (gethash 'f cl-cc/expand:*declaim-inline-registry*) global-policy))
    (expect (cl-cc/compile::%callable-inline-policy declarations
                                                       :name 'f
                                                       :pending-policy pending-policy) :to-be expected))))

(it-sequential "codegen-callable-inline-policy-merge-cases global-inline"
  (destructuring-bind (declarations global-policy pending-policy expected) (list nil :inline nil :inline)
    (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (when global-policy
      (setf (gethash 'f cl-cc/expand:*declaim-inline-registry*) global-policy))
    (expect (cl-cc/compile::%callable-inline-policy declarations
                                                       :name 'f
                                                       :pending-policy pending-policy) :to-be expected))))

(it-sequential "codegen-callable-inline-policy-merge-cases pending-inline"
  (destructuring-bind (declarations global-policy pending-policy expected) (list nil nil :inline :inline)
    (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (when global-policy
      (setf (gethash 'f cl-cc/expand:*declaim-inline-registry*) global-policy))
    (expect (cl-cc/compile::%callable-inline-policy declarations
                                                       :name 'f
                                                       :pending-policy pending-policy) :to-be expected))))

(it-sequential "codegen-callable-inline-policy-merge-cases notinline-wins-local-over-global"
  (destructuring-bind (declarations global-policy pending-policy expected) (list '((notinline f)) :inline nil :notinline)
    (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (when global-policy
      (setf (gethash 'f cl-cc/expand:*declaim-inline-registry*) global-policy))
    (expect (cl-cc/compile::%callable-inline-policy declarations
                                                       :name 'f
                                                       :pending-policy pending-policy) :to-be expected))))

(it-sequential "codegen-callable-inline-policy-merge-cases notinline-wins-global-over-pending"
  (destructuring-bind (declarations global-policy pending-policy expected) (list nil :notinline :inline :notinline)
    (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (when global-policy
      (setf (gethash 'f cl-cc/expand:*declaim-inline-registry*) global-policy))
    (expect (cl-cc/compile::%callable-inline-policy declarations
                                                       :name 'f
                                                       :pending-policy pending-policy) :to-be expected))))

(it-sequential "codegen-let-inline-declaration-propagates-to-lambda-closure"
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-let
               :bindings (list (cons 'f (make-ast-lambda
                                        :params '(x)
                                        :body (list (make-ast-var :name 'x)))))
               :declarations '((inline f))
               :body (list (make-ast-var :name 'f)))))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy inst) :to-be :inline))))

(it-sequential "codegen-defun-global-inline-policy-propagates-to-closure"
  (let ((ctx (make-codegen-ctx))
        (cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (setf (gethash 'my-fn cl-cc/expand:*declaim-inline-registry*) :inline)
    (compile-ast (cl-cc/ast:make-ast-defun :name 'my-fn
                                           :params '(x)
                                           :body (list (make-ast-var :name 'x)))
                 ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy inst) :to-be :inline))))

(it-sequential "codegen-defun-self-tail-call-loops"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (cl-cc/ast:make-ast-defun
      :name 'loop-fn
      :params '(x)
      :body (list (make-ast-if
                   :cond (make-ast-binop :op '=
                                         :lhs (make-ast-var :name 'x)
                                         :rhs (make-ast-int :value 0))
                   :then (make-ast-var :name 'x)
                   :else (make-ast-call
                          :func 'loop-fn
                          :args (list (make-ast-binop :op '-
                                                      :lhs (make-ast-var :name 'x)
                                                      :rhs (make-ast-int :value 1)))))))
     ctx)
    (let ((tail-call (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
          (loop-label (format nil "DEFUN_~A_0" 'loop-fn))
          (self-jump nil))
      (dolist (inst (codegen-instructions ctx))
        (when (and (typep inst 'cl-cc/vm::vm-jump)
                   (string= (cl-cc/vm::vm-label-name inst) loop-label))
          (setf self-jump inst)))
       (expect tail-call :to-be nil)
       (expect self-jump :to-be-truthy))))

(it-sequential "codegen-defun-self-tail-call-loop-snapshots-args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (cl-cc/ast:make-ast-defun
      :name 'swap-loop
      :params '(x y)
      :body (list (make-ast-if
                   :cond (make-ast-binop :op '=
                                         :lhs (make-ast-var :name 'x)
                                         :rhs (make-ast-int :value 0))
                   :then (make-ast-var :name 'y)
                   :else (make-ast-call
                          :func 'swap-loop
                          :args (list (make-ast-var :name 'y)
                                      (make-ast-binop :op '-
                                                      :lhs (make-ast-var :name 'x)
                                                      :rhs (make-ast-int :value 1)))))))
     ctx)
    (let ((tail-call (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
          (loop-label (format nil "DEFUN_~A_0" 'swap-loop))
          (self-jump nil)
          (move-count 0))
      (dolist (inst (codegen-instructions ctx))
        (when (typep inst 'cl-cc/vm::vm-move)
          (incf move-count))
        (when (and (typep inst 'cl-cc/vm::vm-jump)
                   (string= (cl-cc/vm::vm-label-name inst) loop-label))
          (setf self-jump inst)))
      (expect tail-call :to-be nil)
      (expect self-jump :to-be-truthy)
      ;; Two snapshot moves + two parameter rewrite moves.
      (expect (>= move-count 4) :to-be-truthy))))

(it-sequential "codegen-defvar-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defvar :name 'test-codegen-var
                                   :value (make-ast-int :value 99))
                 ctx)
    (expect (gethash 'test-codegen-var (cl-cc/compile:ctx-global-variables ctx)) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-const) :to-be-truthy)))

(it-sequential "codegen-defconstant-inlines-symbol-reference"
  (cl-cc/expand::compiler-macroexpand-all '(defconstant codegen-inline-constant 123))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name 'codegen-inline-constant) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal 123))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-get-global) :to-be nil)))
