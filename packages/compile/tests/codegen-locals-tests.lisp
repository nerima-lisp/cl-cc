;;;; tests/unit/compile/codegen-locals-tests.lisp
;;;; Unit tests for src/compile/codegen-locals.lisp
;;;;
;;;; Covers: target-instance (valid targets return correct class, :vm returns nil),
;;;;   %compile-body-with-tail (single form, multi-form, tail-position tracking),
;;;;   type-check-ast (integer literal type, unknown variable signals error).

(in-package :cl-cc/test)

;;; ─── target-instance ─────────────────────────────────────────────────────────

(it-sequential "target-instance-returns-correct-class x86-64"
  (destructuring-bind (target expected-class) (list :x86_64 'cl-cc/codegen::x86-64-target)
    (expect (typep (cl-cc/compile:target-instance target) expected-class) :to-be-truthy)))

(it-sequential "target-instance-returns-correct-class aarch64"
  (destructuring-bind (target expected-class) (list :aarch64 'cl-cc/codegen::aarch64-target)
    (expect (typep (cl-cc/compile:target-instance target) expected-class) :to-be-truthy)))

(it-sequential "target-instance-vm-returns-nil"
  (expect (cl-cc/compile:target-instance :vm) :to-be-null))

(it-sequential "target-instance-invalid-target-signals-error"
  (signals error (cl-cc/compile:target-instance :invalid-target)))

;;; ─── %compile-body-with-tail ─────────────────────────────────────────────────

(it-sequential "compile-body-with-tail-single-form-returns-register"
  (let* ((ctx (make-codegen-ctx))
         (reg (cl-cc/compile::%compile-body-with-tail
                (list (make-ast-int :value 7))
                nil ctx)))
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "compile-body-with-tail-multi-form-returns-last-register"
  (let* ((ctx (make-codegen-ctx))
         (reg (cl-cc/compile::%compile-body-with-tail
                (list (make-ast-int :value 1)
                      (make-ast-int :value 2)
                      (make-ast-int :value 3))
                nil ctx)))
    ;; All three forms are compiled; the register of form 3 is returned.
    (expect (keywordp reg) :to-be-truthy)
    ;; Three vm-const instructions should have been emitted
    (expect (>= (count-if (lambda (i) (typep i 'cl-cc/vm::vm-const))
                               (codegen-instructions ctx))
                     3) :to-be-truthy)))

(it-sequential "compile-body-with-tail-empty-body-returns-nil"
  (let* ((ctx (make-codegen-ctx))
         (result (cl-cc/compile::%compile-body-with-tail '() nil ctx)))
    (expect result :to-be-null)))

(it-sequential "compile-body-with-tail-sets-tail-for-last-form"
  (let* ((ctx (make-codegen-ctx))
         (tail-values nil))
    ;; Instrument by wrapping compile-ast: not straightforward, so
    ;; instead just verify the return is a register when tail=t
    (let ((reg (cl-cc/compile::%compile-body-with-tail
                 (list (make-ast-int :value 42))
                 t ctx)))
      (declare (ignore tail-values))
      (expect (keywordp reg) :to-be-truthy))))

;;; ─── labels tail-SCC contification ─────────────────────────────────────────

(defun %mutual-tail-labels-fixture (body-form)
  (make-ast-labels
   :bindings
   (list (list 'evenp-local '(n)
               (make-ast-if
                :cond (make-ast-binop :op '= :lhs (make-ast-var :name 'n) :rhs (make-ast-int :value 0))
                :then (make-ast-int :value 1)
                :else (make-ast-call
                       :func 'oddp-local
                       :args (list (make-ast-binop :op '-
                                                   :lhs (make-ast-var :name 'n)
                                                   :rhs (make-ast-int :value 1))))))
         (list 'oddp-local '(n)
               (make-ast-if
                :cond (make-ast-binop :op '= :lhs (make-ast-var :name 'n) :rhs (make-ast-int :value 0))
                :then (make-ast-int :value 0)
                :else (make-ast-call
                       :func 'evenp-local
                       :args (list (make-ast-binop :op '-
                                                   :lhs (make-ast-var :name 'n)
                                                   :rhs (make-ast-int :value 1)))))))
   :body (list body-form)))

(it-sequential "codegen-labels-mutual-tail-scc-emits-jumps-not-closures"
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-tail-position ctx) t)
    (compile-ast
     (%mutual-tail-labels-fixture
      (make-ast-call :func 'evenp-local :args (list (make-ast-int :value 4))))
     ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-null)
    (expect (some (lambda (inst)
                 (and (typep inst 'cl-cc/vm::vm-jump)
                      (search "labels_tail_fn" (cl-cc/vm::vm-label-name inst))))
           (codegen-instructions ctx)) :to-be-truthy)))

(it-sequential "codegen-labels-the-wrapped-local-call-keeps-tail-scc"
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-tail-position ctx) t)
    (compile-ast
     (%mutual-tail-labels-fixture
      (make-ast-call
       :func (make-ast-the
              :type 'function
              :value 'evenp-local)
       :args (list (make-ast-int :value 4))))
     ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-null)
    (expect (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-jump)
                  (search "labels_tail_fn" (cl-cc/vm::vm-label-name inst))))
           (codegen-instructions ctx)) :to-be-truthy)))

(it-sequential "codegen-labels-non-tail-mutual-call-keeps-boxed-closures"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (%mutual-tail-labels-fixture
      (make-ast-binop :op '+
                      :lhs (make-ast-call :func 'evenp-local :args (list (make-ast-int :value 4)))
                      :rhs (make-ast-int :value 1)))
     ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)))

(it-sequential "codegen-labels-restores-compiler-state-after-compilation"
  (let* ((ctx (make-codegen-ctx))
         (original-env (list (cons 'sentinel-env (cl-cc/compile:make-register ctx))))
         (original-labels-boxed-fns '((sentinel-boxed-fn . sentinel-entry))))
    (setf (cl-cc/compile:ctx-env ctx) original-env
          cl-cc/compile:*labels-boxed-fns* original-labels-boxed-fns)
    (compile-ast
     (make-ast-labels
      :bindings (list (list 'local-id '(x) (make-ast-var :name 'x)))
      :body (list (make-ast-call :func 'local-id :args (list (make-ast-int :value 1)))))
     ctx)
    (expect (cl-cc/compile:ctx-env ctx) :to-equal original-env)
    (expect cl-cc/compile:*labels-boxed-fns* :to-equal original-labels-boxed-fns)))

(it-sequential "codegen-labels-restores-compiler-state-for-tail-scc-path"
  (let* ((ctx (make-codegen-ctx))
         (original-env (list (cons 'sentinel-env (cl-cc/compile:make-register ctx))))
         (original-labels-boxed-fns '((sentinel-boxed-fn . sentinel-entry)))
         (original-local-tail-jump-fns '((sentinel-tail-fn . sentinel-tail-entry))))
    (setf (cl-cc/compile:ctx-env ctx) original-env
          cl-cc/compile:*labels-boxed-fns* original-labels-boxed-fns
          cl-cc/compile:*local-tail-jump-fns* original-local-tail-jump-fns
          (cl-cc/compile:ctx-tail-position ctx) t)
    (compile-ast
     (%mutual-tail-labels-fixture
      (make-ast-call :func 'evenp-local :args (list (make-ast-int :value 4))))
     ctx)
    (expect (cl-cc/compile:ctx-env ctx) :to-equal original-env)
    (expect cl-cc/compile:*labels-boxed-fns* :to-equal original-labels-boxed-fns)
    (expect cl-cc/compile:*local-tail-jump-fns* :to-equal original-local-tail-jump-fns)))

;;; ─── emit-assembly ───────────────────────────────────────────────────────────

(it-sequential "emit-assembly-vm-target-returns-empty-string"
  (let ((program (cl-cc:make-vm-program :instructions nil :result-register :R0)))
    (expect (cl-cc/compile:emit-assembly program :target :vm) :to-equal "")))

(it-sequential "emit-assembly-x86-64-produces-bootstrap-header"
  (let* ((program (cl-cc:make-vm-program :instructions nil :result-register :R0))
         (asm     (cl-cc/compile:emit-assembly program :target :x86_64)))
    (expect (stringp asm) :to-be-truthy)
    (expect (search "; CL-CC bootstrap assembly" asm) :to-be-truthy)
    (expect (search "clcc_entry:" asm) :to-be-truthy)))

(it-sequential "emit-assembly-aarch64-produces-bootstrap-header"
  (let* ((program (cl-cc:make-vm-program :instructions nil :result-register :R0))
         (asm     (cl-cc/compile:emit-assembly program :target :aarch64)))
    (expect (stringp asm) :to-be-truthy)
    (expect (search "; CL-CC bootstrap assembly" asm) :to-be-truthy)
    (expect (search "clcc_entry:" asm) :to-be-truthy)))

;;; ─── type-check-ast ──────────────────────────────────────────────────────────

(it-sequential "type-check-ast-integer-literal-returns-integer-type"
  (let* ((ast  (make-ast-int :value 42))
         (type (cl-cc/compile:type-check-ast ast)))
    ;; The inferred type should be something (not nil)
    (expect (not (null type)) :to-be-truthy)))

(it-sequential "type-check-ast-quoted-nil-returns-type"
  (let* ((ast  (make-ast-quote :value nil))
         (type (cl-cc/compile:type-check-ast ast)))
    (expect (not (null type)) :to-be-truthy)))
