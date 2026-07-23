;;;; tests/unit/compile/codegen-clos-tests.lisp — Codegen CLOS Unit Tests

(in-package :cl-cc/test)

(defun make-test-slot (name &key initarg initform)
  "Build a minimal ast-slot-def for use in codegen tests."
  (cl-cc/ast:make-ast-slot-def :name name :initarg initarg :initform initform))

;;; ─── compile-ast: ast-defclass ───────────────────────────────────────────────

(it-sequential "codegen-defclass-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-defclass
                             :name 'my-rect
                             :superclasses nil
                             :slots (list (make-test-slot 'w :initarg :w)
                                          (make-test-slot 'h :initarg :h)))
                           ctx))
         (inst (codegen-find-inst ctx 'cl-cc/vm::vm-class-def)))
    (expect inst :to-be-truthy)
    (expect (cl-cc/vm::vm-slot-names inst) :to-equal '(w h))
    (expect (gethash 'my-rect (cl-cc/compile:ctx-global-classes ctx)) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-defclass-constant-slot-initform-compilation"
  (cl-cc/expand::compiler-macroexpand-all '(defconstant +codegen-defclass-slot-constant+ 41))
  (let* ((ctx (make-codegen-ctx))
         (node (cl-cc/compile:optimize-ast
                (cl-cc/ast:make-ast-defclass
                 :name 'my-inline-box
                 :superclasses nil
                 :slots (list (make-test-slot 'value
                                              :initarg :value
                                              :initform (cl-cc/ast:make-ast-binop
                                                         :op '+
                                                         :lhs (make-ast-var :name '+codegen-defclass-slot-constant+)
                                                         :rhs (make-ast-int :value 1)))))))
         (reg (compile-ast node ctx))
         (const-inst (codegen-find-inst ctx 'cl-cc/vm::vm-const))
         (class-inst (codegen-find-inst ctx 'cl-cc/vm::vm-class-def)))
    (expect const-inst :to-be-truthy)
    (expect (cl-cc::vm-const-value const-inst) :to-equal 42)
    (expect class-inst :to-be-truthy)
    (expect (cl-cc/vm::vm-slot-initform-regs class-inst) :to-equal (list (cons 'value (cl-cc/vm::vm-dst const-inst))))
    (expect (keywordp reg) :to-be-truthy)))

;;; ─── compile-ast: ast-defgeneric ─────────────────────────────────────────────

(it-sequential "codegen-defgeneric-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defgeneric :name 'my-speak :params '(animal))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-class-def) :to-be-truthy)
    (expect (gethash 'my-speak (cl-cc/compile:ctx-global-generics ctx)) :to-be-truthy)))

(it-sequential "codegen-defgeneric-idempotent"
  (let ((ctx (make-codegen-ctx)))
    (let ((r1 (compile-ast (cl-cc/ast:make-ast-defgeneric :name 'my-compute :params '(x)) ctx))
          (r2 (compile-ast (cl-cc/ast:make-ast-defgeneric :name 'my-compute :params '(x)) ctx)))
      (expect r2 :to-be r1))))

;;; ─── compile-ast: ast-defmethod ──────────────────────────────────────────────

(it-sequential "codegen-defmethod-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defgeneric :name 'my-greet :params '(obj)) ctx)
    (compile-ast (cl-cc/ast:make-ast-defmethod
                  :name 'my-greet
                  :specializers (list '(obj . dog))
                  :params '(obj)
                  :body (list (cl-cc/ast:make-ast-int :value 99)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-register-method)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-method-specializer inst) :to-equal 'dog))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-truthy)))

;;; ─── compile-ast: ast-make-instance ─────────────────────────────────────────

(it-sequential "codegen-make-instance-emits-vm-make-obj static"
  (destructuring-bind (ast env-setup) (list (cl-cc/ast:make-ast-make-instance
                       :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                       :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value 'rex)))) nil)
    (let ((ctx (make-codegen-ctx)))
    (when env-setup (setf (cl-cc/compile:ctx-env ctx) env-setup))
    (compile-ast ast ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-truthy))))

(it-sequential "codegen-make-instance-emits-vm-make-obj dynamic"
  (destructuring-bind (ast env-setup) (list (cl-cc/ast:make-ast-make-instance
                       :class (cl-cc/ast:make-ast-var :name 'cls)
                       :initargs nil) (list (cons 'cls :R50)))
    (let ((ctx (make-codegen-ctx)))
    (when env-setup (setf (cl-cc/compile:ctx-env ctx) env-setup))
    (compile-ast ast ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-truthy))))

(it-sequential "codegen-make-instance-static-loads-class-globally"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-make-instance
                  :class (cl-cc/ast:make-ast-the
                          :type 'symbol
                          :value (cl-cc/ast:make-ast-quote :value 'my-cat))
                  :initargs nil)
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-get-global)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm:vm-global-name inst) :to-be 'my-cat))))
