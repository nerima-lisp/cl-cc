;;;; tests/unit/compile/codegen-runtime-tests.lisp — Codegen runtime semantics tests

(in-package :cl-cc/test)
(it-sequential "codegen-values-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-values
                              :forms (list (make-ast-int :value 1)
                                           (make-ast-int :value 2)
                                           (make-ast-int :value 3)
                                           (make-ast-int :value 4)))
                            ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-single-value-compilation-skips-mv-instructions"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-values
                             :forms (list (make-ast-int :value 42)))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values-regs) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-values-run multi-values"
  (destructuring-bind (expected form) (list '(1 2 3) "(multiple-value-list (values 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "codegen-values-run single-value"
  (destructuring-bind (expected form) (list 42 "(values 42)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "codegen-mvb-compilation-cases"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-bind
                              :vars '(a b)
                              :values-form (cl-cc/ast:make-ast-values
                                            :forms (list (make-ast-int :value 1)
                                                         (make-ast-int :value 2)))
                              :body (list (make-ast-var :name 'a)))
                            ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy))
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-bind
                             :vars '(a b)
                             :values-form (make-ast-call :func 'floor
                                                         :args (list (make-ast-int :value 17)
                                                                     (make-ast-int :value 5)))
                             :body (list (make-ast-var :name 'a)))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-single-var-mvb-skips-mv-bind"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-bind
                             :vars '(a)
                             :values-form (make-ast-call :func 'floor
                                                         :args (list (make-ast-int :value 17)
                                                                     (make-ast-int :value 5)))
                             :body (list (make-ast-var :name 'a)))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind-regs) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-mvb-run sum-values"
  (destructuring-bind (expected code) (list 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))")
    (assert-run= expected code)))

(it-sequential "codegen-mvb-run first-value"
  (destructuring-bind (expected code) (list 10 "(multiple-value-bind (x y) (values 10 20) x)")
    (assert-run= expected code)))

(it-sequential "codegen-mv-call-direct-path explicit-values"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-multiple-value-call
             :func (make-ast-function :name '+)
             :args (list (cl-cc/ast:make-ast-values
                          :forms (list (make-ast-int :value 1)
                                       (make-ast-int :value 2))))))
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values-to-list) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy))))

(it-sequential "codegen-mv-call-direct-path no-args"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-multiple-value-call
             :func (make-ast-function :name 'list)
             :args nil))
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values-to-list) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy))))

(it-sequential "codegen-mv-call-mixed-args-still-uses-apply-path"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-call
                             :func (make-ast-function :name '+)
                             :args (list (cl-cc/ast:make-ast-values
                                          :forms (list (make-ast-int :value 1)
                                                       (make-ast-int :value 2)))
                                         (make-ast-call :func 'floor
                                                        :args (list (make-ast-int :value 17)
                                                                    (make-ast-int :value 5)))))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-mv-call-the-wrapped-function-keeps-direct-call"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-call
                             :func (make-ast-the
                                    :type 'function
                                    :value (make-ast-function :name 'list))
                             :args nil)
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-values-to-list) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-mv-call-run-cases"
  (assert-run= 3
    "(multiple-value-call (lambda (a b) (+ a b)) (values 1 2))")
  (expect (run-string "(multiple-value-call #'cons (values 1 2))") :to-equal '(1 . 2)))

(it-sequential "codegen-mv-prog1-compilation"
  (let* ((ctx    (make-codegen-ctx))
         (reg    (compile-ast (cl-cc::make-ast-multiple-value-prog1
                                :first (make-ast-int :value 1)
                                :forms (list (make-ast-int :value 2)
                                             (make-ast-int :value 3)))
                              ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (>= (length consts) 3) :to-be-truthy)))

(it-sequential "codegen-mv-prog1-run-cases"
  (assert-run= 1
    "(multiple-value-prog1 1 2 3)")
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(multiple-value-prog1 42 (print 99))"))))
    (expect (search "99" output) :to-be-truthy)))

(it-sequential "codegen-unwind-protect-emits-establish-handler"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-unwind-protect
                   :protected (make-ast-int :value 42)
                   :cleanup (list (make-ast-int :value 0)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-establish-handler) :to-be-truthy)))

(it-sequential "codegen-handler-case-records-an-exception-table-entry"
  (let ((ctx (make-codegen-ctx))
        (cl-cc/compile::*pending-exception-table-entries* nil))
    (compile-ast (cl-cc/ast:make-ast-handler-case
                   :form (make-ast-int :value 42)
                   :clauses (list (list 'error 'e (make-ast-int :value 0))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-establish-handler) :to-be-falsy)
    (expect (plusp (length cl-cc/compile::*pending-exception-table-entries*)) :to-be-truthy)))

(it-sequential "codegen-exception-form-returns-register unwind-protect"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-unwind-protect
             :protected (make-ast-int :value 7)
             :cleanup (list (make-ast-int :value 0))))
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (expect (keywordp reg) :to-be-truthy))))

(it-sequential "codegen-exception-form-returns-register handler-case"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-handler-case
             :form (make-ast-int :value 10)
             :clauses (list (list 'error nil (make-ast-int :value 0)))))
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (expect (keywordp reg) :to-be-truthy))))

(it-sequential "codegen-unwind-protect-run-cases"
  (assert-run= 42
    "(unwind-protect 42 nil)")
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(unwind-protect 1 (print 99))"))))
    (expect (search "99" output) :to-be-truthy)))

(it-sequential "codegen-handler-case-run-cases"
  (assert-run= 42
    "(handler-case 42 (error (e) -1))")
  (assert-run= 99
    "(handler-case (error \"boom\") (error (e) 99))"))

(it-sequential "codegen-values-run-cardinality-and-mvb-semantics"
  (expect (run-string "(multiple-value-list (values))") :to-equal nil)
  (expect (run-string "(multiple-value-list (values 1))") :to-equal (quote (1)))
  (expect (run-string "(multiple-value-list (values 1 2))") :to-equal (quote (1 2)))
  (expect (run-string "(multiple-value-list (values 1 2 3))") :to-equal (quote (1 2 3)))
  (expect (run-string "(multiple-value-list (values 1 2 3 4))") :to-equal (quote (1 2 3 4)))
  (expect (run-string "(multiple-value-bind (a b c) (values 1 2) (list a b c))")
          :to-equal (quote (1 2 nil)))
  (expect (run-string "(multiple-value-bind (a b) (values 1 2 3 4) (list a b))")
          :to-equal (quote (1 2)))
  (expect (run-string "(let ((x 0)) (multiple-value-bind (a b c) (values (progn (incf x) x) (progn (incf x) x) (progn (incf x) x)) (+ (* 1000 x) (+ (* 100 a) (+ (* 10 b) c)))))")
          :to-equal 3123))
