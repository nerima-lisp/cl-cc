;;;; tests/unit/compile/codegen-core-array-sink-tests.lisp — Codegen array non-escape/sink tests

(in-package :cl-cc/test)

(it-sequential "codegen-let-noescape-array-variable-aset-bypasses-vm-aset"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-call :func 'aset
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-var :name 'i)
                                                       (make-ast-int :value 42)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-var :name 'i)))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aset) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-num-eq) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump-zero) :to-be-truthy)))

(it-sequential "codegen-let-noescape-typed-array-the-wrapper-still-sinks"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-the
                                           :type '(simple-array fixnum (*))
                                           :value (make-ast-call
                                                   :func 'make-array
                                                   :args (list (make-ast-int :value 2))))))
                :body (list (make-ast-call :func 'aset
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 1)
                                                       (make-ast-int :value 42)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 1))))
                :declarations nil)
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aset) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)))

(it-sequential "codegen-let-noescape-array-make-array-function-wrapper-still-sinks"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func (make-ast-the
                                                  :type 'function
                                                  :value (make-ast-var :name 'make-array))
                                           :args (list (make-ast-int :value 2)))))
                :body (list (make-ast-call :func 'aset
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0)
                                                       (make-ast-int :value 42)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0))))
                :declarations nil)
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aset) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)))

(it-sequential "codegen-let-noescape-typed-array-character-default-init"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (make-ast-let
      :bindings (list (cons 'arr (make-ast-the
                                 :type '(simple-array character (*))
                                 :value (make-ast-call
                                         :func 'make-array
                                         :args (list (make-ast-int :value 2))))))
      :body (list (make-ast-call :func 'aref
                                 :args (list (make-ast-var :name 'arr)
                                             (make-ast-int :value 0))))
      :declarations nil)
     ctx)
    (expect (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-const)
                  (characterp (cl-cc/vm::vm-value inst))
                  (char= #\Nul (cl-cc/vm::vm-value inst))))
           (codegen-instructions ctx)) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-element-type-keyword-still-sinks"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)
                                                       (make-ast-var :name :element-type)
                                                       (make-ast-quote :value 'character)))))
                :body (list (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0))))
                :declarations nil)
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-const)
                  (characterp (cl-cc/vm::vm-value inst))
                  (char= #\Nul (cl-cc/vm::vm-value inst))))
           (codegen-instructions ctx)) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-element-type-keyword-through-ast-the-still-sinks"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)
                                                       (make-ast-var :name :element-type)
                                                       (make-ast-the
                                                        :type 'character
                                                        :value (make-ast-quote :value 'character))))))
                :body (list (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0))))
                :declarations nil)
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-const)
                  (characterp (cl-cc/vm::vm-value inst))
                  (char= #\Nul (cl-cc/vm::vm-value inst))))
           (codegen-instructions ctx)) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-element-type-keyword-node-through-ast-the-still-sinks"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)
                                                       (make-ast-the
                                                        :type 'keyword
                                                        :value (make-ast-var :name :element-type))
                                                       (make-ast-quote :value 'character)))))
                :body (list (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0))))
                :declarations nil)
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-const)
                  (characterp (cl-cc/vm::vm-value inst))
                  (char= #\Nul (cl-cc/vm::vm-value inst))))
           (codegen-instructions ctx)) :to-be-truthy)))

(it-sequential "codegen-let-branch-local-array-use-elides-allocation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-if
                             :cond (make-ast-int :value 1)
                             :then (make-ast-call :func 'aref
                                                  :args (list (make-ast-var :name 'arr)
                                                              (make-ast-var :name 'i)))
                             :else (make-ast-int :value 0))))
                ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (const0-positions (loop for inst in insts
                                 for idx from 0
                                 when (and (typep inst 'cl-cc/vm::vm-const)
                                           (eql (cl-cc/vm::vm-value inst) 0))
                                 collect idx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect jump-pos :to-be-truthy)
    (expect const0-positions :to-be-truthy)
    (expect (every (lambda (idx) (> idx jump-pos)) const0-positions) :to-be-truthy)))

(it-sequential "codegen-let-branch-array-escape-preserves-allocation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-if
                             :cond (make-ast-int :value 1)
                             :then (make-ast-var :name 'arr)
                             :else (make-ast-call :func 'aref
                                                  :args (list (make-ast-var :name 'arr)
                                                              (make-ast-var :name 'i))))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)))

(it-sequential "codegen-let-branch-shadowed-array-binding-still-sinks-outer-use"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-if
                             :cond (make-ast-int :value 1)
                             :then (make-ast-let
                                    :bindings (list (cons 'arr (make-ast-call
                                                               :func 'make-array
                                                               :args (list (make-ast-int :value 1)))))
                                    :body (list (make-ast-call :func 'aref
                                                               :args (list (make-ast-var :name 'arr)
                                                                           (make-ast-int :value 0)))))
                             :else (make-ast-call :func 'aref
                                                  :args (list (make-ast-var :name 'arr)
                                                              (make-ast-var :name 'i))))))
               ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)))
    (expect (keywordp reg) :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)))

(it-sequential "codegen-single-inst-emission setq-emits-move"
  (destructuring-bind (scenario expected-inst) (list :setq 'cl-cc/vm::vm-move)
    (let ((ctx (make-codegen-ctx)))
    (ecase scenario
      (:setq
       (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x :R0)))
       (compile-ast (make-ast-setq :var 'x :value (make-ast-int :value 99)) ctx))
      (:print
       (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)))
    (expect (codegen-find-inst ctx expected-inst) :to-be-truthy))))

(it-sequential "codegen-single-inst-emission print-emits-print"
  (destructuring-bind (scenario expected-inst) (list :print 'cl-cc/vm::vm-print)
    (let ((ctx (make-codegen-ctx)))
    (ecase scenario
      (:setq
       (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x :R0)))
       (compile-ast (make-ast-setq :var 'x :value (make-ast-int :value 99)) ctx))
      (:print
       (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)))
    (expect (codegen-find-inst ctx expected-inst) :to-be-truthy))))

(it-sequential "codegen-the-compiles-inner"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc:make-ast-the :type 'integer
                                 :value (make-ast-int :value 42))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error) :to-be-truthy)))

(it-sequential "codegen-hole-signals-typed-hole-message"
  (let ((ctx (make-codegen-ctx)))
    (handler-case
        (progn
          (compile-ast (cl-cc/parse::lower-sexp-to-ast '_) ctx)
          (expect nil :to-be-truthy))
      (cl-cc:ast-compilation-error (e)
        (expect (search "Typed hole" (format nil "~A" e)) :to-be-truthy)))))

(it-sequential "codegen-if-narrows-branch-type-env"
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc/parse::lower-sexp-to-ast
                '(if (numberp x) (the fixnum x) 0))))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast ast ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error) :to-be-null)))

(it-sequential "codegen-if-case-of-case-collapses-redundant-inner-branch"
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc/parse::lower-sexp-to-ast
               '(if (numberp x)
                    (if (numberp x) 1 2)
                    3))))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast ast ctx)
    (expect (= 1 (count-if (lambda (inst)
                            (typep inst 'cl-cc/vm::vm-jump-zero))
                          (codegen-instructions ctx))) :to-be-truthy)))
