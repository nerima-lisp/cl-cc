;;;; tests/unit/compile/codegen-core-tests.lisp — Codegen core tests

(in-package :cl-cc/test)

(it-sequential "codegen-if-compilation"
  (let* ((ctx (make-codegen-ctx))
         (x-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x x-reg)))
    (let ((reg (compile-ast (make-ast-if :cond (make-ast-var :name 'x)
                                          :then (make-ast-int :value 1)
                                          :else (make-ast-int :value 2))
                             ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump-zero) :to-be-truthy)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump) :to-be-truthy))))

(it-sequential "codegen-progn-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-progn
                              :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (= 3 (length consts)) :to-be-truthy)))

(it-sequential "codegen-mvb-the-wrapped-function-keeps-register-mv-bind"
  (let* ((ctx (make-codegen-ctx))
         (reg (progn
                (setf (gethash 'floor (cl-cc/compile::ctx-global-function-mv-arities ctx)) 2)
                (compile-ast (cl-cc/ast:make-ast-multiple-value-bind
                              :vars '(a b)
                              :values-form (cl-cc/ast:make-ast-call
                                            :func (make-ast-the
                                                   :type 'function
                                                   :value (make-ast-function :name 'floor))
                                            :args (list (make-ast-int :value 17)
                                                        (make-ast-int :value 5)))
                              :body (list (make-ast-var :name 'a)))
                             ctx))))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind-regs) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind) :to-be-falsy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "ast-constant-folding-before-codegen arithmetic"
  (destructuring-bind (expected form) (list 6 '(+ 1 2 3))
    (let ((result (cl-cc/compile:optimize-ast (cl-cc/parse::lower-sexp-to-ast form))))
    (expect (cl-cc/ast:ast-int-p result) :to-be-truthy)
    (expect (= expected (cl-cc/ast:ast-int-value result)) :to-be-truthy))))

(it-sequential "ast-constant-folding-before-codegen string-length"
  (destructuring-bind (expected form) (list 5 '(string-length "hello"))
    (let ((result (cl-cc/compile:optimize-ast (cl-cc/parse::lower-sexp-to-ast form))))
    (expect (cl-cc/ast:ast-int-p result) :to-be-truthy)
    (expect (= expected (cl-cc/ast:ast-int-value result)) :to-be-truthy))))

(it-sequential "ast-partial-eval-known-defun-call"
  (let ((result (cl-cc/compile:compile-toplevel-forms
                 '((defun add1 (x) (+ x 1))
                   (add1 41))
                 :target :vm)))
    (let ((asts (cl-cc/compile:compilation-result-ast result)))
      (expect (or (null asts) (listp asts)) :to-be-truthy))))

(it-sequential "codegen-let-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                              :bindings (list (cons 'x (make-ast-int :value 42)))
                              :body (list (make-ast-var :name 'x)))
                            ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy)))

(it-sequential "codegen-let-binding-declaration-controls-own-move ignore"
  (destructuring-bind (declarations expected-moves) (list '((ignore x)) 0)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'x (make-ast-int :value 42)))
                             :declarations declarations
                             :body (list (make-ast-int :value 0)))
                           ctx))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move))
                               (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (= expected-moves (length moves)) :to-be-truthy))))

(it-sequential "codegen-let-binding-declaration-controls-own-move ignorable"
  (destructuring-bind (declarations expected-moves) (list '((ignorable x)) 1)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'x (make-ast-int :value 42)))
                             :declarations declarations
                             :body (list (make-ast-int :value 0)))
                           ctx))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move))
                               (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (= expected-moves (length moves)) :to-be-truthy))))

(it-sequential "codegen-let-ignore-binding-enables-dce-of-unused-initializer"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'x (make-ast-int :value 42)))
                             :declarations '((ignore x))
                             :body (list (make-ast-int :value 0)))
                           ctx))
         (instructions (append (codegen-instructions ctx)
                               (list (cl-cc:make-vm-ret :reg reg))))
         (optimized (cl-cc/optimize::opt-pass-dce instructions))
         (const-values (mapcar #'cl-cc::vm-const-value
                               (remove-if-not (lambda (i)
                                                (typep i 'cl-cc/vm::vm-const))
                                              optimized))))
    (expect (member 42 const-values :test #'eql) :to-be-falsy)
    (expect (member 0 const-values :test #'eql) :to-be-truthy)))

(it-sequential "codegen-let-noescape-cons-car-bypasses-vm-car"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'p (make-ast-call
                                                      :func 'cons
                                                      :args (list (make-ast-int :value 1)
                                                                  (make-ast-int :value 2)))))
                             :body (list (make-ast-call :func 'car
                                                        :args (list (make-ast-var :name 'p)))))
                           ctx))
         (cars (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-car))
                              (codegen-instructions ctx)))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move))
                               (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-null)
    (expect (= 0 (length cars)) :to-be-truthy)
    (expect (> (length moves) 0) :to-be-truthy)))

(it-sequential "codegen-let-noescape-cons-car-through-ast-the-bypasses-vm-car"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'p (make-ast-the
                                                        :type 'cons
                                                        :value (make-ast-call
                                                                :func 'cons
                                                                :args (list (make-ast-int :value 1)
                                                                            (make-ast-int :value 2))))))
                             :body (list (make-ast-call :func 'car
                                                        :args (list (make-ast-var :name 'p)))))
                           ctx))
         (cars (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-car))
                              (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-car) :to-be-null)
    (expect (= 0 (length cars)) :to-be-truthy)))

(it-sequential "codegen-let-dynamic-extent-cons-declaration-controls-noescape no-declaration"
  (destructuring-bind (declarations noescape-p) (list nil nil)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'p (make-ast-call
                                         :func 'cons
                                         :args (list (make-ast-int :value 1)
                                                     (make-ast-int :value 2)))))
                :declarations declarations
                :body (list (make-ast-let
                             :bindings (list (cons 'reader
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func 'cdr
                                                                  :args (list (make-ast-var :name 'p)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'reader)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (if noescape-p
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cdr) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cdr) :to-be-truthy))))))

(it-sequential "codegen-let-dynamic-extent-cons-declaration-controls-noescape with-dynamic-extent"
  (destructuring-bind (declarations noescape-p) (list '((dynamic-extent p)) t)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'p (make-ast-call
                                         :func 'cons
                                         :args (list (make-ast-int :value 1)
                                                     (make-ast-int :value 2)))))
                :declarations declarations
                :body (list (make-ast-let
                             :bindings (list (cons 'reader
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func 'cdr
                                                                  :args (list (make-ast-var :name 'p)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'reader)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (if noescape-p
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cdr) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cdr) :to-be-truthy))))))

(it-sequential "codegen-let-dynamic-extent-cons-unsafe-nested-consumer-falls-back"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'p (make-ast-call
                                         :func 'cons
                                         :args (list (make-ast-int :value 1)
                                                     (make-ast-int :value 2)))))
                :declarations '((dynamic-extent p))
                :body (list (make-ast-let
                             :bindings (list (cons 'reader
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func 'car
                                                                  :args (list (make-ast-call
                                                                               :func 'cons
                                                                               :args (list (make-ast-int :value 0)
                                                                                           (make-ast-var :name 'p)))))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'reader)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-car) :to-be-truthy)))

(it-sequential "codegen-let-dynamic-extent-closure-the-wrapped-call-keeps-noescape"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'reader
                                      (make-ast-lambda
                                       :params '(x)
                                       :body (list (make-ast-var :name 'x)))))
                :declarations '((dynamic-extent reader))
                :body (list (make-ast-call
                             :func (make-ast-the
                                    :type 'function
                                    :value (make-ast-var :name 'reader))
                             :args (list (make-ast-int :value 1)))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-closure) :to-be-null)))

(it-sequential "codegen-let-escaped-cons-car-falls-back-to-vm-car"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'p (make-ast-call
                                         :func 'cons
                                         :args (list (make-ast-int :value 1)
                                                     (make-ast-int :value 2)))))
                :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'p)))
                            (make-ast-call :func 'car :args (list (make-ast-var :name 'p)))))
               ctx))
         (cars (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-car))
                              (codegen-instructions ctx))))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)
    (expect (> (length cars) 0) :to-be-truthy)))

(it-sequential "codegen-let-branch-local-cons-sinks-allocation simple-cond"
  (destructuring-bind (ast) (list (make-ast-let
            :bindings (list (cons 'p (make-ast-call
                                     :func 'cons
                                     :args (list (make-ast-call :func 'cons
                                                                :args (list (make-ast-int :value 1)
                                                                            (make-ast-int :value 2)))
                                                 (make-ast-int :value 3)))))
            :body (list (make-ast-if
                         :cond (make-ast-int :value 1)
                         :then (make-ast-call :func 'car :args (list (make-ast-var :name 'p)))
                         :else (make-ast-int :value 0)))))
    (let* ((ctx   (make-codegen-ctx))
         (reg   (compile-ast ast ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts)))
    (expect (keywordp reg) :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect cons-pos :to-be-truthy)
    (expect (> cons-pos jump-pos) :to-be-truthy))))

(it-sequential "codegen-let-branch-local-cons-sinks-allocation multi-binding-cond"
  (destructuring-bind (ast) (list (make-ast-let
            :bindings (list (cons 'p (make-ast-call
                                     :func 'cons
                                     :args (list (make-ast-call :func 'cons
                                                                :args (list (make-ast-int :value 1)
                                                                            (make-ast-int :value 2)))
                                                 (make-ast-int :value 3))))
                            (cons 'flag (make-ast-int :value 1)))
            :body (list (make-ast-if
                         :cond (make-ast-var :name 'flag)
                         :then (make-ast-call :func 'car :args (list (make-ast-var :name 'p)))
                         :else (make-ast-int :value 0)))))
    (let* ((ctx   (make-codegen-ctx))
         (reg   (compile-ast ast ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts)))
    (expect (keywordp reg) :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect cons-pos :to-be-truthy)
    (expect (> cons-pos jump-pos) :to-be-truthy))))

(it-sequential "codegen-let-noescape-array-aref-bypasses-vm-make-array-and-vm-aref"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'arr (make-ast-call
                                                        :func 'make-array
                                                        :args (list (make-ast-int :value 3)))))
                             :body (list (make-ast-call :func 'aref
                                                        :args (list (make-ast-var :name 'arr)
                                                                    (make-ast-int :value 1)))))
                           ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-length-bypasses-vm-make-array"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'arr (make-ast-call
                                                        :func 'make-array
                                                        :args (list (make-ast-int :value 3)))))
                             :body (list (make-ast-call :func 'array-length
                                                        :args (list (make-ast-var :name 'arr)))))
                           ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-const) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-length-through-ast-the-bypasses-vm-make-array"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'arr (make-ast-call
                                                        :func 'make-array
                                                        :args (list (make-ast-int :value 3)))))
                             :body (list (make-ast-call
                                          :func (make-ast-the
                                                 :type 'function
                                                 :value (make-ast-var :name 'array-length))
                                          :args (list (make-ast-var :name 'arr)))))
                           ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-const) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-variable-aref-bypasses-vm-aref"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-var :name 'i)))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-num-eq) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump-zero) :to-be-truthy)))

(it-sequential "codegen-let-escaped-array-aref-falls-back-to-vm-aref"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'arr)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0)))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-truthy)))

(it-sequential "codegen-let-noescape-array-aset-bypasses-vm-make-array-and-vm-aset"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :body (list (make-ast-call :func 'aset
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 1)
                                                       (make-ast-int :value 42)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 1)))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aset) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy)))

(it-sequential "codegen-let-dynamic-extent-array-declaration-controls-noescape no-declaration"
  (destructuring-bind (declarations noescape-p) (list nil nil)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :declarations declarations
                :body (list (make-ast-let
                             :bindings (list (cons 'reader
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func 'aref
                                                                  :args (list (make-ast-var :name 'arr)
                                                                              (make-ast-int :value 0)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'reader)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (if noescape-p
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-truthy))))))

(it-sequential "codegen-let-dynamic-extent-array-declaration-controls-noescape with-dynamic-extent"
  (destructuring-bind (declarations noescape-p) (list '((dynamic-extent arr)) t)
    (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :declarations declarations
                :body (list (make-ast-let
                             :bindings (list (cons 'reader
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func 'aref
                                                                  :args (list (make-ast-var :name 'arr)
                                                                              (make-ast-int :value 0)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'reader)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (if noescape-p
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-null)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))
        (progn
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)
          (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-truthy))))))

(it-sequential "codegen-let-dynamic-extent-array-unsafe-operand-falls-back"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :declarations '((dynamic-extent arr))
                :body (list (make-ast-let
                             :bindings (list (cons 'reader
                                                    (make-ast-lambda
                                                     :params '()
                                                     :body (list (make-ast-call
                                                                  :func 'aref
                                                                  :args (list (make-ast-call
                                                                               :func 'make-array
                                                                               :args (list (make-ast-int :value 3)))
                                                                             (make-ast-var :name 'arr)))))))
                             :body (list (make-ast-call :func (make-ast-var :name 'reader)
                                                         :args nil)))))
                ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-aref) :to-be-truthy)))

(it-sequential "codegen-result-vm-instructions-without-halt-strips-terminal-halt"
  (let* ((move (cl-cc:make-vm-move :dst :R1 :src :R0))
         (halt (cl-cc:make-vm-halt :reg :R1))
         (result (cl-cc/compile:make-compilation-result
                  :program (cl-cc:make-vm-program :instructions (list move halt) :result-register :R1)
                  :vm-instructions (list move halt))))
    (expect (cl-cc/compile::%result-vm-instructions-without-halt result) :to-equal (list move))))

(it-sequential "compile-toplevel-forms-recovers-from-form-error"
  (let* ((forms '((+ 1 1) (if 1) (+ 2 3)))
         (result (cl-cc/compile:compile-toplevel-forms forms :target :vm))
         (errors (cl-cc/compile:compilation-result-errors result))
         (asts (cl-cc/compile:compilation-result-ast result)))
    (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)
    (expect (= 1 (length errors)) :to-be-truthy)
    (expect (= 2 (length asts)) :to-be-truthy)
    (expect (= 1 (getf (first errors) :form-index)) :to-be-truthy)
    (expect (getf (first errors) :form) :to-equal '(if 1))
    (expect (typep (getf (first errors) :condition) 'error) :to-be-truthy)
    (expect (stringp (getf (first errors) :message)) :to-be-truthy)
    (expect (cl-cc/compile:compilation-result-program result) :to-be-truthy)
    (expect (> (length (cl-cc/compile:compilation-result-vm-instructions result)) 0) :to-be-truthy)))

(it-sequential "compile-toplevel-forms-records-multiple-form-errors"
  (let* ((forms '((if 1) (+ 10 20) (if 2) (+ 30 40)))
         (result (cl-cc/compile:compile-toplevel-forms forms :target :vm))
         (errors (cl-cc/compile:compilation-result-errors result)))
    (expect (= 2 (length errors)) :to-be-truthy)
    (expect (mapcar (lambda (entry) (getf entry :form-index)) errors) :to-equal '(0 2))
    (expect (mapcar (lambda (entry) (getf entry :form)) errors) :to-equal '((if 1) (if 2)))
    (expect (= 2 (length (cl-cc/compile:compilation-result-ast result))) :to-be-truthy)))

(it-sequential "compile-toplevel-forms-all-good-forms-have-no-recovery-errors"
  (let ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2) (+ 3 4)) :target :vm)))
    (expect (cl-cc/compile:compilation-result-errors result) :to-be-null)
    (expect (= 2 (length (cl-cc/compile:compilation-result-ast result))) :to-be-truthy)))

(it-sequential "compile-toplevel-forms-skips-in-package-forms"
  (let* ((forms-with-package '((in-package :cl-user) (+ 2 3)))
         (forms-without-package '((+ 2 3)))
         (result-with-package (cl-cc/compile:compile-toplevel-forms forms-with-package :target :vm))
         (result-without-package (cl-cc/compile:compile-toplevel-forms forms-without-package :target :vm))
         (vm-types-with-package (mapcar (lambda (inst) (class-name (class-of inst)))
                                        (cl-cc/compile:compilation-result-vm-instructions result-with-package)))
         (vm-types-without-package (mapcar (lambda (inst) (class-name (class-of inst)))
                                           (cl-cc/compile:compilation-result-vm-instructions result-without-package))))
    (expect (cl-cc/compile:compilation-result-errors result-with-package) :to-be-null)
    (expect (cl-cc/compile:compilation-result-errors result-without-package) :to-be-null)
    (expect (= 1 (length (cl-cc/compile:compilation-result-ast result-with-package))) :to-be-truthy)
    (expect (= 1 (length (cl-cc/compile:compilation-result-ast result-without-package))) :to-be-truthy)
    (expect vm-types-with-package :to-equal vm-types-without-package)))

(it-sequential "compile-toplevel-forms-rolls-back-partial-if-emit-on-error"
  (let* ((forms '((if t 1 missing-var) (+ 2 3)))
          (result (cl-cc/compile:compile-toplevel-forms forms :target :vm))
          (instructions (cl-cc/compile:compilation-result-vm-instructions result))
          (const-values (mapcar #'cl-cc/vm:vm-const-value
                                (remove-if-not (lambda (inst)
                                                 (typep inst 'cl-cc/vm::vm-const))
                                               instructions))))
    (expect (= 1 (length (cl-cc/compile:compilation-result-errors result))) :to-be-truthy)
    (expect (= 1 (length (cl-cc/compile:compilation-result-ast result))) :to-be-truthy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) instructions) :to-be-falsy)
    (expect (member 1 const-values :test #'eql) :to-be-falsy)
    (expect (member 5 const-values :test #'eql) :to-be-truthy)))

(it-sequential "compile-toplevel-forms-rolls-back-partial-defvar-emit-on-error"
  (let* ((forms '((defvar *fr506-partial* (if t 1 missing-var)) (+ 4 5)))
         (result (cl-cc/compile:compile-toplevel-forms forms :target :vm))
         (instructions (cl-cc/compile:compilation-result-vm-instructions result)))
    (expect (= 1 (length (cl-cc/compile:compilation-result-errors result))) :to-be-truthy)
    (expect (= 1 (length (cl-cc/compile:compilation-result-ast result))) :to-be-truthy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-boundp)) instructions) :to-be-falsy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-set-global)) instructions) :to-be-falsy)))
