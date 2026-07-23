;;;; tests/unit/compile/codegen-phase2-tests.lisp — Phase-2 AST-Introspecting Builtin Handler Tests
;;;
;;; Tests for compile-ast dispatch to Phase-2 handlers in codegen-phase2.lisp.
;;; Each handler is triggered when an ast-call is compiled with a matching
;;; function symbol; returns result-reg on success or nil to fall through.
;;;
;;; Helpers make-codegen-ctx / codegen-instructions / codegen-find-inst are
;;; defined in codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)

;;; ─── Section 1: GETHASH ─────────────────────────────────────────────────────

(it-sequential "codegen-phase2-gethash-cases two-args"
  (destructuring-bind (args default-set-p) (list (list (make-ast-quote :value :k) (make-ast-quote :value :ht)) nil)
    (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast (make-ast-call :func 'gethash :args args) ctx)))
      (let ((inst (codegen-find-inst ctx 'cl-cc:vm-gethash)))
        (expect inst :to-be-truthy)
        (expect (keywordp reg) :to-be-truthy)
        (if default-set-p
            (expect (cl-cc::vm-gethash-default inst) :to-be-truthy)
            (expect (null (cl-cc::vm-gethash-default inst)) :to-be-truthy)))))))

(it-sequential "codegen-phase2-gethash-cases three-args"
  (destructuring-bind (args default-set-p) (list (list (make-ast-quote :value :k) (make-ast-quote :value :ht) (make-ast-int :value 0)) t)
    (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast (make-ast-call :func 'gethash :args args) ctx)))
      (let ((inst (codegen-find-inst ctx 'cl-cc:vm-gethash)))
        (expect inst :to-be-truthy)
        (expect (keywordp reg) :to-be-truthy)
        (if default-set-p
            (expect (cl-cc::vm-gethash-default inst) :to-be-truthy)
            (expect (null (cl-cc::vm-gethash-default inst)) :to-be-truthy)))))))


;;; ─── Section 2: MAPHASH ─────────────────────────────────────────────────────

(it-sequential "codegen-phase2-maphash-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'maphash
                                :args (list (make-ast-quote :value 'my-fn)
                                            (make-ast-quote :value 'my-ht)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-hash-table-keys) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc:vm-call) :to-be-truthy)
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                  (codegen-instructions ctx))))
      (expect (some (lambda (i) (null (cl-cc::vm-const-value i))) consts) :to-be-truthy))))

;;; ─── Section 3: MAKE-ARRAY ──────────────────────────────────────────────────

(it-sequential "codegen-phase2-make-array"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 5)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (expect inst :to-be-truthy)
      (expect (null (cl-cc::vm-make-array-fill-pointer inst)) :to-be-truthy)
      (expect (null (cl-cc::vm-make-array-adjustable inst)) :to-be-truthy)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 0)))
                  ctx)
     (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)))

(it-sequential "codegen-phase2-make-array-element-type"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 3)
                                            (make-ast-var :name :element-type)
                                            (make-ast-quote :value 'character)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-element-type inst) :to-be 'character))))

(it-sequential "codegen-phase2-make-array-dynamic-keywords-lower"
  (let ((ctx (make-ctx-with-vars 'init 'fp 'adj 'etype 'base)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 5)
                                            (make-ast-var :name :initial-element)
                                            (make-ast-var :name 'init)
                                            (make-ast-var :name :fill-pointer)
                                            (make-ast-var :name 'fp)
                                            (make-ast-var :name :adjustable)
                                            (make-ast-var :name 'adj)
                                            (make-ast-var :name :element-type)
                                            (make-ast-var :name 'etype)
                                            (make-ast-var :name :displaced-to)
                                            (make-ast-var :name 'base)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-initial-element inst) :to-be-truthy)
      (expect (cl-cc/vm::vm-fill-pointer-reg inst) :to-be-truthy)
      (expect (cl-cc/vm::vm-adjustable-reg inst) :to-be-truthy)
      (expect (cl-cc/vm::vm-element-type-reg inst) :to-be-truthy)
      (expect (cl-cc/vm::vm-displaced-to-reg inst) :to-be-truthy))))

(it-sequential "codegen-phase2-make-array-unsupported-keywords-fall-through unknown-keyword"
  (destructuring-bind (args) (list (list (make-ast-int :value 5)
                 (make-ast-var :name :unknown-option)
                 (make-ast-int :value 1)))
    (let ((ctx (make-ctx-with-vars 'contents)))
    (compile-ast (make-ast-call :func 'make-array :args args) ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)) :to-be-truthy))))

(it-sequential "codegen-phase2-make-array-unsupported-keywords-fall-through dynamic-initial-contents"
  (destructuring-bind (args) (list (list (make-ast-int :value 5)
                 (make-ast-var :name :initial-contents)
                 (make-ast-var :name 'contents)))
    (let ((ctx (make-ctx-with-vars 'contents)))
    (compile-ast (make-ast-call :func 'make-array :args args) ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)) :to-be-truthy))))

(it-sequential "codegen-phase2-make-array-the-wrapped-initial-contents-lowers"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-array
                                :args (list (make-ast-int :value 3)
                                            (make-ast-var :name :initial-contents)
                                            (make-ast-the
                                             :type 'list
                                             :value (make-ast-quote :value '(1 2 3)))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy)
    (expect (= 3 (codegen-count-inst ctx 'cl-cc/vm::vm-aset)) :to-be-truthy)))

;;; ─── Section 4: MAKE-ADJUSTABLE-VECTOR ─────────────────────────────────────

(it-sequential "codegen-phase2-make-adjustable-vector-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-adjustable-vector
                                :args (list (make-ast-int :value 8)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-array-fill-pointer inst) :to-be-truthy)
      (expect (cl-cc::vm-make-array-adjustable inst) :to-be-truthy))))

;;; ─── Section 5: TYPEP ───────────────────────────────────────────────────────

(it-sequential "codegen-phase2-typep-quoted-emits-vm-typep"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value 'integer)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)))

(it-sequential "codegen-phase2-typep-type-name-stored string"
  (destructuring-bind (type-sym) (list 'string)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value type-sym)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-typep)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-typep-type-name inst) :to-be type-sym)))))

(it-sequential "codegen-phase2-typep-type-name-stored list"
  (destructuring-bind (type-sym) (list 'list)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-quote :value type-sym)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-typep)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-typep-type-name inst) :to-be type-sym)))))

(it-sequential "codegen-phase2-typep-the-wrapped-quoted-type-emits-vm-typep"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 42)
                                            (make-ast-the
                                             :type 'type-specifier
                                             :value (make-ast-quote :value 'string))))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-typep)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-typep-type-name inst) :to-be 'string))))

(it-sequential "codegen-phase2-typep-unquoted-falls-through"
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'integer :R99)))
    (compile-ast (make-ast-call :func 'typep
                                :args (list (make-ast-int :value 1)
                                            (make-ast-var :name 'integer)))
                 ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-typep)) :to-be-truthy)))

;;; ─── Section 6: RT-SLOT-SET / MAKE-STRING / SET-FDEFINITION ───────────────

(it-sequential "codegen-phase2-rt-slot-set-the-wrapped-slot-name-lowers"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'rt-slot-set
                                :args (list (make-ast-int :value 0)
                                            (make-ast-the
                                             :type 'symbol
                                             :value (make-ast-quote :value 'slot-name))
                                            (make-ast-int :value 99)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-write)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-slot-name-sym inst) :to-be 'slot-name))))

(it-sequential "codegen-phase2-make-string-the-wrapped-initial-element-lowers"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-string
                                :args (list (make-ast-int :value 5)
                                            (make-ast-var :name :initial-element)
                                            (make-ast-the
                                             :type 'character
                                             :value (make-ast-quote :value #\x))))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-string-char inst) :to-be-truthy))))

(it-sequential "codegen-phase2-set-fdefinition-the-wrapped-symbol-registers-function"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'set-fdefinition
                                :args (list (make-ast-function :name 'helper)
                                            (make-ast-the
                                             :type 'symbol
                                             :value (make-ast-quote :value 'helper))))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-register-function)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-func-name inst) :to-be 'helper))))

;;; ─── Section 6: FORMAT ──────────────────────────────────────────────────────

(it-sequential "codegen-phase2-format-dispatch nil-dest"
  (destructuring-bind (args scenario) (list (list (make-ast-var :name nil) (make-ast-quote :value "hello")) :nil-dest)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format :args args) ctx)
    (ecase scenario
      (:nil-dest
       (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
         (expect inst :to-be-truthy)
         (expect (cl-cc::vm-const-value inst) :to-equal "hello")
         (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy)
         (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-princ)) :to-be-truthy)))
      (:t-dest
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-princ) :to-be-truthy)
       (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy))
      (:one-arg
       (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy))))))

(it-sequential "codegen-phase2-format-dispatch t-dest"
  (destructuring-bind (args scenario) (list (list (make-ast-var :name t) (make-ast-quote :value "hello")) :t-dest)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format :args args) ctx)
    (ecase scenario
      (:nil-dest
       (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
         (expect inst :to-be-truthy)
         (expect (cl-cc::vm-const-value inst) :to-equal "hello")
         (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy)
         (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-princ)) :to-be-truthy)))
      (:t-dest
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-princ) :to-be-truthy)
       (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy))
      (:one-arg
       (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy))))))

(it-sequential "codegen-phase2-format-dispatch one-arg"
  (destructuring-bind (args scenario) (list (list (make-ast-var :name nil)) :one-arg)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'format :args args) ctx)
    (ecase scenario
      (:nil-dest
       (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
         (expect inst :to-be-truthy)
         (expect (cl-cc::vm-const-value inst) :to-equal "hello")
         (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy)
         (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-princ)) :to-be-truthy)))
      (:t-dest
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-princ) :to-be-truthy)
       (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy))
      (:one-arg
       (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy))))))

;;; ─── Section 7: MAKE-HASH-TABLE ─────────────────────────────────────────────

(it-sequential "codegen-phase2-make-hash-table-cases no-args"
  (destructuring-bind (args test-set-p) (list '() nil)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-hash-table :args args) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (if test-set-p
          (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)
          (expect (null (cl-cc::vm-make-hash-table-test inst)) :to-be-truthy))))))

(it-sequential "codegen-phase2-make-hash-table-cases test-equal"
  (destructuring-bind (args test-set-p) (list (list (make-ast-var :name :test) (make-ast-quote :value 'equal)) t)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-hash-table :args args) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (if test-set-p
          (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)
          (expect (null (cl-cc::vm-make-hash-table-test inst)) :to-be-truthy))))))

;;; ─── Section 8: CONCATENATE ─────────────────────────────────────────────────

(it-sequential "codegen-phase2-concatenate-constant-strings-fold-to-vm-const"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-quote :value "foo")
                                            (make-ast-quote :value "bar")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-const-value inst) :to-equal "foobar")
      (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)) :to-be-truthy))))

(it-sequential "codegen-phase2-concatenate-three-strings-emits-two-concats"
  (let ((ctx (make-ctx-with-vars 'a 'b 'c)))
    (compile-ast (make-ast-call :func 'concatenate
                                :args (list (make-ast-quote :value 'string)
                                            (make-ast-var :name 'a)
                                            (make-ast-var :name 'b)
                                            (make-ast-var :name 'c)))
                 ctx)
    (expect (codegen-count-inst ctx 'cl-cc/vm::vm-concatenate) :to-be 2)))

(it-sequential "codegen-phase2-concatenate-non-string-falls-through non-string-type"
  (destructuring-bind (extra-env args) (list nil (list (make-ast-quote :value 'list)
                 (make-ast-quote :value "a")
                 (make-ast-quote :value "b")))
    (let ((ctx (make-codegen-ctx)))
    (when extra-env
      (setf (cl-cc/compile:ctx-env ctx) extra-env))
    (compile-ast (make-ast-call :func 'concatenate :args args) ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)) :to-be-truthy))))

(it-sequential "codegen-phase2-concatenate-non-string-falls-through unquoted-type"
  (destructuring-bind (extra-env args) (list (list (cons 'string :R99)) (list (make-ast-var :name 'string)
                 (make-ast-quote :value "a")
                 (make-ast-quote :value "b")))
    (let ((ctx (make-codegen-ctx)))
    (when extra-env
      (setf (cl-cc/compile:ctx-env ctx) extra-env))
    (compile-ast (make-ast-call :func 'concatenate :args args) ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)) :to-be-truthy))))
