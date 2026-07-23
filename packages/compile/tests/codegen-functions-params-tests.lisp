;;;; tests/unit/compile/codegen-functions-params-tests.lisp
;;;; Unit tests for src/compile/codegen-functions-params.lisp
;;;;
;;;; Covers: extract-constant-value, dynamic-extent-declared-p,
;;;;   rest-param-stack-alloc-p, build-all-param-bindings.
;;;;
;;;; All tested functions are pure (no ctx/emit side effects), so no
;;;; compilation context is needed.

(in-package :cl-cc/test)

;;; ─── extract-constant-value ──────────────────────────────────────────────

(it-sequential "extract-constant-value-constants int-42"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-int   :value 42) 42 t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-constants int-0"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-int   :value 0) 0 t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-constants int-neg"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-int   :value -7) -7 t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-constants quote-symbol"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-quote :value 'hello) 'hello t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-constants quote-list"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-quote :value '(1 2)) '(1 2) t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-constants var-t"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-var   :name 't) t t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-constants var-nil"
  (destructuring-bind (ast expected-val expected-const) (list (cl-cc/ast:make-ast-var   :name 'nil) nil t)
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-equal expected-val)
    (expect is-const :to-equal expected-const))))

(it-sequential "extract-constant-value-non-constants var-x"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-var  :name 'x))
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-be-null)
    (expect is-const :to-be-falsy))))

(it-sequential "extract-constant-value-non-constants call"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-call  :func 'f :args nil))
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-be-null)
    (expect is-const :to-be-falsy))))

(it-sequential "extract-constant-value-non-constants binop"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-binop :op '+ :lhs (cl-cc/ast:make-ast-int :value 1)
                                                         :rhs (cl-cc/ast:make-ast-int :value 2)))
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-be-null)
    (expect is-const :to-be-falsy))))

(it-sequential "extract-constant-value-non-constants lambda"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-lambda :params '(x) :body nil))
    (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (expect val :to-be-null)
    (expect is-const :to-be-falsy))))

;;; ─── dynamic-extent-declared-p ───────────────────────────────────────────

(it-sequential "dynamic-extent-declared-p-truthy single"
  (destructuring-bind (declarations name) (list '((dynamic-extent rest)) 'rest)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-truthy)))

(it-sequential "dynamic-extent-declared-p-truthy multi"
  (destructuring-bind (declarations name) (list '((dynamic-extent a b rest)) 'rest)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-truthy)))

(it-sequential "dynamic-extent-declared-p-truthy first"
  (destructuring-bind (declarations name) (list '((dynamic-extent args)) 'args)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-truthy)))

(it-sequential "dynamic-extent-declared-p-truthy mixed-decls"
  (destructuring-bind (declarations name) (list '((ignore x) (dynamic-extent r) (type integer n)) 'r)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-truthy)))

(it-sequential "dynamic-extent-declared-p-falsy empty"
  (destructuring-bind (declarations name) (list '() 'rest)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-falsy)))

(it-sequential "dynamic-extent-declared-p-falsy other-decl"
  (destructuring-bind (declarations name) (list '((ignore rest)) 'rest)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-falsy)))

(it-sequential "dynamic-extent-declared-p-falsy wrong-name"
  (destructuring-bind (declarations name) (list '((dynamic-extent other)) 'rest)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-falsy)))

(it-sequential "dynamic-extent-declared-p-falsy nil-decls"
  (destructuring-bind (declarations name) (list nil 'args)
    (expect (cl-cc/compile::dynamic-extent-declared-p declarations name) :to-be-falsy)))

;;; ─── rest-param-stack-alloc-p ────────────────────────────────────────────

(it-sequential "rest-param-stack-alloc-p-non-list-body"
  (expect (cl-cc/compile::rest-param-stack-alloc-p nil 'rest) :to-be-falsy)
  (expect (cl-cc/compile::rest-param-stack-alloc-p 'x  'rest) :to-be-falsy))

(it-sequential "rest-param-stack-alloc-p-safe-consumers"
  (let ((body-forms (list `(car rest))))
    (expect (cl-cc/compile::rest-param-stack-alloc-p body-forms 'rest) :to-be-truthy)))

;;; ─── build-all-param-bindings ────────────────────────────────────────────

(it-sequential "build-all-param-bindings-required-only"
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(a b c) '(:r0 :r1 :r2) nil nil nil)))
    (expect result :to-equal '((a . :r0) (b . :r1) (c . :r2)))))

(it-sequential "build-all-param-bindings-with-optional"
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(x) '(:r0)
                 '((opt . :r1))
                 nil nil)))
    (expect result :to-equal '((x . :r0) (opt . :r1)))))

(it-sequential "build-all-param-bindings-with-rest"
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(x) '(:r0) nil '(rest . :r2) nil)))
    (expect result :to-equal '((x . :r0) (rest . :r2)))))

(it-sequential "build-all-param-bindings-with-key"
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(a) '(:r0) nil nil '((kw . :r3)))))
    (expect result :to-equal '((a . :r0) (kw . :r3)))))

(it-sequential "build-all-param-bindings-combined all-present"
  (destructuring-bind (params param-regs opt-bindings rest-binding key-bindings expected) (list '(a) '(:r0) '((b . :r1)) '(c . :r2) '((d . :r3)) '((a . :r0) (b . :r1) (c . :r2) (d . :r3)))
    (expect (cl-cc/compile::build-all-param-bindings
                 params param-regs opt-bindings rest-binding key-bindings) :to-equal expected)))

(it-sequential "build-all-param-bindings-combined empty"
  (destructuring-bind (params param-regs opt-bindings rest-binding key-bindings expected) (list nil nil nil nil nil nil)
    (expect (cl-cc/compile::build-all-param-bindings
                 params param-regs opt-bindings rest-binding key-bindings) :to-equal expected)))

(it-sequential "build-all-param-bindings-preserves-order"
  (let* ((params '(x y z))
         (regs   '(:ra :rb :rc))
         (result (cl-cc/compile::build-all-param-bindings params regs nil nil nil)))
    (expect (car (first result)) :to-equal 'x)
    (expect (cdr (first result)) :to-equal :ra)
    (expect (car (second result)) :to-equal 'y)
    (expect (car (third result)) :to-equal 'z)))
