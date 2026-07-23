;;;; tests/unit/compile/codegen-fold-eval-tests.lisp
;;;; Unit tests for compile-time partial evaluator in codegen-fold-eval.lisp
;;;;
;;;; Covers: %evaluate-ast (all node types), %evaluate-ast-sequence,
;;;;   *compile-time-multi-arg-fns*, *compile-time-unary-pred-fns*,
;;;;   %compile-time-eval-known-call, %compile-time-pair-bindings,
;;;;   %compile-time-append-env, %compile-time-eval-call.

(in-package :cl-cc/test)

;;; ─── %evaluate-ast ────────────────────────────────────────────────────────

(it-sequential "evaluate-ast-constants"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-int :value 17) 10)
      (expect ok :to-be-truthy)
      (expect (= 17 value) :to-be-truthy))
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-quote :value 'hello) 10)
      (expect ok :to-be-truthy)
      (expect value :to-be 'hello))))

(it-sequential "evaluate-ast-var-found-in-env"
  (multiple-value-bind (value ok)
      (let ((cl-cc/compile::*compile-time-value-env* '((n . 42)))
            (cl-cc/compile::*compile-time-function-env* nil))
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-var :name 'n) 10))
    (expect ok :to-be-truthy)
    (expect (= 42 value) :to-be-truthy)))

(it-sequential "evaluate-ast-unbound-variable-returns-nil-nil"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-var :name 'x) 10)
      (expect value :to-be-null)
      (expect ok :to-be-null))))

(it-sequential "evaluate-ast-exhausted-depth-returns-nil-nil"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-int :value 5) -1)
      (expect value :to-be-null)
      (expect ok :to-be-null))))

(it-sequential "evaluate-ast-arithmetic-binop"
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-binop :op '+ :lhs (cl-cc/ast:make-ast-int :value 3)
                                    :rhs (cl-cc/ast:make-ast-int :value 4))
         10))
    (expect ok :to-be-truthy)
    (expect (= 7 value) :to-be-truthy)))

;;; ─── %evaluate-ast-sequence ───────────────────────────────────────────────

(it-sequential "evaluate-ast-sequence-empty-returns-nil-true"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence nil nil nil 10)
      (expect ok :to-be-truthy)
      (expect value :to-be-null))))

(it-sequential "evaluate-ast-sequence-two-constants-returns-last"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence
         (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2))
         nil nil 10)
      (expect ok :to-be-truthy)
      (expect (= 2 value) :to-be-truthy))))

(it-sequential "evaluate-ast-sequence-unknown-var-returns-nil-nil"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence
         (list (cl-cc/ast:make-ast-var :name 'unk-xyz))
         nil nil 10)
      (expect ok :to-be-null)
      (expect value :to-be-null))))

;;; ─── %evaluate-ast (extended) ─────────────────────────────────────────────

(it-sequential "evaluate-ast-ast-if-cases truthy"
  (destructuring-bind (cond-node expected) (list (cl-cc/ast:make-ast-int   :value 1) 42)
    (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-if :cond cond-node
                                 :then (cl-cc/ast:make-ast-int :value 42)
                                 :else (cl-cc/ast:make-ast-int :value 0))
         10))
    (expect ok :to-be-truthy)
    (expect (= expected value) :to-be-truthy))))

(it-sequential "evaluate-ast-ast-if-cases falsy"
  (destructuring-bind (cond-node expected) (list (cl-cc/ast:make-ast-quote :value nil) 0)
    (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-if :cond cond-node
                                 :then (cl-cc/ast:make-ast-int :value 42)
                                 :else (cl-cc/ast:make-ast-int :value 0))
         10))
    (expect ok :to-be-truthy)
    (expect (= expected value) :to-be-truthy))))

(it-sequential "evaluate-ast-progn-two-forms-returns-last-value"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-progn :forms (list (cl-cc/ast:make-ast-int :value 1)
                                                  (cl-cc/ast:make-ast-int :value 2)))
         10)
      (expect ok :to-be-truthy)
      (expect (= 2 value) :to-be-truthy))))

(it-sequential "evaluate-ast-progn-with-unknown-var-returns-nil-nil"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-progn :forms (list (cl-cc/ast:make-ast-var :name 'unk-xyz)))
         10)
      (expect ok :to-be-null)
      (expect value :to-be-null))))

(it-sequential "evaluate-ast-let-binding-returns-bound-value"
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-let :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 5)))
                                  :body (list (cl-cc/ast:make-ast-var :name 'x)))
         10))
    (expect ok :to-be-truthy)
    (expect (= 5 value) :to-be-truthy)))

(it-sequential "evaluate-ast-the-passes-through-to-inner-value"
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-the :type 'fixnum :value (cl-cc/ast:make-ast-int :value 99))
         10))
    (expect ok :to-be-truthy)
    (expect (= 99 value) :to-be-truthy)))

(it-sequential "evaluate-ast-unknown-node-returns-nil"
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-binop :op 'unknown-op :lhs (cl-cc/ast:make-ast-var :name 'x)
                                    :rhs (cl-cc/ast:make-ast-var :name 'y))
         10))
    (expect ok :to-be-null)
    (expect value :to-be-null)))

;;; ─── *compile-time-multi-arg-fns* / *compile-time-unary-pred-fns* ───────────

(it-sequential "compile-time-multi-arg-fns-contains-all-expected"
  (dolist (sym '(+ - * = < <= > >=))
    (expect (member sym cl-cc/compile::*compile-time-multi-arg-fns* :test #'eq) :to-be-truthy)))

(it-sequential "compile-time-unary-pred-fns-contains-all-expected"
  (dolist (sym '(not zerop plusp minusp oddp evenp numberp integerp consp null symbolp stringp functionp))
    (expect (member sym cl-cc/compile::*compile-time-unary-pred-fns* :test #'eq) :to-be-truthy)))

;;; ─── %compile-time-eval-known-call ───────────────────────────────────────────

(it-sequential "compile-time-eval-known-call-multi-arg-fns add"
  (destructuring-bind (name args expected) (list '+ '(3 4) 7)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns sub"
  (destructuring-bind (name args expected) (list '- '(10 3) 7)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns mul"
  (destructuring-bind (name args expected) (list '* '(4 5) 20)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns eq"
  (destructuring-bind (name args expected) (list '= '(7 7) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns lt"
  (destructuring-bind (name args expected) (list '< '(3 5) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns lte"
  (destructuring-bind (name args expected) (list '<= '(5 5) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns gt"
  (destructuring-bind (name args expected) (list '> '(9 3) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-multi-arg-fns gte"
  (destructuring-bind (name args expected) (list '>= '(4 4) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds not-nil"
  (destructuring-bind (name args expected) (list 'not '(nil) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds not-t"
  (destructuring-bind (name args expected) (list 'not '(t) nil)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds zerop"
  (destructuring-bind (name args expected) (list 'zerop '(0) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds plusp"
  (destructuring-bind (name args expected) (list 'plusp '(5) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds minusp"
  (destructuring-bind (name args expected) (list 'minusp '(-3) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds oddp"
  (destructuring-bind (name args expected) (list 'oddp '(3) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds evenp"
  (destructuring-bind (name args expected) (list 'evenp '(4) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds numberp"
  (destructuring-bind (name args expected) (list 'numberp '(42) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds integerp"
  (destructuring-bind (name args expected) (list 'integerp '(1) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds consp"
  (destructuring-bind (name args expected) (list 'consp '((a)) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds null-nil"
  (destructuring-bind (name args expected) (list 'null '(nil) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds symbolp"
  (destructuring-bind (name args expected) (list 'symbolp '(foo) t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-unary-preds stringp"
  (destructuring-bind (name args expected) (list 'stringp '("hi") t)
    (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (expect ok :to-be-truthy)
    (expect result :to-equal expected))))

(it-sequential "compile-time-eval-known-call-division-integer"
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call '/ '(12 4))
    (expect ok :to-be-truthy)
    (expect (= 3 result) :to-be-truthy)))

(it-sequential "compile-time-eval-known-call-division-ratio"
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call '/ '(7 2))
    (expect ok :to-be-truthy)
    (expect result :to-equal 7/2)))

(it-sequential "compile-time-eval-known-call-division-by-zero"
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call '/ '(5 0))
    (expect ok :to-be-null)
    (expect result :to-be-null)))

(it-sequential "compile-time-eval-known-call-unknown-returns-nil-nil"
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call 'totally-unknown-fn '(1))
    (expect result :to-be-null)
    (expect ok :to-be-null)))

;;; ─── %compile-time-pair-bindings ─────────────────────────────────────────────

(it-sequential "compile-time-pair-bindings-cases empty"
  (destructuring-bind (params args expected) (list nil nil nil)
    (expect (cl-cc/compile::%compile-time-pair-bindings params args) :to-equal expected)))

(it-sequential "compile-time-pair-bindings-cases single"
  (destructuring-bind (params args expected) (list '(x) '(1) '((x . 1)))
    (expect (cl-cc/compile::%compile-time-pair-bindings params args) :to-equal expected)))

(it-sequential "compile-time-pair-bindings-cases multi"
  (destructuring-bind (params args expected) (list '(x y z) '(1 2 3) '((x . 1) (y . 2) (z . 3)))
    (expect (cl-cc/compile::%compile-time-pair-bindings params args) :to-equal expected)))

(it-sequential "compile-time-pair-bindings-cases fewer"
  (destructuring-bind (params args expected) (list '(x y) '(1) '((x . 1)))
    (expect (cl-cc/compile::%compile-time-pair-bindings params args) :to-equal expected)))

;;; ─── %compile-time-append-env ────────────────────────────────────────────────

(it-sequential "compile-time-append-env-prepends-bindings"
  (let ((result (cl-cc/compile::%compile-time-append-env
                 '((a . 1) (b . 2)) '((c . 3)))))
    (expect result :to-equal '((a . 1) (b . 2) (c . 3)))))

(it-sequential "compile-time-append-env-empty-bindings-returns-env"
  (let ((env '((x . 99))))
    (expect (cl-cc/compile::%compile-time-append-env nil env) :to-equal env)))

;;; ─── %evaluate-ast block/return-from ────────────────────────────────────────

(it-sequential "evaluate-ast-block-normal-exit"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-block
          :name 'done
          :body (list (cl-cc/ast:make-ast-int :value 42)))
         10)
      (expect ok :to-be-truthy)
      (expect (= 42 value) :to-be-truthy))))

(it-sequential "evaluate-ast-return-from-exits-block"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-block
          :name 'early
          :body (list
                 (cl-cc/ast:make-ast-return-from
                  :name 'early
                  :value (cl-cc/ast:make-ast-int :value 7))
                 (cl-cc/ast:make-ast-int :value 99)))
         10)
      (expect ok :to-be-truthy)
      (expect (= 7 value) :to-be-truthy))))

;;; ─── %compile-time-eval-call ──────────────────────────────────────────────

(it-sequential "compile-time-eval-call-string-length-folds"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'string-length) (list "hello") 10)
      (expect ok :to-be-truthy)
      (expect (= 5 value) :to-be-truthy))))

(it-sequential "compile-time-eval-call-intern-folds-explicit-package"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'intern) (list "CAR" :cl) 10)
      (expect ok :to-be-truthy)
      (expect value :to-be 'cl:car))))

(it-sequential "compile-time-eval-call-intern-folds-string-package"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'intern) (list "CDR" "CL") 10)
      (expect ok :to-be-truthy)
      (expect value :to-be 'cl:cdr))))

(it-sequential "compile-time-eval-call-intern-rejects-implicit-package"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'intern) (list "CAR") 10)
      (expect ok :to-be-null)
      (expect value :to-be-null))))

(it-sequential "compile-time-eval-call-builtin-not-folds"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'not) (list nil) 10)
      (expect ok :to-be-truthy)
      (expect value :to-be-truthy))))

(it-sequential "compile-time-eval-call-ast-function-folds"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-function :name 'not) (list nil) 10)
      (expect ok :to-be-truthy)
      (expect value :to-be-truthy))))

(it-sequential "compile-time-eval-call-quoted-function-designator-folds"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-quote :value '+) (list 1 2 3) 10)
      (expect ok :to-be-truthy)
      (expect (= 6 value) :to-be-truthy))))

(it-sequential "compile-time-eval-call-lambda-application-folds"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-lambda :params '(x)
                                     :body (list (cl-cc/ast:make-ast-var :name 'x))
                                     :optional-params nil :rest-param nil :key-params nil)
         (list 7) 10)
      (expect ok :to-be-truthy)
      (expect (= 7 value) :to-be-truthy))))

(it-sequential "compile-time-eval-call-ast-the-wrapped-lambda-application-folds"
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-the
          :type 'function
          :value (cl-cc/ast:make-ast-lambda :params '(x)
                                            :body (list (cl-cc/ast:make-ast-var :name 'x))
                                            :optional-params nil :rest-param nil :key-params nil))
         (list 7) 10)
      (expect ok :to-be-truthy)
      (expect (= 7 value) :to-be-truthy))))

(it-sequential "compile-time-eval-call-unknown-function-returns-nil"
  (%with-clean-ct-env
    (expect (cl-cc/compile::%compile-time-eval-call
                  (cl-cc/ast:make-ast-var :name 'completely-unknown-fn-xyz)
                  (list 1) 10) :to-be-null)))
