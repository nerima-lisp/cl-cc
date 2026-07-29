;;;; tests/unit/compile/codegen-fold-tests.lisp
;;;; Unit tests for compile-time fold helpers in codegen-fold.lisp
;;;;
;;;; Covers: %ast-constant-number-value, %fold-ast-binop, *compile-time-eval-fns*,
;;;;   %compile-time-eval-binop, %compile-time-lookup, %ast-constant-node-p,
;;;;   %ast->compile-time-value, %compile-time-value->ast.

(in-package :cl-cc/test)

(defmacro %with-clean-ct-env (&body body)
  "Bind a clean compile-time environment for evaluate-ast tests."
  `(let ((cl-cc/compile::*compile-time-value-env* nil)
         (cl-cc/compile::*compile-time-function-env* nil))
     ,@body))

;;; ─── %ast-constant-number-value ───────────────────────────────────────────

(it-sequential "ast-constant-number-value-extracts-integer ast-int"
  (destructuring-bind (expected node) (list 42 (cl-cc/ast:make-ast-int   :value 42))
    (expect (= expected (cl-cc/compile::%ast-constant-number-value node)) :to-be-truthy)))

(it-sequential "ast-constant-number-value-extracts-integer ast-quote-integer"
  (destructuring-bind (expected node) (list 7 (cl-cc/ast:make-ast-quote :value 7))
    (expect (= expected (cl-cc/compile::%ast-constant-number-value node)) :to-be-truthy)))

(it-sequential "ast-constant-number-value-nil-for-non-integer-quote"
  (expect (cl-cc/compile::%ast-constant-number-value (cl-cc/ast:make-ast-quote :value "hello")) :to-be-null))

(it-sequential "ast-constant-number-value-nil-for-variable-node"
  (expect (cl-cc/compile::%ast-constant-number-value (cl-cc/ast:make-ast-var :name 'x)) :to-be-null))

;;; ─── %ast-constant-node-p ─────────────────────────────────────────────────

(it-sequential "ast-constant-node-p-true-for-constants int"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-int   :value 1))
    (expect (cl-cc/compile::%ast-constant-node-p node) :to-be-truthy)))

(it-sequential "ast-constant-node-p-true-for-constants quote"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-quote :value 'x))
    (expect (cl-cc/compile::%ast-constant-node-p node) :to-be-truthy)))

(it-sequential "ast-constant-node-p-false-for-var"
  (expect (cl-cc/compile::%ast-constant-node-p (cl-cc/ast:make-ast-var :name 'x)) :to-be-null))

;;; ─── %ast->compile-time-value ─────────────────────────────────────────────

(it-sequential "ast->compile-time-value-extracts-integer-from-ast-int"
  (expect (= 99 (cl-cc/compile::%ast->compile-time-value (cl-cc/ast:make-ast-int :value 99))) :to-be-truthy))

(it-sequential "ast->compile-time-value-extracts-list-from-ast-quote"
  (expect (cl-cc/compile::%ast->compile-time-value (cl-cc/ast:make-ast-quote :value '(a b))) :to-equal '(a b)))

(it-sequential "ast->compile-time-value-returns-nil-for-variable"
  (expect (cl-cc/compile::%ast->compile-time-value (cl-cc/ast:make-ast-var :name 'x)) :to-be-null))

(it-sequential "ast-constant-helpers-see-through-ast-the number-value"
  (destructuring-bind (fn node expected) (list #'cl-cc/compile::%ast-constant-number-value (cl-cc/ast:make-ast-the
            :type 'integer
            :value (cl-cc/ast:make-ast-quote :value 7)) 7)
    (if (eq expected t)
      (expect (funcall fn node) :to-be-truthy)
      (expect (funcall fn node) :to-equal expected))))

(it-sequential "ast-constant-helpers-see-through-ast-the constant-node-p"
  (destructuring-bind (fn node expected) (list #'cl-cc/compile::%ast-constant-node-p (cl-cc/ast:make-ast-the
            :type 'integer
            :value (cl-cc/ast:make-ast-int :value 7)) t)
    (if (eq expected t)
      (expect (funcall fn node) :to-be-truthy)
      (expect (funcall fn node) :to-equal expected))))

(it-sequential "ast-constant-helpers-see-through-ast-the compile-time-value"
  (destructuring-bind (fn node expected) (list #'cl-cc/compile::%ast->compile-time-value (cl-cc/ast:make-ast-the
            :type 'integer
            :value (cl-cc/ast:make-ast-quote :value '(a b))) '(a b))
    (if (eq expected t)
      (expect (funcall fn node) :to-be-truthy)
      (expect (funcall fn node) :to-equal expected))))

;;; ─── %compile-time-value->ast ─────────────────────────────────────────────

(it-sequential "compile-time-value->ast-wraps-integers-and-symbols"
  (let* ((proto     (cl-cc/ast:make-ast-int :value 0))
         (int-node  (cl-cc/compile::%compile-time-value->ast 5     proto))
         (sym-node  (cl-cc/compile::%compile-time-value->ast 'hello proto)))
    (expect (typep int-node 'cl-cc::ast-int) :to-be-truthy)
    (expect (= 5 (cl-cc/ast:ast-int-value int-node)) :to-be-truthy)
    (expect (typep sym-node 'cl-cc::ast-quote) :to-be-truthy)
    (expect (cl-cc/ast:ast-quote-value sym-node) :to-be 'hello)))

;;; ─── %compile-time-eval-binop ─────────────────────────────────────────────

(it-sequential "compile-time-eval-binop-arithmetic add"
  (destructuring-bind (op a b expected) (list '+ 3 4 7)
    (expect (= expected (cl-cc/compile::%compile-time-eval-binop op a b)) :to-be-truthy)))

(it-sequential "compile-time-eval-binop-arithmetic sub"
  (destructuring-bind (op a b expected) (list '- 9 4 5)
    (expect (= expected (cl-cc/compile::%compile-time-eval-binop op a b)) :to-be-truthy)))

(it-sequential "compile-time-eval-binop-arithmetic mul"
  (destructuring-bind (op a b expected) (list '* 3 7 21)
    (expect (= expected (cl-cc/compile::%compile-time-eval-binop op a b)) :to-be-truthy)))

(it-sequential "compile-time-eval-binop-division exact"
  (destructuring-bind (lhs rhs expected) (list 12 3 4)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs) :to-be-null))))

(it-sequential "compile-time-eval-binop-division non-exact"
  (destructuring-bind (lhs rhs expected) (list 7 2 nil)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs) :to-be-null))))

(it-sequential "compile-time-eval-binop-division div-zero"
  (destructuring-bind (lhs rhs expected) (list 5 0 nil)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs) :to-be-null))))

(it-sequential "compile-time-eval-binop-unary 1+ integer"
  (destructuring-bind (op lhs rhs expected) (list '1+ 5 nil 6)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop op lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop op lhs rhs) :to-be-null))))

(it-sequential "compile-time-eval-binop-unary 1- integer"
  (destructuring-bind (op lhs rhs expected) (list '1- 5 nil 4)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop op lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop op lhs rhs) :to-be-null))))

(it-sequential "compile-time-eval-binop-unary 1+ non-nil-rhs"
  (destructuring-bind (op lhs rhs expected) (list '1+ 5 99 nil)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop op lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop op lhs rhs) :to-be-null))))

(it-sequential "compile-time-eval-binop-unary 1- non-nil-rhs"
  (destructuring-bind (op lhs rhs expected) (list '1- 5 99 nil)
    (if expected
      (expect (= expected (cl-cc/compile::%compile-time-eval-binop op lhs rhs)) :to-be-truthy)
      (expect (cl-cc/compile::%compile-time-eval-binop op lhs rhs) :to-be-null))))

;;; ─── %compile-time-lookup ─────────────────────────────────────────────────

(it-sequential "compile-time-lookup-cases found"
  (destructuring-bind (name alist expected-value expected-found) (list 'x '((x . 42) (y . 7)) 42 t)
    (multiple-value-bind (value found-p)
      (cl-cc/compile::%compile-time-lookup name alist)
    (expect value :to-equal expected-value)
    (expect found-p :to-equal expected-found))))

(it-sequential "compile-time-lookup-cases not-found"
  (destructuring-bind (name alist expected-value expected-found) (list 'z '((x . 1)  (y . 2)) nil nil)
    (multiple-value-bind (value found-p)
      (cl-cc/compile::%compile-time-lookup name alist)
    (expect value :to-equal expected-value)
    (expect found-p :to-equal expected-found))))

;;; ─── *compile-time-eval-fns* ──────────────────────────────────────────────

(it-sequential "compile-time-eval-fns-registered"
  (dolist (sym '(+ - * / not zerop null numberp integerp symbolp))
    (expect (gethash sym cl-cc/compile::*compile-time-eval-fns*) :to-be-truthy)))

(it-sequential "compile-time-eval-fns-evaluation-cases plus-sum"
  (destructuring-bind (fn-name args expected) (list '+ '(3 4 5) 12)
    (let ((fn (gethash fn-name cl-cc/compile::*compile-time-eval-fns*)))
    (multiple-value-bind (result ok)
        (funcall fn args)
      (expect ok :to-be-truthy)
      (expect result :to-equal expected)))))

(it-sequential "compile-time-eval-fns-evaluation-cases null-nil"
  (destructuring-bind (fn-name args expected) (list 'null '(nil) t)
    (let ((fn (gethash fn-name cl-cc/compile::*compile-time-eval-fns*)))
    (multiple-value-bind (result ok)
        (funcall fn args)
      (expect ok :to-be-truthy)
      (expect result :to-equal expected)))))

;;; ─── %fold-ast-binop ──────────────────────────────────────────────────────

(it-sequential "fold-ast-binop-folds-integer-literals"
  (let* ((node (cl-cc/ast:make-ast-binop
                :op '+
                :lhs (cl-cc/ast:make-ast-int :value 0)
                :rhs (cl-cc/ast:make-ast-int :value 0)))
         (lhs  (cl-cc/ast:make-ast-int :value 10))
         (rhs  (cl-cc/ast:make-ast-int :value 32))
         (result (cl-cc/compile::%fold-ast-binop node lhs rhs)))
    (expect (typep result 'cl-cc::ast-int) :to-be-truthy)
    (expect (= 42 (cl-cc/ast:ast-int-value result)) :to-be-truthy)))

(it-sequential "fold-ast-binop-does-not-fold-non-constants"
  (let* ((node (cl-cc/ast:make-ast-binop
                :op '+
                :lhs (cl-cc/ast:make-ast-int :value 0)
                :rhs (cl-cc/ast:make-ast-int :value 0)))
         (lhs  (cl-cc/ast:make-ast-var :name 'x))
         (rhs  (cl-cc/ast:make-ast-int :value 5))
         (result (cl-cc/compile::%fold-ast-binop node lhs rhs)))
    (expect (typep result 'cl-cc::ast-binop) :to-be-truthy)))

(it-sequential "optimize-ast-folds-literal-intern-call-to-quote"
  (%with-clean-ct-env
    (let* ((node (cl-cc/ast:make-ast-call
                  :func (cl-cc/ast:make-ast-var :name 'intern)
                  :args (list (cl-cc/ast:make-ast-quote :value "CAR")
                              (cl-cc/ast:make-ast-quote :value :cl))))
           (result (cl-cc/compile::optimize-ast node)))
      (expect (typep result 'cl-cc::ast-quote) :to-be-truthy)
      (expect (cl-cc/ast:ast-quote-value result) :to-be 'cl:car))))

(it-sequential "optimize-ast-folds-call-through-ast-the"
  (%with-clean-ct-env
    (let* ((node (cl-cc/ast:make-ast-call
                  :func (cl-cc/ast:make-ast-the
                         :type 'function
                         :value (cl-cc/ast:make-ast-var :name 'intern))
                  :args (list (cl-cc/ast:make-ast-quote :value "CAR")
                              (cl-cc/ast:make-ast-quote :value :cl))))
           (result (cl-cc/compile::optimize-ast node)))
      (expect (typep result 'cl-cc::ast-quote) :to-be-truthy)
      (expect (cl-cc/ast:ast-quote-value result) :to-be 'cl:car))))

(it-sequential "optimize-ast-folds-call-through-ast-function"
  (%with-clean-ct-env
    (let* ((node (cl-cc/ast:make-ast-call
                  :func (cl-cc/ast:make-ast-function :name 'not)
                  :args (list (cl-cc/ast:make-ast-quote :value nil))))
           (result (cl-cc/compile::optimize-ast node)))
      (expect (typep result 'cl-cc::ast-quote) :to-be-truthy)
      (expect (cl-cc/ast:ast-quote-value result) :to-be-truthy))))

(it-sequential "optimize-ast-folds-call-through-ast-quote"
  (%with-clean-ct-env
    (let* ((node (cl-cc/ast:make-ast-call
                  :func (cl-cc/ast:make-ast-quote :value 'not)
                  :args (list (cl-cc/ast:make-ast-quote :value nil))))
           (result (cl-cc/compile::optimize-ast node)))
      (expect (typep result 'cl-cc::ast-quote) :to-be-truthy)
      (expect (cl-cc/ast:ast-quote-value result) :to-be-truthy))))
