;;;; tests/unit/ast/ast-analysis-tests.lisp — AST Analysis Tests
;;;;
;;;; Tests for ast-children (structural data layer) and ast-bound-names
;;;; (scoping data layer), plus free-variable and mutation analysis.
;;;;
;;;; Helper %ast-roundtrip is defined in ast-tests.lisp (loaded first).

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; ast-children — structural data layer
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-children-leaves int"
  (destructuring-bind (node) (list (cl-cc:make-ast-int :value 42))
    (expect (cl-cc:ast-children node) :to-be-null)))

(it-sequential "ast-children-leaves var"
  (destructuring-bind (node) (list (cl-cc:make-ast-var :name 'x))
    (expect (cl-cc:ast-children node) :to-be-null)))

(it-sequential "ast-children-leaves hole"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-hole))
    (expect (cl-cc:ast-children node) :to-be-null)))

(it-sequential "ast-children-leaves quote"
  (destructuring-bind (node) (list (cl-cc:make-ast-quote :value 'hello))
    (expect (cl-cc:ast-children node) :to-be-null)))

(it-sequential "ast-children-leaves function"
  (destructuring-bind (node) (list (cl-cc:make-ast-function :name 'foo))
    (expect (cl-cc:ast-children node) :to-be-null)))

(it-sequential "ast-children-leaves go"
  (destructuring-bind (node) (list (cl-cc:make-ast-go :tag 'start))
    (expect (cl-cc:ast-children node) :to-be-null)))

(it-sequential "ast-children-node-types"
  (let* ((lhs (cl-cc:make-ast-int :value 1))
         (rhs (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-binop :op '+ :lhs lhs :rhs rhs))
         (children (cl-cc:ast-children node)))
    (expect (length children) :to-equal 2)
    (expect (first children) :to-be lhs)
    (expect (second children) :to-be rhs))
  (let* ((c (cl-cc:make-ast-int :value 1))
         (th (cl-cc:make-ast-int :value 2))
         (el (cl-cc:make-ast-int :value 3))
         (node (cl-cc:make-ast-if :cond c :then th :else el)))
    (expect (length (cl-cc:ast-children node)) :to-equal 3))
  (let* ((f1 (cl-cc:make-ast-int :value 1))
         (f2 (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-progn :forms (list f1 f2))))
    (expect (length (cl-cc:ast-children node)) :to-equal 2))
  (let* ((init (cl-cc:make-ast-int :value 1))
         (body (cl-cc:make-ast-var :name 'x))
         (node (cl-cc:make-ast-let :bindings (list (cons 'x init))
                                   :body (list body)))
         (children (cl-cc:ast-children node)))
    (expect (length children) :to-equal 2)
    (expect (member init children :test #'eq) :to-be-truthy)
    (expect (member body children :test #'eq) :to-be-truthy))
  (let* ((body (cl-cc:make-ast-var :name 'x))
         (node (cl-cc:make-ast-lambda :params '(x) :body (list body))))
    (expect (length (cl-cc:ast-children node)) :to-equal 1))
  (let* ((val (cl-cc:make-ast-int :value 42))
         (node (cl-cc:make-ast-setq :var 'x :value val)))
    (expect (length (cl-cc:ast-children node)) :to-equal 1)
    (expect (first (cl-cc:ast-children node)) :to-be val))
  (let* ((arg1 (cl-cc:make-ast-int :value 1))
         (arg2 (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-call :func 'foo :args (list arg1 arg2))))
    (expect (length (cl-cc:ast-children node)) :to-equal 2))
  (let* ((func (cl-cc:make-ast-var :name 'f))
         (arg1 (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-call :func func :args (list arg1))))
    (expect (length (cl-cc:ast-children node)) :to-equal 2)
    (expect (first (cl-cc:ast-children node)) :to-be func))
  (let* ((body (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-block :name 'b :body (list body))))
    (expect (length (cl-cc:ast-children node)) :to-equal 1))
  (let* ((tag (cl-cc:make-ast-var :name 'tag))
         (body (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-catch :tag tag :body (list body))))
    (expect (length (cl-cc:ast-children node)) :to-equal 2)
    (expect (first (cl-cc:ast-children node)) :to-be tag))
  (let* ((tag (cl-cc:make-ast-var :name 'tag))
         (val (cl-cc:make-ast-int :value 42))
         (node (cl-cc:make-ast-throw :tag tag :value val)))
    (expect (length (cl-cc:ast-children node)) :to-equal 2))
  (let* ((val (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-the :type 'fixnum :value val)))
    (expect (length (cl-cc:ast-children node)) :to-equal 1)
    (expect (first (cl-cc:ast-children node)) :to-be val))
  (let* ((val (cl-cc:make-ast-int :value 0))
         (node (cl-cc/ast:make-ast-defvar :name '*x* :value val)))
    (expect (length (cl-cc:ast-children node)) :to-equal 1))
  (let ((node (cl-cc/ast:make-ast-defvar :name '*x*)))
    (expect (cl-cc:ast-children node) :to-be-null)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; ast-bound-names — scoping data layer
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-bound-names-binding-forms let"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-let :bindings (list (cons 'x (cl-cc:make-ast-int :value 1))
                                        (cons 'y (cl-cc:make-ast-int :value 2)))
                        :body (list (cl-cc:make-ast-var :name 'x))) '(x y))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms lambda-required"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-lambda :params '(a b) :body (list (cl-cc:make-ast-var :name 'a))) '(a b))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms lambda-optional"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-lambda :params '(a)
                           :optional-params '((b nil))
                           :body (list (cl-cc:make-ast-var :name 'a))) '(a b))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms lambda-rest"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-lambda :params '(a)
                           :rest-param 'rest
                           :body (list (cl-cc:make-ast-var :name 'a))) '(a rest))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms defun"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-defun :name 'foo :params '(x y)
                           :body (list (cl-cc:make-ast-var :name 'x))) '(x y))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms flet"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-flet :bindings (list (list 'f '(x) (cl-cc:make-ast-var :name 'x)))
                         :body (list (cl-cc:make-ast-int :value 1))) '(f))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms labels"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-labels :bindings (list (list 'g '(x) (cl-cc:make-ast-var :name 'x)))
                           :body (list (cl-cc:make-ast-int :value 1))) '(g))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-binding-forms mvb"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-multiple-value-bind
     :vars '(a b c)
     :values-form (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)))
     :body (list (cl-cc:make-ast-var :name 'a))) '(a b c))
    (expect (cl-cc:ast-bound-names node) :to-equal expected)))

(it-sequential "ast-bound-names-non-binding"
  (expect (cl-cc:ast-bound-names (cl-cc:make-ast-int :value 42)) :to-be-null)
  (expect (cl-cc:ast-bound-names (cl-cc:make-ast-if
                                       :cond (cl-cc:make-ast-int :value 1)
                                       :then (cl-cc:make-ast-int :value 2)
                                       :else (cl-cc:make-ast-int :value 3))) :to-be-null))
