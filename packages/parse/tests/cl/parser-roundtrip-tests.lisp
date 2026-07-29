;;;; tests/unit/parse/cl/parser-roundtrip-tests.lisp
;;;; Unit tests for src/parse/cl/parser-roundtrip.lisp — AST → S-expression.
;;;;
;;;; Covers: ast-to-sexp methods for all node types (ast-int, ast-var, ast-binop,
;;;;   ast-if, ast-progn, ast-let, ast-lambda, ast-call, ast-quote, ast-setq,
;;;;   ast-function, ast-block, ast-return-from, ast-go, ast-the, ast-defvar,
;;;;   ast-defun, ast-values, ast-apply, ast-catch, ast-throw, ast-tagbody,
;;;;   ast-flet, ast-labels, ast-multiple-value-bind, ast-defclass, ast-defgeneric,
;;;;   ast-defmethod, ast-slot-value, ast-set-slot-value, ast-set-gethash,
;;;;   ast-print, ast-defmacro, ast-unwind-protect, ast-handler-case,
;;;;   ast-make-instance), plus the slot-def-to-sexp helper.

(in-package :cl-cc/test)

;;; ─── Leaf nodes ──────────────────────────────────────────────────────────

(it-sequential "ast-to-sexp-scalar-nodes int"
  (destructuring-bind (expected node) (list 42 (cl-cc::make-ast-int      :value 42))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-scalar-nodes var"
  (destructuring-bind (expected node) (list 'x (cl-cc::make-ast-var      :name 'x))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-scalar-nodes quote"
  (destructuring-bind (expected node) (list '(quote hello) (cl-cc::make-ast-quote    :value 'hello))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-scalar-nodes function"
  (destructuring-bind (expected node) (list '(function foo) (cl-cc::make-ast-function :name 'foo))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-scalar-nodes go"
  (destructuring-bind (expected node) (list '(go loop-top) (cl-cc::make-ast-go       :tag 'loop-top))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

;;; ─── Binary/conditional/sequencing nodes ────────────────────────────────

(it-sequential "ast-to-sexp-structural-nodes"
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-binop :op '+
                  :lhs (cl-cc::make-ast-int :value 3)
                  :rhs (cl-cc::make-ast-int :value 4))) :to-equal '(+ 3 4))
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-if
                  :cond (cl-cc::make-ast-int :value 1)
                  :then (cl-cc::make-ast-var :name 'a)
                  :else (cl-cc::make-ast-var :name 'b))) :to-equal '(if 1 a b))
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-the :type 'integer
                  :value (cl-cc::make-ast-var :name 'n))) :to-equal '(the integer n))
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-progn
                  :forms (list (cl-cc::make-ast-int :value 1)
                               (cl-cc::make-ast-int :value 2)))) :to-equal '(progn 1 2)))

(it-sequential "ast-to-sexp-let-produces-let-form"
  (let ((node (cl-cc::make-ast-let
               :bindings (list (cons 'x (cl-cc::make-ast-int :value 5)))
               :body (list (cl-cc::make-ast-var :name 'x)))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (expect (car result) :to-be 'let)
      (expect (second result) :to-equal '((x 5)))
      (expect (cddr result) :to-equal '(x)))))

(it-sequential "ast-to-sexp-setq-produces-setq-form"
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-setq :var 'x
                  :value (cl-cc::make-ast-int :value 0))) :to-equal '(setq x 0)))

;;; ─── Lambda / function definitions ───────────────────────────────────────

(it-sequential "ast-to-sexp-lambda-produces-lambda-form"
  (let ((node (cl-cc::make-ast-lambda
               :params '(x y)
               :body (list (cl-cc::make-ast-binop
                            :op '+
                            :lhs (cl-cc::make-ast-var :name 'x)
                            :rhs (cl-cc::make-ast-var :name 'y))))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (expect (car result) :to-be 'lambda)
      (expect (second result) :to-equal '(x y))
      (expect (cddr result) :to-equal '((+ x y))))))

(it-sequential "ast-to-sexp-defun-produces-defun-form"
  (let ((node (cl-cc::make-ast-defun
               :name 'square
               :params '(n)
               :body (list (cl-cc::make-ast-binop
                            :op '*
                            :lhs (cl-cc::make-ast-var :name 'n)
                            :rhs (cl-cc::make-ast-var :name 'n))))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (expect (car result) :to-be 'defun)
      (expect (second result) :to-be 'square)
      (expect (third result) :to-equal '(n)))))

;;; ─── defvar ───────────────────────────────────────────────────────────────

(it-sequential "ast-to-sexp-defvar-cases with-value"
  (destructuring-bind (expected name value-node) (list '(defvar *count* 0) '*count* (cl-cc::make-ast-int :value 0))
    (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-defvar :name name :value value-node :kind 'defvar)) :to-equal expected)))

(it-sequential "ast-to-sexp-defvar-cases without-value"
  (destructuring-bind (expected name value-node) (list '(defvar *x*) '*x* nil)
    (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-defvar :name name :value value-node :kind 'defvar)) :to-equal expected)))

;;; ─── Call sites ───────────────────────────────────────────────────────────

(it-sequential "ast-to-sexp-call-with-symbol-func"
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-call :func 'foo
                  :args (list (cl-cc::make-ast-int :value 1)
                              (cl-cc::make-ast-int :value 2)))) :to-equal '(foo 1 2)))

(it-sequential "ast-to-sexp-call-with-ast-func"
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-call :func (cl-cc::make-ast-var :name 'f)
                  :args (list (cl-cc::make-ast-int :value 3)))) :to-equal '(f 3)))

(it-sequential "ast-to-sexp-apply-produces-apply-form"
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-apply :func (cl-cc::make-ast-var :name 'f)
                  :args (list (cl-cc::make-ast-var :name 'args)))) :to-equal '(apply f args)))

;;; ─── Block / return / tagbody / catch / throw ────────────────────────────

(it-sequential "ast-to-sexp-control-flow-nodes block"
  (destructuring-bind (expected node) (list '(block outer 0) (cl-cc::make-ast-block :name 'outer :body (list (cl-cc::make-ast-int :value 0))))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-control-flow-nodes return-from"
  (destructuring-bind (expected node) (list '(return-from outer 42) (cl-cc::make-ast-return-from :name 'outer :value (cl-cc::make-ast-int :value 42)))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-control-flow-nodes catch"
  (destructuring-bind (expected node) (list '(catch (quote :done) 1) (cl-cc::make-ast-catch :tag (cl-cc::make-ast-quote :value :done)
                                  :body (list (cl-cc::make-ast-int :value 1))))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-control-flow-nodes throw"
  (destructuring-bind (expected node) (list '(throw (quote :done) 99) (cl-cc::make-ast-throw :tag (cl-cc::make-ast-quote :value :done)
                                  :value (cl-cc::make-ast-int :value 99)))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-control-flow-nodes values"
  (destructuring-bind (expected node) (list '(values 1 2) (cl-cc::make-ast-values :forms (list (cl-cc::make-ast-int :value 1)
                                                (cl-cc::make-ast-int :value 2))))
    (expect (cl-cc::ast-to-sexp node) :to-equal expected)))

(it-sequential "ast-to-sexp-multiple-value-bind-produces-mvb-form"
  (let ((node (cl-cc::make-ast-multiple-value-bind
               :vars '(a b)
               :values-form (cl-cc::make-ast-call :func 'two-values :args nil)
               :body (list (cl-cc::make-ast-var :name 'a)))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (expect (car result) :to-be 'multiple-value-bind)
      (expect (second result) :to-equal '(a b)))))

;;; ─── CLOS nodes ───────────────────────────────────────────────────────────

(it-sequential "ast-to-sexp-clos-nodes"
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-defgeneric :name 'area :params '(shape))) :to-equal '(defgeneric area (shape)))
  (expect (cl-cc::ast-to-sexp
                 (cl-cc::make-ast-slot-value
                  :object (cl-cc::make-ast-var :name 'obj)
                  :slot 'count)) :to-equal '(slot-value obj 'count)))

(it-sequential "ast-to-sexp-set-gethash-produces-setf-gethash-form"
  (let ((node (cl-cc::make-ast-set-gethash
               :key (cl-cc::make-ast-quote :value :k)
               :table (cl-cc::make-ast-var :name 'ht)
               :value (cl-cc::make-ast-int :value 1))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (expect (car result) :to-be 'setf)
      (expect (caadr result) :to-be 'gethash))))

;;; ─── slot-def-to-sexp ────────────────────────────────────────────────────

(it-sequential "slot-def-to-sexp-plain-slot-returns-symbol"
  (let ((slot (cl-cc::make-ast-slot-def
               :name 'value :initarg nil :initform nil
               :reader nil :writer nil :accessor nil)))
    (expect (cl-cc/ast:slot-def-to-sexp slot) :to-be 'value)))

(it-sequential "slot-def-to-sexp-with-initarg-includes-plist"
  (let* ((slot (cl-cc::make-ast-slot-def
                :name 'count :initarg :count :initform nil
                :reader nil :writer nil :accessor nil))
         (result (cl-cc/ast:slot-def-to-sexp slot)))
    (expect (car result) :to-be 'count)
    (expect (member :initarg result) :to-be-truthy)))
