;;;; tests/unit/compile/codegen-core-let-tests.lisp
;;;; Unit tests for codegen-core-let.lisp — let-binding analysis helpers.
;;;;
;;;; Covers:
;;;;   %ast-let-binding-ignored-p, %ast-cons-call-p, %ast-make-array-call-p,
;;;;   %ast-make-array-int-call-p, %binding-mentioned-in-body-p,
;;;;   %ast-lambda-bound-names, %ast-as-body-forms, %ast-wrap-bindings,
;;;;   %ast-let-sink-if-candidate.

(in-package :cl-cc/test)

;;; ─── %ast-let-binding-ignored-p ───────────────────────────────────────────

(it-sequential "ast-let-binding-ignored-p ignore"
  (destructuring-bind (expected name decls) (list t 'x '((ignore x)))
    (assert-bool expected (cl-cc/compile::%ast-let-binding-ignored-p name decls))))

(it-sequential "ast-let-binding-ignored-p ignorable"
  (destructuring-bind (expected name decls) (list nil 'x '((ignorable x)))
    (assert-bool expected (cl-cc/compile::%ast-let-binding-ignored-p name decls))))

(it-sequential "ast-let-binding-ignored-p wrong-name"
  (destructuring-bind (expected name decls) (list nil 'x '((ignore y)))
    (assert-bool expected (cl-cc/compile::%ast-let-binding-ignored-p name decls))))

(it-sequential "ast-let-binding-ignored-p empty"
  (destructuring-bind (expected name decls) (list nil 'x nil)
    (assert-bool expected (cl-cc/compile::%ast-let-binding-ignored-p name decls))))

;;; ─── %ast-cons-call-p ─────────────────────────────────────────────────────

(it-sequential "ast-cons-call-p symbol-func"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'cons
                            :args (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2))) t)
    (assert-bool expected (cl-cc/compile::%ast-cons-call-p node))))

(it-sequential "ast-cons-call-p var-func"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func (cl-cc/ast:make-ast-var :name 'cons)
                            :args (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2))) t)
    (assert-bool expected (cl-cc/compile::%ast-cons-call-p node))))

(it-sequential "ast-cons-call-p wrong-arity"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'cons
                            :args (list (cl-cc/ast:make-ast-int :value 1))) nil)
    (assert-bool expected (cl-cc/compile::%ast-cons-call-p node))))

(it-sequential "ast-cons-call-p non-cons"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'list
                            :args (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2))) nil)
    (assert-bool expected (cl-cc/compile::%ast-cons-call-p node))))

(it-sequential "ast-cons-call-p non-call"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-int :value 5) nil)
    (assert-bool expected (cl-cc/compile::%ast-cons-call-p node))))

;;; ─── %ast-make-array-call-p / %ast-make-array-int-call-p ─────────────────

(it-sequential "ast-make-array-call-p symbol-func"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'make-array
                            :args (list (cl-cc/ast:make-ast-int :value 10))) t)
    (assert-bool expected (cl-cc/compile::%ast-make-array-call-p node))))

(it-sequential "ast-make-array-call-p wrong-arity"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'make-array
                            :args (list (cl-cc/ast:make-ast-int :value 10)
                                        (cl-cc/ast:make-ast-int :value 5))) nil)
    (assert-bool expected (cl-cc/compile::%ast-make-array-call-p node))))

(it-sequential "ast-make-array-int-call-p literal-size"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'make-array
                             :args (list (cl-cc/ast:make-ast-int :value 5))) t)
    (assert-bool expected (cl-cc/compile::%ast-make-array-int-call-p node))))

(it-sequential "ast-make-array-int-call-p var-size"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-call :func 'make-array
                             :args (list (cl-cc/ast:make-ast-var :name 'n))) nil)
    (assert-bool expected (cl-cc/compile::%ast-make-array-int-call-p node))))

;;; ─── %binding-mentioned-in-body-p ─────────────────────────────────────────

(it-sequential "binding-mentioned-in-body-p var-in-body"
  (destructuring-bind (body name expected) (list (list (cl-cc/ast:make-ast-var :name 'x)) 'x t)
    (assert-bool expected (cl-cc/compile::%binding-mentioned-in-body-p body name))))

(it-sequential "binding-mentioned-in-body-p empty-body"
  (destructuring-bind (body name expected) (list nil 'x nil)
    (assert-bool expected (cl-cc/compile::%binding-mentioned-in-body-p body name))))

(it-sequential "binding-mentioned-in-body-p different-var"
  (destructuring-bind (body name expected) (list (list (cl-cc/ast:make-ast-var :name 'y)) 'x nil)
    (assert-bool expected (cl-cc/compile::%binding-mentioned-in-body-p body name))))

;;; ─── %ast-lambda-bound-names ──────────────────────────────────────────────

(it-sequential "ast-lambda-bound-names-required-params"
  (let ((node (cl-cc/ast:make-ast-lambda
               :params '(a b c)
               :body nil)))
    (expect (cl-cc/compile::%ast-lambda-bound-names node) :to-equal '(a b c))))

(it-sequential "ast-lambda-bound-names-includes-rest-param"
  (let ((node (cl-cc/ast:make-ast-lambda
               :params '(x)
               :rest-param 'rest
               :body nil)))
    (let ((names (cl-cc/compile::%ast-lambda-bound-names node)))
      (expect (member 'x names) :to-be-truthy)
      (expect (member 'rest names) :to-be-truthy))))

(it-sequential "ast-lambda-bound-names-includes-optional-params"
  (let ((node (cl-cc/ast:make-ast-lambda
               :params nil
               :optional-params (list (list 'a (cl-cc/ast:make-ast-int :value 0)))
               :body nil)))
    (expect (member 'a (cl-cc/compile::%ast-lambda-bound-names node)) :to-be-truthy)))

;;; ─── %ast-as-body-forms ───────────────────────────────────────────────────

(it-sequential "ast-as-body-forms-unwraps-progn"
  (let* ((f1 (cl-cc/ast:make-ast-int :value 1))
         (f2 (cl-cc/ast:make-ast-int :value 2))
         (node (cl-cc/ast:make-ast-progn :forms (list f1 f2))))
    (expect (cl-cc/compile::%ast-as-body-forms node) :to-equal (list f1 f2))))

(it-sequential "ast-as-body-forms-wraps-non-progn-in-list"
  (let ((node (cl-cc/ast:make-ast-int :value 42)))
    (expect (cl-cc/compile::%ast-as-body-forms node) :to-equal (list node))))

;;; ─── %ast-wrap-bindings ───────────────────────────────────────────────────

(it-sequential "ast-wrap-bindings-no-bindings-single-body-returns-form"
  (let ((f (cl-cc/ast:make-ast-int :value 7)))
    (expect (cl-cc/compile::%ast-wrap-bindings nil (list f)) :to-be f)))

(it-sequential "ast-wrap-bindings-no-bindings-multi-body-returns-progn"
  (let ((result (cl-cc/compile::%ast-wrap-bindings
                 nil
                 (list (cl-cc/ast:make-ast-int :value 1)
                       (cl-cc/ast:make-ast-int :value 2)))))
    (expect (typep result 'cl-cc::ast-progn) :to-be-truthy)))

(it-sequential "ast-wrap-bindings-with-bindings-returns-ast-let"
  (let ((result (cl-cc/compile::%ast-wrap-bindings
                 (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
                 (list (cl-cc/ast:make-ast-var :name 'x)))))
    (expect (typep result 'cl-cc::ast-let) :to-be-truthy)))

;;; ─── %ast-let-sink-if-candidate ───────────────────────────────────────────

(it-sequential "sink-if-candidate-detected-for-single-binding-single-if-body"
  (let* ((node (cl-cc/ast:make-ast-let
                :bindings (list (cons 'arr (cl-cc/ast:make-ast-call
                                            :func 'make-array
                                            :args (list (cl-cc/ast:make-ast-int :value 3)))))
                :body (list (cl-cc/ast:make-ast-if
                             :cond (cl-cc/ast:make-ast-int :value 1)
                             :then (cl-cc/ast:make-ast-call
                                    :func 'aref
                                    :args (list (cl-cc/ast:make-ast-var :name 'arr)
                                                (cl-cc/ast:make-ast-int :value 0)))
                             :else (cl-cc/ast:make-ast-int :value 0)))))
         (result (cl-cc/compile::%ast-let-sink-if-candidate node)))
    (expect result :to-be-truthy)))

(it-sequential "sink-if-candidate-nil-for-non-if-body"
  (let ((node (cl-cc/ast:make-ast-let
               :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
               :body (list (cl-cc/ast:make-ast-var :name 'x)))))
    (expect (cl-cc/compile::%ast-let-sink-if-candidate node) :to-be-null)))

;;; ─── %let-binding-special-p (from codegen-core-let-emit.lisp) ───────────────

(it-sequential "let-binding-special-p earmuff-global"
  (destructuring-bind (sym register-sym expected) (list '*x* '*x* t)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (when register-sym
      (setf (gethash register-sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (assert-bool expected (cl-cc/compile::%let-binding-special-p sym ctx)))))

(it-sequential "let-binding-special-p plain-symbol"
  (destructuring-bind (sym register-sym expected) (list 'x 'x nil)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (when register-sym
      (setf (gethash register-sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (assert-bool expected (cl-cc/compile::%let-binding-special-p sym ctx)))))

(it-sequential "let-binding-special-p earmuff-unregistered"
  (destructuring-bind (sym register-sym expected) (list '*x* nil nil)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (when register-sym
      (setf (gethash register-sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (assert-bool expected (cl-cc/compile::%let-binding-special-p sym ctx)))))

(it-sequential "let-binding-special-p single-star"
  (destructuring-bind (sym register-sym expected) (list '* '* nil)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (when register-sym
      (setf (gethash register-sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (assert-bool expected (cl-cc/compile::%let-binding-special-p sym ctx)))))
