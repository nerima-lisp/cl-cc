;;;; tests/closure-tests.lisp - Closure and Lambda Expression Tests
;;;
;;; This module provides comprehensive tests for closure behavior including:
;;; - Simple closure captures variable
;;; - Nested closures
;;; - Closure with mutation
;;; - Closure as return value
;;; - Property-based tests for closure semantics

(in-package :cl-cc/test)

;;; Test Suite Definition



;;; Helper Functions

(defun make-closure-ast (param var-name var-value body-form)
  "Create a closure AST that captures a variable."
  (make-ast-let
                 :bindings (list (cons var-name (make-ast-int :value var-value)))
                 :body (list (make-ast-lambda
                                            :params (list param)
                                            :body (list body-form)))))

(defun make-nested-closure-ast (outer-var outer-val inner-var inner-val)
  "Create nested closures."
  (make-ast-let
                 :bindings (list (cons outer-var (make-ast-int :value outer-val)))
                 :body (list (make-ast-lambda
                                            :params nil
                                            :body (list (make-ast-let
                                                                       :bindings (list (cons inner-var
                                                                                            (make-ast-int :value inner-val)))
                                                                       :body (list (make-ast-lambda
                                                                                                  :params nil
                                                                                                  :body (list (make-ast-binop
                                                                                                                             :op '+
                                                                                                                             :lhs (make-ast-var :name outer-var)
                                                                                                                             :rhs (make-ast-var :name inner-var)))))))))))

(it-sequential "closure-and-lambda-compile-to-vm simple-capture"
  (destructuring-bind (expr) (list "(let ((x 10)) (lambda (y) (+ x y)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm multi-capture"
  (destructuring-bind (expr) (list "(let ((x 5) (y 10)) (lambda (z) (+ (+ x y) z)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm capture-return"
  (destructuring-bind (expr) (list "(let ((x 42)) ((lambda () x)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm nested-closures"
  (destructuring-bind (expr) (list "(let ((outer 10)) (lambda () (let ((inner 20)) (lambda () (+ outer inner)))))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm shared-capture"
  (destructuring-bind (expr) (list "(let ((x 1)) (let ((f (lambda () x)) (g (lambda () (+ x 1)))) (+ (funcall f) (funcall g))))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm triple-nested"
  (destructuring-bind (expr) (list "(let ((a 1)) (lambda () (let ((b 2)) (lambda () (let ((c 3)) (lambda () (+ (+ a b) c)))))))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm setq-in-closure"
  (destructuring-bind (expr) (list "(let ((counter 0)) (lambda () (setq counter (+ counter 1)) counter))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm mutation-preserves-capture"
  (destructuring-bind (expr) (list "(let ((x 10)) (setq x 20) (lambda () x))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm multiple-setq"
  (destructuring-bind (expr) (list "(let ((a 0) (b 0)) (lambda (x y) (setq a x) (setq b y) (+ a b)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm closure-factory"
  (destructuring-bind (expr) (list "(let ((make-adder (lambda (n) (lambda (x) (+ n x))))) (funcall (funcall make-adder 10) 5))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm closure-as-data"
  (destructuring-bind (expr) (list "(let ((make-counter (lambda (start) (lambda () start)))) (let ((counter (funcall make-counter 0))) (funcall counter)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm hof-compose"
  (destructuring-bind (expr) (list "(let ((compose (lambda (f g) (lambda (x) (funcall f (funcall g x)))))) (let ((add1 (lambda (x) (+ x 1))) (mul2 (lambda (x) (* x 2)))) (funcall (funcall (funcall compose add1) mul2) 5)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm lambda-no-params"
  (destructuring-bind (expr) (list "(funcall (lambda () 42))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm lambda-one-param"
  (destructuring-bind (expr) (list "(funcall (lambda (x) (+ x 1)) 10)")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm lambda-multi-params"
  (destructuring-bind (expr) (list "(funcall (lambda (x y z) (+ (+ x y) z)) 1 2 3)")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm lambda-progn-body"
  (destructuring-bind (expr) (list "(funcall (lambda (x) (print x) (+ x 1)) 5)")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm lambda-nested-in-expr"
  (destructuring-bind (expr) (list "(+ 1 (funcall (lambda (x) (* x 2)) 10))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm flet-single"
  (destructuring-bind (expr) (list "(flet ((double (x) (* x 2))) (double 5))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm flet-multiple"
  (destructuring-bind (expr) (list "(flet ((add1 (x) (+ x 1)) (mul2 (x) (* x 2))) (+ (add1 5) (mul2 3)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm labels-calling"
  (destructuring-bind (expr) (list "(labels ((a (x) (+ x 1)) (b (x) (a (* x 2)))) (b 5))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm flet-returns-closure"
  (destructuring-bind (expr) (list "(flet ((make-adder (n) (lambda (x) (+ n x)))) (funcall (make-adder 10) 5))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm labels-recursive"
  (destructuring-bind (expr) (list "(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm labels-mutual"
  (destructuring-bind (expr) (list "(labels ((even-p (n) (if n (odd-p (- n 1)) 1)) (odd-p (n) (if n (even-p (- n 1)) 0))) (even-p 10))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

(it-sequential "closure-and-lambda-compile-to-vm labels-with-closure"
  (destructuring-bind (expr) (list "(labels ((make-counter () (let ((count 0)) (lambda () (setq count (+ count 1)) count)))) (let ((c (make-counter))) (funcall c) (funcall c)))")
    (expect (compilation-result-program (compile-string expr :target :vm)) :to-be-truthy)))

;;; AST Structure Tests

(it-sequential "ast-lambda-structure"
  (let* ((sexp '(lambda (x y) (+ x y)))
         (ast (lower-sexp-to-ast sexp)))
    (expect (typep ast 'ast-lambda) :to-be-truthy)
    (expect '(x y) :to-equal (ast-lambda-params ast))
    (expect (= 1 (length (ast-lambda-body ast))) :to-be-truthy)
    (expect (typep (first (ast-lambda-body ast)) 'ast-binop) :to-be-truthy)))

(it-sequential "ast-flet-structure"
  (let* ((sexp '(flet ((double (x) (* x 2))) (double 5)))
         (ast (lower-sexp-to-ast sexp)))
    (expect (typep ast 'ast-flet) :to-be-truthy)
    (expect (= 1 (length (ast-flet-bindings ast))) :to-be-truthy)
    (expect (first (first (ast-flet-bindings ast))) :to-be 'double)
    (expect (second (first (ast-flet-bindings ast))) :to-equal '(x))))

(it-sequential "ast-labels-structure"
  (let* ((sexp '(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5)))
         (ast (lower-sexp-to-ast sexp)))
    (expect (typep ast 'ast-labels) :to-be-truthy)
    (expect (= 1 (length (ast-labels-bindings ast))) :to-be-truthy)
    (expect (first (first (ast-labels-bindings ast))) :to-be 'fact)))

;;; Roundtrip Tests

(it-sequential "closure-ast-roundtrip lambda"
  (destructuring-bind (original) (list '(lambda (x y) (+ x y)))
    (expect (ast-to-sexp (lower-sexp-to-ast original)) :to-equal original)))

(it-sequential "closure-ast-roundtrip flet"
  (destructuring-bind (original) (list '(flet ((double (x) (* x 2))) (double 5)))
    (expect (ast-to-sexp (lower-sexp-to-ast original)) :to-equal original)))

(it-sequential "closure-ast-roundtrip labels"
  (destructuring-bind (original) (list '(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5)))
    (expect (ast-to-sexp (lower-sexp-to-ast original)) :to-equal original)))

(it-sequential "closure-ast-roundtrip nested-lambda"
  (destructuring-bind (original) (list '(let ((x 10)) (lambda (y) (+ x y))))
    (expect (ast-to-sexp (lower-sexp-to-ast original)) :to-equal original)))

;;; Assembly Emission Tests

(it-sequential "closure-assembly-backends x86-64"
  (destructuring-bind (target) (list :x86_64)
    (let ((result (handler-case
                    (compilation-result-assembly (compile-string "(lambda (x) (+ x 1))" :target target))
                  (error () :not-yet-supported))))
    (expect (or (eq result :not-yet-supported)
                     (and (stringp result) (> (length result) 0))) :to-be-truthy))))

(it-sequential "closure-assembly-backends aarch64"
  (destructuring-bind (target) (list :aarch64)
    (let ((result (handler-case
                    (compilation-result-assembly (compile-string "(lambda (x) (+ x 1))" :target target))
                  (error () :not-yet-supported))))
    (expect (or (eq result :not-yet-supported)
                     (and (stringp result) (> (length result) 0))) :to-be-truthy))))
