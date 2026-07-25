;;;; tests/pbt/framework-dsl.lisp — PBT property macro and suite definition
;;;
;;; What survives of the home-grown property DSL. cl-weave's native
;;; IT-PROPERTY has replaced it everywhere except generators-typed-ast-utils.lisp,
;;; whose 9 properties still drive the type-expression, typed-AST and Mach-O
;;; generators in generators.lisp / generators-typed-ast.lisp /
;;; generators-macho.lisp. DEFPROPERTY is kept solely for those; do not add new
;;; uses — write CL-WEAVE:IT-PROPERTY instead.

(in-package :cl-cc/pbt)

;;; Property Definition Macro

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-generators (args)
    "Extract generator bindings from argument list.
     ARGS is either a flat list (var gen var gen ...) or a list of (var gen) pairs."
    (if (and (consp args) (consp (car args)))
        ;; Nested format: ((var gen) (var gen) ...)
        (loop for pair in args
              collect (if (and (consp pair) (= (length pair) 2))
                          pair
                          (list (car pair) (cadr pair))))
        ;; Flat format: (var gen var gen ...)
        (loop for (var gen) on args by #'cddr
              collect (list var gen))))

  (defun generate-binding-forms (gen-bindings)
    "Generate LET binding forms from generator bindings."
    (loop for (var gen) in gen-bindings
          collect (list var `(generate ,gen)))))

(defmacro defproperty (name args &body body)
  "Define a property-based test with automatic test generation.

   NAME is the test name (symbol).
   ARGS is a list of (variable generator) pairs.
   BODY is the property to test.

   Note: the generator expression is re-evaluated on every iteration, so a
   generator constructor that randomizes its own shape (as GEN-TYPE-EXPR does)
   varies per case here. CL-WEAVE:IT-PROPERTY builds its generator list once,
   which is why porting such a generator needs GEN-RECURSIVE rather than a
   mechanical substitution.

   Example:
     (defproperty addition-commutativity (a (gen-integer) b (gen-integer))
       (= (+ a b) (+ b a)))"
  (let ((gen-bindings (extract-generators args))
        (test-count-var (gensym "COUNT"))
        (iteration-var (gensym "I"))
        (failure-var (gensym "FAILURE"))
        (args-var (gensym "ARGS")))
    `(deftest ,name
       (let ((,test-count-var *test-count*)
             (,failure-var nil)
             (,args-var nil))
         (loop for ,iteration-var from 1 to ,test-count-var
               do (let* ,(generate-binding-forms gen-bindings)
                    (setf ,args-var (list ,@(mapcar #'car gen-bindings)))
                    (handler-case
                        (let ((result (progn ,@body)))
                          (unless result
                            (setf ,failure-var (list :failed ,iteration-var ,args-var))
                            (return)))
                      (error (e)
                        (setf ,failure-var (list :error ,iteration-var ,args-var e))
                        (return)))))
         (when ,failure-var
           (destructuring-bind (type iteration args &optional error) ,failure-var
              (case type
                (:failed
                 (%fail-test (format nil "Property ~S failed on iteration ~D with args ~S"
                                    ',name iteration args)))
                (:error
                 (%fail-test (format nil "Property ~S raised error ~A on iteration ~D with args ~S"
                                     ',name error iteration args))))))))))

;;; Test Suite Definition
;;;
;;; Lives here because MACRO-PBT-SUITE (macro-pbt-tests.lisp) declares this as
;;; its :parent and this file is loaded first.

(defsuite cl-cc-pbt-suite
  :description "Property-Based Testing suite for CL-CC"
  :parent cl-cc-integration-suite)
