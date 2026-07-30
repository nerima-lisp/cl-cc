;;;; packages/cps/tests/cps-coverage-matrix-tests.lisp
;;;; Table-driven coverage for every AST node type with a CPS transformer.

(in-package :cl-cc/test)



(defun %cps-coverage-int (&optional (value 1))
  "Build a minimal integer AST node for CPS coverage fixtures."
  (cl-cc/ast:make-ast-int :value value))

(defun %cps-transform-succeeds-p (node)
  "Return true when NODE can be transformed by cps-transform-ast*."
  (handler-case
      (progn
        (cl-cc/cps:cps-transform-ast* node)
        t)
    (error () nil)))

(it-sequential "cps-ast-coverage ast-int"
  (destructuring-bind (label node) (list "ast-int" (cl-cc/ast:make-ast-int :value 42))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-var"
  (destructuring-bind (label node) (list "ast-var" (cl-cc/ast:make-ast-var :name 'x))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-binop"
  (destructuring-bind (label node) (list "ast-binop" (cl-cc/ast:make-ast-binop
     :op '+
     :lhs (%cps-coverage-int 1)
     :rhs (%cps-coverage-int 2)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-if"
  (destructuring-bind (label node) (list "ast-if" (cl-cc/ast:make-ast-if
     :cond (%cps-coverage-int 1)
     :then (%cps-coverage-int 2)
     :else (%cps-coverage-int 3)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-progn"
  (destructuring-bind (label node) (list "ast-progn" (cl-cc/ast:make-ast-progn
     :forms (list (%cps-coverage-int 1)
                  (%cps-coverage-int 2))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-print"
  (destructuring-bind (label node) (list "ast-print" (cl-cc/ast:make-ast-print
     :expr (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-let"
  (destructuring-bind (label node) (list "ast-let" (cl-cc/ast:make-ast-let
     :bindings (list (cons 'x (%cps-coverage-int 1)))
     :body (list (cl-cc/ast:make-ast-var :name 'x))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-lambda"
  (destructuring-bind (label node) (list "ast-lambda" (cl-cc/ast:make-ast-lambda
     :params '(x)
     :body (list (cl-cc/ast:make-ast-var :name 'x))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-function"
  (destructuring-bind (label node) (list "ast-function" (cl-cc/ast:make-ast-function :name 'identity))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-block"
  (destructuring-bind (label node) (list "ast-block" (cl-cc/ast:make-ast-block
     :name 'done
     :body (list (%cps-coverage-int 1))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-return-from"
  (destructuring-bind (label node)
      (list "ast-return-from"
            (cl-cc/ast:make-ast-block
             :name 'done
             :body (list (cl-cc/ast:make-ast-return-from
                          :name 'done
                          :value (%cps-coverage-int 1)))))
    (declare (ignore label))
    (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-tagbody"
  (destructuring-bind (label node) (list "ast-tagbody" (cl-cc/ast:make-ast-tagbody
     :tags (list (cons 'start (list (%cps-coverage-int 1))))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-go"
  (destructuring-bind (label node)
      (list "ast-go"
            (cl-cc/ast:make-ast-tagbody
             :tags (list (cons (quote start)
                               (list (cl-cc/ast:make-ast-go
                                      :tag (quote start)))))))
    (declare (ignore label))
    (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-catch"
  (destructuring-bind (label node) (list "ast-catch" (cl-cc/ast:make-ast-catch
     :tag (cl-cc/ast:make-ast-quote :value 'tag)
     :body (list (%cps-coverage-int 1))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-throw"
  (destructuring-bind (label node) (list "ast-throw" (cl-cc/ast:make-ast-throw
     :tag (cl-cc/ast:make-ast-quote :value 'tag)
     :value (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-unwind-protect"
  (destructuring-bind (label node) (list "ast-unwind-protect" (cl-cc/ast:make-ast-unwind-protect
     :protected (%cps-coverage-int 1)
     :cleanup (list (%cps-coverage-int 0))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-flet"
  (destructuring-bind (label node) (list "ast-flet" (cl-cc/ast:make-ast-flet
     :bindings (list (list 'local-id '(x) (cl-cc/ast:make-ast-var :name 'x)))
     :body (list (%cps-coverage-int 1))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-labels"
  (destructuring-bind (label node) (list "ast-labels" (cl-cc/ast:make-ast-labels
     :bindings (list (list 'local-id '(x) (cl-cc/ast:make-ast-var :name 'x)))
     :body (list (%cps-coverage-int 1))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-setq"
  (destructuring-bind (label node) (list "ast-setq" (cl-cc/ast:make-ast-setq
     :var 'x
     :value (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-defvar"
  (destructuring-bind (label node) (list "ast-defvar" (cl-cc/ast:make-ast-defvar
     :name '*coverage-var*
     :kind 'defparameter
     :value (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-defun"
  (destructuring-bind (label node) (list "ast-defun" (cl-cc/ast:make-ast-defun
     :name 'coverage-function
     :params '(x)
     :body (list (cl-cc/ast:make-ast-var :name 'x))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-defmacro"
  (destructuring-bind (label node) (list "ast-defmacro" (cl-cc/ast:make-ast-defmacro
     :name 'coverage-macro
     :lambda-list '(x)
     :body '((list 'quote x))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-handler-case"
  (destructuring-bind (label node) (list "ast-handler-case" (cl-cc/ast:make-ast-handler-case
     :form (%cps-coverage-int 1)
     :clauses (list (list 'error 'e (%cps-coverage-int 0)))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-make-instance"
  (destructuring-bind (label node) (list "ast-make-instance" (cl-cc/ast:make-ast-make-instance
     :class (cl-cc/ast:make-ast-quote :value 'coverage-class)
     :initargs (list :x (%cps-coverage-int 1))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-slot-value"
  (destructuring-bind (label node) (list "ast-slot-value" (cl-cc/ast:make-ast-slot-value
     :object (cl-cc/ast:make-ast-var :name 'object)
     :slot 'x))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-set-slot-value"
  (destructuring-bind (label node) (list "ast-set-slot-value" (cl-cc/ast:make-ast-set-slot-value
     :object (cl-cc/ast:make-ast-var :name 'object)
     :slot 'x
     :value (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-defclass"
  (destructuring-bind (label node) (list "ast-defclass" (cl-cc/ast:make-ast-defclass
     :name 'coverage-class
     :superclasses nil
     :slots nil))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-defgeneric"
  (destructuring-bind (label node) (list "ast-defgeneric" (cl-cc/ast:make-ast-defgeneric
     :name 'coverage-generic
     :params '(object)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-defmethod"
  (destructuring-bind (label node) (list "ast-defmethod" (cl-cc/ast:make-ast-defmethod
     :name 'coverage-generic
     :params '(object)
     :specializers '(t)
     :body (list (cl-cc/ast:make-ast-var :name 'object))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-quote"
  (destructuring-bind (label node) (list "ast-quote" (cl-cc/ast:make-ast-quote :value '(a b c)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-the"
  (destructuring-bind (label node) (list "ast-the" (cl-cc/ast:make-ast-the
     :type 'integer
     :value (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-values"
  (destructuring-bind (label node) (list "ast-values" (cl-cc/ast:make-ast-values
     :forms (list (%cps-coverage-int 1)
                  (%cps-coverage-int 2))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-multiple-value-bind"
  (destructuring-bind (label node) (list "ast-multiple-value-bind" (cl-cc/ast:make-ast-multiple-value-bind
     :vars '(a b)
     :values-form (cl-cc/ast:make-ast-values
                   :forms (list (%cps-coverage-int 1)
                                (%cps-coverage-int 2)))
     :body (list (cl-cc/ast:make-ast-var :name 'a))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-multiple-value-prog1"
  (destructuring-bind (label node) (list "ast-multiple-value-prog1" (cl-cc/ast:make-ast-multiple-value-prog1
     :first (%cps-coverage-int 1)
     :forms (list (%cps-coverage-int 2))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-multiple-value-call"
  (destructuring-bind (label node) (list "ast-multiple-value-call" (cl-cc/ast:make-ast-multiple-value-call
     :func (cl-cc/ast:make-ast-function :name 'list)
     :args (list (%cps-coverage-int 1)
                 (%cps-coverage-int 2))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-apply"
  (destructuring-bind (label node) (list "ast-apply" (cl-cc/ast:make-ast-apply
     :func (cl-cc/ast:make-ast-function :name 'list)
     :args (list (%cps-coverage-int 1)
                 (cl-cc/ast:make-ast-quote :value '(2 3)))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-call"
  (destructuring-bind (label node) (list "ast-call" (cl-cc/ast:make-ast-call
     :func 'list
     :args (list (%cps-coverage-int 1)
                 (%cps-coverage-int 2))))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(it-sequential "cps-ast-coverage ast-set-gethash"
  (destructuring-bind (label node) (list "ast-set-gethash" (cl-cc/ast:make-ast-set-gethash
     :key (cl-cc/ast:make-ast-quote :value 'key)
     :table (cl-cc/ast:make-ast-var :name 'table)
     :value (%cps-coverage-int 1)))
    (declare (ignore label)) (expect (%cps-transform-succeeds-p node) :to-be-truthy)))

(defun %cps-if-result-and-output (condition)
  (let* ((node (make-ast-if
                :cond (make-ast-quote :value condition)
                :then (make-ast-print :expr (%cps-coverage-int 11))
                :else (make-ast-print :expr (%cps-coverage-int 22))))
         (function (eval (cl-cc/cps:cps-transform-ast* node)))
         (result nil)
         (output (with-output-to-string (*standard-output*)
                   (setf result (funcall function (function identity))))))
    (values result output)))

(it-sequential "fr-373-cps-if uses Common Lisp truthiness"
  (dolist (case (list (cons nil 22)
                      (cons 0 11)
                      (cons 0.0 11)
                      (cons (complex 0 0) 11)
                      (cons :non-nil 11)))
    (destructuring-bind (condition . expected) case
      (multiple-value-bind (result output)
          (%cps-if-result-and-output condition)
        (expect (= expected result) :to-be-truthy)
        (expect (string= (format nil "~%~S " expected) output)
                :to-be-truthy)))))
