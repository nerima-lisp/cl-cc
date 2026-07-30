;;;; tests/unit/compile/cps-ast-tests.lisp — CPS AST transformer tests

(in-package :cl-cc/test)



(it-sequential "cps-ast-int-expands-to-funcall"
  (let ((result (cl-cc/cps::cps-transform-ast (cl-cc:make-ast-int :value 42) 'k)))
    (expect (car result) :to-be 'funcall)
    (expect (second result) :to-be 'k)
    (expect (third result) :to-equal 42)))

(it-sequential "cps-ast-binop-uses-two-continuations"
  (let ((result (format nil "~S"
                        (cl-cc/cps::cps-transform-ast
                         (cl-cc:make-ast-binop
                          :op '+
                          :lhs (cl-cc:make-ast-int :value 1)
                          :rhs (cl-cc:make-ast-int :value 2))
                         'k))))
    (expect (search "LAMBDA" result) :to-be-truthy)
    (expect (search "FUNCALL" result) :to-be-truthy)))

(it-sequential "cps-ast-sequence-and-if-are-recursive"
  (let ((seq (cl-cc/cps::cps-transform-sequence (list (cl-cc:make-ast-int :value 1)
                                                  (cl-cc:make-ast-int :value 2)) 'k))
        (iff (cl-cc/cps::cps-transform-ast
              (cl-cc:make-ast-if
               :cond (cl-cc:make-ast-int :value 1)
               :then (cl-cc:make-ast-int :value 10)
               :else (cl-cc:make-ast-int :value 20))
              'k)))
    (expect (car seq) :to-be 'funcall)
    (expect (car iff) :to-be 'let)
    (expect (search "IF" (format nil "~S" iff)) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage values"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-values
             :forms (list (cl-cc:make-ast-int :value 1) (cl-cc:make-ast-int :value 2))) "MULTIPLE-VALUE-CALL")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage mvb"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-multiple-value-bind
             :vars '(a b)
             :values-form (cl-cc/ast:make-ast-values
                            :forms (list (cl-cc:make-ast-int :value 1)
                                         (cl-cc:make-ast-int :value 2)))
             :body (list (cl-cc:make-ast-var :name 'a))) "MULTIPLE-VALUE-BIND")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage apply"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-apply
             :func (cl-cc:make-ast-function :name 'list)
             :args (list (cl-cc:make-ast-quote :value '(1 2)))) "APPLY")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage defvar"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-defvar :name '*x :kind 'defparameter
                                   :value (cl-cc:make-ast-int :value 1)) "DEFPARAMETER")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage handler-case"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-handler-case
             :form (cl-cc:make-ast-int :value 1)
             :clauses (list (list 'error 'e (cl-cc:make-ast-int :value 0)))) "HANDLER-CASE")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage make-instance"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-make-instance
             :class (cl-cc:make-ast-quote :value 'foo)
             :initargs (list :x (cl-cc:make-ast-int :value 1))) "MAKE-INSTANCE")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage slot-value"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-slot-value :object (cl-cc:make-ast-var :name 'obj) :slot 'bar) "SLOT-VALUE")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage set-slot-value"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-set-slot-value
             :object (cl-cc:make-ast-var :name 'obj) :slot 'bar
             :value (cl-cc:make-ast-int :value 2)) "SETF")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage defclass"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-defclass :name 'foo :superclasses nil :slots nil) "DEFCLASS")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage defgeneric"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-defgeneric :name 'gf :params '(x) :combination nil) "DEFGENERIC")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage defmethod"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-defmethod :name 'gf :qualifier nil :specializers '(t)
                                      :params '(x) :body (list (cl-cc:make-ast-var :name 'x))) "DEFMETHOD")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

(it-sequential "cps-ast-conservative-coverage set-gethash"
  (destructuring-bind (ast expected-keyword) (list (cl-cc/ast:make-ast-set-gethash
             :key (cl-cc:make-ast-quote :value 'a)
             :table (cl-cc:make-ast-var :name 'tbl)
             :value (cl-cc:make-ast-int :value 1)) "GETHASH")
    (expect (search expected-keyword (format nil "~S" (cl-cc/cps::cps-transform-ast ast 'k))) :to-be-truthy)))

;;; ─── %cps-expand-let-bindings (extracted recursive helper) ──────────────

(it-sequential "cps-expand-let-bindings-empty-bindings"
  (let* ((body (list (cl-cc:make-ast-int :value 99)))
         (result (cl-cc/cps::%cps-expand-let-bindings nil body 'k)))
    (expect (consp result) :to-be-truthy)
    (expect (car result) :to-be 'funcall)
    (expect (second result) :to-be 'k)
    (expect (= 99 (third result)) :to-be-truthy)))

(it-sequential "cps-expand-let-bindings-single-binding"
  (let* ((binding (cons 'x (cl-cc:make-ast-int :value 1)))
         (body    (list (cl-cc:make-ast-var :name 'x)))
         (result  (format nil "~S"
                          (cl-cc/cps::%cps-expand-let-bindings
                           (list binding) body 'k))))
    (expect (search "LAMBDA" result) :to-be-truthy)
    (expect (search "LET"    result) :to-be-truthy)))

(it-sequential "cps-expand-let-bindings-two-bindings-nest"
  (let* ((b1     (cons 'x (cl-cc:make-ast-int :value 1)))
         (b2     (cons 'y (cl-cc:make-ast-int :value 2)))
         (body   (list (cl-cc:make-ast-var :name 'x)))
         (result (format nil "~S"
                         (cl-cc/cps::%cps-expand-let-bindings
                          (list b1 b2) body 'k)))
         (lambda-count (let ((count 0) (pos 0))
                         (loop
                           (let ((found (search "LAMBDA" result :start2 pos)))
                             (if found
                                 (progn (incf count) (setf pos (1+ found)))
                                 (return count)))))))
    (expect (>= lambda-count 2) :to-be-truthy)))

(it-sequential "cps-ast-unsupported-node-signals-dedicated-condition"
  (let ((node (cl-cc/ast::make-ast-hole)))
    (handler-case
        (progn
          (cl-cc/cps:cps-transform-ast node (quote k))
          (expect nil :to-be-truthy))
      (cl-cc/cps:unsupported-cps-ast (condition)
        (expect (cl-cc/cps:unsupported-cps-ast-node condition) :to-be node)
        (expect (cl-cc/cps:unsupported-cps-ast-node-type condition)
                :to-be (type-of node))))))
