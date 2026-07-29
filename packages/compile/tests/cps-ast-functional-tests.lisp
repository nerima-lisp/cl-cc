;;;; tests/unit/compile/cps-ast-functional-tests.lisp
;;;; Unit tests for src/compile/cps-ast-functional.lisp
;;;;
;;;; Covers: cps-transform-ast for functional/multi-value forms:
;;;;   ast-multiple-value-bind, ast-multiple-value-call,
;;;;   ast-multiple-value-prog1, ast-defvar, ast-handler-case,
;;;;   ast-make-instance, ast-slot-value, ast-set-slot-value,
;;;;   ast-defclass, ast-set-gethash.
;;;;
;;;; Structural inspection — tests verify the shape of produced S-expressions
;;;; rather than evaluating them (which would require a full runtime).

(in-package :cl-cc/test)

(defun %cps-k (node)
  "CPS-transform NODE with a fixed continuation symbol K."
  (cl-cc/cps::cps-transform-ast node 'k))

(defun %form-contains-p (form sym)
  "Return T if SYM appears anywhere in the s-expression FORM."
  (cond ((eq form sym) t)
        ((consp form) (or (%form-contains-p (car form) sym)
                          (%form-contains-p (cdr form) sym)))
        (t nil)))

(defun %form-contains-equal-p (form target)
  "Return T if TARGET appears anywhere in FORM using EQUAL comparison."
  (cond ((equal form target) t)
        ((consp form) (or (%form-contains-equal-p (car form) target)
                          (%form-contains-equal-p (cdr form) target)))
        (t nil)))

;;; ─── ast-defvar ──────────────────────────────────────────────────────────────

(it-sequential "cps-defvar-emits-defvar-and-funcall with-value"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-defvar :name '*x* :kind 'defvar
                             :value (cl-cc/ast:make-ast-int :value 0)))
    (let ((result (%cps-k node)))
    (expect (%form-contains-p result 'defvar) :to-be-truthy)
    (expect (%form-contains-p result 'funcall) :to-be-truthy))))

(it-sequential "cps-defvar-emits-defvar-and-funcall without-value"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-defvar :name '*y* :kind 'defvar :value nil))
    (let ((result (%cps-k node)))
    (expect (%form-contains-p result 'defvar) :to-be-truthy)
    (expect (%form-contains-p result 'funcall) :to-be-truthy))))

(it-sequential "cps-defvar-returns-quoted-name with-value"
  (destructuring-bind (name node) (list '*x* (cl-cc/ast:make-ast-defvar :name '*x* :kind 'defvar
                                :value (cl-cc/ast:make-ast-int :value 0)))
    (expect (%form-contains-equal-p (%cps-k node) (list 'quote name)) :to-be-truthy)))

(it-sequential "cps-defvar-returns-quoted-name without-value"
  (destructuring-bind (name node) (list '*y* (cl-cc/ast:make-ast-defvar :name '*y* :kind 'defvar :value nil))
    (expect (%form-contains-equal-p (%cps-k node) (list 'quote name)) :to-be-truthy)))

;;; ─── ast-handler-case ────────────────────────────────────────────────────────

(it-sequential "cps-handler-case-produces-host-handler-case"
  (let* ((body-node (cl-cc/ast:make-ast-int :value 0))
         (node (cl-cc/ast:make-ast-handler-case
                :form (cl-cc/ast:make-ast-int :value 1)
                :clauses (list (list 'error 'e body-node))))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'handler-case) :to-be-truthy)))

;;; ─── ast-make-instance ───────────────────────────────────────────────────────

(it-sequential "cps-make-instance-no-initargs-contains-make-instance"
  (let* ((node (cl-cc/ast:make-ast-make-instance
                :class (cl-cc/ast:make-ast-quote :value 'dog)
                :initargs nil))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'make-instance) :to-be-truthy)))

(it-sequential "cps-make-instance-with-initargs-contains-lambda"
  (let* ((node (cl-cc/ast:make-ast-make-instance
                :class (cl-cc/ast:make-ast-quote :value 'point)
                :initargs (list :x (cl-cc/ast:make-ast-int :value 1)
                                :y (cl-cc/ast:make-ast-int :value 2))))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'make-instance) :to-be-truthy)
    (expect (%form-contains-p result 'lambda) :to-be-truthy)))

;;; ─── ast-slot-value ──────────────────────────────────────────────────────────

(it-sequential "cps-slot-value-contains-slot-value"
  (let* ((node (cl-cc/ast:make-ast-slot-value
                :object (cl-cc/ast:make-ast-var :name 'obj)
                :slot 'x))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'slot-value) :to-be-truthy)))

(it-sequential "cps-set-slot-value-contains-setf-and-slot-value"
  (let* ((node (cl-cc/ast:make-ast-set-slot-value
                :object (cl-cc/ast:make-ast-var :name 'obj)
                :slot 'x
                :value (cl-cc/ast:make-ast-int :value 42)))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'slot-value) :to-be-truthy)
    (expect (%form-contains-p result 'setf) :to-be-truthy)))

;;; ─── ast-defclass ────────────────────────────────────────────────────────────

(it-sequential "cps-defclass-emits-progn-defclass-funcall-k"
  (let* ((node (cl-cc/ast:make-ast-defclass
                :name 'animal :superclasses nil :slots nil))
         (result (%cps-k node)))
    (expect (car result) :to-be 'progn)
    (expect (caadr result) :to-be 'defclass)
    (let ((last-form (car (last result))))
      (expect (car last-form) :to-be 'funcall)
      (expect (second last-form) :to-be 'k))))

(it-sequential "cps-defclass-superclasses-appear-in-defclass-form"
  (let* ((node (cl-cc/ast:make-ast-defclass
                :name 'dog :superclasses '(animal) :slots nil))
         (result (%cps-k node)))
    (let ((defclass-form (cadr result)))
      (expect (third defclass-form) :to-equal '(animal)))))

;;; ─── ast-defgeneric ──────────────────────────────────────────────────────────

(it-sequential "cps-defgeneric-with-multiple-params"
  (let* ((node (cl-cc/ast:make-ast-defgeneric :name 'draw :params '(shape context)))
         (result (%cps-k node)))
    (let ((defgeneric-form (cadr result)))
      (expect (car defgeneric-form) :to-be 'defgeneric)
      (expect (third defgeneric-form) :to-equal '(shape context)))))

;;; ─── ast-defmethod ───────────────────────────────────────────────────────────

(it-sequential "cps-defmethod-produces-progn-defmethod-funcall-k"
  (let* ((node (cl-cc/ast:make-ast-defmethod
                :name 'area
                :params '(shape)
                :specializers '(circle)
                :body (list (cl-cc/ast:make-ast-int :value 0))))
         (result (%cps-k node)))
    (expect (car result) :to-be 'progn)
    (expect (caadr result) :to-be 'defmethod)
    (expect (%form-contains-p result 'funcall) :to-be-truthy)))

;;; ─── ast-set-gethash ─────────────────────────────────────────────────────────

(it-sequential "cps-set-gethash-contains-setf-gethash"
  (let* ((node (cl-cc/ast:make-ast-set-gethash
                :key (cl-cc/ast:make-ast-quote :value :x)
                :table (cl-cc/ast:make-ast-var :name 'ht)
                :value (cl-cc/ast:make-ast-int :value 99)))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'gethash) :to-be-truthy)
    (expect (%form-contains-p result 'setf) :to-be-truthy)))

;;; ─── ast-multiple-value-bind ─────────────────────────────────────────────────

(it-sequential "cps-mvb-ast-values-producer-emits-multiple-value-bind"
  (let* ((values-node (cl-cc/ast:make-ast-values
                        :forms (list (cl-cc/ast:make-ast-int :value 1)
                                     (cl-cc/ast:make-ast-int :value 2))))
         (node (cl-cc/ast:make-ast-multiple-value-bind
                :vars '(a b)
                :values-form values-node
                :body (list (cl-cc/ast:make-ast-var :name 'a))))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'multiple-value-bind) :to-be-truthy)))

(it-sequential "cps-mvb-non-ast-values-emits-let-with-nil-bindings"
  (let* ((call-node (cl-cc/ast:make-ast-call :func 'floor :args (list (cl-cc/ast:make-ast-int :value 7))))
         (node (cl-cc/ast:make-ast-multiple-value-bind
                :vars '(q r)
                :values-form call-node
                :body (list (cl-cc/ast:make-ast-var :name 'q))))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'let) :to-be-truthy)
    (expect (%form-contains-p result 'nil) :to-be-truthy)))

;;; ─── ast-multiple-value-call ─────────────────────────────────────────────────

(it-sequential "cps-mvc-single-arg-spreads-via-apply"
  (let* ((node (cl-cc/ast:make-ast-multiple-value-call
                :func (cl-cc/ast:make-ast-var :name 'f)
                :args (list (cl-cc/ast:make-ast-int :value 1))))
         (result (%cps-k node)))
    (expect (%form-contains-p result 'apply) :to-be-truthy)))

(it-sequential "cps-mvc-multi-arg-uses-collect-pattern"
  (let* ((node (cl-cc/ast:make-ast-multiple-value-call
                :func (cl-cc/ast:make-ast-var :name 'list)
                :args (list (cl-cc/ast:make-ast-int :value 1)
                            (cl-cc/ast:make-ast-int :value 2))))
         (result (%cps-k node)))
    (expect (or (%form-contains-p result 'push)
                     (%form-contains-p result 'nreverse)) :to-be-truthy)))

;;; ─── ast-multiple-value-prog1 ────────────────────────────────────────────────

(it-sequential "cps-mvprog1-delivers-first-form-result"
  (let* ((node (cl-cc::make-ast-multiple-value-prog1
                :first (cl-cc/ast:make-ast-int :value 42)
                :forms (list (cl-cc/ast:make-ast-call :func 'print
                                                    :args (list (cl-cc/ast:make-ast-int :value 0))))))
         (result (%cps-k node)))
    ;; The result register is bound first, side effects follow, then funcall k result
    (expect (%form-contains-p result 'funcall) :to-be-truthy)
    (expect (%form-contains-p result 'lambda) :to-be-truthy)))

;;; ─── %cps-thread-values-forms (extracted helper) ────────────────────────────

(it-sequential "cps-thread-values-forms-empty"
  (let ((result (cl-cc/cps::%cps-thread-values-forms nil nil 'k nil)))
    (expect (car result) :to-be 'multiple-value-call)
    (expect (second result) :to-be 'k)))

(it-sequential "cps-thread-values-forms-single"
  (let* ((form (cl-cc/ast:make-ast-int :value 7))
         (temps '(t0))
         (result (format nil "~S"
                         (cl-cc/cps::%cps-thread-values-forms (list form) temps 'k temps))))
    (expect (search "LAMBDA" result) :to-be-truthy)
    (expect (search "MULTIPLE-VALUE-CALL" result) :to-be-truthy)))

;;; ─── %cps-thread-mvb-forms (extracted helper) ───────────────────────────────

(it-sequential "cps-thread-mvb-forms-empty"
  (let* ((body  (list (cl-cc/ast:make-ast-int :value 1)))
         (result (cl-cc/cps::%cps-thread-mvb-forms nil nil '(a b) body 'k '(t0 t1))))
    (expect (car result) :to-be 'multiple-value-bind)
    (expect (second result) :to-equal '(a b))))

;;; ─── %cps-collect-mv-call-args (extracted helper) ───────────────────────────

(it-sequential "cps-collect-mv-call-args-empty"
  (let ((result (cl-cc/cps::%cps-collect-mv-call-args nil 'acc 'f 'k)))
    (expect (%form-contains-p result 'nreverse) :to-be-truthy)
    (expect (%form-contains-p result 'apply) :to-be-truthy)))

(it-sequential "cps-collect-mv-call-args-one-arg-wraps-lambda"
  (let* ((arg    (cl-cc/ast:make-ast-int :value 5))
         (result (format nil "~S"
                         (cl-cc/cps::%cps-collect-mv-call-args (list arg) 'acc 'f 'k))))
    (expect (search "LAMBDA" result) :to-be-truthy)
    (expect (search "PUSH" result) :to-be-truthy)))
