;;;; tests/clos-tests.lisp - CLOS Compilation Tests
;;;
;;; Comprehensive tests for CLOS support: defclass, defgeneric, defmethod,
;;; make-instance, slot-value, reader/writer/accessor methods, and
;;; generic function dispatch.

(in-package :cl-cc/test)


;;; AST Parsing Tests

(it-sequential "clos-parse-defclass"
  (let ((ast (lower-sexp-to-ast '(defclass point ()
                                    ((x :initarg :x :reader point-x)
                                     (y :initarg :y :reader point-y))))))
    (expect (typep ast 'ast-defclass) :to-be-truthy)
    (expect (ast-defclass-name ast) :to-be 'point)
    (expect (ast-defclass-superclasses ast) :to-be-null)
    (expect (= 2 (length (ast-defclass-slots ast))) :to-be-truthy)
    (let ((x-slot (first (ast-defclass-slots ast))))
      (expect (ast-slot-name x-slot) :to-be 'x)
      (expect (ast-slot-initarg x-slot) :to-be :x)
      (expect (ast-slot-reader x-slot) :to-be 'point-x))))

(it-sequential "clos-parse-defclass-with-superclass"
  (let ((ast (lower-sexp-to-ast '(defclass colored-point (point)
                                    ((color :initarg :color))))))
    (expect (typep ast 'ast-defclass) :to-be-truthy)
    (expect (ast-defclass-name ast) :to-be 'colored-point)
    (expect (ast-defclass-superclasses ast) :to-equal '(point))))

(it-sequential "clos-parse-defgeneric"
  (let ((ast (lower-sexp-to-ast '(defgeneric area (shape)))))
    (expect (typep ast 'ast-defgeneric) :to-be-truthy)
    (expect (ast-defgeneric-name ast) :to-be 'area)
    (expect (ast-defgeneric-params ast) :to-equal '(shape))))

(it-sequential "clos-parse-defmethod"
  (let ((ast (lower-sexp-to-ast '(defmethod area ((s circle))
                                   (* 3 (slot-value s 'radius))))))
    (expect (typep ast 'ast-defmethod) :to-be-truthy)
    (expect (ast-defmethod-name ast) :to-be 'area)
    (expect (ast-defmethod-params ast) :to-equal '(s))
    (expect (= 1 (length (ast-defmethod-body ast))) :to-be-truthy)
    (let ((specs (ast-defmethod-specializers ast)))
      (expect (= 1 (length specs)) :to-be-truthy)
      (expect (first specs) :to-equal '(s . circle)))))

(it-sequential "clos-parse-make-instance"
  (let ((ast (lower-sexp-to-ast '(make-instance 'point :x 10 :y 20))))
    (expect (typep ast 'ast-make-instance) :to-be-truthy)
    (expect (typep (ast-make-instance-class ast) 'ast-quote) :to-be-truthy)
    (expect (= 2 (length (ast-make-instance-initargs ast))) :to-be-truthy)
    (expect (car (first (ast-make-instance-initargs ast))) :to-be :x)
    (expect (car (second (ast-make-instance-initargs ast))) :to-be :y)))

(it-sequential "clos-parse-slot-value"
  (let ((ast (lower-sexp-to-ast '(slot-value obj 'x))))
    (expect (typep ast 'ast-slot-value) :to-be-truthy)
    (expect (ast-slot-value-slot ast) :to-be 'x)
    (expect (typep (ast-slot-value-object ast) 'ast-var) :to-be-truthy)))

;;; defgeneric options tests

(it-sequential "clos-defgeneric-with-options documentation"
  (destructuring-bind (expected-name expected-params form) (list 'area '(shape) '(defgeneric area (shape) (:documentation "Compute area")))
    (let ((ast (lower-sexp-to-ast form)))
    (expect (typep ast 'ast-defgeneric) :to-be-truthy)
    (expect (ast-defgeneric-name ast) :to-be expected-name)
    (when expected-params
      (expect (ast-defgeneric-params ast) :to-equal expected-params)))))

(it-sequential "clos-defgeneric-with-options precedence+class"
  (destructuring-bind (expected-name expected-params form) (list 'combine nil '(defgeneric combine (a b)
              (:argument-precedence-order b a)
              (:generic-function-class standard-generic-function)))
    (let ((ast (lower-sexp-to-ast form)))
    (expect (typep ast 'ast-defgeneric) :to-be-truthy)
    (expect (ast-defgeneric-name ast) :to-be expected-name)
    (when expected-params
      (expect (ast-defgeneric-params ast) :to-equal expected-params)))))

(it-sequential "clos-defgeneric-inline-method-parse single-method"
  (destructuring-bind (form expected-form-count expected-method-name) (list '(defgeneric area (shape)
              (:method ((s circle)) (* 3 (slot-value s 'radius)))) 2 'area)
    (let ((ast (lower-sexp-to-ast form)))
    (expect (typep ast 'ast-progn) :to-be-truthy)
    (let ((forms (ast-progn-forms ast)))
      (expect (= expected-form-count (length forms)) :to-be-truthy)
      (expect (typep (first forms) 'ast-defgeneric) :to-be-truthy)
      (dolist (f (cdr forms))
        (expect (typep f 'ast-defmethod) :to-be-truthy))
      (expect (ast-defmethod-name (second forms)) :to-be expected-method-name)))))

(it-sequential "clos-defgeneric-inline-method-parse multiple-methods"
  (destructuring-bind (form expected-form-count expected-method-name) (list '(defgeneric describe-it (x)
              (:documentation "Describe an object")
              (:method ((x integer)) (format nil "int:~A" x))
              (:method ((x string)) (format nil "str:~A" x))) 3 'describe-it)
    (let ((ast (lower-sexp-to-ast form)))
    (expect (typep ast 'ast-progn) :to-be-truthy)
    (let ((forms (ast-progn-forms ast)))
      (expect (= expected-form-count (length forms)) :to-be-truthy)
      (expect (typep (first forms) 'ast-defgeneric) :to-be-truthy)
      (dolist (f (cdr forms))
        (expect (typep f 'ast-defmethod) :to-be-truthy))
      (expect (ast-defmethod-name (second forms)) :to-be expected-method-name)))))

(it-sequential "clos-defgeneric-inline-method-compile"
  (let ((result (run-string "
    (defgeneric greet (who)
      (:method ((who string))
        (concatenate 'string \"Hello \" who)))
    (greet \"World\")")))
    (expect result :to-equal "Hello World")))

;;; AST Roundtrip Tests

(it-sequential "clos-ast-roundtrip-forms defclass"
  (destructuring-bind (form check) (list '(defclass point nil
              ((x :initarg :x :reader point-x)
               (y :initarg :y :reader point-y))) (lambda (result)
             (expect (first result) :to-be 'defclass)
             (expect (second result) :to-be 'point)
             (expect (third result) :to-be-null)
             (expect (= 2 (length (fourth result))) :to-be-truthy)))
    (funcall check (ast-to-sexp (lower-sexp-to-ast form)))))

(it-sequential "clos-ast-roundtrip-forms defgeneric"
  (destructuring-bind (form check) (list '(defgeneric compute (obj)) (lambda (result)
             (expect result :to-equal '(defgeneric compute (obj)))))
    (funcall check (ast-to-sexp (lower-sexp-to-ast form)))))

(it-sequential "clos-ast-roundtrip-forms slot-value"
  (destructuring-bind (form check) (list '(slot-value obj 'x) (lambda (result)
             (expect (first result) :to-be 'slot-value)
             (expect (third result) :to-equal '(quote x))))
    (funcall check (ast-to-sexp (lower-sexp-to-ast form)))))

(it-sequential "clos-ast-roundtrip-forms setf-slot-value"
  (destructuring-bind (form check) (list '(setf (slot-value obj (quote x)) 42) (lambda (result)
             (expect (first result) :to-be 'setf)
             (expect (first (second result)) :to-be 'slot-value)
             (expect (third (second result)) :to-equal '(quote x))))
    (funcall check (ast-to-sexp (lower-sexp-to-ast form)))))

;;; Compilation and Execution Tests
