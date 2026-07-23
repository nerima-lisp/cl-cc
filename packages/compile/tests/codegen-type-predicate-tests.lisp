;;;; tests/unit/compile/codegen-type-predicate-tests.lisp — Type predicate codegen tests

(in-package :cl-cc/test)

;;; ─── %proven-fixnum-type-p / %proven-float-type-p / %float-literal-node-p ───

(it-sequential "proven-type-p-cases fixnum-proven"
  (destructuring-bind (pred-fn spec expected) (list #'cl-cc/compile::%proven-fixnum-type-p 'fixnum t)
    (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (expect (funcall pred-fn ty) :to-be-truthy)
        (expect (funcall pred-fn ty) :to-be-falsy)))))

(it-sequential "proven-type-p-cases fixnum-nil"
  (destructuring-bind (pred-fn spec expected) (list #'cl-cc/compile::%proven-fixnum-type-p nil nil)
    (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (expect (funcall pred-fn ty) :to-be-truthy)
        (expect (funcall pred-fn ty) :to-be-falsy)))))

(it-sequential "proven-type-p-cases float-proven"
  (destructuring-bind (pred-fn spec expected) (list #'cl-cc/compile::%proven-float-type-p 'float t)
    (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (expect (funcall pred-fn ty) :to-be-truthy)
        (expect (funcall pred-fn ty) :to-be-falsy)))))

(it-sequential "proven-type-p-cases float-not-fixnum"
  (destructuring-bind (pred-fn spec expected) (list #'cl-cc/compile::%proven-float-type-p 'fixnum nil)
    (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (expect (funcall pred-fn ty) :to-be-truthy)
        (expect (funcall pred-fn ty) :to-be-falsy)))))

(it-sequential "proven-type-p-cases symbol-proven"
  (destructuring-bind (pred-fn spec expected) (list #'cl-cc/compile::%proven-symbol-type-p 'symbol t)
    (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (expect (funcall pred-fn ty) :to-be-truthy)
        (expect (funcall pred-fn ty) :to-be-falsy)))))

(it-sequential "proven-type-p-cases symbol-not-fixnum"
  (destructuring-bind (pred-fn spec expected) (list #'cl-cc/compile::%proven-symbol-type-p 'fixnum nil)
    (let ((ty (when spec (cl-cc/type:parse-type-specifier spec))))
    (if expected
        (expect (funcall pred-fn ty) :to-be-truthy)
        (expect (funcall pred-fn ty) :to-be-falsy)))))

(it-sequential "float-literal-node-p-cases float-quote"
  (destructuring-bind (node expected) (list (make-ast-quote :value 3.14) t)
    (if expected
      (expect (cl-cc/compile::%float-literal-node-p node) :to-be-truthy)
      (expect (cl-cc/compile::%float-literal-node-p node) :to-be-falsy))))

(it-sequential "float-literal-node-p-cases int-quote"
  (destructuring-bind (node expected) (list (make-ast-quote :value 42) nil)
    (if expected
      (expect (cl-cc/compile::%float-literal-node-p node) :to-be-truthy)
      (expect (cl-cc/compile::%float-literal-node-p node) :to-be-falsy))))

(it-sequential "float-literal-node-p-cases non-quote-ast"
  (destructuring-bind (node expected) (list (make-ast-int :value 1) nil)
    (if expected
      (expect (cl-cc/compile::%float-literal-node-p node) :to-be-truthy)
      (expect (cl-cc/compile::%float-literal-node-p node) :to-be-falsy))))

(it-sequential "float-literal-node-p-sees-through-ast-the"
  (expect (cl-cc/compile::%float-literal-node-p
    (make-ast-the :type 'float
                  :value (make-ast-quote :value 3.14))) :to-be-truthy))
