;;;; tests/unit/vm/vm-dispatch-tests.lisp — VM dispatch helper tests

(in-package :cl-cc/test)



(it-sequential "vm-classify-arg-primitive integer"
  (destructuring-bind (value expected-class) (list 42 'integer)
    (expect (cl-cc/vm::vm-classify-arg value nil) :to-be expected-class)))

(it-sequential "vm-classify-arg-primitive string"
  (destructuring-bind (value expected-class) (list "hello" 'string)
    (expect (cl-cc/vm::vm-classify-arg value nil) :to-be expected-class)))

(it-sequential "vm-classify-arg-primitive symbol"
  (destructuring-bind (value expected-class) (list 'foo 'symbol)
    (expect (cl-cc/vm::vm-classify-arg value nil) :to-be expected-class)))

(it-sequential "vm-classify-arg-hash-table-no-class"
  (let ((ht (make-hash-table :test #'eq)))
    (expect (cl-cc/vm::vm-classify-arg ht nil) :to-be t)))

(it-sequential "vm-classify-arg-hash-table-with-class"
  (let* ((class-ht (make-hash-table :test #'eq))
         (obj-ht   (make-hash-table :test #'eq)))
    (setf (gethash :__name__ class-ht) 'my-class)
    (setf (gethash :__class__ obj-ht) class-ht)
    (expect (cl-cc/vm::vm-classify-arg obj-ht nil) :to-be 'my-class)))

(it-sequential "vm-generic-function-p plain-hash-table"
  (destructuring-bind (value verify) (list (make-hash-table :test #'eq) (lambda (value)
             (expect (cl-cc/vm::vm-generic-function-p value) :to-be-falsy)))
    (funcall verify value)))

(it-sequential "vm-generic-function-p integer"
  (destructuring-bind (value verify) (list 99 (lambda (value)
             (expect (cl-cc/vm::vm-generic-function-p value) :to-be-falsy)))
    (funcall verify value)))

(it-sequential "vm-generic-function-p hash-with-methods"
  (destructuring-bind (value verify) (list (let ((ht (make-hash-table :test #'eq)))
             (setf (gethash :__methods__ ht)
                   (make-hash-table :test #'equal))
             ht) (lambda (value)
             (expect (cl-cc/vm::vm-generic-function-p value) :to-be-truthy)))
    (funcall verify value)))
