;;;; tests/unit/vm/vm-dispatch-tests.lisp — VM dispatch helper tests

(in-package :cl-cc/test)

(it-sequential "vm-generic-function-p hash-with-methods"
  (let ((ht (make-hash-table :test (function eq))))
    (setf (gethash :__methods__ ht) (make-hash-table :test (function equal)))
    (expect (cl-cc/vm/execution:vm-generic-function-p ht) :to-be-truthy)))
