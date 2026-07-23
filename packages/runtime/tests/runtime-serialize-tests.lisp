;;;; packages/runtime/tests/runtime-serialize-tests.lisp — FR-683 tests

(in-package :cl-cc/test)


(defclass serialize-test-node ()
  ((name :initarg :name :accessor serialize-test-node-name)
   (next :initarg :next :accessor serialize-test-node-next)))

(defun assert-runtime-serialization-roundtrip (value &key (test #'equalp))
  (let ((copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize value))))
    (expect (funcall test value copy) :to-be-truthy)
    copy))

(it-sequential "runtime-serialize-basic-types-roundtrip"
  (let* ((table (make-hash-table :test #'equal))
         (value (list 42 3.5 "x" :kw #(1 2))))
    (setf (gethash "value" table) value)
    (let ((copy (assert-runtime-serialization-roundtrip table)))
      (expect (equalp value (gethash "value" copy)) :to-be-truthy))))

(it-sequential "runtime-serialize-circular-cons-roundtrip"
  (let ((cell (cons :head nil)))
    (setf (cdr cell) cell)
    (let ((copy (cl-cc/runtime:deserialize (cl-cc/runtime:serialize cell))))
      (expect (cdr copy) :to-be copy)
      (expect (car copy) :to-be :head))))

(it-sequential "runtime-serialize-clos-instance-roundtrip"
  (let ((node (make-instance 'serialize-test-node :name "root")))
    (setf (serialize-test-node-next node) node)
    (assert-runtime-serialization-roundtrip
     node
     :test (lambda (expected copy)
             (and (not (eq expected copy))
                  (typep copy 'serialize-test-node)
                  (equal "root" (serialize-test-node-name copy))
                  (eq copy (serialize-test-node-next copy)))))))
