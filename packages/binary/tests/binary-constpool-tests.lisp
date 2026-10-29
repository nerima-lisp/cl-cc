;;;; packages/binary/tests/binary-constpool-tests.lisp — FR-724 constant pool tests

(in-package :cl-cc/test)



(defun %constpool-ub8 (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

