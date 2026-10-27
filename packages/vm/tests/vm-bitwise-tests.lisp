;;;; tests/unit/vm/vm-bitwise-tests.lisp — VM bitwise integer instruction tests

(in-package :cl-cc/test)



(defun %make-unary (ctor src)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-binary (ctor lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-pred2 (ctor lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

