;;;; tests/unit/vm/vm-extensions-tests.lisp — VM language extension instruction tests

(in-package :cl-cc/test)



(defun %vm-ext-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-ext-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(it-sequential "vm-system-property-storage-is-separate"
  (let* ((s (make-test-vm))
         (sym (gensym "VM-SYSTEM-PROPS-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :user)
    (cl-cc:vm-reg-set s 3 :value)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (cl-cc/vm::vm-system-property-set s sym :function 'compiled-fn)
    (cl-cc/vm::vm-system-property-set s sym :type 'function)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 4 :src 1) s)
    (expect (getf (cl-cc:vm-reg-get s 4) :user) :to-equal :value)
    (expect (getf (cl-cc:vm-reg-get s 4) :function) :to-be-null)
    (expect (cl-cc/vm::vm-system-property-get s sym :function) :to-equal 'compiled-fn)
    (expect (getf (cl-cc/vm::vm-system-property-plist s sym) :type) :to-equal 'function)
    (expect (getf (symbol-plist sym) :function) :to-be-null)))

