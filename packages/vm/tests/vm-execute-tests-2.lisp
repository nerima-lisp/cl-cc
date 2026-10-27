;;;; tests/unit/vm/vm-execute-tests-2.lisp — Core VM Execute-Instruction Tests (Part 2)
;;;;
;;;; Continuation of vm-execute-tests.lisp:
;;;; multiple-values ops, vm-apply, values-buffer management,
;;;; side-effect instructions, and make-closure/closure-ref-idx.

(in-package :cl-cc/test)


(it-sequential "vm-execute-vm-apply-spreads-php-array-final-list"
  (let* ((s (make-test-vm))
         (array (make-hash-table :test #'equal))
         (order-key (let ((package (find-package :cl-cc/php)))
                      (when package
                        (multiple-value-bind (symbol status)
                            (find-symbol "+PHP-ARRAY-ORDER-KEY+" package)
                          (declare (ignore status))
                          (when symbol (symbol-value symbol)))))))
    (expect order-key :to-be-truthy)
    (setf (gethash order-key array) '(a b c))
    (setf (gethash 'a array) 10
          (gethash 'b array) 20
          (gethash 'c array) 30)
    (cl-cc:vm-reg-set s :R1 #'+)
    (cl-cc:vm-reg-set s :R2 1)
    (cl-cc:vm-reg-set s :R3 2)
    (cl-cc:vm-reg-set s :R4 array)
    (multiple-value-bind (next-pc halted result)
        (cl-cc/vm:execute-instruction
         (cl-cc:make-vm-apply :dst :R0 :func :R1 :args '(:R2 :R3 :R4))
         s 9 (make-hash-table :test #'equal))
      (declare (ignore result))
      (expect (= 10 next-pc) :to-be-truthy)
      (expect halted :to-be-falsy)
      (expect (= 63 (cl-cc:vm-reg-get s :R0)) :to-be-truthy))))

(progn)
