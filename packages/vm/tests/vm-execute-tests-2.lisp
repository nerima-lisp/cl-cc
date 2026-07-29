;;;; tests/unit/vm/vm-execute-tests-2.lisp — Core VM Execute-Instruction Tests (Part 2)
;;;;
;;;; Continuation of vm-execute-tests.lisp:
;;;; multiple-values ops, vm-apply, values-buffer management,
;;;; side-effect instructions, and make-closure/closure-ref-idx.

(in-package :cl-cc/test)


(it-sequential "vm-execute-vm-values-stores-all"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 10)
    (cl-cc:vm-reg-set s :R2 20)
    (cl-cc:vm-reg-set s :R3 30)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-values :dst :R0 :src-regs (list :R1 :R2 :R3)) s 0 (make-hash-table :test #'equal))
    (expect (= 10 (cl-cc:vm-reg-get s :R0)) :to-be-truthy)
    (expect (cl-cc/vm::vm-values-list s) :to-equal '(10 20 30))))

(it-sequential "vm-execute-mv-bind-distributes"
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(1 2 3))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-mv-bind :dst-regs (list :R0 :R1 :R2)) s 0 (make-hash-table :test #'equal))
    (expect (= 1 (cl-cc:vm-reg-get s :R0)) :to-be-truthy)
    (expect (= 2 (cl-cc:vm-reg-get s :R1)) :to-be-truthy)
    (expect (= 3 (cl-cc:vm-reg-get s :R2)) :to-be-truthy)))

(it-sequential "vm-execute-values-to-list-copies"
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(7 8 9))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-values-to-list :dst :R0) s 0 (make-hash-table :test #'equal))
    (expect (cl-cc:vm-reg-get s :R0) :to-equal '(7 8 9))))

(it-sequential "vm-execute-spread-values-roundtrip"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 '(100 200 300))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-spread-values :dst :R0 :src :R1) s 0 (make-hash-table :test #'equal))
    (expect (= 100 (cl-cc:vm-reg-get s :R0)) :to-be-truthy)
    (expect (cl-cc/vm::vm-values-list s) :to-equal '(100 200 300))))

(it-sequential "vm-execute-vm-apply-spreads-final-list-on-host-function"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 #'+)
    (cl-cc:vm-reg-set s :R2 10)
    (cl-cc:vm-reg-set s :R3 20)
    (cl-cc:vm-reg-set s :R4 '(30 40))
    (multiple-value-bind (next-pc halted result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-apply :dst :R0 :func :R1 :args '(:R2 :R3 :R4))
         s 5 (make-hash-table :test #'equal))
      (declare (ignore result))
      (expect (= 6 next-pc) :to-be-truthy)
      (expect halted :to-be-falsy)
      (expect (= 100 (cl-cc:vm-reg-get s :R0)) :to-be-truthy))))

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
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-apply :dst :R0 :func :R1 :args '(:R2 :R3 :R4))
         s 9 (make-hash-table :test #'equal))
      (declare (ignore result))
      (expect (= 10 next-pc) :to-be-truthy)
      (expect halted :to-be-falsy)
      (expect (= 63 (cl-cc:vm-reg-get s :R0)) :to-be-truthy))))

(it-sequential "vm-execute-vm-values-buffer-management"
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(1 2 3))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-clear-values) s 0 (make-hash-table :test #'equal))
    (expect (cl-cc/vm::vm-values-list s) :to-be-null))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 55)
    (setf (cl-cc/vm::vm-values-list s) nil)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-ensure-values :src :R0) s 0 (make-hash-table :test #'equal))
    (expect (cl-cc/vm::vm-values-list s) :to-equal '(55))))

(it-sequential "vm-execute-set-global-stores-value"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-set-global :name 'myvar :src :R0) s 0 (make-hash-table :test #'equal))
    (expect (= 42 (gethash 'myvar (cl-cc/vm::vm-global-vars s))) :to-be-truthy)))

(it-sequential "vm-execute-get-global-loads-value"
  (let ((s (make-test-vm)))
    (setf (gethash 'myvar2 (cl-cc/vm::vm-global-vars s)) 99)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-get-global :dst :R0 :name 'myvar2) s 0 (make-hash-table :test #'equal))
    (expect (= 99 (cl-cc:vm-reg-get s :R0)) :to-be-truthy)))

(it-sequential "vm-execute-print-writes-to-stream"
  (let* ((str (make-string-output-stream))
         (s   (make-instance 'cl-cc/vm::vm-io-state :output-stream str)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-print :reg :R0) s 0 (make-hash-table :test #'equal))
    (expect (get-output-stream-string str) :to-equal (format nil "42~%"))))

(it-sequential "vm-execute-register-function-stores-in-registry"
  (let ((s (make-test-vm)))
    (let ((closure (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label "myfn"
                                  :params nil
                                  :captured-regs #()
                                  :captured-vals #())))
      (cl-cc:vm-reg-set s :R0 closure)
       (cl-cc/vm::execute-instruction
        (cl-cc:make-vm-register-function :name 'myfn :src :R0) s 0 (make-hash-table :test #'equal))
      (expect (not (null (gethash 'myfn (cl-cc/vm::vm-function-registry s)))) :to-be-truthy)
      (expect (cl-cc/vm::vm-closure-dispatch-tag closure) :to-equal '(:known-function . myfn)))))

(it-sequential "vm-defunctionalized-dispatch-resolves-known-function-tag"
  (let ((s (make-test-vm)))
    (let* ((registered (make-instance 'cl-cc/vm::vm-closure-object
                                      :entry-label "fn-registered"
                                      :params nil
                                      :captured-regs #()
                                      :captured-vals #()))
           (placeholder (make-instance 'cl-cc/vm::vm-closure-object
                                       :entry-label "fn-placeholder"
                                       :params nil
                                       :captured-regs #()
                                       :captured-vals #()
                                       :dispatch-tag '(:known-function . myfn))))
      (setf (gethash 'myfn (cl-cc/vm::vm-function-registry s)) registered)
      (expect (cl-cc/vm::%vm-defunctionalized-dispatch s placeholder) :to-be registered))))

(it-sequential "vm-execute-make-closure-stores-vector-captures"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 10)
    (cl-cc:vm-reg-set s :R2 20)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-make-closure :dst :R0 :label "L" :params nil :env-regs '(:R1 :R2))
     s 0 (%labels))
    (let* ((addr (cl-cc:vm-reg-get s :R0))
           (closure (cl-cc:vm-heap-get s addr)))
      (expect (vectorp (cl-cc/vm::vm-closure-captured-vals closure)) :to-be-truthy)
      (expect (= 2 (length (cl-cc/vm::vm-closure-captured-vals closure))) :to-be-truthy)
      (cl-cc:vm-reg-set s :R3 addr)
      (cl-cc:execute-instruction
       (cl-cc:make-vm-closure-ref-idx :dst :R4 :closure :R3 :index 1)
       s 0 (%labels))
      (expect (= 20 (cl-cc:vm-reg-get s :R4)) :to-be-truthy))))

(progn (it-sequential "vm-execute-vm-closure-propagates-dispatch-tag" (let ((s (make-test-vm))) (cl-cc:execute-instruction (cl-cc:make-vm-closure :dst :R0 :label "L_TAG" :params nil :optional-params nil :rest-param nil :key-params nil :rest-stack-alloc-p nil :inline-policy nil :dispatch-tag (cons :known-function (quote tagged)) :captured nil) s 0 (%labels)) (let ((closure (cl-cc:vm-reg-get s :R0))) (expect (cl-cc/vm::vm-closure-dispatch-tag closure) :to-equal (cons :known-function (quote tagged)))))) (it-sequential "vm-multiple-values-grow-beyond-initial-capacity" (let* ((s (make-test-vm)) (expected (loop for i below 96 collect i)) (registers (loop for i below 96 collect (intern (format nil "MV~D" i) :keyword)))) (loop for register in registers for value in expected do (cl-cc:vm-reg-set s register value)) (cl-cc:execute-instruction (cl-cc:make-vm-values :src-regs registers) s 0 (make-hash-table :test (function equal))) (expect (cl-cc/vm::vm-values-list s) :to-equal expected) (setf (cl-cc/vm::vm-values-list s) expected) (cl-cc:execute-instruction (cl-cc:make-vm-values-to-list :dst :R0) s 0 (make-hash-table :test (function equal))) (expect (cl-cc:vm-reg-get s :R0) :to-equal expected))) (it-sequential "vm-apply-preserves-more-than-64-host-values" (let* ((s (make-test-vm)) (expected (loop for i below 96 collect i))) (cl-cc:vm-reg-set s :FN (lambda (&rest values) (values-list values))) (cl-cc:vm-reg-set s :ARGS expected) (cl-cc:execute-instruction (cl-cc:make-vm-apply :dst :R0 :func :FN :args (list :ARGS)) s 0 (make-hash-table :test (function equal))) (expect (cl-cc:vm-reg-get s :R0) :to-equal 0) (expect (cl-cc/vm::vm-values-list s) :to-equal expected))) (it-sequential "vm-call-preserves-more-than-64-host-values" (let* ((s (make-test-vm)) (expected (loop for i below 96 collect i))) (cl-cc:vm-reg-set s :FN (lambda (values) (values-list values))) (cl-cc:vm-reg-set s :src-regs expected) (cl-cc:execute-instruction (cl-cc:make-vm-call :dst :R0 :func :FN :args (list :src-regs)) s 0 (make-hash-table :test (function equal))) (expect (cl-cc:vm-reg-get s :R0) :to-equal 0) (expect (cl-cc/vm::vm-values-list s) :to-equal expected))))
