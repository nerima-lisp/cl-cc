;;;; tests/unit/vm/vm-run-fusion-tests.lisp — VM2 superinstruction fusion and extended run-vm tests

(in-package :cl-cc/test)


(it-sequential "run-vm-chains-arithmetic-correctly"
  (let ((s (cl-cc:make-vm2-state)))
    (let ((code (make-bytecode cl-cc:+op2-const+ 1 3   nil
                               cl-cc:+op2-const+ 2 4   nil
                               cl-cc:+op2-add2+  3 1   2
                               cl-cc:+op2-const+ 4 5   nil
                               cl-cc:+op2-mul2+  0 3   4
                               cl-cc:+op2-halt2+ 0 nil nil)))
      (expect (= 35 (cl-cc/vm::run-vm code s)) :to-be-truthy))))

(it-sequential "run-vm-loads-any-cl-object"
  (let ((s (cl-cc:make-vm2-state)))
    (let ((code (make-bytecode cl-cc:+op2-const+ 0 :hello nil
                               cl-cc:+op2-halt2+ 0 nil    nil)))
      (expect (cl-cc/vm::run-vm code s) :to-equal :hello))))

(it-sequential "run-vm-preserves-non-result-registers"
  (let ((s (cl-cc:make-vm2-state)))
    (let ((code (make-bytecode cl-cc:+op2-const+ 1 10  nil
                               cl-cc:+op2-const+ 2 20  nil
                               cl-cc:+op2-const+ 0 99  nil
                               cl-cc:+op2-halt2+ 0 nil nil)))
      (cl-cc/vm::run-vm code s)
      (expect (= 10 (cl-cc/vm/protocols:vm-reg-get s 1)) :to-be-truthy)
      (expect (= 20 (cl-cc/vm/protocols:vm-reg-get s 2)) :to-be-truthy))))

(it-sequential "vm2-top-candidates-sorts-by-frequency"
  (let* ((code (make-bytecode cl-cc:+op2-const+ 1 3 nil
                              cl-cc:+op2-add-imm2+ 0 1 4
                              cl-cc:+op2-halt2+ 0 nil nil
                              cl-cc:+op2-const+ 1 5 nil
                              cl-cc:+op2-add-imm2+ 0 1 6
                              cl-cc:+op2-halt2+ 0 nil nil
                              cl-cc:+op2-move+ 2 1 nil
                              cl-cc:+op2-halt2+ 2 nil nil))
         (top (cl-cc/vm::vm2-top-superoperator-candidates code :limit 2))
         (add-imm2-name (aref cl-cc/vm:*opcode-name-table* cl-cc:+op2-add-imm2+))
         (halt2-name    (aref cl-cc/vm:*opcode-name-table* cl-cc:+op2-halt2+)))
    (expect (= 2 (second (first top))) :to-be-truthy)
    (expect (member (list add-imm2-name halt2-name) (mapcar #'first top) :test #'equal) :to-be-truthy)))

(it-sequential "run-vm-fusion-const-add-produces-fused-bytecode"
  (let* ((code (make-bytecode cl-cc:+op2-const+ 2 4 nil
                              cl-cc:+op2-add2+  0 1 2
                              cl-cc:+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm/execution::vm2-fuse-immediate-superinstructions code))
         (s (cl-cc:make-vm2-state)))
    (cl-cc/vm/protocols:vm-reg-set s 1 3)
    (expect (= 7 (cl-cc/vm::run-vm code s)) :to-be-truthy)
    (expect (= 8 (length fused)) :to-be-truthy)))

(it-sequential "run-vm-fusion-const-halt-computes-result"
  (expect (= 42 (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-const+ 0 42 nil
                                                cl-cc:+op2-halt2+ 0 nil nil)
                                 (cl-cc:make-vm2-state))) :to-be-truthy))

(it-sequential "run-vm-fusion-specialized-opcodes-execute-correctly"
  (let ((s (cl-cc:make-vm2-state)))
    (expect (= 7 (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-const+ 1 3 nil
                                             cl-cc:+op2-add-imm2+ 0 1 4
                                             cl-cc:+op2-halt2+ 0 nil nil)
                               s)) :to-be-truthy)
    (expect (= 1 (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-const+ 1 7 nil
                                             cl-cc:+op2-num-gt-imm2+ 0 1 5
                                             cl-cc:+op2-halt2+ 0 nil nil)
                               s)) :to-be-truthy)))

(it-sequential "run-vm-with-opcode-bigrams-counts-executed-pairs"
  (let ((s (cl-cc:make-vm2-state)))
    (multiple-value-bind (result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams
         (make-bytecode cl-cc:+op2-const+ 1 3 nil
                        cl-cc:+op2-add-imm2+ 0 1 4
                        cl-cc:+op2-halt2+ 0 nil nil)
         s)
      (expect (= 7 result) :to-be-truthy)
      (expect (= 1 (gethash '(cl-cc:const cl-cc:add-imm2) counts)) :to-be-truthy)
      (expect (= 1 (gethash '(cl-cc:add-imm2 cl-cc:halt2) counts)) :to-be-truthy))))

(defclass vm2-test-instance ()
  ((x :initarg :x :reader vm2-test-instance-x)))

(it-sequential "run-vm-make-instance-opcode"
  (let ((s (cl-cc:make-vm2-state)))
    (cl-cc/vm/protocols:vm-reg-set s 0 :x)
    (cl-cc/vm/protocols:vm-reg-set s 1 42)
    (let ((result (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-make-instance+ 2 'vm2-test-instance 2
                                                cl-cc:+op2-halt2+ 2 nil nil)
                                 s)))
      (expect (typep result 'vm2-test-instance) :to-be-truthy)
      (expect (= 42 (vm2-test-instance-x result)) :to-be-truthy))))

(it-sequential "run-vm-edge-cases sub-negative"
  (destructuring-bind (code expected) (list (make-bytecode cl-cc:+op2-const+ 1 3   nil
                          cl-cc:+op2-const+ 2 10  nil
                          cl-cc:+op2-sub2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) -7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm code s) :to-equal expected))))

(it-sequential "run-vm-edge-cases mul-by-zero"
  (destructuring-bind (code expected) (list (make-bytecode cl-cc:+op2-const+ 1 12  nil
                          cl-cc:+op2-const+ 2 0   nil
                          cl-cc:+op2-mul2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) 0)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm code s) :to-equal expected))))

(it-sequential "run-vm-edge-cases large-immediate"
  (destructuring-bind (code expected) (list (make-bytecode cl-cc:+op2-const+ 0 1000000 nil
                          cl-cc:+op2-halt2+ 0 nil     nil) 1000000)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm code s) :to-equal expected))))

(it-sequential "run-vm-edge-cases nil-immediate"
  (destructuring-bind (code expected) (list (make-bytecode cl-cc:+op2-const+ 0 nil nil
                          cl-cc:+op2-halt2+ 0 nil nil) nil)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm code s) :to-equal expected))))

(it-sequential "run-vm-edge-cases move-chain"
  (destructuring-bind (code expected) (list (make-bytecode cl-cc:+op2-const+ 1 55  nil
                          cl-cc:+op2-move+  2 1   nil
                          cl-cc:+op2-move+  0 2   nil
                          cl-cc:+op2-halt2+ 0 nil nil) 55)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm code s) :to-equal expected))))

