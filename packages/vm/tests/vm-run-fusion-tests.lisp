;;;; tests/unit/vm/vm-run-fusion-tests.lisp — VM2 superinstruction fusion and extended run-vm tests

(in-package :cl-cc/test)


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

