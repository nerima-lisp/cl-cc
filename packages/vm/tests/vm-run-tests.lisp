;;;; tests/unit/vm/vm-run-tests.lisp — VM error dispatch tests

(in-package :cl-cc/test)



(it-sequential "vm-error-type-matches string-matches-error"
  (destructuring-bind (error-val handler-type expected-result) (list "boom" 'error t)
    (let ((actual (cl-cc/vm::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (expect actual :to-be-truthy)
        (expect actual :to-be-falsy)))))

(it-sequential "vm-error-type-matches string-matches-condition"
  (destructuring-bind (error-val handler-type expected-result) (list "boom" 'condition t)
    (let ((actual (cl-cc/vm::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (expect actual :to-be-truthy)
        (expect actual :to-be-falsy)))))

(it-sequential "vm-error-type-matches string-matches-t"
  (destructuring-bind (error-val handler-type expected-result) (list "boom" 't t)
    (let ((actual (cl-cc/vm::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (expect actual :to-be-truthy)
        (expect actual :to-be-falsy)))))

(it-sequential "vm-error-type-matches string-no-match-specific-subtype"
  (destructuring-bind (error-val handler-type expected-result) (list "boom" 'type-error nil)
    (let ((actual (cl-cc/vm::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (expect actual :to-be-truthy)
        (expect actual :to-be-falsy)))))

(it-sequential "vm-error-type-matches condition-object-matches-t"
  (destructuring-bind (error-val handler-type expected-result) (list (make-condition 'simple-error :format-control "x") 't t)
    (let ((actual (cl-cc/vm::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (expect actual :to-be-truthy)
        (expect actual :to-be-falsy)))))

(it-sequential "build-label-table-uses-integer-keyed-buckets"
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :r0 :value 1)
                             (cl-cc:make-vm-label :name :done)
                             (cl-cc:make-vm-halt :reg :r0)))
         (labels (cl-cc/vm::build-label-table instructions)))
    ;; SBCL's HASH-TABLE-TEST returns the symbol 'EQL, not the function object.
    (expect (hash-table-test labels) :to-be 'eql)
    (expect (= 0 (cl-cc/vm::vm-label-table-lookup labels "entry")) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::vm-label-table-lookup labels :done)) :to-be-truthy)))

(it-sequential "vm-print-backtrace-resolves-return-pc-to-nearest-label"
  (let* ((instructions (list (cl-cc:make-vm-label :name "caller")
                             (cl-cc:make-vm-const :dst :r0 :value 1)
                             (cl-cc:make-vm-call :dst :r1 :func :r2 :args '(:r0))
                             (cl-cc:make-vm-const :dst :r3 :value 2)
                             (cl-cc:make-vm-label :name "callee")
                             (cl-cc:make-vm-ret :reg :r1)))
         (labels (cl-cc/vm::build-label-table instructions))
         (state (make-test-vm))
         (stream (make-string-output-stream)))
    (cl-cc:vm-reg-set state :arg0 42)
    (cl-cc/vm::vm-push-call-frame state 3 :r1)
    (cl-cc:vm-print-backtrace state :labels labels :stream stream)
    (let ((output (get-output-stream-string stream)))
      (expect (search "VM backtrace:" output) :to-be-truthy)
      (expect (search "0: caller" output) :to-be-truthy)
      (expect (search "return-pc=3" output) :to-be-truthy)
      (expect (search "dst=R1" output) :to-be-truthy)
      (expect (search "args=(42)" output) :to-be-truthy))))

(it-sequential "vm-print-backtrace-handles-empty-call-stack"
  (let ((stream (make-string-output-stream)))
    (cl-cc:vm-print-backtrace (make-test-vm) :labels nil :stream stream)
    (expect (search "<empty>" (get-output-stream-string stream)) :to-be-truthy)))

(it-sequential "vm-signal-error-prints-backtrace-before-unhandled-host-error"
  (let* ((instructions (list (cl-cc:make-vm-label :name "caller")
                             (cl-cc:make-vm-call :dst :r1 :func :r2 :args '(:arg0))
                             (cl-cc:make-vm-const :dst :r3 :value 2)
                             (cl-cc:make-vm-label :name "callee")
                             (cl-cc:make-vm-signal-error :error-reg :err)))
         (labels (cl-cc/vm::build-label-table instructions))
         (state (make-test-vm))
         (stream (make-string-output-stream)))
    (cl-cc:vm-reg-set state :arg0 42)
    (cl-cc:vm-reg-set state :err "boom")
    (cl-cc/vm::vm-push-call-frame state 2 :r1)
    (let ((*error-output* stream)
          (signaled-p nil))
      (handler-case
          (cl-cc:execute-instruction
           (cl-cc:make-vm-signal-error :error-reg :err)
           state
           4
           labels)
        (error ()
          (setf signaled-p t)))
      (expect signaled-p :to-be-truthy))
    (let ((output (get-output-stream-string stream)))
      (expect (search "VM backtrace:" output) :to-be-truthy)
      (expect (search "0: caller" output) :to-be-truthy)
      (expect (search "return-pc=2" output) :to-be-truthy)
      (expect (search "dst=R1" output) :to-be-truthy)
      (expect (search "args=(42)" output) :to-be-truthy))))

(it-sequential "vm-jump-uses-label-table-lookup"
  (let* ((instructions (list (cl-cc:make-vm-jump :label "target")
                             (cl-cc:make-vm-label :name "target")
                             (cl-cc:make-vm-halt :reg :r0)))
         (labels (cl-cc/vm::build-label-table instructions))
         (state (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction (first instructions) state 0 labels)
      (declare (ignore result))
      (expect (= 1 next-pc) :to-be-truthy)
      (expect halt-p :to-be-falsy))))

;;; ─── VM2 defopcode / run-vm tests ───────────────────────────────────────────

(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))

(it-sequential "vm2-opcode-registration"
  (expect (numberp cl-cc:+op2-const+) :to-be-truthy)
  (expect (not (null (aref cl-cc/vm::*opcode-dispatch-table* cl-cc:+op2-const+))) :to-be-truthy)
  (expect (not (null (aref cl-cc/vm::*opcode-dispatch-table* cl-cc:+op2-add2+))) :to-be-truthy)
  (expect (aref cl-cc/vm::*opcode-name-table* cl-cc:+op2-add2+) :to-equal 'cl-cc:add2)
  (expect (= cl-cc:+op2-const+ (gethash 'cl-cc:const cl-cc/vm::*opcode-encoder-table*)) :to-be-truthy)
  (expect (= cl-cc:+op2-move+ (gethash 'cl-cc:move  cl-cc/vm::*opcode-encoder-table*)) :to-be-truthy)
  (expect (= cl-cc:+op2-add2+ (gethash 'cl-cc:add2  cl-cc/vm::*opcode-encoder-table*)) :to-be-truthy)
  (expect (= cl-cc:+op2-add-imm2+ (gethash 'cl-cc:add-imm2 cl-cc/vm::*opcode-encoder-table*)) :to-be-truthy))

(it-sequential "vm2-opcode-distinct-values"
  (let ((ops (list cl-cc:+op2-const+
                   cl-cc:+op2-move+
                   cl-cc:+op2-add2+
                   cl-cc:+op2-add-imm2+
                   cl-cc:+op2-sub2+
                   cl-cc:+op2-sub-imm2+
                   cl-cc:+op2-mul2+
                   cl-cc:+op2-mul-imm2+
                   cl-cc:+op2-num-eq-imm2+
                   cl-cc:+op2-num-lt-imm2+
                   cl-cc:+op2-num-gt-imm2+
                   cl-cc:+op2-num-le-imm2+
                   cl-cc:+op2-num-ge-imm2+
                   cl-cc:+op2-halt2+)))
    (expect (= (length ops) (length (remove-duplicates ops))) :to-be-truthy)))

(it-sequential "vm2-state-structure"
  (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc:vm2-state-p s) :to-be-truthy)
    (expect (simple-vector-p (cl-cc:vm2-state-registers s)) :to-be-truthy)
    (expect (= 256 (length (cl-cc:vm2-state-registers s))) :to-be-truthy)
    (dotimes (i 256)
      (expect (null (cl-cc/vm::vm2-reg-get s i)) :to-be-truthy))
    (expect (cl-cc:vm2-state-values-buffer s) :to-be-null)
    (expect (not (null (gethash '*features* (cl-cc:vm2-state-global-vars s)))) :to-be-truthy))
  (let* ((str (make-string-output-stream))
         (s   (cl-cc:make-vm2-state :output-stream str)))
    (expect (cl-cc:vm2-state-output-stream s) :to-equal str)))

(it-sequential "vm2-reg-operations"
  (let ((s (cl-cc:make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 0 42)
    (expect (= 42 (cl-cc/vm::vm2-reg-get s 0)) :to-be-truthy)
    ;; set returns the written value
    (expect (= 99 (cl-cc/vm::vm2-reg-set s 5 99)) :to-be-truthy)
    ;; overwrite
    (cl-cc/vm::vm2-reg-set s 3 100)
    (cl-cc/vm::vm2-reg-set s 3 200)
    (expect (= 200 (cl-cc/vm::vm2-reg-get s 3)) :to-be-truthy)
    ;; all 256 slots are independent
    (dotimes (i 256)
      (cl-cc/vm::vm2-reg-set s i i))
    (dotimes (i 256)
      (expect (= i (cl-cc/vm::vm2-reg-get s i)) :to-be-truthy))))

(it-sequential "vm-run-profiles-basic-block-and-branch-counters"
  (let* ((insts (list (cl-cc:make-vm-label :name "entry")
                      (cl-cc:make-vm-const :dst :r0 :value 0)
                      (cl-cc:make-vm-jump-zero :reg :r0 :label "taken")
                      (cl-cc:make-vm-const :dst :r1 :value 1)
                      (cl-cc:make-vm-halt :reg :r1)
                      (cl-cc:make-vm-label :name "taken")
                      (cl-cc:make-vm-const :dst :r1 :value 2)
                      (cl-cc:make-vm-halt :reg :r1)))
         (program (cl-cc/vm::make-vm-program :instructions insts :result-register :r1))
         (state (make-test-vm)))
    (setf (cl-cc:vm-profile-enabled-p state) t)
    (expect (= 2 (cl-cc:run-compiled program :state state)) :to-be-truthy)
    (let* ((bb (cl-cc/vm:vm-get-profile-bb-counts state))
           (branches (cl-cc/vm:vm-get-profile-branch-counts state))
           (jump-kind (type-of (cl-cc:make-vm-jump-zero :reg :r0 :label "taken"))))
      (expect (> (gethash 0 bb 0) 0) :to-be-truthy)
      (expect (> (gethash 2 bb 0) 0) :to-be-truthy)
      (expect (> (gethash 5 bb 0) 0) :to-be-truthy)
      (expect (> (gethash (list jump-kind 2 5) branches 0) 0) :to-be-truthy))))
