;;;; tests/regalloc-tests.lisp - Register Allocator Tests

(in-package :cl-cc/test)


;;; Instruction Def/Use Tests
(it-sequential "regalloc-defs-and-uses vm-const"
  (destructuring-bind (inst expected-defs expected-uses) (list (make-vm-const     :dst :r0 :value 42) '(:r0) nil)
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

(it-sequential "regalloc-defs-and-uses vm-binop"
  (destructuring-bind (inst expected-defs expected-uses) (list (make-vm-add       :dst :r2 :lhs :r0 :rhs :r1) '(:r2) '(:r0 :r1))
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

(it-sequential "regalloc-defs-and-uses vm-call"
  (destructuring-bind (inst expected-defs expected-uses) (list (make-vm-call      :dst :r3 :func :r0 :args '(:r1 :r2)) '(:r3) '(:r0 :r1 :r2))
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

(it-sequential "regalloc-defs-and-uses vm-jump-zero"
  (destructuring-bind (inst expected-defs expected-uses) (list (make-vm-jump-zero :reg :r0 :label "L1") nil '(:r0))
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

(it-sequential "regalloc-defs-and-uses vm-simd-vector-op"
  (destructuring-bind (inst expected-defs expected-uses) (list (make-vm-simd-vector-op :op :add :dst-array :r3 :lhs-array :r1
                                                        :rhs-array :r2 :index-reg :r4 :lanes 4
                                                        :element-type :i32) nil '(:r3 :r1 :r2 :r4))
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

(it-sequential "regalloc-defs-and-uses vm-label"
  (destructuring-bind (inst expected-defs expected-uses) (list (make-vm-label     :name "L1") nil nil)
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

(it-sequential "regalloc-defs-and-uses vm-tail-call"
  (destructuring-bind (inst expected-defs expected-uses) (list (cl-cc:make-vm-tail-call :dst :r9 :func :r0 :args '(:r1 :r2)) nil '(:r0 :r1 :r2))
    (expect (instruction-defs inst) :to-equal expected-defs) (expect (instruction-uses inst) :to-equal expected-uses)))

;;; Liveness Analysis Tests

(it-sequential "regalloc-liveness-three-overlapping-intervals"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (intervals (compute-live-intervals instructions)))
    (expect (= 3 (length intervals)) :to-be-truthy)
    (let ((r0-int (find :r0 intervals :key #'interval-vreg)))
      (expect (null r0-int) :to-be-falsy)
      (expect (= 0 (interval-start r0-int)) :to-be-truthy)
      (expect (= 2 (interval-end r0-int)) :to-be-truthy))
    (let ((r1-int (find :r1 intervals :key #'interval-vreg)))
      (expect (null r1-int) :to-be-falsy)
      (expect (= 1 (interval-start r1-int)) :to-be-truthy)
      (expect (= 2 (interval-end r1-int)) :to-be-truthy))
    (let ((r2-int (find :r2 intervals :key #'interval-vreg)))
      (expect (null r2-int) :to-be-falsy)
      (expect (= 2 (interval-start r2-int)) :to-be-truthy)
      (expect (= 3 (interval-end r2-int)) :to-be-truthy))))

(it-sequential "regalloc-liveness-disjoint-intervals-do-not-overlap"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-halt :reg :r0)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-halt :reg :r1)))
         (intervals (compute-live-intervals instructions)))
    (expect (= 2 (length intervals)) :to-be-truthy)
    (let ((r0-int (find :r0 intervals :key #'interval-vreg))
          (r1-int (find :r1 intervals :key #'interval-vreg)))
      (expect (<= (interval-end r0-int) (interval-start r1-int)) :to-be-truthy))))

(it-sequential "regalloc-liveness-forward-branch-extends-interval"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-jump-zero :reg :r1 :label "L1")
                             (make-vm-const :dst :r2 :value 2)
                             (make-vm-label :name "L1")
                             (make-vm-add :dst :r3 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r3)))
         (intervals (compute-live-intervals instructions))
         (r0-int (find :r0 intervals :key #'interval-vreg)))
    (expect (null r0-int) :to-be-falsy)
     (expect (= 0 (interval-start r0-int)) :to-be-truthy)
     (expect (= 4 (interval-end r0-int)) :to-be-truthy)))

;;; BURS Instruction Selection Tests

(it-sequential "burs-select-instructions-selects-minimum-cost-cover"
  (let ((cl-cc/emit:*burs-rules* nil))
    (cl-cc/emit:register-burs-rule '(add lhs rhs) '(slow-add lhs rhs) 10)
    (cl-cc/emit:register-burs-rule '(add lhs rhs) '(fast-add lhs rhs) 1)
    (multiple-value-bind (rules cost)
        (cl-cc/emit:burs-select-instructions '(add r1 r2))
      (expect (= 2001 cost) :to-be-truthy)
      (expect (mapcar #'cl-cc/emit::burs-rule-replacement rules) :to-equal '((identity r1) (identity r2) (fast-add lhs rhs))))))

(it-sequential "burs-select-instructions-prefers-earlier-rule-on-cost-tie"
  (let ((cl-cc/emit:*burs-rules* nil))
    (cl-cc/emit:register-burs-rule '(add lhs rhs) '(first-add lhs rhs) 1)
    (cl-cc/emit:register-burs-rule '(add lhs rhs) '(second-add lhs rhs) 1)
    (multiple-value-bind (rules cost)
        (cl-cc/emit:burs-select-instructions '(add r1 r2))
      (expect (= 2001 cost) :to-be-truthy)
      (expect (cl-cc/emit::burs-rule-replacement (third rules)) :to-equal '(first-add lhs rhs)))))

(it-sequential "burs-select-instructions-matches-nested-tile-patterns"
  (let ((cl-cc/emit:*burs-rules* nil))
    (cl-cc/emit:register-burs-rule '(add (load addr) reg) '(add reg (mem addr)) 2)
    (cl-cc/emit:register-burs-rule '(add reg1 reg2) '(add reg1 reg2) 1)
    (multiple-value-bind (rules cost)
        (cl-cc/emit:burs-select-instructions '(add (load local) r1))
      (expect (= 2002 cost) :to-be-truthy)
      (expect (mapcar #'cl-cc/emit::burs-rule-replacement rules) :to-equal '((identity local) (identity r1) (add reg (mem addr)))))))

(it-sequential "burs-select-instructions-signals-on-uncoverable-nonterminal"
  (let ((cl-cc/emit:*burs-rules* nil))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/emit:burs-select-instructions '(unknown r1))) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))))

;;; FR-068: Post-RA Instruction Scheduling

(defun %test-regalloc-result (pairs)
  (let ((assignment (make-hash-table :test #'eq)))
    (dolist (pair pairs)
      (setf (gethash (car pair) assignment) (cdr pair)))
    (cl-cc/regalloc::make-regalloc-result
     :assignment assignment
     :spill-map (make-hash-table :test #'eq)
     :spill-count 0
     :instructions nil)))

(it-sequential "post-ra-scheduler-reorders-independent-instructions"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-mul :dst :r2 :lhs :r3 :rhs :r4)
                      (make-vm-halt :reg :r0)))
         (ra (%test-regalloc-result '((:r0 . :rax) (:r2 . :rdx)
                                      (:r3 . :rbx) (:r4 . :rcx))))
         (scheduled (cl-cc/codegen:schedule-post-ra insts ra)))
    (expect (typep (first scheduled) 'vm-mul) :to-be-truthy)
    (expect (typep (second scheduled) 'vm-const) :to-be-truthy)
    (expect (typep (third scheduled) 'vm-halt) :to-be-truthy)))

(it-sequential "post-ra-scheduler-preserves-raw-dependencies"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-add :dst :r1 :lhs :r0 :rhs :r2)
                      (make-vm-halt :reg :r1)))
         (ra (%test-regalloc-result '((:r0 . :rax) (:r1 . :rcx) (:r2 . :rdx))))
         (scheduled (cl-cc/codegen:schedule-post-ra insts ra)))
    (expect (typep (first scheduled) 'vm-const) :to-be-truthy)
    (expect (typep (second scheduled) 'vm-add) :to-be-truthy)
    (expect (typep (third scheduled) 'vm-halt) :to-be-truthy)))

(it-sequential "post-ra-scheduler-preserves-physical-waw-dependencies"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-halt :reg :r1)))
         (ra (%test-regalloc-result '((:r0 . :rax) (:r1 . :rax))))
         (scheduled (cl-cc/codegen:schedule-post-ra insts ra)))
    (expect (vm-dst (first scheduled)) :to-be :r0)
    (expect (vm-dst (second scheduled)) :to-be :r1)
    (expect (typep (third scheduled) 'vm-halt) :to-be-truthy)))

(it-sequential "post-ra-scheduler-treats-unknown-registers-as-barriers"
  (let* ((unknown (make-vm-const :dst :unknown :value 7))
         (insts (list (make-vm-const :dst :r0 :value 1)
                      unknown
                      (make-vm-mul :dst :r2 :lhs :r3 :rhs :r4)
                      (make-vm-halt :reg :r0)))
         (ra (%test-regalloc-result '((:r0 . :rax) (:r2 . :rdx)
                                      (:r3 . :rbx) (:r4 . :rcx))))
         (scheduled (cl-cc/codegen:schedule-post-ra insts ra)))
    (expect (typep (first scheduled) 'vm-const) :to-be-truthy)
    (expect (second scheduled) :to-be unknown)
    (expect (typep (third scheduled) 'vm-mul) :to-be-truthy)
    (expect (typep (fourth scheduled) 'vm-halt) :to-be-truthy)))

(it-sequential "post-ra-scheduler-integrates-with-native-backends"
  (let ((program (cl-cc/vm::make-vm-program
                  :instructions (list (make-vm-const :dst :r0 :value 1)
                                      (make-vm-const :dst :r1 :value 2)
                                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                                      (make-vm-halt :reg :r2))
                  :result-register :r2
                  :leaf-p t)))
    (dolist (bytes (list (cl-cc/codegen:compile-to-x86-64-bytes program)
                         (cl-cc/codegen:compile-to-aarch64-bytes program)
                         (cl-cc/codegen:compile-to-riscv64-bytes program)))
      (expect (typep bytes '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
      (expect (plusp (length bytes)) :to-be-truthy))))

(it-sequential "regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain"
  (let* ((insts (list
                 ;; leaf callee
                 (make-vm-label :name "leaf")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret :reg :r0)
                 ;; non-leaf caller that calls leaf via func-ref
                 (make-vm-label :name "caller")
                 (make-vm-func-ref :dst :f :label "leaf")
                 (make-vm-call :dst :r1 :func :f :args nil)
                 (make-vm-ret :reg :r1)
                 ;; root calls caller (callee not leaf)
                 (make-vm-label :name "root")
                 (make-vm-func-ref :dst :g :label "caller")
                 (make-vm-call :dst :r2 :func :g :args nil)
                 (make-vm-ret :reg :r2)))
         (hints (cl-cc/regalloc::regalloc-compute-interprocedural-hints insts)))
    (expect (getf (gethash "leaf" hints) :leaf-p) :to-be-truthy)
    (expect (getf (gethash "caller" hints) :leaf-callee-chain-p) :to-be-truthy)
    (expect (getf (gethash "root" hints) :leaf-callee-chain-p) :to-be-falsy)))

(it-sequential "regalloc-interprocedural-policy-hook-derives-preferences"
  (let* ((insts (list
                 (make-vm-label :name "leaf")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret :reg :r0)
                 (make-vm-label :name "caller")
                 (make-vm-func-ref :dst :f :label "leaf")
                 (make-vm-call :dst :r1 :func :f :args nil)
                 (make-vm-ret :reg :r1)
                 (make-vm-label :name "root")
                 (make-vm-func-ref :dst :g :label "caller")
                 (make-vm-call :dst :r2 :func :g :args nil)
                 (make-vm-ret :reg :r2)))
         (hints (cl-cc/regalloc::regalloc-compute-interprocedural-hints insts))
         (leaf-policy (cl-cc/regalloc::regalloc-build-allocation-policy-from-hints hints "leaf"))
         (root-policy (cl-cc/regalloc::regalloc-build-allocation-policy-from-hints hints "root")))
    (expect (getf leaf-policy :prefer-caller-saved-p) :to-be-truthy)
    (expect (getf leaf-policy :prefer-callee-saved-p) :to-be-falsy)
    (expect (getf root-policy :prefer-callee-saved-p) :to-be-truthy)
    (expect (getf root-policy :prefer-caller-saved-p) :to-be-falsy)))

;;; Linear Scan Allocation Tests

(it-sequential "regalloc-allocate-fits-in-physical-regs-with-distinct-assignments"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (= 0 (regalloc-spill-count result)) :to-be-truthy)
    (expect (null (regalloc-lookup result :r0)) :to-be-falsy)
    (expect (null (regalloc-lookup result :r1)) :to-be-falsy)
    (expect (null (regalloc-lookup result :r2)) :to-be-falsy)
    (expect (eq (regalloc-lookup result :r0)
                      (regalloc-lookup result :r1)) :to-be-falsy)))

(it-sequential "regalloc-allocate-coalesces-move-to-same-physical-reg"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-halt :reg :r1)))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (regalloc-lookup result :r1) :to-be (regalloc-lookup result :r0))))

(it-sequential "regalloc-allocate-zero-spills-with-register-reuse"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-const :dst :r2 :value 2)
                             (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (= 0 (regalloc-spill-count result)) :to-be-truthy)))

(it-sequential "regalloc-allocate-empty-sequence-has-zero-spills"
  (let ((result (allocate-registers nil *x86-64-target*)))
    (expect (= 0 (regalloc-spill-count result)) :to-be-truthy)))

(it-sequential "regalloc-x86-and-aarch64-convention-properties"
  (expect (null *x86-64-target*) :to-be-falsy)
  (expect (null *aarch64-target*) :to-be-falsy)
  (expect (= 13 (length (target-allocatable-regs *x86-64-target*))) :to-be-truthy)
  (expect (target-ret-reg *x86-64-target*) :to-be :rax)
  (expect (first (target-scratch-regs *x86-64-target*)) :to-be :r11)
  (expect (target-fp-arg-regs *x86-64-target*) :to-equal '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7))
  (expect (target-fp-ret-reg *x86-64-target*) :to-be :xmm0)
  (expect (target-ret-reg *aarch64-target*) :to-be :x0)
  (expect (target-fp-arg-regs *aarch64-target*) :to-equal '(:v0 :v1 :v2 :v3 :v4 :v5 :v6 :v7))
  (expect (target-fp-ret-reg *aarch64-target*) :to-be :v0))

(it-sequential "regalloc-float-vregs-allocated-to-distinct-xmm-registers"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1.0d0)
                             (make-vm-const :dst :r1 :value 2.0d0)
                             (make-vm-float-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (float-vregs (let ((ht (make-hash-table :test #'eq)))
                        (setf (gethash :r0 ht) t
                              (gethash :r1 ht) t
                              (gethash :r2 ht) t)
                        ht))
         (result (allocate-registers instructions *x86-64-target* float-vregs)))
    (flet ((xmm-p (k)
             (and (keywordp k)
                  (let ((s (symbol-name k)))
                    (and (>= (length s) 3)
                         (string= "XMM" (subseq s 0 3)))))))
      (let ((p0 (regalloc-lookup result :r0))
            (p1 (regalloc-lookup result :r1))
            (p2 (regalloc-lookup result :r2)))
        (expect (xmm-p p0) :to-be-truthy)
        (expect (xmm-p p1) :to-be-truthy)
        (expect (xmm-p p2) :to-be-truthy)
        (expect (eq p0 p1) :to-be-falsy)))))

(it-sequential "regalloc-abi-return-value-prefers-rax"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 42)
                             (make-vm-ret :reg :r0)))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (regalloc-lookup result :r0) :to-be :rax)))

(it-sequential "regalloc-abi-live-in-params-use-arg-registers"
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (regalloc-lookup result :r0) :to-be :rdi)
    (expect (regalloc-lookup result :r1) :to-be :rsi)))

(it-sequential "regalloc-abi-dead-arg-register-is-recycled"
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-const :dst :r3 :value 7)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (regalloc-lookup result :r1) :to-be :rsi)
    (expect (regalloc-lookup result :r3) :to-be :rsi)))

(it-sequential "regalloc-spill-live-across-call-prefers-callee-saved"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-call :dst :r2 :func :r3 :args '(:r4))
                             (make-vm-add :dst :r5 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r5)))
         (intervals (compute-live-intervals instructions))
         (r0-int (find :r0 intervals :key #'cl-cc:interval-vreg))
         (result (allocate-registers instructions *x86-64-target*)))
    (expect (cl-cc/regalloc::interval-crosses-call-p r0-int) :to-be-truthy)
    (expect (member (regalloc-lookup result :r0)
                         (cl-cc/target:target-callee-saved *x86-64-target*)
                         :test #'eq) :to-be-truthy)))

(it-sequential "regalloc-interprocedural-policy-caller-saved-respects-call-crossing-safety"
  (let* ((interval (make-live-interval :vreg :r0 :start 0 :end 10 :crosses-call-p t))
         (free-regs (copy-list (cl-cc/target:target-caller-saved *x86-64-target*)))
         (cl-cc/regalloc::*current-allocation-policy*
           (list :prefer-callee-saved-p nil :prefer-caller-saved-p t)))
    (expect (cl-cc/regalloc::%hint-policy-preferred-reg
                  interval
                  *x86-64-target*
                  free-regs) :to-be-null)))

(it-sequential "regalloc-interprocedural-policy-end-to-end-keeps-call-crossing-safe"
  (let* ((instructions (list (make-vm-label :name 'main)
                             (make-vm-const :dst :r0 :value 1)
                             (make-vm-call :dst :r2 :func :leaf :args '(:r4))
                             (make-vm-add :dst :r5 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r5)
                             (make-vm-label :name 'leaf)
                             (make-vm-const :dst :r6 :value 7)
                             (make-vm-halt :reg :r6)))
         (hints (cl-cc/regalloc::regalloc-compute-interprocedural-hints instructions))
         (policy (cl-cc/regalloc::regalloc-build-allocation-policy-from-hints hints 'main))
         (result (allocate-registers instructions *x86-64-target* nil policy)))
    (expect (member (regalloc-lookup result :r0)
                         (cl-cc/target:target-callee-saved *x86-64-target*)
                         :test #'eq) :to-be-truthy)))

(it-sequential "regalloc-interprocedural-policy-prefers-callee-saved-on-call-crossing"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-call :dst :r2 :func :r3 :args '(:r4))
                             (make-vm-add :dst :r5 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r5)))
         (policy (list :prefer-callee-saved-p t
                       :prefer-caller-saved-p nil))
         (result (allocate-registers instructions *x86-64-target* nil policy)))
    (expect (member (regalloc-lookup result :r0)
                         (cl-cc/target:target-callee-saved *x86-64-target*)
                         :test #'eq) :to-be-truthy)))

(it-sequential "regalloc-spill-pressure-exceeds-pool-causes-spills"
  (let* ((cc (cl-cc/target:make-target-desc
              :name :x86-64
              :gpr-names #(:rdi :rsi :r11)
              :arg-regs '(:rdi :rsi)
              :ret-reg :rax
              :callee-saved nil
              :scratch-regs '(:r11)))
         (instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-const :dst :r2 :value 3)
                             (make-vm-move :dst :r3 :src :r0)
                             (make-vm-move :dst :r4 :src :r2)
                             (make-vm-const :dst :r5 :value 5)
                             (make-vm-const :dst :r6 :value 6)
                             (make-vm-move :dst :r7 :src :r1)
                             (make-vm-halt :reg :r0)))
         (result (allocate-registers instructions cc)))
    (expect (null (regalloc-lookup result :r0)) :to-be-falsy)
    (expect (null (gethash :r1 (cl-cc:regalloc-spill-map result))) :to-be-falsy)
    (expect (= 2 (cl-cc:regalloc-spill-count result)) :to-be-truthy)))

(it-sequential "regalloc-spill-rewrite-two-spilled-srcs-use-distinct-scratch-regs"
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target*))
           (loads (remove-if-not #'cl-cc/regalloc::vm-spill-load-p out))
           (rewritten (find-if #'cl-cc:vm-add-p out)))
      (expect (length loads) :to-equal 2)
      (expect (eq (cl-cc/regalloc::vm-spill-dst (first loads)) (cl-cc/regalloc::vm-spill-dst (second loads))) :to-be-falsy)
      (expect (eq (vm-lhs rewritten) (vm-rhs rewritten)) :to-be-falsy))))

(it-sequential "regalloc-spill-rewrite-mul-high-avoids-internal-r11-scratch"
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (cl-cc:make-vm-integer-mul-high-u :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (setf (gethash :r3 spill-map) 3)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target*))
           (rewritten (find-if (lambda (item)
                                 (typep item 'cl-cc/vm::vm-integer-mul-high-u))
                               out)))
      (expect (null rewritten) :to-be-falsy)
      (expect (eq (vm-lhs rewritten) :r11) :to-be-falsy)
      (expect (eq (vm-rhs rewritten) :r11) :to-be-falsy)
      (expect (eq (vm-dst rewritten) :r11) :to-be-falsy)
      (expect (eq (vm-lhs rewritten) (vm-rhs rewritten)) :to-be-falsy))))

(it-sequential "regalloc-spill-rewrite-spilled-src-and-dst-use-separate-scratch"
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-move :dst :r2 :src :r1)))
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target*))
           (load (find-if #'cl-cc/regalloc::vm-spill-load-p out))
           (store (find-if #'cl-cc/regalloc::vm-spill-store-p out))
           (rewritten (find-if #'cl-cc:vm-move-p out)))
      (expect (null load) :to-be-falsy)
      (expect (null store) :to-be-falsy)
      (expect (eq (cl-cc/regalloc::vm-spill-dst load) (cl-cc/regalloc::vm-spill-src store)) :to-be-falsy)
      (expect (cl-cc/regalloc::vm-spill-dst load) :to-be (vm-src rewritten))
      (expect (cl-cc/regalloc::vm-spill-src store) :to-be (vm-dst rewritten)))))

(it-sequential "regalloc-integration-rematerializes-spilled-constant-as-vm-const"
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (remat-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (setf (gethash :r1 remat-map) 42)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target* remat-map))
           (const-inst (find-if #'cl-cc:vm-const-p out))
           (load-inst (find-if #'cl-cc/regalloc::vm-spill-load-p out)))
      (expect (null const-inst) :to-be-falsy)
      (expect (cl-cc:vm-value const-inst) :to-equal 42)
      (expect (null load-inst) :to-be-falsy)
      (expect (eq (cl-cc:vm-dst const-inst) (cl-cc/regalloc::vm-spill-dst load-inst)) :to-be-falsy))))

(it-sequential "regalloc-integration-compile-and-allocate-produces-zero-spills"
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
         (program (compilation-result-program result))
         (instructions (vm-program-instructions program))
         (alloc (allocate-registers instructions *x86-64-target*)))
    (expect (= 0 (regalloc-spill-count alloc)) :to-be-truthy)
    (let ((all-vregs nil))
      (dolist (inst instructions)
        (dolist (v (instruction-defs inst)) (when v (pushnew v all-vregs)))
        (dolist (v (instruction-uses inst)) (when v (pushnew v all-vregs))))
      (dolist (vreg all-vregs)
        (expect (null (regalloc-lookup alloc vreg)) :to-be-falsy)))))

;;; ─── lsa-state struct + helpers ────────────────────────────────────────────────

(it-sequential "lsa-state-initial-values-are-empty"
  (let ((s (cl-cc/regalloc::make-lsa-state :free-regs '(:rax :rbx) :free-fp-regs '(:xmm0))))
    (expect (= 0 (hash-table-count (cl-cc/regalloc::lsa-assignment s))) :to-be-truthy)
    (expect (= 0 (hash-table-count (cl-cc/regalloc::lsa-spill-map s))) :to-be-truthy)
    (expect (= 0 (cl-cc/regalloc::lsa-spill-count s)) :to-be-truthy)
    (expect (cl-cc/regalloc::lsa-active s) :to-be-null)
    (expect (cl-cc/regalloc::lsa-free-regs s) :to-equal '(:rax :rbx))
    (expect (cl-cc/regalloc::lsa-free-fp-regs s) :to-equal '(:xmm0))))

(it-sequential "lsa-state-pool-selection-and-mutation"
  (let ((s     (cl-cc/regalloc::make-lsa-state :free-regs '(:rax) :free-fp-regs '(:xmm0)))
        (i-gpr (cl-cc/regalloc::make-live-interval :vreg :r0))
        (i-fp  (cl-cc/regalloc::make-live-interval :vreg :r1 :fp-p t)))
    (expect (cl-cc/regalloc::%lsa-interval-pool s i-gpr) :to-equal '(:rax))
    (expect (cl-cc/regalloc::%lsa-interval-pool s i-fp) :to-equal '(:xmm0))
    (cl-cc/regalloc::%lsa-set-interval-pool s i-gpr '(:rcx))
    (expect (cl-cc/regalloc::lsa-free-regs s) :to-equal '(:rcx))
    (expect (cl-cc/regalloc::lsa-free-fp-regs s) :to-equal '(:xmm0))
    (cl-cc/regalloc::%lsa-set-interval-pool s i-fp '(:xmm1))
    (expect (cl-cc/regalloc::lsa-free-fp-regs s) :to-equal '(:xmm1))))

(it-sequential "lsa-state-spill-current-increments-count-and-records-slot"
  (let ((s   (cl-cc/regalloc::make-lsa-state))
        (int (cl-cc/regalloc::make-live-interval :vreg :r5)))
    (cl-cc/regalloc::%lsa-spill-current s int)
    (expect (= 1 (cl-cc/regalloc::lsa-spill-count s)) :to-be-truthy)
    (expect (= 1 (gethash :r5 (cl-cc/regalloc::lsa-spill-map s))) :to-be-truthy)))

(it-sequential "lsa-state-expire-old-removes-finished-intervals"
  (let ((s    (cl-cc/regalloc::make-lsa-state :free-regs '()))
        (done (cl-cc/regalloc::make-live-interval :vreg :r0 :start 0 :end 2 :phys-reg :rax))
        (live (cl-cc/regalloc::make-live-interval :vreg :r1 :start 0 :end 5 :phys-reg :rbx))
        (cur  (cl-cc/regalloc::make-live-interval :vreg :r2 :start 3 :end 8)))
    (setf (cl-cc/regalloc::lsa-active s) (list done live))
    (cl-cc/regalloc::%lsa-expire-old s cur)
    (expect (member done (cl-cc/regalloc::lsa-active s)) :to-be-falsy)
    (expect (member live (cl-cc/regalloc::lsa-active s)) :to-be-truthy)
    (expect (cl-cc/regalloc::lsa-free-regs s) :to-equal '(:rax))))

(it-sequential "lsa-state-best-spill-candidate-returns-live-interval"
  (let* ((cur   (cl-cc/regalloc::make-live-interval :vreg :r2 :start 3 :end 10))
         (near  (cl-cc/regalloc::make-live-interval :vreg :r0 :start 0 :end 8
                                                :use-positions '(4) :phys-reg :rax))
         (far   (cl-cc/regalloc::make-live-interval :vreg :r1 :start 0 :end 12
                                                :use-positions '(9) :phys-reg :rbx))
         (s     (cl-cc/regalloc::make-lsa-state :active (list near far))))
    (expect (typep (cl-cc/regalloc::%lsa-best-spill-candidate s cur) 'cl-cc/regalloc::live-interval) :to-be-truthy)))

(it-sequential "lsa-state-best-spill-candidate-returns-current-when-active-empty"
  (let* ((cur (cl-cc/regalloc::make-live-interval :vreg :r0 :start 5 :end 10))
         (s   (cl-cc/regalloc::make-lsa-state :active nil)))
    (expect (cl-cc/regalloc::%lsa-best-spill-candidate s cur) :to-be cur)))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Graph-coloring allocation (regalloc-color.lisp, FR-061 / FR-199)
;;;
;;; The :color allocation strategy (regalloc-allocate.lisp dispatch) is not
;;; exercised by the linear-scan-focused tests above, so the Chaitin-Briggs
;;; coloring path and stack-slot sharing were previously unrun.  These tests
;;; drive it directly through its internal and public entry points.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %rc-interval (vreg start end &key use-positions crosses-call-p return-value-p fp-p)
  "Construct a live-interval for graph-coloring tests."
  (cl-cc/regalloc::make-live-interval
   :vreg vreg :start start :end end
   :use-positions (or use-positions (list start end))
   :crosses-call-p crosses-call-p
   :return-value-p return-value-p
   :fp-p fp-p))

;;; ─── %intervals-overlap-p ──────────────────────────────────────────────

(it-sequential "color-intervals-overlap-p exact"
  (destructuring-bind (sa ea sb eb expected) (list 0 10 0 10 t)
    (assert-bool expected
               (cl-cc/regalloc::%intervals-overlap-p (%rc-interval :a sa ea)
                                                     (%rc-interval :b sb eb)))))

(it-sequential "color-intervals-overlap-p partial"
  (destructuring-bind (sa ea sb eb expected) (list 0 8 5 12 t)
    (assert-bool expected
               (cl-cc/regalloc::%intervals-overlap-p (%rc-interval :a sa ea)
                                                     (%rc-interval :b sb eb)))))

(it-sequential "color-intervals-overlap-p contained"
  (destructuring-bind (sa ea sb eb expected) (list 0 20 5 10 t)
    (assert-bool expected
               (cl-cc/regalloc::%intervals-overlap-p (%rc-interval :a sa ea)
                                                     (%rc-interval :b sb eb)))))

(it-sequential "color-intervals-overlap-p touching-end"
  (destructuring-bind (sa ea sb eb expected) (list 0 10 10 20 t)
    (assert-bool expected
               (cl-cc/regalloc::%intervals-overlap-p (%rc-interval :a sa ea)
                                                     (%rc-interval :b sb eb)))))

(it-sequential "color-intervals-overlap-p disjoint"
  (destructuring-bind (sa ea sb eb expected) (list 0 5 6 10 nil)
    (assert-bool expected
               (cl-cc/regalloc::%intervals-overlap-p (%rc-interval :a sa ea)
                                                     (%rc-interval :b sb eb)))))

;;; ─── %color-build-interference-graph ───────────────────────────────────

(it-sequential "color-build-graph-overlap-adds-mutual-edges"
  (let ((g (cl-cc/regalloc::%color-build-interference-graph
            (list (%rc-interval :a 0 10) (%rc-interval :b 5 15)))))
    (expect (member :b (gethash :a g)) :to-be-truthy)
    (expect (member :a (gethash :b g)) :to-be-truthy)))

(it-sequential "color-build-graph-disjoint-has-no-edges"
  (let ((g (cl-cc/regalloc::%color-build-interference-graph
            (list (%rc-interval :a 0 5) (%rc-interval :b 6 10)))))
    (expect (gethash :a g) :to-be-null)
    (expect (gethash :b g) :to-be-null)))

(it-sequential "color-build-graph-skips-nil-vreg"
  (let ((g (cl-cc/regalloc::%color-build-interference-graph
            (list (%rc-interval nil 0 10) (%rc-interval :b 0 10)))))
    (expect (= 1 (hash-table-count g)) :to-be-truthy)
    (expect (gethash nil g) :to-be-null)))

;;; ─── %color-simplify ───────────────────────────────────────────────────

(it-sequential "color-simplify-no-spill-when-k-sufficient"
  (let* ((a (%rc-interval :a 0 10))
         (b (%rc-interval :b 5 15))
         (g (cl-cc/regalloc::%color-build-interference-graph (list a b)))
         (imap (cl-cc/regalloc::%interval-map (list a b)))
         (stack (cl-cc/regalloc::%color-simplify g imap 2)))
    (expect (= 2 (length stack)) :to-be-truthy)
    (expect (every (lambda (e) (not (cdr e))) stack) :to-be-truthy)))

(it-sequential "color-simplify-marks-spill-under-pressure"
  (let* ((a (%rc-interval :a 0 20 :use-positions '(0 5 10 15)))
         (b (%rc-interval :b 0 20 :use-positions '(1 6 11 16)))
         (c (%rc-interval :c 0 20 :use-positions '(2)))
         (g (cl-cc/regalloc::%color-build-interference-graph (list a b c)))
         (imap (cl-cc/regalloc::%interval-map (list a b c)))
         (stack (cl-cc/regalloc::%color-simplify g imap 2)))
    (expect (= 3 (length stack)) :to-be-truthy)
    (expect (some #'cdr stack) :to-be-truthy)))

;;; ─── %color-spill-priority / %color-spill-candidate ────────────────────

(it-sequential "color-spill-priority-ml-disabled uses-over-length"
  (destructuring-bind (start end uses call ret expected) (list 0 10 '(1 2 3 4 5) nil nil 1/2)
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
    (expect (= expected (cl-cc/regalloc::%color-spill-priority
               (%rc-interval :v start end :use-positions uses
                             :crosses-call-p call :return-value-p ret))) :to-be-truthy))))

(it-sequential "color-spill-priority-ml-disabled call-cross-bonus"
  (destructuring-bind (start end uses call ret expected) (list 0 10 '(1) t nil 11/10)
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
    (expect (= expected (cl-cc/regalloc::%color-spill-priority
               (%rc-interval :v start end :use-positions uses
                             :crosses-call-p call :return-value-p ret))) :to-be-truthy))))

(it-sequential "color-spill-priority-ml-disabled return-bonus"
  (destructuring-bind (start end uses call ret expected) (list 0 10 '(1) nil t 11/10)
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
    (expect (= expected (cl-cc/regalloc::%color-spill-priority
               (%rc-interval :v start end :use-positions uses
                             :crosses-call-p call :return-value-p ret))) :to-be-truthy))))

(it-sequential "color-spill-priority-ml-disabled length-clamp"
  (destructuring-bind (start end uses call ret expected) (list 5 5 '(5) nil nil 1)
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
    (expect (= expected (cl-cc/regalloc::%color-spill-priority
               (%rc-interval :v start end :use-positions uses
                             :crosses-call-p call :return-value-p ret))) :to-be-truthy))))

(it-sequential "color-spill-candidate-picks-lowest-priority"
  (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
         (low  (%rc-interval :low  0 20 :use-positions '(1)))
         (high (%rc-interval :high 0 10 :use-positions '(0 1 2 3 4 5 6 7 8 9 10)))
         (g (cl-cc/regalloc::%color-build-interference-graph (list low high)))
         (imap (cl-cc/regalloc::%interval-map (list low high))))
    (expect (cl-cc/regalloc::%color-spill-candidate g imap) :to-be :low)))

;;; ─── %color-select-register ────────────────────────────────────────────

(it-sequential "color-select-register-avoids-neighbor-colors"
  (let* ((a (%rc-interval :a 0 10))
         (b (%rc-interval :b 0 10))
         (g (cl-cc/regalloc::%color-build-interference-graph (list a b)))
         (assign (make-hash-table :test #'eq)))
    (setf (gethash :b assign) :r0)
    (expect (cl-cc/regalloc::%color-select-register :a g assign '(:r0 :r1)) :to-be :r1)))

(it-sequential "color-select-register-nil-when-all-conflict"
  (let* ((a (%rc-interval :a 0 10))
         (b (%rc-interval :b 0 10))
         (g (cl-cc/regalloc::%color-build-interference-graph (list a b)))
         (assign (make-hash-table :test #'eq)))
    (setf (gethash :b assign) :r0)
    (expect (cl-cc/regalloc::%color-select-register :a g assign '(:r0)) :to-be-null)))

;;; ─── color-allocate (public Chaitin-Briggs driver) ─────────────────────

(it-sequential "color-allocate-assigns-when-registers-available"
  (multiple-value-bind (assignment spill-map spill-count)
      (cl-cc/regalloc::color-allocate (list (%rc-interval :a 0 10)
                                            (%rc-interval :b 12 20))
                                      '(:r0 :r1))
    (expect (gethash :a assignment) :to-be-truthy)
    (expect (gethash :b assignment) :to-be-truthy)
    (expect (= 0 (hash-table-count spill-map)) :to-be-truthy)
    (expect (= 0 spill-count) :to-be-truthy)))

(it-sequential "color-allocate-spills-when-k-exceeded"
  (multiple-value-bind (assignment spill-map spill-count)
      (cl-cc/regalloc::color-allocate (list (%rc-interval :a 0 20)
                                            (%rc-interval :b 0 20)
                                            (%rc-interval :c 0 20))
                                      '(:r0 :r1))
    (declare (ignore assignment))
    (expect (= 1 (hash-table-count spill-map)) :to-be-truthy)
    (expect (>= spill-count 1) :to-be-truthy)))

(it-sequential "color-allocate-empty-intervals"
  (multiple-value-bind (assignment spill-map spill-count)
      (cl-cc/regalloc::color-allocate '() '(:r0 :r1))
    (expect (= 0 (hash-table-count assignment)) :to-be-truthy)
    (expect (= 0 (hash-table-count spill-map)) :to-be-truthy)
    (expect (= 0 spill-count) :to-be-truthy)))

(it-sequential "color-allocate-for-target-separates-gpr-and-fp"
  (let* ((gpr (%rc-interval :gpr 0 10 :fp-p nil))
         (fp  (%rc-interval :fpr 0 10 :fp-p t))
         (cc  (cl-cc/target::make-target-desc
               :name :x86-64
               :gpr-names #(:rdi :rsi :rdx :rcx :r8 :r9 :rbx :r12)
               :arg-regs '(:rdi :rsi :rdx :rcx :r8 :r9)
               :ret-reg :rax
               :fp-arg-regs '(:xmm0 :xmm1 :xmm2 :xmm3)
               :fp-ret-reg :xmm0
               :callee-saved '(:rbx :r12)
               :scratch-regs nil)))
    (multiple-value-bind (assignment spill-map spill-count)
        (cl-cc/regalloc::color-allocate-for-target (list gpr fp) cc)
      (expect (gethash :gpr assignment) :to-be-truthy)
      (expect (gethash :fpr assignment) :to-be-truthy)
      (expect (= 0 (hash-table-count spill-map)) :to-be-truthy)
      (expect (= 0 spill-count) :to-be-truthy))))

;;; ─── stack-slot sharing (color-spill-slots / regalloc-color-spill-slots) ─

(it-sequential "color-spill-slots-non-overlapping-share-slot"
  (let ((m (cl-cc/regalloc::color-spill-slots (list (%rc-interval :a 0 5)
                                                    (%rc-interval :b 6 10))
                                              0)))
    (expect (= (gethash :a m) (gethash :b m)) :to-be-truthy)))

(it-sequential "color-spill-slots-overlapping-distinct-slots"
  (let ((m (cl-cc/regalloc::color-spill-slots (list (%rc-interval :a 0 10)
                                                    (%rc-interval :b 5 15))
                                              0)))
    (expect (/= (gethash :a m) (gethash :b m)) :to-be-truthy)))

(it-sequential "color-spill-slots-respects-offset"
  (let ((m (cl-cc/regalloc::color-spill-slots (list (%rc-interval :a 0 5)) 3)))
    (expect (>= (gethash :a m) 4) :to-be-truthy)))

(it-sequential "regalloc-color-spill-slots-passthrough-when-none-spilled"
  (let* ((orig (make-hash-table :test #'eq))
         (result (cl-cc/regalloc::regalloc-color-spill-slots
                  (list (%rc-interval :a 0 10)) orig 0)))
    (expect result :to-be orig)))

(it-sequential "regalloc-color-spill-slots-applies-coloring"
  (let* ((spill-map (let ((ht (make-hash-table :test #'eq)))
                      (setf (gethash :a ht) 1 (gethash :b ht) 2)
                      ht))
         (result (cl-cc/regalloc::regalloc-color-spill-slots
                  (list (%rc-interval :a 0 5) (%rc-interval :b 6 10))
                  spill-map 0)))
    (expect (= (gethash :a result) (gethash :b result)) :to-be-truthy)))
