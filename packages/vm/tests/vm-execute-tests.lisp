;;;; tests/unit/vm/vm-execute-tests.lisp — Core VM Execute-Instruction Tests
;;;;
;;;; Tests for src/vm/vm-execute.lisp:
;;;; vm-falsep, vm-classify-arg, vm-save/restore-registers,
;;;; vm-list-to-lisp-list, and execute-instruction for const/move/halt/jump-zero.

(in-package :cl-cc/test)


(it-sequential "vm-execute-safety-level-bounds-checking"
  (let ((cl-cc/vm::*safety-level* 1))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/vm::vm-check-index "abc" 3 'char)) (type-error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))
  (let ((cl-cc/vm::*safety-level* 0))
    (expect (cl-cc/vm::vm-check-index "abc" 99 'char) :to-equal 99)))

(it-sequential "vm-execute-stack-canary-verification"
  (let ((cl-cc/vm::*security-canaries* t)
        (snapshot (make-hash-table :test #'eq)))
    (setf (gethash :__stack-canary__ snapshot) 1
          (gethash :__stack-canary-check__ snapshot) 2)
    (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/vm::vm-verify-stack-canary snapshot)) (cl-cc/vm::memory-fault () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))))

(it-sequential "vm-execute-cow-array-make-and-adjust"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 3)
    (cl-cc:vm-reg-set s :R2 9)
    (exec1 (cl-cc:make-vm-make-array :dst :R0 :size-reg :R1 :initial-element :R2
                                      :adjustable t :copy-on-write t) s)
    (expect (cl-cc/vm::vm-cow-vector-p (cl-cc:vm-reg-get s :R0)) :to-be-truthy)
    (cl-cc:vm-reg-set s :R3 5)
    (exec1 (cl-cc:make-vm-adjust-array :dst :R4 :arr :R0 :dims :R3 :initial-element nil) s)
    (expect (array-total-size (cl-cc:vm-reg-get s :R4)) :to-equal 5)))

;;; ─── vm-falsep ──────────────────────────────────────────────────────────────

(it-sequential "vm-execute-vm-falsep-semantics"
  (expect (cl-cc/vm::vm-falsep nil) :to-be-truthy)
  (expect (cl-cc/vm::vm-falsep 0) :to-be-truthy)
  (dolist (val '(1 -1 "hello" foo t (1 2 3)))
    (expect (cl-cc/vm::vm-falsep val) :to-be-falsy)))

;;; ─── vm-classify-arg ────────────────────────────────────────────────────────

(it-sequential "vm-execute-vm-classify-arg integer"
  (destructuring-bind (arg expected) (list 42 'integer)
    (let ((s (make-test-vm)))
    (expect (cl-cc/vm::vm-classify-arg arg s) :to-be expected))))

(it-sequential "vm-execute-vm-classify-arg string"
  (destructuring-bind (arg expected) (list "hello" 'string)
    (let ((s (make-test-vm)))
    (expect (cl-cc/vm::vm-classify-arg arg s) :to-be expected))))

(it-sequential "vm-execute-vm-classify-arg symbol"
  (destructuring-bind (arg expected) (list 'foo 'symbol)
    (let ((s (make-test-vm)))
    (expect (cl-cc/vm::vm-classify-arg arg s) :to-be expected))))

(it-sequential "vm-execute-vm-classify-arg list"
  (destructuring-bind (arg expected) (list '(a b c) 'cons)
    (let ((s (make-test-vm)))
    (expect (cl-cc/vm::vm-classify-arg arg s) :to-be expected))))

(it-sequential "vm-execute-vm-classify-arg float"
  (destructuring-bind (arg expected) (list 3.14 'float)
    (let ((s (make-test-vm)))
    (expect (cl-cc/vm::vm-classify-arg arg s) :to-be expected))))

(it-sequential "vm-execute-vm-classify-arg hash-table"
  (destructuring-bind (arg expected) (list (make-hash-table) 't)
    (let ((s (make-test-vm)))
    (expect (cl-cc/vm::vm-classify-arg arg s) :to-be expected))))

;;; ─── vm-save-registers / vm-restore-registers ───────────────────────────────

(it-sequential "vm-execute-register-save-restore"
  (let ((s (make-test-vm)))
    ;; Full save: snapshot is immutable
    (cl-cc:vm-reg-set s :R0 99)
    (let ((snap (cl-cc/vm::vm-save-registers s)))
      (cl-cc:vm-reg-set s :R0 200)
      (expect (= 99 (gethash :R0 snap)) :to-be-truthy))
    ;; Full restore: overwrites
    (cl-cc:vm-reg-set s :R0 42)
    (let ((snap (cl-cc/vm::vm-save-registers s)))
      (cl-cc:vm-reg-set s :R0 999)
      (cl-cc/vm::vm-restore-registers s snap)
      (expect (= 42 (cl-cc:vm-reg-get s :R0)) :to-be-truthy))
    ;; Subset save: only saves :R1
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc:vm-reg-set s :R1 99)
    (let ((snap (cl-cc/vm::vm-save-registers-subset s '(:R1))))
      (expect (gethash :R0 snap) :to-be-falsy)
      (expect (= 99 (gethash :R1 snap)) :to-be-truthy))
    ;; Subset restore: only restores :R1, leaves :R0 as-is
    (cl-cc:vm-reg-set s :R0 1)
    (cl-cc:vm-reg-set s :R1 2)
    (let ((snap (cl-cc/vm::vm-save-registers-subset s '(:R1))))
      (cl-cc:vm-reg-set s :R0 10)
      (cl-cc:vm-reg-set s :R1 20)
      (cl-cc/vm::vm-restore-registers-subset s snap)
      (expect (= 10 (cl-cc:vm-reg-get s :R0)) :to-be-truthy)
      (expect (= 2 (cl-cc:vm-reg-get s :R1)) :to-be-truthy))))

;;; ─── vm-list-to-lisp-list ───────────────────────────────────────────────────

(it-sequential "vm-execute-vm-list-to-lisp-list nil"
  (destructuring-bind (input expected setup) (list nil nil nil)
    (let ((s (make-test-vm)))
    (when setup
      (funcall setup s))
    (expect (cl-cc/vm::vm-list-to-lisp-list s input) :to-equal expected))))

(it-sequential "vm-execute-vm-list-to-lisp-list proper-list"
  (destructuring-bind (input expected setup) (list '(1 2 3) '(1 2 3) nil)
    (let ((s (make-test-vm)))
    (when setup
      (funcall setup s))
    (expect (cl-cc/vm::vm-list-to-lisp-list s input) :to-equal expected))))

(it-sequential "vm-execute-vm-list-to-lisp-list atom-wraps"
  (destructuring-bind (input expected setup) (list 42 '(42) nil)
    (let ((s (make-test-vm)))
    (when setup
      (funcall setup s))
    (expect (cl-cc/vm::vm-list-to-lisp-list s input) :to-equal expected))))

(it-sequential "vm-execute-vm-list-to-lisp-list string-wraps"
  (destructuring-bind (input expected setup) (list "hi" '("hi") nil)
    (let ((s (make-test-vm)))
    (when setup
      (funcall setup s))
    (expect (cl-cc/vm::vm-list-to-lisp-list s input) :to-equal expected))))

(it-sequential "vm-execute-vm-list-to-lisp-list plain-integer-ignores-heap-slots"
  (destructuring-bind (input expected setup) (list 12 '(12) (lambda (state)
             (cl-cc/vm::vm-heap-set
              state 12
              (make-instance 'cl-cc/vm::vm-cons-cell :car 99 :cdr nil))))
    (let ((s (make-test-vm)))
    (when setup
      (funcall setup s))
    (expect (cl-cc/vm::vm-list-to-lisp-list s input) :to-equal expected))))

(it-sequential "vm-execute-vm-list-to-lisp-list-cow-list"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(1 2 3))
    (exec1 (cl-cc:make-vm-copy-list :dst 0 :src 1) s)
    (expect (cl-cc/vm::vm-list-to-lisp-list s (cl-cc:vm-reg-get s 0)) :to-equal '(1 2 3))))

;;; ─── execute-instruction: vm-const ──────────────────────────────────────────

(it-sequential "vm-execute-vm-const-loads-values integer"
  (destructuring-bind (dst val expected) (list :R0 42 42)
    (let ((s (make-test-vm)))
    (exec1 (cl-cc:make-vm-const :dst dst :value val) s)
    (expect (cl-cc:vm-reg-get s dst) :to-equal expected))))

(it-sequential "vm-execute-vm-const-loads-values nil"
  (destructuring-bind (dst val expected) (list :R1 nil nil)
    (let ((s (make-test-vm)))
    (exec1 (cl-cc:make-vm-const :dst dst :value val) s)
    (expect (cl-cc:vm-reg-get s dst) :to-equal expected))))

(it-sequential "vm-execute-vm-const-loads-values string"
  (destructuring-bind (dst val expected) (list :R2 "x" "x")
    (let ((s (make-test-vm)))
    (exec1 (cl-cc:make-vm-const :dst dst :value val) s)
    (expect (cl-cc:vm-reg-get s dst) :to-equal expected))))

(it-sequential "vm-execute-vm-const-loads-values neg"
  (destructuring-bind (dst val expected) (list :R3 -7 -7)
    (let ((s (make-test-vm)))
    (exec1 (cl-cc:make-vm-const :dst dst :value val) s)
    (expect (cl-cc:vm-reg-get s dst) :to-equal expected))))

;;; ─── execute-instruction: vm-halt ───────────────────────────────────────────

(it-sequential "vm-execute-pc-advancing"
  (let ((s (make-test-vm)))
    (multiple-value-bind (next-pc halt-p)
        (exec1 (cl-cc:make-vm-const :dst :R0 :value 0) s)
      (expect (= 1 next-pc) :to-be-truthy)
      (expect halt-p :to-be-falsy))
    (cl-cc:vm-reg-set s :R1 'hello)
    (exec1 (cl-cc:make-vm-move :dst :R0 :src :R1) s)
    (expect (cl-cc:vm-reg-get s :R0) :to-be 'hello)))

(it-sequential "vm-execute-halt-signal"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 'done)
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc:make-vm-halt :reg :R0) s)
      (expect next-pc :to-be-null)
      (expect halt-p :to-be-truthy)
      (expect result :to-be 'done))))

;;; ─── execute-instruction: vm-jump-zero ──────────────────────────────────────

(it-sequential "vm-execute-vm-jump-zero-branching taken-false"
  (destructuring-bind (reg-val current-pc label label-pc expected-pc) (list nil 0 "target" 99 99)
    (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'eql)))
    (cl-cc/vm::vm-label-table-store lbls label label-pc)
    (cl-cc:vm-reg-set s :R0 reg-val)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-jump-zero :reg :R0 :label label) s current-pc lbls)
      (declare (ignore halt-p))
      (expect (= expected-pc next-pc) :to-be-truthy)))))

(it-sequential "vm-execute-vm-jump-zero-branching not-taken-true"
  (destructuring-bind (reg-val current-pc label label-pc expected-pc) (list 1 5 "target" 99 6)
    (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'eql)))
    (cl-cc/vm::vm-label-table-store lbls label label-pc)
    (cl-cc:vm-reg-set s :R0 reg-val)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-jump-zero :reg :R0 :label label) s current-pc lbls)
      (declare (ignore halt-p))
      (expect (= expected-pc next-pc) :to-be-truthy)))))

(it-sequential "vm-execute-vm-jump-zero-branching taken-zero"
  (destructuring-bind (reg-val current-pc label label-pc expected-pc) (list 0 10 "loop" 3 3)
    (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'eql)))
    (cl-cc/vm::vm-label-table-store lbls label label-pc)
    (cl-cc:vm-reg-set s :R0 reg-val)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-jump-zero :reg :R0 :label label) s current-pc lbls)
      (declare (ignore halt-p))
      (expect (= expected-pc next-pc) :to-be-truthy)))))

(it-sequential "vm-execute-vm-label-advances-pc"
  (let ((s (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-label :name "entry") s 4 (make-hash-table :test #'equal))
      (declare (ignore result))
      (expect (= 5 next-pc) :to-be-truthy)
      (expect halt-p :to-be-falsy))))

(it-sequential "vm-execute-vm-jump-taken"
  (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'equal)))
    (cl-cc/vm::vm-label-table-store lbls "entry" 11)
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-jump :label "entry") s 2 lbls)
      (declare (ignore result))
      (expect (= 11 next-pc) :to-be-truthy)
      (expect halt-p :to-be-falsy))))

(it-sequential "vm-execute-vm-func-ref-resolves-cl-host-function"
  (let ((s (make-test-vm))
        (labels (make-hash-table :test #'equal)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-func-ref :dst :R0 :label "1+") s 7 labels)
      (declare (ignore result))
      (expect (= 8 next-pc) :to-be-truthy)
      (expect halt-p :to-be-falsy))
    (let ((fn (cl-cc:vm-reg-get s :R0)))
      (expect (functionp fn) :to-be-truthy)
      (expect (= 42 (funcall fn 41)) :to-be-truthy))))

(it-sequential "vm-execute-call/cc-restores-copied-stack"
  (let* ((program (cl-cc:make-vm-program
                   :instructions
                   (list (cl-cc:make-vm-closure :dst :FN :label "fn" :params '(:K)
                                               :optional-params nil :rest-param nil
                                               :key-params nil :rest-stack-alloc-p nil
                                               :inline-policy nil :dispatch-tag nil :captured nil)
                         (cl-cc:make-vm-call/cc :dst :R0 :func :FN)
                         (cl-cc:make-vm-halt :reg :R0)
                         (cl-cc:make-vm-label :name "fn")
                         (cl-cc:make-vm-const :dst :V :value 42)
                         (cl-cc:make-vm-call :dst :IGNORED :func :K :args '(:V))
                         (cl-cc:make-vm-const :dst :BAD :value 9)
                         (cl-cc:make-vm-ret :reg :BAD))
                   :result-register :R0)))
    (expect (= 42 (cl-cc:run-compiled program)) :to-be-truthy)))

(it-sequential "vm-execute-abort-to-prompt-jumps-to-delimiter"
  (let* ((program (cl-cc:make-vm-program
                   :instructions
                   (list (cl-cc:make-vm-closure :dst :FN :label "fn" :params nil
                                               :optional-params nil :rest-param nil
                                               :key-params nil :rest-stack-alloc-p nil
                                               :inline-policy nil :dispatch-tag nil :captured nil)
                         (cl-cc:make-vm-const :dst :PROMPT :value 'p)
                         (cl-cc:make-vm-call-with-prompt :dst :R0 :func :FN :prompt :PROMPT)
                         (cl-cc:make-vm-halt :reg :R0)
                         (cl-cc:make-vm-label :name "fn")
                         (cl-cc:make-vm-const :dst :P2 :value 'p)
                         (cl-cc:make-vm-const :dst :V :value 77)
                         (cl-cc:make-vm-abort-to-prompt :prompt :P2 :value :V)
                         (cl-cc:make-vm-const :dst :BAD :value 9)
                         (cl-cc:make-vm-ret :reg :BAD))
                   :result-register :R0)))
    (expect (= 77 (cl-cc:run-compiled program)) :to-be-truthy)))

(it-sequential "vm-execute-mv-buffer-frame-protocol"
  (let ((s (make-test-vm))
        (labels nil))
    (cl-cc:vm-reg-set s :A 10)
    (cl-cc:vm-reg-set s :B 20)
    (cl-cc:vm-reg-set s :C 30)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-values :dst :R :src-regs '(:A :B :C)) s 0 labels)
    (expect (= 3 (cl-cc/vm::vm-mv-count s)) :to-be-truthy)
    (expect (= 20 (aref (cl-cc/vm::vm-mv-buffer s) 1)) :to-be-truthy)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-nth-value :dst :N :index 2) s 1 labels)
    (expect (= 30 (cl-cc:vm-reg-get s :N)) :to-be-truthy)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-mv-bind :dst-regs '(:X :Y :Z :MISSING)) s 2 labels)
    (expect (= 10 (cl-cc:vm-reg-get s :X)) :to-be-truthy)
    (expect (= 20 (cl-cc:vm-reg-get s :Y)) :to-be-truthy)
    (expect (= 30 (cl-cc:vm-reg-get s :Z)) :to-be-truthy)
    (expect (cl-cc:vm-reg-get s :MISSING) :to-be nil)))

(it-sequential "vm-execute-empty-values-buffer"
  (let ((s (make-test-vm)))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-values :dst :R :src-regs nil) s 0 nil)
    (expect (= 0 (cl-cc/vm::vm-mv-count s)) :to-be-truthy)
    (expect (aref (cl-cc/vm::vm-mv-buffer s) 0) :to-be nil)
    (expect (cl-cc:vm-reg-get s :R) :to-be nil)))
