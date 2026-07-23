;;;; tests/unit/emit/aarch64-emit-tests.lisp — AArch64 Assembly Emit Tests
;;;;
;;;; Tests for src/emit/aarch64.lisp:
;;;; aarch64-target, target-register, emit-instruction methods

(in-package :cl-cc/test)



;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun %aarch64-emit (target inst)
  "Emit INST to a string using TARGET and return the result."
  (let ((s (make-string-output-stream)))
    (cl-cc/codegen::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %collect-a64-bytes (emit-fn inst)
  "Collect bytes emitted by EMIT-FN for INST into a list."
  (let ((bytes nil))
    (funcall emit-fn inst (lambda (b) (push b bytes)))
    (nreverse bytes)))

(defun %make-aarch64-target ()
  (make-instance 'cl-cc/codegen::aarch64-target))

;;; ─── target-register ──────────────────────────────────────────────────────────

(it-sequential "aarch64-target-register-pool r0"
  (destructuring-bind (vreg expected) (list :r0 "x0")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r1"
  (destructuring-bind (vreg expected) (list :r1 "x1")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r2"
  (destructuring-bind (vreg expected) (list :r2 "x2")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r3"
  (destructuring-bind (vreg expected) (list :r3 "x3")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r4"
  (destructuring-bind (vreg expected) (list :r4 "x4")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r5"
  (destructuring-bind (vreg expected) (list :r5 "x5")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r6"
  (destructuring-bind (vreg expected) (list :r6 "x6")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-pool r7"
  (destructuring-bind (vreg expected) (list :r7 "x7")
    (let ((tgt (%make-aarch64-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "aarch64-target-register-overflow"
  ;; The caller-saved pool holds x0-x17 (18 registers); :R18 is the first
  ;; virtual register beyond it and must signal (spilling required).
  (let ((tgt (%make-aarch64-target)))
    (signals error (cl-cc/codegen::target-register tgt :r18))))

;;; ─── emit-instruction methods ──────────────────────────────────────────────────

(it-sequential "aarch64-emit-const"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-const :dst :r0 :value 42))))
    (expect (search "mov" asm) :to-be-truthy)
    (expect (search "x0" asm) :to-be-truthy)
    (expect (search "#42" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-move"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-move :dst :r0 :src :r1))))
    (expect (search "mov" asm) :to-be-truthy)
    (expect (search "x0" asm) :to-be-truthy)
    (expect (search "x1" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-add"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (search "add" asm) :to-be-truthy)
    (expect (search "x0" asm) :to-be-truthy)
    (expect (search "x1" asm) :to-be-truthy)
    (expect (search "x2" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-arithmetic-mnemonics sub"
  (destructuring-bind (inst expected-mnemonic) (list (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) "sub")
    (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt inst)))
    (expect (search expected-mnemonic asm) :to-be-truthy))))

(it-sequential "aarch64-emit-arithmetic-mnemonics mul"
  (destructuring-bind (inst expected-mnemonic) (list (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2) "mul")
    (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt inst)))
    (expect (search expected-mnemonic asm) :to-be-truthy))))

(it-sequential "aarch64-emit-label"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-label :name "loop"))))
    (expect (search ".align 4" asm) :to-be-truthy)
    (expect (search "loop:" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-jump"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-jump :label "done"))))
    (expect (search "b " asm) :to-be-truthy)
    (expect (search "done" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-jump-zero"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-jump-zero :reg :r0 :label "else"))))
    (expect (search "cmp" asm) :to-be-truthy)
    (expect (search "#0" asm) :to-be-truthy)
    (expect (search "b.eq" asm) :to-be-truthy)
    (expect (search "else" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-halt"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-halt :reg :r0))))
    (expect (search "mov x0" asm) :to-be-truthy)
    (expect (search "ret" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-print-calls-bl-rt-print"
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-print :reg :r0))))
    (expect (search "bl rt-print" asm) :to-be-truthy)
    (expect (search "x0" asm) :to-be-truthy)))

(it-sequential "aarch64-emit-unsupported-instruction-signals-error"
  (let ((tgt (%make-aarch64-target)))
    (signals error (%aarch64-emit tgt (make-vm-ret :reg :r0)))))

(it-sequential "aarch64-emit-spill-operations"
  (let ((tgt (%make-aarch64-target)))
    (let ((asm (%aarch64-emit tgt (make-vm-spill-store :src-reg :x19 :slot 2))))
      (expect (search "str" asm) :to-be-truthy)
      (expect (search "x29" asm) :to-be-truthy)
      (expect (search "16" asm) :to-be-truthy)
      (expect (search "x19" asm) :to-be-truthy))
    (let ((asm (%aarch64-emit tgt (make-vm-spill-load :dst-reg :x20 :slot 3))))
      (expect (search "ldr" asm) :to-be-truthy)
      (expect (search "x20" asm) :to-be-truthy)
      (expect (search "x29" asm) :to-be-truthy)
      (expect (search "24" asm) :to-be-truthy))))

;;; ─── Checked arithmetic emitters (FR-303) ────────────────────────────────────

(it-sequential "aarch64-emit-add-checked-emits-12-bytes"
  (let* ((inst (cl-cc:make-vm-add-checked :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-a64-bytes #'cl-cc/codegen::emit-a64-vm-add-checked inst)))
    (expect (= 12 (length bytes)) :to-be-truthy)))

(it-sequential "aarch64-emit-sub-checked-emits-12-bytes"
  (let* ((inst (cl-cc:make-vm-sub-checked :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-a64-bytes #'cl-cc/codegen::emit-a64-vm-sub-checked inst)))
    (expect (= 12 (length bytes)) :to-be-truthy)))

(it-sequential "aarch64-emit-mul-checked-emits-24-bytes"
  (let* ((inst (cl-cc:make-vm-mul-checked :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-a64-bytes #'cl-cc/codegen::emit-a64-vm-mul-checked inst)))
    (expect (= 24 (length bytes)) :to-be-truthy)))
