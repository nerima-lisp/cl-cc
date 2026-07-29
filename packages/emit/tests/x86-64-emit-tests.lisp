;;;; tests/unit/emit/x86-64-emit-tests.lisp — x86-64 Assembly Emit Tests
;;;;
;;;; Tests for src/emit/x86-64.lisp:
;;;; target base class, x86-64-target, target-register, emit-instruction methods,
;;;; *phys-reg-to-asm-string*, spill-store/spill-load emission

(in-package :cl-cc/test)



;;; ─── Helper: emit an instruction to string ─────────────────────────────────────

(defun %x86-emit (target inst)
  "Emit INST to a string using TARGET and return the result."
  (let ((s (make-string-output-stream)))
    (cl-cc/codegen::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %make-x86-target ()
  "Create a plain x86-64-target with no regalloc (fallback mapping)."
  (make-instance 'cl-cc/codegen::x86-64-target))

;;; ─── *phys-reg-to-asm-string* table ─────────────────────────────────────────

(it-sequential "x86-phys-reg-table-has-15-entries"
  (expect (length cl-cc/codegen::*phys-reg-to-asm-string*) :to-equal 15))

(it-sequential "x86-phys-reg-table-entries rax"
  (destructuring-bind (phys-key expected-str) (list :rax "rax")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries rcx"
  (destructuring-bind (phys-key expected-str) (list :rcx "rcx")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries rdx"
  (destructuring-bind (phys-key expected-str) (list :rdx "rdx")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries rbx"
  (destructuring-bind (phys-key expected-str) (list :rbx "rbx")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries rbp"
  (destructuring-bind (phys-key expected-str) (list :rbp "rbp")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries rsi"
  (destructuring-bind (phys-key expected-str) (list :rsi "rsi")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries rdi"
  (destructuring-bind (phys-key expected-str) (list :rdi "rdi")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries r8"
  (destructuring-bind (phys-key expected-str) (list :r8 "r8")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

(it-sequential "x86-phys-reg-table-entries r15"
  (destructuring-bind (phys-key expected-str) (list :r15 "r15")
    (expect (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*)) :to-equal expected-str)))

;;; ─── target-register: fallback (no regalloc) ──────────────────────────────────

(it-sequential "x86-target-register-fallback r0"
  (destructuring-bind (vreg expected) (list :r0 "rax")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r1"
  (destructuring-bind (vreg expected) (list :r1 "rbx")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r2"
  (destructuring-bind (vreg expected) (list :r2 "rcx")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r3"
  (destructuring-bind (vreg expected) (list :r3 "rdx")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r4"
  (destructuring-bind (vreg expected) (list :r4 "r8")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r5"
  (destructuring-bind (vreg expected) (list :r5 "r9")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r6"
  (destructuring-bind (vreg expected) (list :r6 "r10")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback r7"
  (destructuring-bind (vreg expected) (list :r7 "r11")
    (let ((tgt (%make-x86-target)))
    (expect (cl-cc/codegen::target-register tgt vreg) :to-equal expected))))

(it-sequential "x86-target-register-fallback-signals-error-for-r8-plus"
  (let ((tgt (%make-x86-target)))
    (signals error (cl-cc/codegen::target-register tgt :r8))))

(it-sequential "x86-target-register-with-regalloc-uses-assignment"
  (let* ((ht (make-hash-table))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment ht))
         (tgt (make-instance 'cl-cc/codegen::x86-64-target :regalloc ra)))
    (setf (gethash :r0 ht) :rax)
    (expect (cl-cc/codegen::target-register tgt :r0) :to-equal "rax")
    (signals error (cl-cc/codegen::target-register tgt :r99))))

;;; ─── emit-instruction methods ──────────────────────────────────────────────────

(it-sequential "x86-emit-const-emits-mov-with-value"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-const :dst :r0 :value 42))))
    (expect (search "mov" asm) :to-be-truthy)
    (expect (search "rax" asm) :to-be-truthy)
    (expect (search "42" asm) :to-be-truthy)))

(it-sequential "x86-emit-move-emits-mov-between-regs"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-move :dst :r0 :src :r1))))
    (expect (search "mov" asm) :to-be-truthy)
    (expect (search "rax" asm) :to-be-truthy)
    (expect (search "rbx" asm) :to-be-truthy)))

(it-sequential "x86-emit-add-emits-mov-and-add-mnemonic"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (search "mov" asm) :to-be-truthy)
    (expect (search "add" asm) :to-be-truthy)))

(it-sequential "x86-emit-arithmetic-mnemonics sub"
  (destructuring-bind (inst expected-mnemonic) (list (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) "sub")
    (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt inst)))
    (expect (search expected-mnemonic asm) :to-be-truthy))))

(it-sequential "x86-emit-arithmetic-mnemonics mul"
  (destructuring-bind (inst expected-mnemonic) (list (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2) "imul")
    (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt inst)))
    (expect (search expected-mnemonic asm) :to-be-truthy))))

(it-sequential "x86-emit-label-emits-align-and-colon-name"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-label :name "loop"))))
    (expect (search ".align 4" asm) :to-be-truthy)
    (expect (search "loop:" asm) :to-be-truthy)))

(it-sequential "x86-emit-jump-emits-jmp-mnemonic"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-jump :label "done"))))
    (expect (search "jmp" asm) :to-be-truthy)
    (expect (search "done" asm) :to-be-truthy)))

(it-sequential "x86-emit-jump-zero-emits-cmp-and-je"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-jump-zero :reg :r0 :label "else"))))
    (expect (search "cmp" asm) :to-be-truthy)
    (expect (search "je" asm) :to-be-truthy)
    (expect (search "else" asm) :to-be-truthy)))

(it-sequential "x86-emit-halt-emits-mov-rax-and-ret"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-halt :reg :r0))))
    (expect (search "mov rax" asm) :to-be-truthy)
    (expect (search "ret" asm) :to-be-truthy)))

(it-sequential "x86-emit-print-calls-rt-print-with-rdi"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-print :reg :r0))))
    (expect (search "call rt-print" asm) :to-be-truthy)
    (expect (search "rdi" asm) :to-be-truthy)))

(it-sequential "x86-emit-ret-emits-mov-rax-and-ret"
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-ret :reg :r0))))
    (expect (search "mov rax" asm) :to-be-truthy)
    (expect (search "ret" asm) :to-be-truthy)))

;;; ─── Spill code emission ──────────────────────────────────────────────────────

(it-sequential "x86-emit-spill-operations"
  (let ((tgt (%make-x86-target)))
    (let ((asm (%x86-emit tgt (make-vm-spill-store :src-reg :rax :slot 2))))
      (expect (search "mov" asm) :to-be-truthy)
      (expect (search "rbp" asm) :to-be-truthy)
      (expect (search "16" asm) :to-be-truthy)
      (expect (search "rax" asm) :to-be-truthy))
    (let ((asm (%x86-emit tgt (make-vm-spill-load :dst-reg :rbx :slot 3))))
      (expect (search "mov" asm) :to-be-truthy)
      (expect (search "rbx" asm) :to-be-truthy)
      (expect (search "rbp" asm) :to-be-truthy)
      (expect (search "24" asm) :to-be-truthy))))

(it-sequential "x86-emit-spill-operations-rsp-red-zone"
  (let ((tgt (make-instance 'cl-cc/codegen::x86-64-target :spill-base-reg :rsp)))
    (let ((asm (%x86-emit tgt (make-vm-spill-store :src-reg :rax :slot 1))))
      (expect (search "rsp" asm) :to-be-truthy)
      (expect (search "rbp" asm) :to-be-falsy)
      (expect (search "8" asm) :to-be-truthy))
    (let ((asm (%x86-emit tgt (make-vm-spill-load :dst-reg :rbx :slot 1))))
      (expect (search "rsp" asm) :to-be-truthy)
      (expect (search "rbp" asm) :to-be-falsy)
      (expect (search "rbx" asm) :to-be-truthy))))
