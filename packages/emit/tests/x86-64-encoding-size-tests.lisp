;;;; tests/unit/emit/x86-64-encoding-size-tests.lisp — x86-64 Instruction Size & Layout Tests
;;;;
;;;; Continuation of x86-64-encoding-tests.lisp.
;;;; Tests for instruction-size dispatch, label offset calculation,
;;;; emitter table integrity, memory displacement encoding, and
;;;; full program output verification.

(in-package :cl-cc/test)


(it-sequential "x86-instruction-size-zero-cases vm-label"
  (destructuring-bind (inst) (list (cl-cc:make-vm-label :name "test"))
    (expect (cl-cc/codegen::instruction-size inst) :to-equal 0)))

(it-sequential "x86-instruction-size-zero-cases unknown"
  (destructuring-bind (inst) (list (list :not-a-real-inst))
    (expect (cl-cc/codegen::instruction-size inst) :to-equal 0)))

;;; ─── build-label-offsets ────────────────────────────────────────────────

(it-sequential "x86-build-label-offsets-simple single-at-start"
  (destructuring-bind (insts prologue label expected-offset) (list (list (cl-cc:make-vm-label :name "L")) 6 "L" 6)
    (expect (gethash label (cl-cc/codegen::build-label-offsets insts prologue)) :to-equal expected-offset)))

(it-sequential "x86-build-label-offsets-simple after-vm-const"
  (destructuring-bind (insts prologue label expected-offset) (list (list (cl-cc:make-vm-const :dst :R0 :value 42)
                 (cl-cc:make-vm-label :name "L")) 6 "L" 16)
    (expect (gethash label (cl-cc/codegen::build-label-offsets insts prologue)) :to-equal expected-offset)))

(it-sequential "x86-build-label-offsets-simple after-vm-add"
  (destructuring-bind (insts prologue label expected-offset) (list (list (cl-cc:make-vm-add :dst :R0 :lhs :R1 :rhs :R2)
                 (cl-cc:make-vm-label :name "L")) 0 "L" 6)
    (expect (gethash label (cl-cc/codegen::build-label-offsets insts prologue)) :to-equal expected-offset)))

(it-sequential "x86-build-label-offsets-multi"
  (expect (hash-table-count (cl-cc/codegen::build-label-offsets nil 6)) :to-equal 0)
  (let* ((insts (list (cl-cc:make-vm-label :name "L0")
                      (cl-cc:make-vm-move :dst :R0 :src :R1)    ; 3 bytes
                      (cl-cc:make-vm-label :name "L1")
                      (cl-cc:make-vm-add :dst :R0 :lhs :R1 :rhs :R2) ; 6 bytes
                      (cl-cc:make-vm-label :name "L2")))
         (offsets (cl-cc/codegen::build-label-offsets insts 0)))
    (expect (gethash "L0" offsets) :to-equal 0)
    (expect (gethash "L1" offsets) :to-equal 3)
    (expect (gethash "L2" offsets) :to-equal 9)))

;;; ─── *x86-64-emitter-table* completeness ────────────────────────────────

(it-sequential "x86-emitter-table-integrity"
  (expect (>= (hash-table-count cl-cc/codegen::*x86-64-emitter-table*) 40) :to-be-truthy)
  (let ((all-ok t))
    (maphash (lambda (key fn)
               (declare (ignore key))
               (unless (functionp fn)
                 (setf all-ok nil)))
             cl-cc/codegen::*x86-64-emitter-table*)
    (expect all-ok :to-be-truthy)))

;;; ─── High-register encoding (REX.R / REX.B) and memory displacement ────

(it-sequential "x86-mov-mem-displacement-sizes rm64-no-disp"
  (destructuring-bind (emit-fn expected-len) (list (lambda (s) (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+  0 s)) 3)
    (expect (length (%x86-collect-bytes emit-fn)) :to-equal expected-len)))

(it-sequential "x86-mov-mem-displacement-sizes rm64-disp8"
  (destructuring-bind (emit-fn expected-len) (list (lambda (s) (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+  8 s)) 4)
    (expect (length (%x86-collect-bytes emit-fn)) :to-equal expected-len)))

(it-sequential "x86-mov-mem-displacement-sizes mr64-disp8-16"
  (destructuring-bind (emit-fn expected-len) (list (lambda (s) (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rcx+ 16 cl-cc/codegen::+rax+ s)) 4)
    (expect (length (%x86-collect-bytes emit-fn)) :to-equal expected-len)))

(it-sequential "x86-mov-exact-bytes rm64-indexed-disp8"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-mov-rm64-indexed cl-cc/codegen::+rax+ cl-cc/codegen::+rbx+ cl-cc/codegen::+rcx+ 8 16 s)) '(#x48 #x8B #x44 #xCB #x10))
    (expect (%x86-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-mov-exact-bytes mr64-indexed-disp8"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-mov-mr64-indexed cl-cc/codegen::+rbx+ cl-cc/codegen::+rcx+ 4 8 cl-cc/codegen::+rax+ s)) '(#x48 #x89 #x44 #x8B #x08))
    (expect (%x86-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-mov-exact-bytes rm64-indexed-scale1-zero"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-mov-rm64-indexed cl-cc/codegen::+rax+ cl-cc/codegen::+rbx+ cl-cc/codegen::+rcx+ 1 0 s)) '(#x48 #x8B #x04 #x0B))
    (expect (%x86-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-mov-exact-bytes load-rsp-disp8"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rsp+ -8 s)) '(#x48 #x8B #x44 #x24 #xF8))
    (expect (%x86-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-mov-exact-bytes store-rsp-disp8"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rsp+ -16 cl-cc/codegen::+rax+ s)) '(#x48 #x89 #x44 #x24 #xF0))
    (expect (%x86-collect-bytes emit-fn) :to-equal expected-bytes)))

;;; ─── emit-mov-rr64 ModR/M correctness ──────────────────────────────────

(it-sequential "x86-mov-rr64-modrm-encoding rax-rcx"
  (destructuring-bind (dst src modrm) (list cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ #xC8)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-rr64 dst src s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x48)
    (expect (second bytes) :to-equal #x89)
    (expect (third bytes) :to-equal modrm))))

(it-sequential "x86-mov-rr64-modrm-encoding rax-rbx"
  (destructuring-bind (dst src modrm) (list cl-cc/codegen::+rax+ cl-cc/codegen::+rbx+ #xD8)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-rr64 dst src s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x48)
    (expect (second bytes) :to-equal #x89)
    (expect (third bytes) :to-equal modrm))))

(it-sequential "x86-mov-rr64-modrm-encoding rax-rax"
  (destructuring-bind (dst src modrm) (list cl-cc/codegen::+rax+ cl-cc/codegen::+rax+ #xC0)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-rr64 dst src s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x48)
    (expect (second bytes) :to-equal #x89)
    (expect (third bytes) :to-equal modrm))))

;;; ─── PUSH/POP single-byte opcode coverage ───────────────────────────────

(it-sequential "x86-push-pop-single-byte-opcodes push-rax"
  (destructuring-bind (reg emit-fn opcode) (list cl-cc/codegen::+rax+ #'cl-cc/codegen::emit-push-r64 #x50)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-single-byte-opcodes push-rcx"
  (destructuring-bind (reg emit-fn opcode) (list cl-cc/codegen::+rcx+ #'cl-cc/codegen::emit-push-r64 #x51)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-single-byte-opcodes pop-rax"
  (destructuring-bind (reg emit-fn opcode) (list cl-cc/codegen::+rax+ #'cl-cc/codegen::emit-pop-r64 #x58)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-single-byte-opcodes push-rbx"
  (destructuring-bind (reg emit-fn opcode) (list cl-cc/codegen::+rbx+ #'cl-cc/codegen::emit-push-r64 #x53)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-single-byte-opcodes pop-rcx"
  (destructuring-bind (reg emit-fn opcode) (list cl-cc/codegen::+rcx+ #'cl-cc/codegen::emit-pop-r64 #x59)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-single-byte-opcodes pop-rdx"
  (destructuring-bind (reg emit-fn opcode) (list cl-cc/codegen::+rdx+ #'cl-cc/codegen::emit-pop-r64 #x5A)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding push-r8"
  (destructuring-bind (reg emit-fn opcode) (list 8 #'cl-cc/codegen::emit-push-r64 #x50)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding push-r12"
  (destructuring-bind (reg emit-fn opcode) (list 12 #'cl-cc/codegen::emit-push-r64 #x54)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding push-r13"
  (destructuring-bind (reg emit-fn opcode) (list 13 #'cl-cc/codegen::emit-push-r64 #x55)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding push-r14"
  (destructuring-bind (reg emit-fn opcode) (list 14 #'cl-cc/codegen::emit-push-r64 #x56)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding push-r15"
  (destructuring-bind (reg emit-fn opcode) (list 15 #'cl-cc/codegen::emit-push-r64 #x57)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding pop-r12"
  (destructuring-bind (reg emit-fn opcode) (list 12 #'cl-cc/codegen::emit-pop-r64 #x5C)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding pop-r13"
  (destructuring-bind (reg emit-fn opcode) (list 13 #'cl-cc/codegen::emit-pop-r64 #x5D)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-pop-extended-regs-two-byte-encoding pop-r15"
  (destructuring-bind (reg emit-fn opcode) (list 15 #'cl-cc/codegen::emit-pop-r64 #x5F)
    (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (expect (length bytes) :to-equal 2)
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal opcode))))

(it-sequential "x86-push-r64-byte-size rax"
  (destructuring-bind (reg expected) (list 0 1)
    (expect (cl-cc/codegen::push-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-push-r64-byte-size rbp"
  (destructuring-bind (reg expected) (list 5 1)
    (expect (cl-cc/codegen::push-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-push-r64-byte-size rdi"
  (destructuring-bind (reg expected) (list 7 1)
    (expect (cl-cc/codegen::push-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-push-r64-byte-size r8"
  (destructuring-bind (reg expected) (list 8 2)
    (expect (cl-cc/codegen::push-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-push-r64-byte-size r12"
  (destructuring-bind (reg expected) (list 12 2)
    (expect (cl-cc/codegen::push-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-push-r64-byte-size r15"
  (destructuring-bind (reg expected) (list 15 2)
    (expect (cl-cc/codegen::push-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-pop-r64-byte-size rax"
  (destructuring-bind (reg expected) (list 0 1)
    (expect (cl-cc/codegen::pop-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-pop-r64-byte-size rbp"
  (destructuring-bind (reg expected) (list 5 1)
    (expect (cl-cc/codegen::pop-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-pop-r64-byte-size rdi"
  (destructuring-bind (reg expected) (list 7 1)
    (expect (cl-cc/codegen::pop-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-pop-r64-byte-size r8"
  (destructuring-bind (reg expected) (list 8 2)
    (expect (cl-cc/codegen::pop-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-pop-r64-byte-size r12"
  (destructuring-bind (reg expected) (list 12 2)
    (expect (cl-cc/codegen::pop-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-pop-r64-byte-size r15"
  (destructuring-bind (reg expected) (list 15 2)
    (expect (cl-cc/codegen::pop-r64-byte-size reg) :to-equal expected)))

(it-sequential "x86-prologue-with-r12-callee-saved-uses-two-byte-push"
  (let* ((assignment (let ((ht (make-hash-table :test #'eq)))
                       (setf (gethash :R0 ht) :r12)
                       ht))
         (ra (cl-cc/regalloc::make-regalloc-result
              :assignment assignment
              :spill-count 0
              :instructions nil))
          (prog (cl-cc/vm::make-vm-program
                 :instructions (list (cl-cc:make-vm-halt :reg :R0))
                 :result-register :R0
                 :leaf-p nil))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                  (%x86-collect-bytes
                   (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    ;; Prologue: PUSH R12 must be 2-byte encoding with REX.B prefix
    (expect (first bytes) :to-equal #x41)
    (expect (second bytes) :to-equal #x54)
    ;; Body: VM halt moves the :R0 value (allocated to R12) back to RAX.
    (expect (subseq bytes 2 5) :to-equal '(#x4C #x89 #xE0))
    ;; Epilogue: POP R12 must be 2-byte encoding with REX.B prefix
    (expect (nth 5 bytes) :to-equal #x41)
    (expect (nth 6 bytes) :to-equal #x5C)
    ;; RET follows immediately
    (expect (nth 7 bytes) :to-equal #xC3)
    (expect (length bytes) :to-equal 8)))

;;; ─── SETcc opcode2 values for each comparison ───────────────────────────

(it-sequential "x86-setcc-opcode2-values setl"
  (destructuring-bind (opcode2) (list #x9C)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x0F)
    (expect (second bytes) :to-equal opcode2))))

(it-sequential "x86-setcc-opcode2-values setge"
  (destructuring-bind (opcode2) (list #x9D)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x0F)
    (expect (second bytes) :to-equal opcode2))))

(it-sequential "x86-setcc-opcode2-values setle"
  (destructuring-bind (opcode2) (list #x9E)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x0F)
    (expect (second bytes) :to-equal opcode2))))

(it-sequential "x86-setcc-opcode2-values setg"
  (destructuring-bind (opcode2) (list #x9F)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x0F)
    (expect (second bytes) :to-equal opcode2))))

(it-sequential "x86-setcc-opcode2-values sete"
  (destructuring-bind (opcode2) (list #x94)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x0F)
    (expect (second bytes) :to-equal opcode2))))

(it-sequential "x86-setcc-opcode2-values setne"
  (destructuring-bind (opcode2) (list #x95)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (expect (length bytes) :to-equal 3)
    (expect (first bytes) :to-equal #x0F)
    (expect (second bytes) :to-equal opcode2))))

;;; ─── vm-reg-to-x86 mapping ───────────────────────────────────────────────

(it-sequential "x86-vm-reg-to-x86-mapping R0"
  (destructuring-bind (vm-reg expected-code) (list :R0 0)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R1"
  (destructuring-bind (vm-reg expected-code) (list :R1 1)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R2"
  (destructuring-bind (vm-reg expected-code) (list :R2 2)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R3"
  (destructuring-bind (vm-reg expected-code) (list :R3 3)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R4"
  (destructuring-bind (vm-reg expected-code) (list :R4 6)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R5"
  (destructuring-bind (vm-reg expected-code) (list :R5 7)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R6"
  (destructuring-bind (vm-reg expected-code) (list :R6 8)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

(it-sequential "x86-vm-reg-to-x86-mapping R7"
  (destructuring-bind (vm-reg expected-code) (list :R7 9)
    (expect (cl-cc/codegen::vm-reg-to-x86 vm-reg) :to-equal expected-code)))

;;; ─── *phys-reg-to-x86-code* completeness ────────────────────────────────

(it-sequential "x86-phys-reg-map"
  (expect (cdr (assoc :rax cl-cc/codegen::*phys-reg-to-x86-code*)) :to-equal 0)
  (expect (cdr (assoc :rbp cl-cc/codegen::*phys-reg-to-x86-code*)) :to-equal 5)
  (expect (cdr (assoc :r15 cl-cc/codegen::*phys-reg-to-x86-code*)) :to-equal 15)
  (expect (length cl-cc/codegen::*phys-reg-to-x86-code*) :to-equal 15))

;;; ─── emit-vm-const and emit-vm-move sizes ───────────────────────────────

(it-sequential "x86-vm-const-bool-immediate nil"
  (destructuring-bind (val expected-byte) (list nil 0)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-vm-const
                             (cl-cc:make-vm-const :dst :R0 :value val) s)))))
    (expect (length bytes) :to-equal 10)
    (expect (third bytes) :to-equal expected-byte)
    (expect (fourth bytes) :to-equal 0))))

(it-sequential "x86-vm-const-bool-immediate t"
  (destructuring-bind (val expected-byte) (list t 1)
    (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-vm-const
                             (cl-cc:make-vm-const :dst :R0 :value val) s)))))
    (expect (length bytes) :to-equal 10)
    (expect (third bytes) :to-equal expected-byte)
    (expect (fourth bytes) :to-equal 0))))

(it-sequential "x86-vm-move-halt-elision move-cross"
  (destructuring-bind (cross-emitter zero-emitter) (list (lambda (s) (cl-cc/codegen::emit-vm-move
                                     (cl-cc:make-vm-move :dst :R0 :src :R1) s)) (lambda (s) (cl-cc/codegen::emit-vm-move
                                     (cl-cc:make-vm-move :dst :R0 :src :R0) s)))
    (let ((cross-bytes (%x86-collect-bytes cross-emitter))
        (zero-bytes  (%x86-collect-bytes zero-emitter)))
    (expect (length cross-bytes) :to-equal 3)
    (expect (second cross-bytes) :to-equal #x89)
    (expect (length zero-bytes) :to-equal 0))))

(it-sequential "x86-vm-move-halt-elision halt"
  (destructuring-bind (cross-emitter zero-emitter) (list (lambda (s) (cl-cc/codegen::emit-vm-halt-inst
                                     (cl-cc:make-vm-halt :reg :R1) s)) (lambda (s) (cl-cc/codegen::emit-vm-halt-inst
                                     (cl-cc:make-vm-halt :reg :R0) s)))
    (let ((cross-bytes (%x86-collect-bytes cross-emitter))
        (zero-bytes  (%x86-collect-bytes zero-emitter)))
    (expect (length cross-bytes) :to-equal 3)
    (expect (second cross-bytes) :to-equal #x89)
    (expect (length zero-bytes) :to-equal 0))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(it-sequential "x86-idiv-sequence-size quotient"
  (destructuring-bind (remainder-p) (list nil)
    (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-idiv-sequence cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ remainder-p s)))))
    (expect (length bytes) :to-equal 18))))

(it-sequential "x86-idiv-sequence-size remainder"
  (destructuring-bind (remainder-p) (list t)
    (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-idiv-sequence cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ remainder-p s)))))
    (expect (length bytes) :to-equal 18))))

;;; ─── emit-vm-program prologue/epilogue ───────────────────────────────────

(it-sequential "x86-vm-program-output"
  (let* ((non-empty-insts (list (cl-cc:make-vm-halt :reg :R0)
                                (cl-cc:make-vm-ret :reg :R0)))
         (full-prog (cl-cc/vm::make-vm-program :instructions non-empty-insts :result-register :R0))
         (full-bytes (cl-cc/codegen::compile-to-x86-64-bytes full-prog))
         (empty-prog (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (empty-bytes (cl-cc/codegen::compile-to-x86-64-bytes empty-prog)))
    (expect (> (length full-bytes) 0) :to-be-truthy)
    (expect (> (length empty-bytes) 0) :to-be-truthy)))

(it-sequential "x86-vm-program-leaf-red-zone-spills-skip-rbp-frame"
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc:make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                           :spill-count 1
                                           :instructions (cl-cc/vm::vm-program-instructions prog)))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                   (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (expect (= #x55 (first bytes)) :to-be-falsy)
    (expect (subseq bytes 0 5) :to-equal '(#x48 #x89 #x44 #x24 #xF8))
    (expect (subseq bytes 5 10) :to-equal '(#x48 #x8B #x5C #x24 #xF8))
    (expect (car (last bytes)) :to-equal #xC3)))

(it-sequential "x86-vm-program-default-fpe-allocates-rsp-spill-frame"
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc:make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p nil))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions prog)))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                  (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (expect (= #x55 (first bytes)) :to-be-falsy)
    (expect (subseq bytes 0 7) :to-equal '(#x48 #x81 #xEC #x08 #x00 #x00 #x00))
    (expect (subseq bytes 7 11) :to-equal '(#x48 #x89 #x04 #x24))
    (expect (subseq bytes 11 15) :to-equal '(#x48 #x8B #x1C #x24))
    (expect (subseq bytes 15 22) :to-equal '(#x48 #x81 #xC4 #x08 #x00 #x00 #x00))
    (expect (nth 22 bytes) :to-equal #xC3)))

(it-sequential "x86-vm-program-debug-opt-out-keeps-rbp-spills"
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc:make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p nil))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions prog)))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra)
                      (cl-cc/codegen::*x86-64-omit-frame-pointer* nil))
                  (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (expect (first bytes) :to-equal #x55)
    (expect (subseq bytes 1 4) :to-equal '(#x48 #x89 #xE5))
    (expect (subseq bytes 4 8) :to-equal '(#x48 #x89 #x45 #xF8))
    (expect (subseq bytes 8 12) :to-equal '(#x48 #x8B #x5D #xF8))
    (expect (nth 12 bytes) :to-equal #xC9)
    (expect (nth 13 bytes) :to-equal #xC3)))

(it-sequential "x86-stack-probe-count-thresholds below-page"
  (destructuring-bind (frame-size expected) (list 4095 0)
    (expect (= expected (cl-cc/codegen::stack-probe-count frame-size)) :to-be-truthy)))

(it-sequential "x86-stack-probe-count-thresholds one-page"
  (destructuring-bind (frame-size expected) (list 4096 1)
    (expect (= expected (cl-cc/codegen::stack-probe-count frame-size)) :to-be-truthy)))

(it-sequential "x86-stack-probe-count-thresholds two-pages"
  (destructuring-bind (frame-size expected) (list 8192 2)
    (expect (= expected (cl-cc/codegen::stack-probe-count frame-size)) :to-be-truthy)))

(it-sequential "x86-stack-probe-emits-non-mutating-rsp-page-touch"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-or-mem-rsp-disp32-imm8 -4096 0 s)))))
    (expect bytes :to-equal '(#x48 #x83 #x8C #x24 #x00 #xF0 #xFF #xFF #x00))))

(it-sequential "x86-large-spill-frame-inserts-stack-probe-before-rsp-allocation"
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions nil
                :result-register :R0
                :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                    :spill-count 513
                                                    :instructions nil))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                   (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (expect (subseq bytes 0 9) :to-equal '(#x48 #x83 #x8C #x24 #x00 #xF0 #xFF #xFF #x00))
    (expect (subseq bytes 9 16) :to-equal '(#x48 #x81 #xEC #x08 #x10 #x00 #x00))
    (expect (subseq bytes 16 23) :to-equal '(#x48 #x81 #xC4 #x08 #x10 #x00 #x00))
    (expect (= #xC3 (nth 23 bytes)) :to-be-truthy)))

;;; ─── instruction-size for specific types ────────────────────────────────

(it-sequential "x86-instruction-size-values vm-const"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-const    :dst :R0 :value 0) 10)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-move"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-move     :dst :R0 :src :R1) 3)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-add"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-add      :dst :R0 :lhs :R1 :rhs :R2) 6)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-sub"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-sub      :dst :R0 :lhs :R1 :rhs :R2) 6)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-mul"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-mul      :dst :R0 :lhs :R1 :rhs :R2) 7)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-integer-mul-high-u"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-integer-mul-high-u :dst :R0 :lhs :R1 :rhs :R2) 19)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-integer-mul-high-s"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-integer-mul-high-s :dst :R0 :lhs :R1 :rhs :R2) 19)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-jump"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-jump     :label "L") 2)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-ret"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-ret) 1)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-abs"
  (destructuring-bind (inst expected) (list (make-vm-abs             :dst :R0 :src :R1) 15)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-ash"
  (destructuring-bind (inst expected) (list (make-vm-ash             :dst :R0 :lhs :R1 :rhs :R2) 24)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-div"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-div      :dst :R0 :lhs :R1 :rhs :R2) 34)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-mod"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-mod      :dst :R0 :lhs :R1 :rhs :R2) 37)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-logtest"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-logtest  :dst :R0 :lhs :R1 :rhs :R2) 14)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))

(it-sequential "x86-instruction-size-values vm-logbitp"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-logbitp  :dst :R0 :lhs :R1 :rhs :R2) 15)
    (expect (cl-cc/codegen::instruction-size inst) :to-equal expected)))
