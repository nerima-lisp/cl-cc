(in-package :cl-cc/test)



(it-sequential "riscv64-source-scaffold-loads"
  :timeout
  30
  (expect (asdf:find-system :cl-cc-emit nil) :to-be-truthy)
  (expect (fboundp 'cl-cc/emit:make-riscv64-assembler) :to-be-truthy)
  (expect (fboundp 'cl-cc/emit:riscv64-emit-instruction) :to-be-truthy))

(it-sequential "riscv64-emits-concrete-addi-encoding"
  :timeout
  10
  (let ((assembler (cl-cc/emit:make-riscv64-assembler)))
    (cl-cc/emit:riscv64-emit-instruction
     assembler
     '(:addi :a0 :zero 42))
    (expect (cl-cc/emit:riscv64-emit-bytes assembler)
            :to-equalp
            #(19 5 160 2))))

(defun %riscv64-collect-bytes (emit-fn)
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

(defun %riscv64-word (bytes offset)
  (logior (nth offset bytes)
          (ash (nth (+ offset 1) bytes) 8)
           (ash (nth (+ offset 2) bytes) 16)
           (ash (nth (+ offset 3) bytes) 24)))

(defun %riscv64-emit-list (&rest instructions)
  (let ((asm (cl-cc/emit:make-riscv64-assembler)))
    (dolist (inst instructions)
      (cl-cc/emit:riscv64-emit-instruction asm inst))
    (coerce (cl-cc/emit:riscv64-emit-bytes asm) 'list)))

(it-sequential "riscv64-r-type-encodings"
  (expect (%riscv64-emit-list '(:add :a0 :a1 :a2)) :to-equal '(#x33 #x85 #xc5 #x00))
  (expect (%riscv64-emit-list '(:sub :a0 :a1 :a2)) :to-equal '(#x33 #x85 #xc5 #x40))
  (expect (%riscv64-emit-list '(:and :a0 :a1 :a2)) :to-equal '(#x33 #xf5 #xc5 #x00))
  (expect (%riscv64-emit-list '(:or :a0 :a1 :a2)) :to-equal '(#x33 #xe5 #xc5 #x00))
  (expect (%riscv64-emit-list '(:xor :a0 :a1 :a2)) :to-equal '(#x33 #xc5 #xc5 #x00))
  (expect (%riscv64-emit-list '(:sll :a0 :a1 :a2)) :to-equal '(#x33 #x95 #xc5 #x00))
  (expect (%riscv64-emit-list '(:srl :a0 :a1 :a2)) :to-equal '(#x33 #xd5 #xc5 #x00))
  (expect (%riscv64-emit-list '(:sra :a0 :a1 :a2)) :to-equal '(#x33 #xd5 #xc5 #x40))
  (expect (%riscv64-emit-list '(:mul :a0 :a1 :a2)) :to-equal '(#x33 #x85 #xc5 #x02))
  (expect (%riscv64-emit-list '(:div :a0 :a1 :a2)) :to-equal '(#x33 #xc5 #xc5 #x02))
  (expect (%riscv64-emit-list '(:rem :a0 :a1 :a2)) :to-equal '(#x33 #xe5 #xc5 #x02)))

(it-sequential "riscv64-i-load-s-branch-u-j-encodings"
  (expect (%riscv64-emit-list '(:addi :a0 :a1 -1)) :to-equal '(#x13 #x85 #xf5 #xff))
  (expect (%riscv64-emit-list '(:andi :a0 :a1 255)) :to-equal '(#x13 #xf5 #xf5 #x0f))
  (expect (%riscv64-emit-list '(:slli :a0 :a1 3)) :to-equal '(#x13 #x95 #x35 #x00))
  (expect (%riscv64-emit-list '(:ld :a0 :sp 8)) :to-equal '(#x03 #x35 #x81 #x00))
  (expect (%riscv64-emit-list '(:lw :a0 :sp 4)) :to-equal '(#x03 #x25 #x41 #x00))
  (expect (%riscv64-emit-list '(:sd :ra :sp 0)) :to-equal '(#x23 #x30 #x11 #x00))
  (expect (%riscv64-emit-list '(:sw :a0 :sp 4)) :to-equal '(#x23 #x22 #xa1 #x00))
  (expect (%riscv64-emit-list '(:beq :a0 :a1 16)) :to-equal '(#x63 #x08 #xb5 #x00))
  (expect (%riscv64-emit-list '(:lui :a0 #x12345000)) :to-equal '(#x37 #x55 #x34 #x12))
  (expect (%riscv64-emit-list '(:auipc :a0 0)) :to-equal '(#x17 #x05 #x00 #x00))
  (expect (%riscv64-emit-list '(:jal :ra 2048)) :to-equal '(#xef #x00 #x10 #x00))
  (expect (%riscv64-emit-list '(:ret)) :to-equal '(#x67 #x80 #x00 #x00)))

(it-sequential "riscv64-immediate-and-pic-sequences"
  (expect (%riscv64-emit-list '(:li :a0 #x12367)) :to-equal '(#x37 #x25 #x01 #x00 #x13 #x05 #x75 #x36))
  (expect (%riscv64-emit-list '(:pic-call :ra 1024 :t0)) :to-equal '(#x97 #x02 #x00 #x00 #xe7 #x80 #x02 #x40)))

(it-sequential "riscv64-floating-point-encodings"
  (expect (%riscv64-emit-list '(:fadd.d :fa0 :fa1 :fa2)) :to-equal '(#x53 #x85 #xc5 #x02))
  (expect (%riscv64-emit-list '(:fsub.d :fa0 :fa1 :fa2)) :to-equal '(#x53 #x85 #xc5 #x0a))
  (expect (%riscv64-emit-list '(:fmul.d :fa0 :fa1 :fa2)) :to-equal '(#x53 #x85 #xc5 #x12))
  (expect (%riscv64-emit-list '(:fdiv.d :fa0 :fa1 :fa2)) :to-equal '(#x53 #x85 #xc5 #x1a))
  (expect (%riscv64-emit-list '(:fld :fa0 :sp 8)) :to-equal '(#x07 #x35 #x81 #x00))
  (expect (%riscv64-emit-list '(:fsd :fa0 :sp 8)) :to-equal '(#x27 #x34 #xa1 #x00))
  (expect (%riscv64-emit-list '(:fmv.d.x :fa0 :a0)) :to-equal '(#x53 #x05 #x05 #xf2))
  (expect (%riscv64-emit-list '(:fmv.x.d :a0 :fa0)) :to-equal '(#x53 #x05 #x05 #xe2)))

(it-sequential "riscv64-compressed-encodings"
  (expect (%riscv64-emit-list '(:c.addi :sp -16)) :to-equal '(#x41 #x11))
  (expect (%riscv64-emit-list '(:c.ld :s0 :s1 8)) :to-equal '(#x80 #x64))
  (expect (%riscv64-emit-list '(:c.sd :s0 :s1 8)) :to-equal '(#x80 #xe4))
  (expect (%riscv64-emit-list '(:c.j 4)) :to-equal '(#x11 #xa0))
  (expect (%riscv64-emit-list '(:c.jr :ra)) :to-equal '(#x82 #x80))
  (expect (%riscv64-emit-list '(:c.beqz :s0 4)) :to-equal '(#x11 #xc0))
  (expect (%riscv64-emit-list '(:c.bnez :s0 4)) :to-equal '(#x11 #xe0)))

(it-sequential "riscv64-function-prologue-epilogue"
  (let ((bytes (coerce (cl-cc/emit:riscv64-emit-function
                        'sample '((:add :a0 :a0 :a1))
                        :save-regs '(:s0 :s1) :stack-size 8)
                       'list)))
    (expect (length bytes) :to-equal 36)
    (expect (subseq bytes 0 4) :to-equal '(#x13 #x01 #x01 #xfe))
    (expect (subseq bytes 4 8) :to-equal '(#x23 #x30 #x11 #x00))
    (expect (subseq bytes 16 20) :to-equal '(#x33 #x05 #xb5 #x00))
    (expect (subseq bytes 20 24) :to-equal '(#x83 #x30 #x01 #x00))
    (expect (subseq bytes 28 32) :to-equal '(#x13 #x01 #x01 #x02))
    (expect (subseq bytes 32 36) :to-equal '(#x67 #x80 #x00 #x00))))

(it-sequential "riscv64-zicond-encoding-fields"
  (let ((eqz (cl-cc/codegen::encode-rv-czero-eqz 1 2 3))
        (nez (cl-cc/codegen::encode-rv-czero-nez 4 5 6)))
    (expect (ldb (byte 7 0) eqz) :to-equal #x33)
    (expect (ldb (byte 3 12) eqz) :to-equal #b101)
    (expect (ldb (byte 7 25) eqz) :to-equal cl-cc/codegen::+rv-zicond-funct7+)
    (expect (ldb (byte 7 0) nez) :to-equal #x33)
    (expect (ldb (byte 3 12) nez) :to-equal #b111)
    (expect (ldb (byte 7 25) nez) :to-equal cl-cc/codegen::+rv-zicond-funct7+)))

(it-sequential "riscv64-emit-zicond-pseudo-ops"
  (let ((eqz (cl-cc/emit::encode-rv-czero-eqz 10 11 12))
        (nez (cl-cc/emit::encode-rv-czero-nez 10 11 12)))
    (expect (%riscv64-emit-list '(:czero.eqz :a0 :a1 :a2)) :to-equal (list (ldb (byte 8 0) eqz)
                        (ldb (byte 8 8) eqz)
                        (ldb (byte 8 16) eqz)
                        (ldb (byte 8 24) eqz)))
    (expect (%riscv64-emit-list '(:czero.nez :a0 :a1 :a2)) :to-equal (list (ldb (byte 8 0) nez)
                        (ldb (byte 8 8) nez)
                        (ldb (byte 8 16) nez)
                        (ldb (byte 8 24) nez)))))

(it-sequential "riscv64-select-emits-zicond-sequence"
  (let* ((assignment (make-hash-table :test #'eq))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment assignment))
         (inst (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)))
    (setf (gethash :R0 assignment) :a0
          (gethash :R1 assignment) :a1
          (gethash :R2 assignment) :a2
          (gethash :R3 assignment) :a3)
    (let* ((bytes (let ((cl-cc/codegen::*current-riscv64-regalloc* ra))
                    (%riscv64-collect-bytes
                     (lambda (s) (cl-cc/codegen::emit-riscv64-vm-select inst s)))))
           (w0 (%riscv64-word bytes 0))
           (w1 (%riscv64-word bytes 4))
           (w2 (%riscv64-word bytes 8)))
      (expect (length bytes) :to-equal 12)
      (expect w0 :to-equal (cl-cc/codegen::encode-rv-czero-eqz
                     cl-cc/codegen::+rv-t0+ cl-cc/codegen::+rv-a2+ cl-cc/codegen::+rv-a1+))
      (expect w1 :to-equal (cl-cc/codegen::encode-rv-czero-nez
                     cl-cc/codegen::+rv-a0+ cl-cc/codegen::+rv-a3+ cl-cc/codegen::+rv-a1+))
      (expect w2 :to-equal (cl-cc/codegen::encode-rv-or
                     cl-cc/codegen::+rv-a0+ cl-cc/codegen::+rv-a0+ cl-cc/codegen::+rv-t0+))
      (dolist (word (list w0 w1 w2))
        (expect (= (ldb (byte 7 0) word) #x63) :to-be-falsy)))))

(it-sequential "riscv64-vm-select-size-accounts-for-zicond-sequence"
  (expect (cl-cc/codegen::riscv64-instruction-size
                 (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)) :to-equal 12))
