;;;; tests/unit/emit/aarch64-encoding-tests.lisp — AArch64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/aarch64-codegen.lisp encoding functions:
;;;; encode-movz, encode-movk, encode-mov-rr, encode-add, encode-sub,
;;;; encode-mul, encode-umulh, encode-smulh, encode-cbz, encode-b,
;;;; encode-blr, encode-stur, encode-ldur,
;;;; encode-stp-pre, encode-ldp-post, +a64-ret+, emit-a64-instr.

(in-package :cl-cc/test)



;;; ─── encode-movz ─────────────────────────────────────────────────────────

(it-sequential "a64-movz-base-cases x0"
  (destructuring-bind (expected rd) (list #xD2800000 0)
    (expect (cl-cc/codegen::encode-movz rd 0) :to-equal expected)))

(it-sequential "a64-movz-base-cases x1"
  (destructuring-bind (expected rd) (list #xD2800001 1)
    (expect (cl-cc/codegen::encode-movz rd 0) :to-equal expected)))

(it-sequential "a64-movz-imm16-at-bits-20-5"
  (let ((word (cl-cc/codegen::encode-movz 0 42)))
    (expect (logand (ash word -5) #xFFFF) :to-equal 42)
    (expect (logand word #x1F) :to-equal 0)))

(it-sequential "a64-movz-hw-field-at-bits-22-21"
  (let ((word (cl-cc/codegen::encode-movz 0 1 1)))
    (expect (logand (ash word -21) 3) :to-equal 1)
    (expect (logand (ash word -5) #xFFFF) :to-equal 1)))

(it-sequential "a64-movz-max-imm16-roundtrips"
  (let ((word (cl-cc/codegen::encode-movz 0 #xFFFF)))
    (expect (logand (ash word -5) #xFFFF) :to-equal #xFFFF)))

;;; ─── encode-movk ─────────────────────────────────────────────────────────

(it-sequential "a64-movk-encodes-base-opcode-and-imm16-hw-fields"
  (let ((base-word (cl-cc/codegen::encode-movk 0 0))
        (imm-word  (cl-cc/codegen::encode-movk 0 #x1234 2)))
    (expect (logand base-word #xFF800000) :to-equal #xF2800000)
    (expect (logand (ash imm-word -5) #xFFFF) :to-equal #x1234)
    (expect (logand (ash imm-word -21) 3) :to-equal 2)))

(it-sequential "a64-mov-rr-encodes-rd-and-rm-correctly"
  (let ((w01 (cl-cc/codegen::encode-mov-rr 0 1))
        (w55 (cl-cc/codegen::encode-mov-rr 5 5)))
    (expect (logand w01 #x1F) :to-equal 0)
    (expect (logand (ash w01 -16) #x1F) :to-equal 1)
    (expect (logand w55 #x1F) :to-equal 5)
    (expect (logand (ash w55 -16) #x1F) :to-equal 5)))

;;; ─── encode-add ──────────────────────────────────────────────────────────

(it-sequential "a64-add-register-fields low-regs"
  (destructuring-bind (rd rn rm expected-opcode-bits) (list 0 1 2 #x8B000000)
    (let ((word (cl-cc/codegen::encode-add rd rn rm)))
    (expect (logand word #x1F) :to-equal rd)
    (expect (logand (ash word -5) #x1F) :to-equal rn)
    (expect (logand (ash word -16) #x1F) :to-equal rm)
    (expect (logand word #xFFE00000) :to-equal expected-opcode-bits))))

(it-sequential "a64-add-register-fields high-regs"
  (destructuring-bind (rd rn rm expected-opcode-bits) (list 28 29 30 #x8B000000)
    (let ((word (cl-cc/codegen::encode-add rd rn rm)))
    (expect (logand word #x1F) :to-equal rd)
    (expect (logand (ash word -5) #x1F) :to-equal rn)
    (expect (logand (ash word -16) #x1F) :to-equal rm)
    (expect (logand word #xFFE00000) :to-equal expected-opcode-bits))))

;;; ─── encode-sub ──────────────────────────────────────────────────────────

(it-sequential "a64-sub-encodes-rd-rn-rm-and-opcode"
  (let ((word (cl-cc/codegen::encode-sub 0 1 2)))
    (expect (logand word #x1F) :to-equal 0)
    (expect (logand (ash word -5) #x1F) :to-equal 1)
    (expect (logand (ash word -16) #x1F) :to-equal 2)
    (expect (logand word #xFFE00000) :to-equal #xCB000000)))

(it-sequential "a64-sub-opcode-differs-from-add-but-registers-match"
  (let ((add (cl-cc/codegen::encode-add 0 1 2))
        (sub (cl-cc/codegen::encode-sub 0 1 2)))
    (expect (= add sub) :to-be-falsy)
    (expect (logand sub #x1FFFFF) :to-equal (logand add #x1FFFFF))))

(it-sequential "a64-mul-encodes-as-madd-with-xzr-accumulator"
  (let ((word (cl-cc/codegen::encode-mul 0 1 2)))
    (expect (logand word #x1F) :to-equal 0)
    (expect (logand (ash word -5) #x1F) :to-equal 1)
    (expect (logand (ash word -16) #x1F) :to-equal 2)
    (expect (logand (ash word -10) #x1F) :to-equal 31)))

(it-sequential "a64-mul-high-encoders"
  (expect (cl-cc/codegen::encode-umulh 0 1 2) :to-equal #x9BC27C20)
  (expect (cl-cc/codegen::encode-smulh 0 1 2) :to-equal #x9B427C20))

(it-sequential "a64-neon-simd-encoders"
  (expect (cl-cc/codegen::encode-neon-add4s 0 1 2) :to-equal #x4EA28420)
  (expect (cl-cc/codegen::encode-neon-sub4s 0 1 2) :to-equal #x6EA28420)
  (expect (cl-cc/codegen::encode-neon-mul4s 0 1 2) :to-equal #x4EA29C20)
  (expect (cl-cc/codegen::encode-neon-and16b 0 1 2) :to-equal #x4E221C20)
  (expect (cl-cc/codegen::encode-neon-orr16b 0 1 2) :to-equal #x4EA21C20)
  (expect (cl-cc/codegen::encode-neon-eor16b 0 1 2) :to-equal #x6E221C20)
  (expect (cl-cc/codegen::encode-neon-ld1-4s 0 1) :to-equal #x4C407820)
  (expect (cl-cc/codegen::encode-neon-st1-4s 0 1) :to-equal #x4C007820))

(it-sequential "a64-sve-and-sve2-representative-encoders"
  (let ((rdvl (cl-cc/codegen::encode-rdvl 3 4))
        (whilelt (cl-cc/codegen::encode-whilelt-d 2 5 6))
        (sve-add (cl-cc/codegen::encode-sve-add-z 7 3 8 9))
        (sve2-eor (cl-cc/codegen::encode-sve2-eor-z 10 4 11 12)))
    (expect (logand rdvl #xFFFFF000) :to-equal #x04BF5000)
    (expect (logand rdvl #x1F) :to-equal 3)
    (expect (logand (ash rdvl -5) #x3F) :to-equal 4)
    (expect (logand whilelt #xF) :to-equal 2)
    (expect (logand (ash whilelt -5) #x1F) :to-equal 5)
    (expect (logand (ash whilelt -16) #x1F) :to-equal 6)
    (expect (logand sve-add #x1F) :to-equal 7)
    (expect (logand (ash sve-add -10) #x7) :to-equal 3)
    (expect (logand (ash sve-add -5) #x1F) :to-equal 8)
    (expect (logand (ash sve-add -16) #x1F) :to-equal 9)
    (expect (logand sve2-eor #xFFE00000) :to-equal #x04C00000)
    (expect (logand sve2-eor #x1F) :to-equal 10)))

(it-sequential "a64-sme-representative-encoders-and-gate"
  (expect (cl-cc/codegen:encode-smstart :sm) :to-equal #xD503437F)
  (expect (cl-cc/codegen:encode-smstart :za) :to-equal #xD503457F)
  (expect (cl-cc/codegen:encode-smstart :both) :to-equal #xD503477F)
  (expect (cl-cc/codegen:encode-smstop :sm) :to-equal #xD503427F)
  (let ((fmopa (cl-cc/codegen:encode-fmopa 1 2 3 4 5)))
    (expect (logand fmopa #x1F) :to-equal 1)
    (expect (logand (ash fmopa -10) #x7) :to-equal 2)
    (expect (logand (ash fmopa -13) #x7) :to-equal 3)
    (expect (logand (ash fmopa -5) #x1F) :to-equal 4)
    (expect (logand (ash fmopa -16) #x1F) :to-equal 5))
  (let ((cl-cc/codegen:*sme-enabled* nil))
    (expect (cl-cc/codegen:aarch64-supports-sme-p) :to-be-falsy))
  (let ((cl-cc/codegen:*sme-enabled* t))
    (expect (cl-cc/codegen:aarch64-supports-sme-p) :to-be-truthy)))

(it-sequential "a64-mte-safe-stack-and-xom-marker-encoders"
  (let ((load-bytes nil)
        (store-bytes nil))
    (let ((cl-cc/codegen:*aarch64-safe-stack-enabled* nil))
      (cl-cc/codegen::emit-a64-safe-stack-load-pointer 0 (lambda (b) (push b load-bytes))))
    (expect load-bytes :to-be-null)
    (let ((cl-cc/codegen:*aarch64-safe-stack-enabled* t))
      (cl-cc/codegen::emit-a64-safe-stack-load-pointer 0 (lambda (b) (push b load-bytes)))
      (cl-cc/codegen::emit-a64-safe-stack-store-pointer 1 (lambda (b) (push b store-bytes))))
    (expect (length load-bytes) :to-equal 8)
    (expect (length store-bytes) :to-equal 8))
  (expect (logand (cl-cc/codegen::encode-brk 0) #xFFE0001F) :to-equal #xD4200000)
  (expect (cl-cc/codegen::encode-brk 1) :to-equal #xD4200020))

(it-sequential "a64-simd-marker-lowers-to-neon-sequence"
  (let ((bytes nil)
        (inst (make-vm-simd-vector-op :op :add :dst-array :r3 :lhs-array :r1
                                      :rhs-array :r2 :index-reg :r4 :lanes 4
                                      :element-type :i32)))
    (cl-cc/codegen::emit-a64-vm-simd-vector-op inst (lambda (b) (push b bytes)))
    (setf bytes (nreverse bytes))
    (expect (length bytes) :to-equal 40)
    (expect (cl-cc/codegen::a64-instruction-size inst) :to-equal 40)
    (expect (search '(#x00 #x7A #x40 #x4C) bytes :test #'=) :to-be-truthy)
    (expect (search '(#x00 #x84 #xA1 #x4E) bytes :test #'=) :to-be-truthy)
    (expect (search '(#x00 #x7A #x00 #x4C) bytes :test #'=) :to-be-truthy)))

(it-sequential "a64-fsqrt-encoder"
  (expect (cl-cc/codegen::encode-fsqrt 0 1) :to-equal #x1E61C020))

;;; ─── encode-cbz ──────────────────────────────────────────────────────────

(it-sequential "a64-cbz-encodes-rt-imm19-and-opcode"
  (let ((w0 (cl-cc/codegen::encode-cbz 0 1))
        (w5 (cl-cc/codegen::encode-cbz 5 10)))
    (expect (logand w0 #x1F) :to-equal 0)
    (expect (logand (ash w0 -5) #x7FFFF) :to-equal 1)
    (expect (logand w0 #xFF000000) :to-equal #xB4000000)
    (expect (logand w5 #x1F) :to-equal 5)
    (expect (logand (ash w5 -5) #x7FFFF) :to-equal 10)))

(it-sequential "a64-b-encodes-imm26-and-opcode"
  (let ((w2 (cl-cc/codegen::encode-b 2)))
    (expect (logand w2 #x3FFFFFF) :to-equal 2)
    (expect (logand w2 #xFC000000) :to-equal #x14000000))
  (expect (cl-cc/codegen::encode-b 0) :to-equal #x14000000))

(it-sequential "a64-blr-encodes-register-at-bits-9-5"
  (let ((w0  (cl-cc/codegen::encode-blr 0))
        (w30 (cl-cc/codegen::encode-blr 30)))
    (expect (logand (ash w0  -5) #x1F) :to-equal 0)
    (expect (logand w0 #xFFFFFC00) :to-equal #xD63F0000)
    (expect (logand (ash w30 -5) #x1F) :to-equal 30)))

;;; ─── +a64-ret+ ──────────────────────────────────────────────────────────

;;; ─── encode-stur ─────────────────────────────────────────────────────────

(it-sequential "a64-stur-encodes-rt-rn-and-simm9"
  (let ((w8 (cl-cc/codegen::encode-stur 0 29 8))
        (w0 (cl-cc/codegen::encode-stur 1 31 0)))
    (expect (logand w8 #x1F) :to-equal 0)
    (expect (logand (ash w8 -5) #x1F) :to-equal 29)
    (expect (logand (ash w8 -12) #x1FF) :to-equal 8)
    (expect (logand w0 #x1F) :to-equal 1)
    (expect (logand (ash w0 -5) #x1F) :to-equal 31)
    (expect (logand (ash w0 -12) #x1FF) :to-equal 0)))

(it-sequential "a64-ldur-encodes-correctly-with-load-opcode"
  (let ((word (cl-cc/codegen::encode-ldur 0 29 8)))
    (expect (logand word #x1F) :to-equal 0)
    (expect (logand (ash word -5) #x1F) :to-equal 29)
    (expect (logand (ash word -12) #x1FF) :to-equal 8)
    (expect (logand word #xFFE00000) :to-equal #xF8400000)))

(it-sequential "a64-ldur-and-stur-differ-only-in-opcode"
  (let ((ld (cl-cc/codegen::encode-ldur 0 1 8))
        (st (cl-cc/codegen::encode-stur 0 1 8)))
    (expect (= ld st) :to-be-falsy)
    (expect (logand st #x1FFFFF) :to-equal (logand ld #x1FFFFF))))

;;; ─── encode-stp-pre ──────────────────────────────────────────────────────

(it-sequential "a64-stack-pair-encoding"
  (let ((stp (cl-cc/codegen::encode-stp-pre 29 30 31 (logand -2 #x7F))))
    (expect (logand stp #x1F) :to-equal 29)
    (expect (logand (ash stp -10) #x1F) :to-equal 30)
    (expect (logand (ash stp -5) #x1F) :to-equal 31))
  (let ((ldp (cl-cc/codegen::encode-ldp-post 29 30 31 2)))
    (expect (logand ldp #x1F) :to-equal 29)
    (expect (logand (ash ldp -10) #x1F) :to-equal 30)
    (expect (logand (ash ldp -5) #x1F) :to-equal 31)
    (expect (logand (ash ldp -15) #x7F) :to-equal 2)))

;;; ─── +a64-ret+ and emit-a64-instr ──────────────────────────────────────

(it-sequential "a64-ret-constant-has-fixed-encoding"
  (expect cl-cc/codegen::+a64-ret+ :to-equal #xD65F03C0))

(it-sequential "a64-emit-instr-writes-four-little-endian-bytes"
  (let ((bytes-ret nil)
        (bytes-zero nil))
    (cl-cc/codegen::emit-a64-instr #xD65F03C0 (lambda (b) (push b bytes-ret)))
    (setf bytes-ret (nreverse bytes-ret))
    (expect (length bytes-ret) :to-equal 4)
    (expect (first bytes-ret) :to-equal #xC0)
    (expect (second bytes-ret) :to-equal #x03)
    (expect (third bytes-ret) :to-equal #x5F)
    (expect (fourth bytes-ret) :to-equal #xD6)
    (cl-cc/codegen::emit-a64-instr 0 (lambda (b) (push b bytes-zero)))
    (expect (length (nreverse bytes-zero)) :to-equal 4)
    (expect (every #'zerop bytes-zero) :to-be-truthy)))

;;; ─── Special register constants ──────────────────────────────────────────

(it-sequential "a64-special-register-constants sp"
  (destructuring-bind (constant expected) (list cl-cc/codegen::+a64-sp+ 31)
    (expect constant :to-equal expected)))

(it-sequential "a64-special-register-constants fp"
  (destructuring-bind (constant expected) (list cl-cc/codegen::+a64-fp+ 29)
    (expect constant :to-equal expected)))

(it-sequential "a64-special-register-constants lr"
  (destructuring-bind (constant expected) (list cl-cc/codegen::+a64-lr+ 30)
    (expect constant :to-equal expected)))

;;; ─── Register mapping ───────────────────────────────────────────────────

(it-sequential "a64-reg-number-table x0"
  (destructuring-bind (reg verify) (list :x0 (lambda (entry)
             (assert-equal 0 (cdr entry))))
    (let ((entry (assoc reg cl-cc/codegen::*aarch64-reg-number*)))
    (funcall verify entry))))

(it-sequential "a64-reg-number-table x7"
  (destructuring-bind (reg verify) (list :x7 (lambda (entry)
             (assert-equal 7 (cdr entry))))
    (let ((entry (assoc reg cl-cc/codegen::*aarch64-reg-number*)))
    (funcall verify entry))))

(it-sequential "a64-reg-number-table x30"
  (destructuring-bind (reg verify) (list :x30 (lambda (entry)
             (assert-equal 30 (cdr entry))))
    (let ((entry (assoc reg cl-cc/codegen::*aarch64-reg-number*)))
    (funcall verify entry))))

(it-sequential "a64-reg-number-table x18-absent"
  (destructuring-bind (reg verify) (list :x18 (lambda (entry)
             (assert-null entry)))
    (let ((entry (assoc reg cl-cc/codegen::*aarch64-reg-number*)))
    (funcall verify entry))))
