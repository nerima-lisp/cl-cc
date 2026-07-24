;;;; packages/emit/src/x86-64-encoding-instrs.lisp - x86-64 Instruction Emitters
;;;
;;; Individual instruction emit functions: MOV, ADD, SUB, IMUL, MUL, CMP, PUSH,
;;; POP, RET, CALL, JMP/JE, TEST, XMM scalar-double
;;; (MOVSD/ADDSD/SUBSD/MULSD/DIVSD), MOVQ, POPCNT, BSR.
;;;
;;; Depends on x86-64-encoding.lisp (emit-byte, emit-dword, emit-qword,
;;; rex-prefix, modrm, sib, scale->sib-bits, %emit-modrm-address,
;;; %emit-modrm-indexed-address, with-output-to-vector).

(in-package :cl-cc/codegen)

;;; ── MOV instructions ─────────────────────────────────────────────────────────

(defun emit-mov-rr64 (dst src stream)
  "MOV dst, src (64-bit register to register).

   Encoding: MOV r/m64, r64 (0x89): REG=src, R/M=dst
             REX.R extends REG (src), REX.B extends R/M (dst)"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x89 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-mov-ri64 (dst imm stream)
  "MOV dst, imm64 (64-bit immediate to register).

   Encoding: REX.W + B8+ rd"
  (emit-byte (rex-prefix :w 1 :b (ash dst -3)) stream)
  (emit-byte (+ #xB8 (logand dst #x7)) stream)
  (emit-qword imm stream))

(defun emit-mov-rm64 (dst base offset stream)
  "MOV dst, [base + offset] (load from memory).

    For offset = 0: REX.W + 8B /r (mod=00)
    For offset fits in byte: REX.W + 8B /r (mod=01)"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash base -3)) stream)
  (emit-byte #x8B stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst base offset stream))

(defun emit-mov-mr64 (base offset src stream)
  "MOV [base + offset], src (store to memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src base offset stream))

(defun emit-mov-rm64-rip (dst disp32 stream)
  "MOV dst, [RIP + disp32] (RIP-relative load).

   Encoding: REX.W + 8B /r with MOD=00, R/M=101, followed by disp32.
   DISP32 is relative to the address immediately after this instruction."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3)) stream)
  (emit-byte #x8B stream)
  (%emit-modrm-rip-relative dst disp32 stream))

(defun emit-mov-mr64-rip (disp32 src stream)
  "MOV [RIP + disp32], src (RIP-relative store).

   Encoding: REX.W + 89 /r with MOD=00, R/M=101, followed by disp32.
   DISP32 is relative to the address immediately after this instruction."
  (emit-byte (rex-prefix :w 1 :r (ash src -3)) stream)
  (emit-byte #x89 stream)
  (%emit-modrm-rip-relative src disp32 stream))

(defun emit-mov-rm64-fs-disp32 (dst disp32 stream)
  "MOV dst, FS:[disp32] using absolute disp32 addressing.

   Encoding: 64 REX.W 8B /r (mod=00 r/m=100) SIB(00,100,101) disp32"
  (emit-byte #x64 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst -3)) stream)
  (emit-byte #x8B stream)
  (emit-byte (modrm 0 dst 4) stream)
  (emit-byte (sib 0 4 5) stream)
  (emit-dword disp32 stream))

(defun emit-cmp-rm64-fs-disp32 (reg disp32 stream)
  "CMP reg, FS:[disp32] using absolute disp32 addressing.

   Encoding: 64 REX.W 3B /r (mod=00 r/m=100) SIB(00,100,101) disp32"
  (emit-byte #x64 stream)
  (emit-byte (rex-prefix :w 1 :r (ash reg -3)) stream)
  (emit-byte #x3B stream)
  (emit-byte (modrm 0 reg 4) stream)
  (emit-byte (sib 0 4 5) stream)
  (emit-dword disp32 stream))

(defun emit-mov-rm64-indexed (dst base index scale offset stream)
  "MOV dst, [base + index*scale + offset] (load from indexed memory)."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8B stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                               dst base index scale offset stream))

(defun emit-mov-mr64-indexed (base index scale offset src stream)
  "MOV [base + index*scale + offset], src (store to indexed memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                                src base index scale offset stream))

(defun emit-prefetch-mem (locality base offset stream)
  "Emit PREFETCHT0/PREFETCHNTA [BASE+OFFSET].  LOCALITY is :T0 or :NTA."
  (let ((opcode-ext (ecase locality
                      (:nta 0)
                      (:t0 1))))
    (emit-x86-64-address-rex-if-needed opcode-ext base nil stream)
    (emit-byte #x0F stream)
    (emit-byte #x18 stream)
    (%emit-modrm-address (x86-64-memory-mod base offset)
                         opcode-ext base offset stream)))

(defun emit-prefetch-mem-indexed (locality base index scale offset stream)
  "Emit PREFETCHT0/PREFETCHNTA [BASE+INDEX*SCALE+OFFSET]."
  (let ((opcode-ext (ecase locality
                      (:nta 0)
                      (:t0 1))))
    (emit-x86-64-address-rex-if-needed opcode-ext base index stream)
    (emit-byte #x0F stream)
    (emit-byte #x18 stream)
    (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                                 opcode-ext base index scale offset stream)))

;;; ── LEA instruction (FR-171) ─────────────────────────────────────────────────

(defun emit-lea-rr64 (dst base index stream &optional (scale 1))
  "LEA dst, [base + index*SCALE] — 64-bit load effective address.
SCALE must be 1, 2, 4, or 8.  Uses memory form with SIB byte.
Safety: base=RBP/R13 requires disp8/disp32 with mod=00; index=RSP/R12 is invalid.
The caller (emit-vm-integer-add) ensures base/index come from regalloc which
avoids RBP/R13/RSP/R12 for LEA operands."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8D stream)
  ;; MOD=00, REG=dst, R/M=100 (SIB follows).  Safe because regalloc avoids
  ;; RBP/R13 as base (would need disp8/32) and RSP/R12 as index (invalid).
  (emit-byte (logior (ash dst 3) #x04) stream)
  (emit-byte (sib (scale->sib-bits scale) index base) stream))

(defun emit-lea-rr64-offset (dst base offset stream)
  "LEA dst, [base + OFFSET] — 64-bit load effective address with displacement.
OFFSET must fit in signed 32 bits.  No index register or scale."
  (let ((mod (x86-64-memory-mod base offset)))
    (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash base -3)) stream)
     (emit-byte #x8D stream)
     (%emit-modrm-address mod dst base offset stream)))

(defun emit-lea-rip-relative (dst disp32 stream)
  "LEA dst, [RIP + disp32] — materialize a RIP-relative address."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3)) stream)
  (emit-byte #x8D stream)
  (%emit-modrm-rip-relative dst disp32 stream))

(defun emit-lea-indexed (dst base index scale offset stream)
  "LEA dst, [base + index*SCALE + OFFSET] — full indexed LEA.
SCALE must be 1, 2, 4, or 8.  OFFSET must fit in signed 32 bits."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8D stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                                 dst base index scale offset stream))

(defun emit-lea (dst base index scale offset stream)
  "LEA dst, [base + optional index*SCALE + OFFSET].

Encodes REX.W + 8D /r with ModR/M, optional SIB, and disp8/disp32.
SCALE must be one of 1, 2, 4, or 8 when INDEX is non-NIL."
  (if index
      (emit-lea-indexed dst base index scale offset stream)
      (emit-lea-rr64-offset dst base offset stream)))

;;; ── Integer arithmetic and compare ──────────────────────────────────────────

;;; These emitters are pure opcode-table entries: same REX/ModRM skeleton,
;;; one differing byte. The three defmacros below capture the skeleton (logic)
;;; so each instruction is declared as a single data row (mnemonic + opcode).

(defmacro define-alu-rr64 (name opcode description)
  "Define EMIT-<name>-RR64 — a REX.W <opcode> /r 64-bit reg,reg ALU emitter."
  `(defun ,(intern (format nil "EMIT-~A-RR64" name)) (dst src stream)
     ,description
     (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
     (emit-byte ,opcode stream)
     (emit-byte (modrm 3 src dst) stream)))

(defmacro define-alu-ri32 (name ext description)
  "Define EMIT-<name>-RI32 — a REX.W 81 /<ext> id 64-bit reg,imm32 ALU emitter."
  `(defun ,(intern (format nil "EMIT-~A-RI32" name)) (reg imm stream)
     ,description
     (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
     (emit-byte #x81 stream)
     (emit-byte (modrm 3 ,ext reg) stream)
     (emit-dword imm stream)))

(defmacro define-f7-unary-rm64 (name ext description)
  "Define EMIT-<name>-RM64 — a REX.W F7 /<ext> 64-bit unary r/m64 emitter."
  `(defun ,(intern (format nil "EMIT-~A-RM64" name)) (src stream)
     ,description
     (emit-byte (rex-prefix :w 1 :b (ash src -3)) stream)
     (emit-byte #xF7 stream)
     (emit-byte (modrm 3 ,ext src) stream)))

(define-alu-rr64 add #x01 "ADD dst, src (64-bit). Encoding: REX.W + 01 /r")
(define-alu-rr64 sub #x29 "SUB dst, src (64-bit). Encoding: REX.W + 29 /r")
(define-alu-rr64 cmp #x39 "CMP dst, src (64-bit). Encoding: REX.W + 39 /r")

(define-alu-ri32 add 0 "ADD reg, imm32 (sign-extended to 64-bit). REX.W + 81 /0 id")
(define-alu-ri32 sub 5 "SUB reg, imm32 (sign-extended to 64-bit). REX.W + 81 /5 id")

(define-f7-unary-rm64 mul  4 "MUL r/m64 (unsigned 64-bit multiply, implicit RAX and RDX:RAX result). REX.W + F7 /4")
(define-f7-unary-rm64 imul 5 "IMUL r/m64 (signed 64-bit multiply, implicit RAX and RDX:RAX result). REX.W + F7 /5")
(define-f7-unary-rm64 div  6 "DIV r/m64 (unsigned 64-bit divide RDX:RAX by SRC). REX.W + F7 /6. Quotient to RAX, remainder to RDX.")
(define-f7-unary-rm64 idiv 7 "IDIV r/m64 (signed 64-bit divide RDX:RAX by SRC). REX.W + F7 /7. Quotient to RAX, remainder to RDX.")

(defun emit-imul-rr64 (dst src stream)
  "IMUL dst, src (64-bit signed multiply).

   Encoding: REX.W + 0F AF /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xAF stream)
  (emit-byte (modrm 3 dst src) stream))

;;; ── Stack, control flow ───────────────────────────────────────────────────────

(defun emit-push-r64 (reg stream)
  "PUSH reg (64-bit).

   Encoding: [REX.B] 50+ rd. REX.B (#x41) required for R8-R15."
  (when (>= reg 8)
    (emit-byte #x41 stream))
  (emit-byte (+ #x50 (logand reg #x7)) stream))

(defun emit-pop-r64 (reg stream)
  "POP reg (64-bit).

   Encoding: [REX.B] 58+ rd. REX.B (#x41) required for R8-R15."
  (when (>= reg 8)
    (emit-byte #x41 stream))
  (emit-byte (+ #x58 (logand reg #x7)) stream))

(defun emit-leave (stream)
  "LEAVE (restore frame pointer): equivalent to MOV RSP,RBP; POP RBP.

   Encoding: C9"
  (emit-byte #xC9 stream))

(defun emit-ret (stream)
  "RET (return).

   Encoding: C3"
  (emit-byte #xC3 stream))

(defun emit-or-mem-rsp-disp32-imm8 (disp imm stream)
  "OR qword ptr [RSP + DISP32], IMM8.

   Used for stack probing: touching one address per guard page with
   OR [RSP-page], 0 preserves memory contents while faulting early if the
   page is not committed. Encoding: REX.W + 83 /1 id ib with RSP SIB."
  (emit-byte (rex-prefix :w 1) stream)
  (emit-byte #x83 stream)
  (emit-byte (modrm 2 1 4) stream)
  (emit-byte (sib 0 4 4) stream)
  (emit-dword disp stream)
  (emit-byte imm stream))

(defun emit-mov-m64-imm32 (base offset imm stream)
  "MOV qword ptr [BASE+OFFSET], sign-extended IMM32."
  (emit-byte (rex-prefix :w 1 :b (ash base -3)) stream)
  (emit-byte #xC7 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) 0 base offset stream)
  (emit-dword imm stream))

(defun emit-x86-64-lfence (stream)
  "Emit LFENCE."
  (emit-byte #x0F stream)
  (emit-byte #xAE stream)
  (emit-byte #xE8 stream))

(defun emit-x86-64-safe-stack-load-pointer (dst stream)
  "Load the SafeStack unsafe-stack pointer from FS TLS into DST when enabled."
  (when *x86-64-safe-stack-enabled*
    (emit-mov-rm64-fs-disp32 dst +x86-64-safe-stack-tls-disp32+ stream)))

(defun emit-x86-64-safe-stack-store-pointer (src stream)
  "Store SRC as the SafeStack unsafe-stack pointer in FS TLS when enabled."
  (when *x86-64-safe-stack-enabled*
    (emit-byte #x64 stream)
    (emit-byte (rex-prefix :w 1 :r (ash src -3)) stream)
    (emit-byte #x89 stream)
    (emit-byte (modrm 0 src 4) stream)
    (emit-byte (sib 0 4 5) stream)
    (emit-dword +x86-64-safe-stack-tls-disp32+ stream)))

(defun emit-call-r64 (reg stream)
  "CALL r/m64 (indirect call through register).

   Encoding: REX.W + FF /2"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xFF stream)
  (emit-byte (modrm 3 2 reg) stream))

(defun emit-jmp-r64 (reg stream)
  "JMP r/m64 (indirect jump through register).

   Encoding: REX.W + FF /4"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xFF stream)
  (emit-byte (modrm 3 4 reg) stream))

(defun emit-jmp-rel32 (offset stream)
  "JMP rel32 (near jump).

   Encoding: E9 cd"
  (emit-byte #xE9 stream)
  (emit-dword offset stream))

(defun emit-je-rel32 (offset stream)
  "JE rel32 (jump if equal).

   Encoding: 0F 84 cd"
  (emit-byte #x0F stream)
  (emit-byte #x84 stream)
  (emit-dword offset stream))

(defun emit-jo-rel32 (offset stream)
  "JO rel32 (jump if overflow).

   Encoding: 0F 80 cd"
  (emit-byte #x0F stream)
  (emit-byte #x80 stream)
  (emit-dword offset stream))

(defun emit-jno-rel32 (offset stream)
  "JNO rel32 (jump if not overflow).

   Encoding: 0F 81 cd"
  (emit-byte #x0F stream)
  (emit-byte #x81 stream)
  (emit-dword offset stream))

;;; ── Immediate compare and test ───────────────────────────────────────────────

(defun emit-cmp-ri64 (reg imm stream)
  "CMP reg, imm32 (compare register with 32-bit sign-extended immediate).

   Encoding: REX.W + 81 /7 id"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 7 reg) stream)
  (emit-dword imm stream))

(defun emit-cmp-ri32 (reg imm stream)
  "CMP reg32, imm32.

   Encoding: 81 /7 id (+ optional REX.B for reg >= 8)."
  (when (>= reg 8)
    (emit-byte (rex-prefix :b (ash reg -3)) stream))
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 7 reg) stream)
  (emit-dword imm stream))

(defun emit-test-rr64 (reg1 reg2 stream)
  "TEST reg1, reg2 (bitwise AND, set flags, discard result).

   Encoding: REX.W + 85 /r"
  (emit-byte (rex-prefix :w 1 :r (ash reg2 -3) :b (ash reg1 -3)) stream)
  (emit-byte #x85 stream)
  (emit-byte (modrm 3 reg2 reg1) stream))

(defun emit-je-short (offset stream)
  "JE rel8 (short conditional jump if equal/zero). 74 cb"
  (emit-byte #x74 stream)
  (emit-byte (logand offset #xFF) stream))

;; FR-403: Short jump encoders for branch displacement optimization

(defun emit-jmp-rel8 (offset stream)
  "JMP rel8 (short unconditional jump). EB cb"
  (emit-byte #xEB stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-jne-rel8 (offset stream)
  "JNE rel8 (short jump if not equal/not zero). 75 cb"
  (emit-byte #x75 stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-jns-rel8 (offset stream)
  "JNS rel8 (short jump if not sign / positive). 79 cb"
  (emit-byte #x79 stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-jge-rel8 (offset stream)
  "JGE rel8 (short jump if greater or equal, signed). 7D cb"
  (emit-byte #x7D stream)
  (emit-byte (logand offset #xFF) stream))

;;; ── Bit-manipulation instructions ────────────────────────────────────────────

(defun emit-popcnt-rr64 (dst src stream)
  "POPCNT dst, src (64-bit population count).

   Encoding: F3 REX.W 0F B8 /r"
  (emit-byte #xF3 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xB8 stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-lzcnt-rr64 (dst src stream)
  "LZCNT dst, src (64-bit leading-zero count).

   Encoding: F3 REX.W 0F BD /r.  This opcode is BSR with an F3 prefix on
   processors without LZCNT support."
  (emit-byte #xF3 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xBD stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-vex-0f38-rvm64 (opcode pp dst src1 src2 stream)
  "Emit a VEX.NDS.LZ.0F38.W1 three-register GPR instruction.

   DST is encoded in ModR/M.reg, SRC1 in inverted VEX.vvvv, and SRC2 in
   ModR/M.r/m.  PP is the mandatory-prefix field: 0=none, 2=F3, 3=F2."
  (let* ((r (logand (ash dst -3) 1))
         (b (logand (ash src2 -3) 1))
         (vvvv (logxor src1 #xF)))
    (emit-byte #xC4 stream)
    (emit-byte (logior (ash (logxor r 1) 7)
                       #x40
                       (ash (logxor b 1) 5)
                       #x02)
               stream)
    (emit-byte (logior #x80 (ash (logand vvvv #xF) 3) pp) stream)
    (emit-byte opcode stream)
    (emit-byte (modrm 3 dst src2) stream)))

(defun emit-bextr-rrr64 (dst src control stream)
  "BEXTR dst, src, control (64-bit bit-field extract).

   Encoding: VEX.NDS.LZ.0F38.W1 F7 /r.  CONTROL encodes START in bits 7:0 and
   LENGTH in bits 15:8."
  (emit-vex-0f38-rvm64 #xF7 0 dst control src stream))

(defun emit-pext-rrr64 (dst src mask stream)
  "PEXT dst, src, mask (64-bit parallel bit extract).

   Encoding: VEX.NDS.LZ.F3.0F38.W1 F5 /r."
  (emit-vex-0f38-rvm64 #xF5 2 dst src mask stream))

(defun emit-pdep-rrr64 (dst src mask stream)
  "PDEP dst, src, mask (64-bit parallel bit deposit).

   Encoding: VEX.NDS.LZ.F2.0F38.W1 F5 /r."
  (emit-vex-0f38-rvm64 #xF5 3 dst src mask stream))

(defun emit-bsr-rr64 (dst src stream)
  "BSR dst, src (64-bit bit-scan reverse).

   Encoding: REX.W + 0F BD /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xBD stream)
  (emit-byte (modrm 3 dst src) stream))

;;; ── AVX-512 / APX experimental emitters ─────────────────────────────────────

(defun emit-avx512-evex-zzz (opcode map dst-zmm src1-zmm src2-zmm stream &key (w 0) (mask +k0+))
  "Emit an EVEX.512 register-register vector instruction."
  (emit-evex-prefix stream :map map :w w :vvvv src1-zmm :pp +vex-pp-66+
                    :r (ash dst-zmm -3) :b (ash src2-zmm -3)
                    :r2 (ash dst-zmm -4) :ll 2 :aaa mask)
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-zmm src2-zmm) stream))

(defun emit-vpaddd-zmm (dst src1 src2 stream &key (mask +k0+))
  "VPADDD zmm{k}, zmm, zmm."
  (emit-avx512-evex-zzz #xFE +evex-map-0f+ dst src1 src2 stream :mask mask))

(defun emit-vmovdqu64-zmm-load (dst base offset stream &key (mask +k0+))
  "VMOVDQU64 zmm{k}, [base+offset]."
  (emit-evex-prefix stream :map +evex-map-0f+ :w 1 :vvvv #xF :pp +vex-pp-f3+
                    :r (ash dst -3) :b (ash base -3) :r2 (ash dst -4)
                    :ll 2 :aaa mask)
  (emit-byte #x6F stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst base offset stream))

(defun emit-vmovdqu64-zmm-store (base offset src stream &key (mask +k0+))
  "VMOVDQU64 [base+offset]{k}, zmm."
  (emit-evex-prefix stream :map +evex-map-0f+ :w 1 :vvvv #xF :pp +vex-pp-f3+
                    :r (ash src -3) :b (ash base -3) :r2 (ash src -4)
                    :ll 2 :aaa mask)
  (emit-byte #x7F stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src base offset stream))

(defun emit-vpxord-zmm (dst src1 src2 stream &key (mask +k0+))
  "VPXORD zmm{k}, zmm, zmm."
  (emit-avx512-evex-zzz #xEF +evex-map-0f+ dst src1 src2 stream :mask mask))

(defun emit-vpbroadcastd-zmm-r32 (dst src-gpr stream &key (mask +k0+))
  "VPBROADCASTD zmm{k}, r32."
  (emit-evex-prefix stream :map +evex-map-0f38+ :w 0 :vvvv #xF :pp +vex-pp-66+
                    :r (ash dst -3) :b (ash src-gpr -3) :r2 (ash dst -4)
                    :ll 2 :aaa mask)
  (emit-byte #x58 stream)
  (emit-byte (modrm 3 dst src-gpr) stream))

(defun emit-vpaddb-zmm (dst src1 src2 stream &key (mask +k0+))
  "VPADDB zmm{k}, zmm, zmm."
  (emit-avx512-evex-zzz #xFC +evex-map-0f+ dst src1 src2 stream :mask mask))

(defun emit-vpaddw-zmm (dst src1 src2 stream &key (mask +k0+))
  "VPADDW zmm{k}, zmm, zmm."
  (emit-avx512-evex-zzz #xFD +evex-map-0f+ dst src1 src2 stream :mask mask))

(defun emit-apx-add-ndd-rrr64 (dst src1 src2 stream &key no-flags-p)
  "Experimental APX NDD/NF ADD lowering.  Falls back to MOV+ADD unless APX is enabled."
  (declare (ignore no-flags-p))
  (unless *x86-64-apx-enabled* (emit-mov-rr64 dst src1 stream))
  (emit-add-rr64 dst src2 stream))

(defun emit-apx-sub-ndd-rrr64 (dst src1 src2 stream &key no-flags-p)
  "Experimental APX NDD/NF SUB lowering.  Falls back to MOV+SUB unless APX is enabled."
  (declare (ignore no-flags-p))
  (unless *x86-64-apx-enabled* (emit-mov-rr64 dst src1 stream))
  (emit-sub-rr64 dst src2 stream))

(defun emit-apx-imul-ndd-rrr64 (dst src1 src2 stream &key no-flags-p)
  "Experimental APX NDD/NF IMUL lowering.  Falls back to MOV+IMUL unless APX is enabled."
  (declare (ignore no-flags-p))
  (unless *x86-64-apx-enabled* (emit-mov-rr64 dst src1 stream))
  (emit-imul-rr64 dst src2 stream))
