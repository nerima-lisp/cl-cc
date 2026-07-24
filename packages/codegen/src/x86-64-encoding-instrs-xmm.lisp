;;;; packages/codegen/src/x86-64-encoding-instrs-xmm.lisp - x86-64 XMM / scalar-double emitters
;;;
;;; SSE/SSE2/SSE4 XMM and scalar-double instruction emitters split out of
;;; x86-64-encoding-instrs.lisp. Depends on x86-64-encoding.lisp (emit-byte,
;;; rex-prefix, modrm, ...) exactly as the parent file does.

(in-package :cl-cc/codegen)

;;; ── XMM / scalar-double instructions ─────────────────────────────────────────

(defun emit-sse-prefix-rex-if-needed (reg rm stream &optional index)
  "Emit an SSE REX prefix when REG/RM/INDEX use high architectural registers."
  (let ((r (if (>= reg 8) 1 0))
        (x (if (and index (>= index 8)) 1 0))
        (b (if (>= rm 8) 1 0)))
    (when (or (= r 1) (= x 1) (= b 1))
      (emit-byte (rex-prefix :r r :x x :b b) stream))))

(defun emit-sse66-0f-xx (opcode dst-xmm src-xmm stream)
  "Emit a 66 0F /r XMM register instruction."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm src-xmm stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-sse66-0f-xm (opcode dst-xmm base offset stream)
  "Emit a 66 0F /r XMM load-like instruction with a memory source."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defun emit-sse66-0f-mx (opcode base offset src-xmm stream)
  "Emit a 66 0F /r XMM store-like instruction with a memory destination."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed src-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-xmm base offset stream))

(defun emit-sse66-0f38-xx (opcode dst-xmm src-xmm stream)
  "Emit a 66 0F 38 /r XMM register instruction."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm src-xmm stream)
  (emit-byte #x0F stream)
  (emit-byte #x38 stream)
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-sse66-0f38-xm (opcode dst-xmm base offset stream)
  "Emit a 66 0F 38 /r XMM instruction with a memory source."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte #x38 stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defmacro define-sse66-0f-xmm-op (name opcode description)
  "Define register and memory-source forms for a 66 0F XMM operation."
  (let ((rr-name (intern (format nil "EMIT-~A-XX" name)))
        (rm-name (intern (format nil "EMIT-~A-XM" name))))
    `(progn
       (defun ,rr-name (dst-xmm src-xmm stream)
         ,description
         (emit-sse66-0f-xx ,opcode dst-xmm src-xmm stream))
       (defun ,rm-name (dst-xmm base offset stream)
         ,description
         (emit-sse66-0f-xm ,opcode dst-xmm base offset stream)))))

(defmacro define-sse66-0f38-xmm-op (name opcode description)
  "Define register and memory-source forms for a 66 0F 38 XMM operation."
  (let ((rr-name (intern (format nil "EMIT-~A-XX" name)))
        (rm-name (intern (format nil "EMIT-~A-XM" name))))
    `(progn
       (defun ,rr-name (dst-xmm src-xmm stream)
         ,description
         (emit-sse66-0f38-xx ,opcode dst-xmm src-xmm stream))
       (defun ,rm-name (dst-xmm base offset stream)
         ,description
         (emit-sse66-0f38-xm ,opcode dst-xmm base offset stream)))))

(defun emit-movdqa-xx (dst-xmm src-xmm stream)
  "MOVDQA xmm, xmm (aligned packed 128-bit copy)."
  (emit-sse66-0f-xx #x6F dst-xmm src-xmm stream))

(defun emit-movdqa-xm (dst-xmm base offset stream)
  "MOVDQA xmm, [base + offset] (aligned packed 128-bit load)."
  (emit-sse66-0f-xm #x6F dst-xmm base offset stream))

(defun emit-movdqa-mx (base offset src-xmm stream)
  "MOVDQA [base + offset], xmm (aligned packed 128-bit store)."
  (emit-sse66-0f-mx #x7F base offset src-xmm stream))

(defun emit-ssef3-0f-xm (opcode dst-xmm base offset stream)
  "Emit an F3 0F /r XMM load-like instruction with a memory source."
  (emit-byte #xF3 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defun emit-ssef3-0f-mx (opcode base offset src-xmm stream)
  "Emit an F3 0F /r XMM store-like instruction with a memory destination."
  (emit-byte #xF3 stream)
  (emit-sse-prefix-rex-if-needed src-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-xmm base offset stream))

(defun emit-movdqu-xm (dst-xmm base offset stream)
  "MOVDQU xmm, [base + offset] (unaligned packed 128-bit load)."
  (emit-ssef3-0f-xm #x6F dst-xmm base offset stream))

(defun emit-movdqu-mx (base offset src-xmm stream)
  "MOVDQU [base + offset], xmm (unaligned packed 128-bit store)."
  (emit-ssef3-0f-mx #x7F base offset src-xmm stream))

(define-sse66-0f-xmm-op paddd #xFE
  "PADDD xmm, xmm/m128 (packed signed/unsigned dword add).")
(define-sse66-0f-xmm-op psubd #xFA
  "PSUBD xmm, xmm/m128 (packed dword subtract).")
(define-sse66-0f-xmm-op pxor #xEF
  "PXOR xmm, xmm/m128 (packed bitwise xor).")
(define-sse66-0f-xmm-op pand #xDB
  "PAND xmm, xmm/m128 (packed bitwise and).")
(define-sse66-0f-xmm-op por #xEB
  "POR xmm, xmm/m128 (packed bitwise or).")

(define-sse66-0f38-xmm-op pmulld #x40
  "PMULLD xmm, xmm/m128 (SSE4.1 packed signed dword multiply).")
(define-sse66-0f38-xmm-op pblendvb #x10
  "PBLENDVB xmm, xmm/m128 (SSE4.1 variable byte blend using XMM0 mask).")
(define-sse66-0f38-xmm-op pminsd #x39
  "PMINSD xmm, xmm/m128 (SSE4.1 packed signed dword minimum).")
(define-sse66-0f38-xmm-op pmaxsd #x3D
  "PMAXSD xmm, xmm/m128 (SSE4.1 packed signed dword maximum).")

(defun emit-avx2-vex-yyy (opcode map dst-ymm src1-ymm src2-ymm stream)
  "Emit an AVX2 VEX.256.66 MAP.W0 register instruction."
  (emit-vex-prefix stream
                   :map map
                   :w 0
                   :vvvv src1-ymm
                   :l 1
                   :pp +vex-pp-66+
                   :r (ash dst-ymm -3)
                   :b (ash src2-ymm -3))
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-ymm src2-ymm) stream))

(defun emit-avx2-vex-yym (opcode map dst-ymm src1-ymm base offset stream)
  "Emit an AVX2 VEX.256.66 MAP.W0 instruction with a memory source."
  (emit-vex-prefix stream
                   :map map
                   :w 0
                   :vvvv src1-ymm
                   :l 1
                   :pp +vex-pp-66+
                   :r (ash dst-ymm -3)
                   :b (ash base -3))
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-ymm base offset stream))

(defun emit-vpaddd-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPADDD ymm, ymm, ymm (AVX2 packed dword add)."
  (emit-avx2-vex-yyy #xFE +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpaddd-yym (dst-ymm src1-ymm base offset stream)
  "VPADDD ymm, ymm, [base + offset] (AVX2 packed dword add)."
  (emit-avx2-vex-yym #xFE +vex-map-0f+ dst-ymm src1-ymm base offset stream))

(defun emit-vpsubd-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPSUBD ymm, ymm, ymm (AVX2 packed dword subtract)."
  (emit-avx2-vex-yyy #xFA +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpand-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPAND ymm, ymm, ymm (AVX2 packed bitwise and)."
  (emit-avx2-vex-yyy #xDB +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpor-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPOR ymm, ymm, ymm (AVX2 packed bitwise or)."
  (emit-avx2-vex-yyy #xEB +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpxor-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPXOR ymm, ymm, ymm (AVX2 packed bitwise xor)."
  (emit-avx2-vex-yyy #xEF +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpmulld-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPMULLD ymm, ymm, ymm (AVX2 packed signed dword multiply)."
  (emit-avx2-vex-yyy #x40 +vex-map-0f38+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpmulld-yym (dst-ymm src1-ymm base offset stream)
  "VPMULLD ymm, ymm, [base + offset] (AVX2 packed signed dword multiply)."
  (emit-avx2-vex-yym #x40 +vex-map-0f38+ dst-ymm src1-ymm base offset stream))

(defun emit-vmovdqu-ym (dst-ymm base offset stream)
  "VMOVDQU ymm, [base + offset] (AVX unaligned 256-bit load)."
  (emit-vex-prefix stream
                   :map +vex-map-0f+ :w 0 :vvvv #xF :l 1 :pp +vex-pp-f3+
                   :r (ash dst-ymm -3) :b (ash base -3))
  (emit-byte #x6F stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-ymm base offset stream))

(defun emit-vmovdqu-my (base offset src-ymm stream)
  "VMOVDQU [base + offset], ymm (AVX unaligned 256-bit store)."
  (emit-vex-prefix stream
                   :map +vex-map-0f+ :w 0 :vvvv #xF :l 1 :pp +vex-pp-f3+
                   :r (ash src-ymm -3) :b (ash base -3))
  (emit-byte #x7F stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-ymm base offset stream))

(defun emit-vpermd-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPERMD ymm, ymm, ymm (AVX2 permute dwords)."
  (emit-avx2-vex-yyy #x36 +vex-map-0f38+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpermd-yym (dst-ymm src1-ymm base offset stream)
  "VPERMD ymm, ymm, [base + offset] (AVX2 permute dwords)."
  (emit-avx2-vex-yym #x36 +vex-map-0f38+ dst-ymm src1-ymm base offset stream))

(defun emit-movq-xmm-r64 (dst-xmm src-gpr stream)
  "MOVQ xmm, r64 (copy raw 64 bits from GPR to XMM low lane)."
  (emit-byte #x66 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst-xmm -3) :b (ash src-gpr -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #x6E stream)
  (emit-byte (modrm 3 dst-xmm src-gpr) stream))

(defun emit-movsd-xx (dst-xmm src-xmm stream)
  "MOVSD xmm, xmm (scalar double copy)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x10 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-movsd-xm (dst-xmm base offset stream)
  "MOVSD xmm, [base + offset] (scalar double load)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= base 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash base -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x10 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defun emit-movsd-mx (base offset src-xmm stream)
  "MOVSD [base + offset], xmm (scalar double store)."
  (emit-byte #xF2 stream)
  (when (or (>= src-xmm 8) (>= base 8))
    (emit-byte (rex-prefix :r (ash src-xmm -3) :b (ash base -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x11 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-xmm base offset stream))

(defun emit-addsd-xx (dst-xmm src-xmm stream)
  "ADDSD xmm, xmm (scalar double add)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x58 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-subsd-xx (dst-xmm src-xmm stream)
  "SUBSD xmm, xmm (scalar double subtract)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x5C stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-mulsd-xx (dst-xmm src-xmm stream)
  "MULSD xmm, xmm (scalar double multiply)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x59 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-vfmadd132sd-xxx (dst-xmm add-xmm mul-xmm stream)
  "VFMADD132SD dst, add, mul: dst = dst * mul + add."
  (let* ((r (logand (ash dst-xmm -3) 1))
         (b (logand (ash mul-xmm -3) 1))
         (vvvv (logxor add-xmm #xF)))
    ;; VEX.DDS.LIG.66.0F38.W1 99 /r
    (emit-byte #xC4 stream)
    (emit-byte (logior (ash (logxor r 1) 7)
                       #x40
                       (ash (logxor b 1) 5)
                       #x02)
               stream)
    (emit-byte (logior #x80 (ash vvvv 3) #x01) stream)
    (emit-byte #x99 stream)
    (emit-byte (modrm 3 dst-xmm mul-xmm) stream)))

(defun emit-divsd-xx (dst-xmm src-xmm stream)
  "DIVSD xmm, xmm (scalar double divide)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x5E stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-sqrtsd-xx (dst-xmm src-xmm stream)
  "SQRTSD xmm, xmm (scalar double square root)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x51 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))
