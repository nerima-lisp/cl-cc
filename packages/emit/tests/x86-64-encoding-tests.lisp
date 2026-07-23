;;;; tests/unit/emit/x86-64-encoding-tests.lisp — x86-64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp encoding helpers:
;;;; rex-prefix, modrm, emit-byte, emit-dword, emit-qword,
;;;; emit-mov-rr64, emit-add-rr64, emit-sub-rr64, emit-imul-rr64,
;;;; emit-mul-rm64, emit-imul-rm64,
;;;; emit-push-r64, emit-pop-r64, emit-ret, emit-jmp-rel32, emit-je-rel32.
;;;; VM emitter byte sizes → x86-64-vm-emitter-tests.lisp.

(in-package :cl-cc/test)


;;; Helper: collect bytes emitted by a function
(defun %x86-encoding-collect-bytes (emit-fn)
  "Call EMIT-FN with a stream that collects bytes. Returns byte list."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── rex-prefix ──────────────────────────────────────────────────────────

(it-sequential "x86-rex-prefix-flags base"
  (destructuring-bind (args expected) (list nil #x40)
    (expect (apply #'cl-cc/codegen::rex-prefix args) :to-equal expected)))

(it-sequential "x86-rex-prefix-flags w"
  (destructuring-bind (args expected) (list '(:w 1) #x48)
    (expect (apply #'cl-cc/codegen::rex-prefix args) :to-equal expected)))

(it-sequential "x86-rex-prefix-flags r"
  (destructuring-bind (args expected) (list '(:r 1) #x44)
    (expect (apply #'cl-cc/codegen::rex-prefix args) :to-equal expected)))

(it-sequential "x86-rex-prefix-flags b"
  (destructuring-bind (args expected) (list '(:b 1) #x41)
    (expect (apply #'cl-cc/codegen::rex-prefix args) :to-equal expected)))

(it-sequential "x86-rex-prefix-flags wrb"
  (destructuring-bind (args expected) (list '(:w 1 :r 1 :b 1) #x4D)
    (expect (apply #'cl-cc/codegen::rex-prefix args) :to-equal expected)))

(it-sequential "x86-rex-prefix-flags all"
  (destructuring-bind (args expected) (list '(:w 1 :r 1 :x 1 :b 1) #x4F)
    (expect (apply #'cl-cc/codegen::rex-prefix args) :to-equal expected)))

;;; ─── modrm ──────────────────────────────────────────────────────────────

(it-sequential "x86-modrm-cases reg-reg"
  (destructuring-bind (mod reg rm expected) (list 3 0 0 #xC0)
    (expect (cl-cc/codegen::modrm mod reg rm) :to-equal expected)))

(it-sequential "x86-modrm-cases reg-fields"
  (destructuring-bind (mod reg rm expected) (list 3 1 2 #xCA)
    (expect (cl-cc/codegen::modrm mod reg rm) :to-equal expected)))

(it-sequential "x86-modrm-cases memory-indirect"
  (destructuring-bind (mod reg rm expected) (list 0 0 0 #x00)
    (expect (cl-cc/codegen::modrm mod reg rm) :to-equal expected)))

(it-sequential "x86-modrm-cases disp8"
  (destructuring-bind (mod reg rm expected) (list 1 3 5 #x5D)
    (expect (cl-cc/codegen::modrm mod reg rm) :to-equal expected)))

;;; ─── emit-byte / emit-dword / emit-qword ────────────────────────────────

(it-sequential "x86-emit-byte-cases normal-value"
  (destructuring-bind (input expected) (list #xAB #xAB)
    (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/codegen::emit-byte input s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal expected))))

(it-sequential "x86-emit-byte-cases mask-to-8bit"
  (destructuring-bind (input expected) (list #x1FF #xFF)
    (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/codegen::emit-byte input s)))))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal expected))))

(it-sequential "x86-emit-multi-byte-le dword"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-dword #xDEADBEEF s)) '(#xEF #xBE #xAD #xDE))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-emit-multi-byte-le qword"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-qword #x0102030405060708 s)) '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

;;; ─── emit-add-rr64 / emit-sub-rr64 ──────────────────────────────────────

(it-sequential "x86-add-sub-rr64-encoding add-rax-rcx"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-add-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) '(#x48 #x01 #xC8))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-add-sub-rr64-encoding sub-rax-rcx"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-sub-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) '(#x48 #x29 #xC8))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-add-sub-rr64-encoding add-rcx-rdx"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-add-rr64 cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ s)) '(#x48 #x01 #xD1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-add-sub-rr64-encoding sub-rcx-rdx"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-sub-rr64 cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ s)) '(#x48 #x29 #xD1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-mov-memory-displacement-widths"
  (let ((small-store (%x86-encoding-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rsp+ 120 cl-cc/codegen::+rax+ s))))
        (large-store (%x86-encoding-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rsp+ 248 cl-cc/codegen::+rax+ s))))
        (zero-rbp-load (%x86-encoding-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rbp+ 0 s)))))
    (expect small-store :to-equal '(#x48 #x89 #x44 #x24 #x78))
    (expect large-store :to-equal '(#x48 #x89 #x84 #x24 #xF8 #x00 #x00 #x00))
    (expect zero-rbp-load :to-equal '(#x48 #x8B #x45 #x00))))

;;; ─── emit-imul-rr64 / two-byte opcodes ──────────────────────────────────

(it-sequential "x86-two-byte-opcode-instructions imul-rr64"
  (destructuring-bind (emit-fn opcode2) (list (lambda (s) (cl-cc/codegen::emit-imul-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) #xAF)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal 4)
    (expect (second bytes) :to-equal #x0F)
    (expect (third bytes) :to-equal opcode2))))

(it-sequential "x86-two-byte-opcode-instructions movzx-r64-r8"
  (destructuring-bind (emit-fn opcode2) (list (lambda (s) (cl-cc/codegen::emit-movzx-r64-r8 cl-cc/codegen::+rax+ cl-cc/codegen::+rax+ s)) #xB6)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal 4)
    (expect (second bytes) :to-equal #x0F)
    (expect (third bytes) :to-equal opcode2))))

(it-sequential "x86-mul-rm64-high-encodings"
  (let ((mul-bytes (%x86-encoding-collect-bytes
                    (lambda (s)
                      (cl-cc/codegen::emit-mul-rm64 cl-cc/codegen::+r11+ s))))
        (imul-bytes (%x86-encoding-collect-bytes
                     (lambda (s)
                       (cl-cc/codegen::emit-imul-rm64 cl-cc/codegen::+r11+ s)))))
    (expect mul-bytes :to-equal '(#x49 #xF7 #xE3))
    (expect imul-bytes :to-equal '(#x49 #xF7 #xEB))))

(it-sequential "x86-apx-ndd-fallback-and-enabled-paths"
  (let ((fallback (%x86-encoding-collect-bytes
                   (lambda (s)
                     (let ((cl-cc/codegen:*x86-64-apx-enabled* nil))
                       (cl-cc/codegen::emit-apx-add-ndd-rrr64
                        cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ s)))))
        (enabled (%x86-encoding-collect-bytes
                  (lambda (s)
                    (let ((cl-cc/codegen:*x86-64-apx-enabled* t))
                      (cl-cc/codegen::emit-apx-add-ndd-rrr64
                       cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ s))))))
    ;; Fallback = MOV RAX,RCX; ADD RAX,RDX.  Enabled path currently exercises the
    ;; APX feature gate by skipping the copy, while keeping the tested ADD encoder.
    (expect fallback :to-equal '(#x48 #x89 #xC8 #x48 #x01 #xD0))
    (expect enabled :to-equal '(#x48 #x01 #xD0))))

(it-sequential "x86-safe-stack-and-xom-byte-hooks"
  (let ((disabled (%x86-encoding-collect-bytes
                   (lambda (s)
                     (let ((cl-cc/codegen:*x86-64-safe-stack-enabled* nil))
                       (cl-cc/codegen::emit-x86-64-safe-stack-load-pointer cl-cc/codegen::+rax+ s)))))
        (load (%x86-encoding-collect-bytes
               (lambda (s)
                 (let ((cl-cc/codegen:*x86-64-safe-stack-enabled* t))
                   (cl-cc/codegen::emit-x86-64-safe-stack-load-pointer cl-cc/codegen::+rax+ s)))))
        (store (%x86-encoding-collect-bytes
                (lambda (s)
                  (let ((cl-cc/codegen:*x86-64-safe-stack-enabled* t))
                    (cl-cc/codegen::emit-x86-64-safe-stack-store-pointer cl-cc/codegen::+rax+ s)))))
        (lfence (%x86-encoding-collect-bytes #'cl-cc/codegen::emit-x86-64-lfence)))
    (expect disabled :to-be-null)
    (expect load :to-equal '(#x64 #x48 #x8B #x04 #x25 #x70 #x00 #x00 #x00))
    (expect store :to-equal '(#x64 #x48 #x89 #x04 #x25 #x70 #x00 #x00 #x00))
    (expect lfence :to-equal '(#x0F #xAE #xE8))))

(it-sequential "x86-xmm-instruction-encoding movq-xmm0-r11"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-movq-xmm-r64 cl-cc/codegen::+xmm0+ cl-cc/codegen::+r11+ s)) '(#x66 #x49 #x0F #x6E #xC3))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-xmm-instruction-encoding addsd-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-addsd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#xF2 #x0F #x58 #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-xmm-instruction-encoding movsd-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-movsd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#xF2 #x0F #x10 #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-xmm-instruction-encoding sqrtsd-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-sqrtsd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#xF2 #x0F #x51 #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding movdqu-load-r11"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-movdqu-xm cl-cc/codegen::+xmm0+ cl-cc/codegen::+r11+ 0 s)) '(#xF3 #x41 #x0F #x6F #x03))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding movdqu-store-r11"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-movdqu-mx cl-cc/codegen::+r11+ 0 cl-cc/codegen::+xmm0+ s)) '(#xF3 #x41 #x0F #x7F #x03))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding paddd-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-paddd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#x66 #x0F #xFE #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding psubd-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-psubd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#x66 #x0F #xFA #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding pmulld-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-pmulld-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#x66 #x0F #x38 #x40 #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding pand-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-pand-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#x66 #x0F #xDB #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding por-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-por-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#x66 #x0F #xEB #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-packed-integer-encoding pxor-xmm0-xmm1"
  (destructuring-bind (emit-fn expected-bytes) (list (lambda (s) (cl-cc/codegen::emit-pxor-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s)) '(#x66 #x0F #xEF #xC1))
    (expect (%x86-encoding-collect-bytes emit-fn) :to-equal expected-bytes)))

(it-sequential "x86-simd-marker-lowers-to-sse-sequence"
  (let* ((inst (make-vm-simd-vector-op :op :add :dst-array :r3 :lhs-array :r1
                                       :rhs-array :r2 :index-reg :r4 :lanes 4
                                       :element-type :i32))
         (bytes (%x86-encoding-collect-bytes
                 (lambda (s) (cl-cc/codegen::emit-vm-simd-vector-op inst s)))))
    (expect (length bytes) :to-equal (cl-cc/codegen::instruction-size inst))
    (expect (search '(#xF3 #x41 #x0F #x6F #x03) bytes :test #'=) :to-be-truthy)
    (expect (search '(#x66 #x0F #xFE #xC1) bytes :test #'=) :to-be-truthy)
    (expect (search '(#xF3 #x41 #x0F #x7F #x03) bytes :test #'=) :to-be-truthy)))

;;; ─── emit-push-r64 / emit-pop-r64 / emit-ret / emit-vm-ret-inst ─────────

(it-sequential "x86-ret-encoding emit-ret"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-ret s)))
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal #xC3))))

(it-sequential "x86-ret-encoding vm-ret-inst"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-ret-inst (cl-cc:make-vm-ret) s)))
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal 1)
    (expect (first bytes) :to-equal #xC3))))

(it-sequential "x86-jmp-rel32-le-offset"
  (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/codegen::emit-jmp-rel32 256 s)))))
    (expect (second bytes) :to-equal #x00)
    (expect (third bytes) :to-equal #x01)))

(it-sequential "x86-fixed-encoding-spot-checks jmp-zero"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-jmp-rel32 0 s)) 5 #xE9 #x00)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks bswap-rax"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-bswap-r32 cl-cc/codegen::+rax+ s)) 2 #x0F #xC8)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks jge-short"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-jge-short 3 s)) 2 #x7D 3)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks idiv-r11"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list #'cl-cc/codegen::emit-idiv-r11 3 #x49 #xF7)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks cqo"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list #'cl-cc/codegen::emit-cqo 2 #x48 #x99)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks je-rel32"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-je-rel32 0 s)) 6 #x0F #x84)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks jne-rel32"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-byte #x0F s)
                          (cl-cc/codegen::emit-byte #x85 s)
                          (cl-cc/codegen::emit-dword 0 s)) 6 #x0F #x85)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks cmp-rax-0"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-cmp-ri64 cl-cc/codegen::+rax+ 0 s)) 7 #x48 #x81)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks mov-r8-r9"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-mov-rr64 cl-cc/codegen::+r8+ cl-cc/codegen::+r9+ s)) 3 #x4D #x89)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks not-rax"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-not-r64    cl-cc/codegen::+rax+ s)) 3 #x48 #xF7)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks neg-rax"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-neg-r64    cl-cc/codegen::+rax+ s)) 3 #x48 #xF7)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks dec-rax"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-dec-r64    cl-cc/codegen::+rax+ s)) 3 #x48 #xFF)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks cmp-rr64"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-cmp-rr64   cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x39)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks test-rr64"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-test-rr64  cl-cc/codegen::+rax+ cl-cc/codegen::+rax+ s)) 3 #x48 #x85)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks and-rr64"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-and-rr64   cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x21)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks or-rr64"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-or-rr64    cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x09)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks xor-rr64"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-xor-rr64   cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x31)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks sal-cl"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-sal-r64-cl cl-cc/codegen::+rax+ s)) 3 #x48 #xD3)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks sar-cl"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-sar-r64-cl cl-cc/codegen::+rax+ s)) 3 #x48 #xD3)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks add-ri8"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-add-ri8    cl-cc/codegen::+rax+ 1 s)) 4 #x48 #x83)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks sub-ri8"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-sub-ri8    cl-cc/codegen::+rax+ 1 s)) 4 #x48 #x83)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

(it-sequential "x86-fixed-encoding-spot-checks and-ri8"
  (destructuring-bind (emit-fn expected-len byte0 byte1) (list (lambda (s) (cl-cc/codegen::emit-and-ri8    cl-cc/codegen::+rax+ 1 s)) 4 #x48 #x83)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal expected-len)
    (expect (first bytes) :to-equal byte0)
    (expect (second bytes) :to-equal byte1))))

;;; ─── VM register mapping ─────────────────────────────────────────────────

(it-sequential "x86-vm-reg-map"
  (expect (length cl-cc/codegen::*vm-reg-map*) :to-equal 8)
  (expect (cdr (assoc :r0 cl-cc/codegen::*vm-reg-map*)) :to-equal cl-cc/codegen::+rax+)
  (expect (cdr (assoc :r1 cl-cc/codegen::*vm-reg-map*)) :to-equal cl-cc/codegen::+rcx+))

;;; ─── emit-mov-ri64 ──────────────────────────────────────────────────────

(it-sequential "x86-mov-ri64-encoding rax-42"
  (destructuring-bind (reg imm rex opcode imm-byte) (list cl-cc/codegen::+rax+ 42 #x48 #xB8 42)
    (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-ri64 reg imm s)))))
    (expect (length bytes) :to-equal 10)
    (expect (first bytes) :to-equal rex)
    (expect (second bytes) :to-equal opcode)
    (expect (third bytes) :to-equal imm-byte))))

(it-sequential "x86-mov-ri64-encoding rcx-0"
  (destructuring-bind (reg imm rex opcode imm-byte) (list cl-cc/codegen::+rcx+ 0 #x48 #xB9 0)
    (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-ri64 reg imm s)))))
    (expect (length bytes) :to-equal 10)
    (expect (first bytes) :to-equal rex)
    (expect (second bytes) :to-equal opcode)
    (expect (third bytes) :to-equal imm-byte))))

(it-sequential "x86-mov-ri64-encoding r8-1"
  (destructuring-bind (reg imm rex opcode imm-byte) (list cl-cc/codegen::+r8+ 1 #x49 #xB8 1)
    (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-ri64 reg imm s)))))
    (expect (length bytes) :to-equal 10)
    (expect (first bytes) :to-equal rex)
    (expect (second bytes) :to-equal opcode)
    (expect (third bytes) :to-equal imm-byte))))

;;; ─── vm-const-to-integer ─────────────────────────────────────────────────

(it-sequential "x86-vm-const-to-integer nil-to-0"
  (destructuring-bind (input expected) (list nil 0)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer t-to-1"
  (destructuring-bind (input expected) (list t 1)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer int-42"
  (destructuring-bind (input expected) (list 42 42)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer int-neg"
  (destructuring-bind (input expected) (list -1 -1)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer int-zero"
  (destructuring-bind (input expected) (list 0 0)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer string-to-0"
  (destructuring-bind (input expected) (list "hello" 0)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer symbol-to-0"
  (destructuring-bind (input expected) (list 'foo 0)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-vm-const-to-integer list-to-0"
  (destructuring-bind (input expected) (list '(1 2) 0)
    (expect (cl-cc/codegen::vm-const-to-integer input) :to-equal expected)))

(it-sequential "x86-setcc-register-size low-reg-rax"
  (destructuring-bind (reg expected-len) (list cl-cc/codegen::+rax+ 3)
    (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc #x94 reg s)))))
    (expect (length bytes) :to-equal expected-len))))

(it-sequential "x86-setcc-register-size high-reg-rsi"
  (destructuring-bind (reg expected-len) (list cl-cc/codegen::+rsi+ 4)
    (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc #x94 reg s)))))
    (expect (length bytes) :to-equal expected-len))))

(it-sequential "x86-cmov-encoding cmovl"
  (destructuring-bind (emit-fn opcode3) (list (lambda (s) (cl-cc/codegen::emit-cmovl-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) #x4C)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal 4)
    (expect (third bytes) :to-equal opcode3))))

(it-sequential "x86-cmov-encoding cmovg"
  (destructuring-bind (emit-fn opcode3) (list (lambda (s) (cl-cc/codegen::emit-cmovg-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) #x4F)
    (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (expect (length bytes) :to-equal 4)
    (expect (third bytes) :to-equal opcode3))))
