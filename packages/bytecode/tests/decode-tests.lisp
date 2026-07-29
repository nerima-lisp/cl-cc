;;;; tests/unit/bytecode/decode-tests.lisp - Bytecode ISA v2 Decoder Tests
;;;
;;; Tests for field extraction, instruction-format classifier,
;;; and disassemble-instruction.  Includes regression for
;;; tail-call :3op classification.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Field extraction: decode-opcode / decode-dst / decode-src1 / decode-src2
;;; ------------------------------------------------------------

(it-sequential "decode-field-extraction"
  (let ((w1 (ash #xAB 24)))
    (expect (= #xAB (cl-cc/bytecode:decode-opcode w1)) :to-be-truthy))
  (let ((w2 (logior (ash #x20 24) #xFFFFFF)))
    (expect (= #x20 (cl-cc/bytecode:decode-opcode w2)) :to-be-truthy))
  (let ((w3 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 7 2 3)))
    (expect (= 7 (cl-cc/bytecode:decode-dst w3)) :to-be-truthy))
  (let ((w4 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 9 3)))
    (expect (= 9 (cl-cc/bytecode:decode-src1 w4)) :to-be-truthy))
  (let ((w5 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 11)))
    (expect (= 11 (cl-cc/bytecode:decode-src2 w5)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Field extraction: %sign-extend
;;; ------------------------------------------------------------

(it-sequential "sign-extend-cases 8-bit-positive"
  (destructuring-bind (raw bits expected) (list 127 8 127)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

(it-sequential "sign-extend-cases 8-bit-negative"
  (destructuring-bind (raw bits expected) (list 128 8 -128)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

(it-sequential "sign-extend-cases 8-bit-max"
  (destructuring-bind (raw bits expected) (list 255 8 -1)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

(it-sequential "sign-extend-cases 16-bit-positive"
  (destructuring-bind (raw bits expected) (list 32767 16 32767)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

(it-sequential "sign-extend-cases 16-bit-negative"
  (destructuring-bind (raw bits expected) (list 32768 16 -32768)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

(it-sequential "sign-extend-cases 16-bit-max"
  (destructuring-bind (raw bits expected) (list 65535 16 -1)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

(it-sequential "sign-extend-cases 24-bit-zero"
  (destructuring-bind (raw bits expected) (list 0 24 0)
    (expect (= expected (cl-cc/bytecode::%sign-extend raw bits)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Field extraction: decode-imm16
;;; ------------------------------------------------------------

(it-sequential "decode-imm16-cases positive"
  (destructuring-bind (encoded-imm expected-value) (list 100 100)
    (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 encoded-imm)))
    (expect (= expected-value (cl-cc/bytecode:decode-imm16 w)) :to-be-truthy))))

(it-sequential "decode-imm16-cases zero"
  (destructuring-bind (encoded-imm expected-value) (list 0 0)
    (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 encoded-imm)))
    (expect (= expected-value (cl-cc/bytecode:decode-imm16 w)) :to-be-truthy))))

(it-sequential "decode-imm16-cases negative-one"
  (destructuring-bind (encoded-imm expected-value) (list -1 -1)
    (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 encoded-imm)))
    (expect (= expected-value (cl-cc/bytecode:decode-imm16 w)) :to-be-truthy))))

(it-sequential "decode-imm16-cases min"
  (destructuring-bind (encoded-imm expected-value) (list -32768 -32768)
    (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 encoded-imm)))
    (expect (= expected-value (cl-cc/bytecode:decode-imm16 w)) :to-be-truthy))))

(it-sequential "decode-imm16-cases max"
  (destructuring-bind (encoded-imm expected-value) (list 32767 32767)
    (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 encoded-imm)))
    (expect (= expected-value (cl-cc/bytecode:decode-imm16 w)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; Field extraction: decode-offset24
;;; ------------------------------------------------------------

(it-sequential "decode-offset24-cases positive"
  (destructuring-bind (offset) (list 200)
    (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)))
    (expect (= offset (cl-cc/bytecode:decode-offset24 w)) :to-be-truthy))))

(it-sequential "decode-offset24-cases zero"
  (destructuring-bind (offset) (list 0)
    (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)))
    (expect (= offset (cl-cc/bytecode:decode-offset24 w)) :to-be-truthy))))

(it-sequential "decode-offset24-cases negative"
  (destructuring-bind (offset) (list -10)
    (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)))
    (expect (= offset (cl-cc/bytecode:decode-offset24 w)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; instruction-format classifier
;;; ------------------------------------------------------------

;;; Regression note: +op-tail-call+ was previously classified as :2op,
;;; causing nargs to be silently dropped during disassembly.
(it-sequential "decode-format-classification add"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-add+ :3op)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification call"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-call+ :3op)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification tail-call"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-tail-call+ :3op)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification move"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-move+ :2op)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification neg"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-neg+ :2op)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification load-fixnum"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-load-fixnum+ :imm)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification jump"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-jump+ :branch)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification nop"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-nop+ :special)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

(it-sequential "decode-format-classification return-nil"
  (destructuring-bind (opcode expected-format) (list cl-cc/bytecode:+op-return-nil+ :special)
    (expect (cl-cc/bytecode:instruction-format opcode) :to-equal expected-format)))

;;; ------------------------------------------------------------
;;; disassemble-instruction round-trip
;;; ------------------------------------------------------------

(it-sequential "decode-disassemble-3op-round-trip"
  (let* ((w    (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 3))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (getf info :format) :to-equal :3op)
    (expect (getf info :opcode-name) :to-equal "ADD")
    (expect (= 1 (getf info :dst)) :to-be-truthy)
    (expect (= 2 (getf info :src1)) :to-be-truthy)
    (expect (= 3 (getf info :src2)) :to-be-truthy)))

;;; Regression: verify tail-call nargs is preserved through decode.
(it-sequential "decode-disassemble-tail-call-preserves-nargs"
  (let* ((w    (cl-cc/bytecode:encode-tail-call 5 3))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (getf info :format) :to-equal :3op)
    (expect (getf info :opcode-name) :to-equal "TAIL_CALL")
    ;; func=5 is in src1, nargs=3 is in src2
    (expect (= 5 (getf info :src1)) :to-be-truthy)
    (expect (= 3 (getf info :src2)) :to-be-truthy)))

(it-sequential "decode-disassemble-2op-round-trip"
  (let* ((w    (cl-cc/bytecode:encode-move 4 7))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (getf info :format) :to-equal :2op)
    (expect (getf info :opcode-name) :to-equal "MOVE")
    (expect (= 4 (getf info :dst)) :to-be-truthy)
    (expect (= 7 (getf info :src)) :to-be-truthy)))

(it-sequential "decode-disassemble-imm-round-trip-positive"
  (let* ((w    (cl-cc/bytecode:encode-load-fixnum 2 42))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (getf info :format) :to-equal :imm)
    (expect (getf info :opcode-name) :to-equal "LOAD_FIXNUM")
    (expect (= 2 (getf info :dst)) :to-be-truthy)
    (expect (= 42 (getf info :imm16)) :to-be-truthy)))

(it-sequential "decode-disassemble-imm-round-trip-negative"
  (let* ((w    (cl-cc/bytecode:encode-load-fixnum 0 -5))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (= -5 (getf info :imm16)) :to-be-truthy)))

(it-sequential "decode-disassemble-branch-round-trip"
  (let* ((w    (cl-cc/bytecode:encode-jump 99))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (getf info :format) :to-equal :branch)
    (expect (getf info :opcode-name) :to-equal "JUMP")
    (expect (= 99 (getf info :offset24)) :to-be-truthy)))

(it-sequential "decode-disassemble-special-nop"
  (let* ((w    (cl-cc/bytecode:encode-nop))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (expect (getf info :format) :to-equal :special)
    (expect (getf info :opcode-name) :to-equal "NOP")))

;;; ------------------------------------------------------------
;;; Opcode name table
;;; ------------------------------------------------------------

(it-sequential "decode-opcode-name-cases add"
  (destructuring-bind (opcode expected-name) (list cl-cc/bytecode:+op-add+ "ADD")
    (expect (gethash opcode cl-cc/bytecode:*opcode-names*) :to-equal expected-name)))

(it-sequential "decode-opcode-name-cases tail-call"
  (destructuring-bind (opcode expected-name) (list cl-cc/bytecode:+op-tail-call+ "TAIL_CALL")
    (expect (gethash opcode cl-cc/bytecode:*opcode-names*) :to-equal expected-name)))

(it-sequential "decode-opcode-name-cases nop"
  (destructuring-bind (opcode expected-name) (list cl-cc/bytecode:+op-nop+ "NOP")
    (expect (gethash opcode cl-cc/bytecode:*opcode-names*) :to-equal expected-name)))

;;; ------------------------------------------------------------
;;; disassemble-chunk
;;; ------------------------------------------------------------

(it-sequential "disassemble-chunk-empty"
  (let* ((chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code      (make-array 0 :element-type '(unsigned-byte 32))
                 :constants (vector)))
         (out   (with-output-to-string (s)
                  (cl-cc/bytecode:disassemble-chunk chunk s))))
    (expect (search "0 instruction" out) :to-be-truthy)))

(it-sequential "disassemble-chunk-single-instruction"
  (let* ((chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code      (make-array 1
                                        :element-type '(unsigned-byte 32)
                                        :initial-contents (list (cl-cc/bytecode:encode-nop)))
                  :constants (vector)))
         (out   (with-output-to-string (s)
                   (cl-cc/bytecode:disassemble-chunk chunk s))))
    (expect (search "NOP" out) :to-be-truthy)))

(it-sequential "disassemble-chunk-with-constants"
  (let* ((chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code      (make-array 1
                                        :element-type '(unsigned-byte 32)
                                        :initial-contents (list (cl-cc/bytecode:encode-nop)))
                  :constants (vector 42 :foo)))
         (out   (with-output-to-string (s)
                   (cl-cc/bytecode:disassemble-chunk chunk s))))
    (expect (search "Constant pool" out) :to-be-truthy)
    (expect (search "42" out) :to-be-truthy)))

(it-sequential "disassemble-chunk-multi-instruction"
  (let* ((code (make-array 6
                           :element-type '(unsigned-byte 32)
                           :initial-contents
                           (list (cl-cc/bytecode:encode-load-fixnum 0 42)   ;; imm
                                 (cl-cc/bytecode:encode-load-fixnum 1 7)    ;; imm
                                 (cl-cc/bytecode:encode-add 2 0 1)          ;; 3op
                                 (cl-cc/bytecode:encode-return 2)           ;; 2op
                                 (cl-cc/bytecode:encode-jump 0)             ;; branch
                                 (cl-cc/bytecode:encode-nop))))             ;; special
         (chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code code
                 :constants (vector)))
         (out   (with-output-to-string (s)
                  (cl-cc/bytecode:disassemble-chunk chunk s))))
    ;; Verify instruction count
    (expect (search "6 instruction" out) :to-be-truthy)
    ;; Verify mnemonics for each format type
    (expect (search "LOAD_FIXNUM" out) :to-be-truthy)
    (expect (search "ADD" out) :to-be-truthy)
    (expect (search "RETURN" out) :to-be-truthy)
    (expect (search "JUMP" out) :to-be-truthy)
    (expect (search "NOP" out) :to-be-truthy)
    ;; Verify operand formatting
    (expect (search "r0, 42" out) :to-be-truthy)
    (expect (search "r1, 7" out) :to-be-truthy)
    (expect (search "r2, r0, r1" out) :to-be-truthy)
    (expect (search "r2" out) :to-be-truthy)
    ;; Verify hex word display
    (expect (search "00000000" out) :to-be-truthy)))
