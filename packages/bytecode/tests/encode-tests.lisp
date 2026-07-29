;;;; tests/unit/bytecode/encode-tests.lisp - Bytecode ISA v2 Encoder Tests
;;;
;;; Tests for opcode constants, 32-bit instruction word encoding,
;;; bytecode-builder emit/build, and round-trip encoding correctness.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Opcode constants
;;; ------------------------------------------------------------

(it-sequential "bytecode-opcode-constants nop"
  (destructuring-bind (constant expected) (list cl-cc/bytecode:+op-nop+ #x00)
    (expect (= expected constant) :to-be-truthy)))

(it-sequential "bytecode-opcode-constants load-const"
  (destructuring-bind (constant expected) (list cl-cc/bytecode:+op-load-const+ #x01)
    (expect (= expected constant) :to-be-truthy)))

(it-sequential "bytecode-opcode-constants add"
  (destructuring-bind (constant expected) (list cl-cc/bytecode:+op-add+ #x10)
    (expect (= expected constant) :to-be-truthy)))

(it-sequential "bytecode-opcode-constants return"
  (destructuring-bind (constant expected) (list cl-cc/bytecode:+op-return+ #x35)
    (expect (= expected constant) :to-be-truthy)))

(it-sequential "bytecode-opcode-constants wide"
  (destructuring-bind (constant expected) (list cl-cc/bytecode:+op-wide+ #xFE)
    (expect (= expected constant) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; encode-nop
;;; ------------------------------------------------------------

(it-sequential "bytecode-encode-nop-is-zero"
  (expect (= 0 (cl-cc/bytecode:encode-nop)) :to-be-truthy))

;;; ------------------------------------------------------------
;;; encode-3op
;;; ------------------------------------------------------------

(it-sequential "bytecode-encode-3op-cases basic"
  (destructuring-bind (op dst src1 src2) (list cl-cc/bytecode:+op-add+ 1 2 3)
    (assert-bitfield (cl-cc/bytecode:encode-3op op dst src1 src2)
    (24 8 op) (16 8 dst) (8 8 src1) (0 8 src2))))

(it-sequential "bytecode-encode-3op-cases max-registers"
  (destructuring-bind (op dst src1 src2) (list cl-cc/bytecode:+op-add+ 255 255 255)
    (assert-bitfield (cl-cc/bytecode:encode-3op op dst src1 src2)
    (24 8 op) (16 8 dst) (8 8 src1) (0 8 src2))))

(it-sequential "bytecode-encode-3op-cases zero-registers"
  (destructuring-bind (op dst src1 src2) (list cl-cc/bytecode:+op-mul+ 0 0 0)
    (assert-bitfield (cl-cc/bytecode:encode-3op op dst src1 src2)
    (24 8 op) (16 8 dst) (8 8 src1) (0 8 src2))))

;;; ------------------------------------------------------------
;;; encode-2op
;;; ------------------------------------------------------------

(it-sequential "bytecode-encode-2op-packs-bit-fields-correctly"
  (assert-bitfield (cl-cc/bytecode:encode-2op cl-cc/bytecode:+op-neg+ 4 5 0)
    (24 8 cl-cc/bytecode:+op-neg+) (16 8 4) (8 8 5) (0 8 0)))

;;; ------------------------------------------------------------
;;; encode-imm
;;; ------------------------------------------------------------

(it-sequential "bytecode-encode-imm-cases positive"
  (destructuring-bind (reg imm expected-low16) (list 3 100 100)
    (assert-bitfield (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ reg imm)
    (24 8 cl-cc/bytecode:+op-load-fixnum+) (16 8 reg) (0 16 expected-low16))))

(it-sequential "bytecode-encode-imm-cases zero"
  (destructuring-bind (reg imm expected-low16) (list 0 0 0)
    (assert-bitfield (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ reg imm)
    (24 8 cl-cc/bytecode:+op-load-fixnum+) (16 8 reg) (0 16 expected-low16))))

(it-sequential "bytecode-encode-imm-cases negative"
  (destructuring-bind (reg imm expected-low16) (list 1 -1 #xFFFF)
    (assert-bitfield (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ reg imm)
    (24 8 cl-cc/bytecode:+op-load-fixnum+) (16 8 reg) (0 16 expected-low16))))

(it-sequential "bytecode-encode-imm-cases min"
  (destructuring-bind (reg imm expected-low16) (list 0 -32768 #x8000)
    (assert-bitfield (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ reg imm)
    (24 8 cl-cc/bytecode:+op-load-fixnum+) (16 8 reg) (0 16 expected-low16))))

;;; ------------------------------------------------------------
;;; encode-branch
;;; ------------------------------------------------------------

(it-sequential "bytecode-encode-branch-cases zero"
  (destructuring-bind (offset expected-low24) (list 0 0)
    (assert-bitfield (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)
    (24 8 cl-cc/bytecode:+op-jump+) (0 24 expected-low24))))

(it-sequential "bytecode-encode-branch-cases positive"
  (destructuring-bind (offset expected-low24) (list 50 50)
    (assert-bitfield (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)
    (24 8 cl-cc/bytecode:+op-jump+) (0 24 expected-low24))))

(it-sequential "bytecode-encode-branch-cases negative"
  (destructuring-bind (offset expected-low24) (list -1 #xFFFFFF)
    (assert-bitfield (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)
    (24 8 cl-cc/bytecode:+op-jump+) (0 24 expected-low24))))

;;; ------------------------------------------------------------
;;; bytecode-builder: emit and build
;;; ------------------------------------------------------------

(it-sequential "bytecode-builder-empty-chunk"
  (let ((b (cl-cc/bytecode:make-bytecode-builder)))
    (let ((chunk (cl-cc/bytecode:build-bytecode b)))
      (expect (= 0 (length (cl-cc/bytecode:bytecode-chunk-code chunk))) :to-be-truthy))))

(it-sequential "bytecode-builder-emit-one"
  (let ((b (cl-cc/bytecode:make-bytecode-builder)))
    (cl-cc/bytecode:emit (cl-cc/bytecode:encode-nop) b)
    (let ((chunk (cl-cc/bytecode:build-bytecode b)))
      (expect (= 1 (length (cl-cc/bytecode:bytecode-chunk-code chunk))) :to-be-truthy))))

(it-sequential "bytecode-builder-emit-preserves-word"
  (let* ((b (cl-cc/bytecode:make-bytecode-builder))
         (w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 5 6 7)))
    (cl-cc/bytecode:emit w b)
    (let* ((chunk (cl-cc/bytecode:build-bytecode b))
           (code  (cl-cc/bytecode:bytecode-chunk-code chunk)))
      (expect (= w (aref code 0)) :to-be-truthy))))

(it-sequential "bytecode-builder-emit-constant"
  (let ((b (cl-cc/bytecode:make-bytecode-builder)))
    (let ((idx (cl-cc/bytecode:emit-constant 'hello b)))
      (expect (= 0 idx) :to-be-truthy)
      (let ((chunk (cl-cc/bytecode:build-bytecode b)))
        (expect (= 1 (length (cl-cc/bytecode:bytecode-chunk-constants chunk))) :to-be-truthy)
        (expect (aref (cl-cc/bytecode:bytecode-chunk-constants chunk) 0) :to-equal 'hello)))))

(it-sequential "bytecode-builder-emit-multiple"
  (let ((b  (cl-cc/bytecode:make-bytecode-builder))
        (w0 (cl-cc/bytecode:encode-nop))
        (w1 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 3))
        (w2 (cl-cc/bytecode:encode-return 0)))
    (cl-cc/bytecode:emit w0 b)
    (cl-cc/bytecode:emit w1 b)
    (cl-cc/bytecode:emit w2 b)
    (let* ((chunk (cl-cc/bytecode:build-bytecode b))
           (code  (cl-cc/bytecode:bytecode-chunk-code chunk)))
      (expect (= 3 (length code)) :to-be-truthy)
      (expect (= w0 (aref code 0)) :to-be-truthy)
      (expect (= w1 (aref code 1)) :to-be-truthy)
      (expect (= w2 (aref code 2)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; Specific instruction encoders
;;; ------------------------------------------------------------

(it-sequential "bytecode-encode-add"
  (assert-bitfield (cl-cc/bytecode:encode-add 1 2 3)
    (24 8 cl-cc/bytecode:+op-add+) (16 8 1) (8 8 2) (0 8 3)))

(it-sequential "bytecode-encode-move"
  (let ((w (cl-cc/bytecode:encode-move 5 10)))
    (expect (= cl-cc/bytecode:+op-move+ (ldb (byte 8 24) w)) :to-be-truthy)))

(it-sequential "bytecode-encode-jump"
  (assert-bitfield (cl-cc/bytecode:encode-jump 10)
    (24 8 cl-cc/bytecode:+op-jump+) (0 24 10)))

(it-sequential "bytecode-encode-return"
  (assert-bitfield (cl-cc/bytecode:encode-return 3)
    (24 8 cl-cc/bytecode:+op-return+) (16 8 3)))

(it-sequential "bytecode-encode-tail-call-is-3op"
  (assert-bitfield (cl-cc/bytecode:encode-tail-call 5 3)
    (24 8 cl-cc/bytecode:+op-tail-call+) (16 8 0) (8 8 5) (0 8 3)))
