;;;; packages/bytecode/src/decode.lisp - CL-CC Bytecode ISA v2 Field Extraction
;;;;
;;;; Contains: field extraction, opcode name table.
;;;; Disassembler (instruction-format, disassemble-instruction, disassemble-chunk)
;;;; is in decode-disasm.lisp (loads after).

(in-package :cl-cc/bytecode)

;;; ------------------------------------------------------------
;;; Field Extraction (hot-path, all inlined)
;;; ------------------------------------------------------------

(declaim (inline decode-opcode decode-dst decode-src1 decode-src2
                 decode-imm16 decode-offset24 %sign-extend))

(defun %sign-extend (raw bits)
  "Sign-extend RAW from BITS-wide two's complement to a signed integer."
  (declare (type fixnum raw bits)
           (optimize (speed 3) (safety 1)))
  (if (logbitp (1- bits) raw)
      (- raw (ash 1 bits))
      raw))

(defun decode-opcode (word)
  "Extract opcode byte from bits [31:24]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 24) word))

(defun decode-dst (word)
  "Extract dst field from bits [23:16]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 16) word))

(defun decode-src1 (word)
  "Extract src1 field from bits [15:8]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 8) word))

(defun decode-src2 (word)
  "Extract src2 field from bits [7:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 0) word))

(defun decode-imm16 (word)
  "Extract signed 16-bit immediate from bits [15:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (%sign-extend (ldb (byte 16 0) word) 16))

(defun decode-offset24 (word)
  "Extract signed 24-bit branch offset from bits [23:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (%sign-extend (ldb (byte 24 0) word) 24))

;;; ------------------------------------------------------------
;;; Opcode Information Table
;;; ------------------------------------------------------------

;;; Single source of truth for every known opcode: (constant mnemonic format).
;;; #. evaluates each constant at read time; the loops below derive the
;;; mnemonic and instruction-format lookup tables from this one table so the
;;; two can never drift out of sync.
;;;
;;; Instruction formats:
;;;   :3op     — [opcode:8][dst:8][src1:8][src2:8]
;;;   :2op     — [opcode:8][dst:8][src:8][pad:8]
;;;   :imm     — [opcode:8][dst:8][imm16:16]
;;;   :branch  — [opcode:8][offset:24]
;;;   :special — zero-operand / raw (NOP, RETURN_NIL, POP_HANDLER, ...)
(defparameter *opcode-info-data*
  '(;; Misc
    (#.+op-nop+          "NOP"          :special)
    ;; Load / Move
    (#.+op-load-const+   "LOAD_CONST"   :3op)
    (#.+op-move+         "MOVE"         :2op)
    (#.+op-load-nil+     "LOAD_NIL"     :2op)
    (#.+op-load-true+    "LOAD_TRUE"    :2op)
    (#.+op-load-fixnum+  "LOAD_FIXNUM"  :imm)
    ;; Arithmetic
    (#.+op-add+          "ADD"          :3op)
    (#.+op-sub+          "SUB"          :3op)
    (#.+op-mul+          "MUL"          :3op)
    (#.+op-div+          "DIV"          :3op)
    (#.+op-mod+          "MOD"          :3op)
    (#.+op-neg+          "NEG"          :2op)
    (#.+op-inc+          "INC"          :2op)
    (#.+op-dec+          "DEC"          :2op)
    ;; Comparison
    (#.+op-eq+           "EQ"           :3op)
    (#.+op-eql+          "EQL"          :3op)
    (#.+op-equal+        "EQUAL"        :3op)
    (#.+op-num-lt+       "NUM_LT"       :3op)
    (#.+op-num-gt+       "NUM_GT"       :3op)
    (#.+op-num-le+       "NUM_LE"       :3op)
    (#.+op-num-ge+       "NUM_GE"       :3op)
    (#.+op-num-eq+       "NUM_EQ"       :3op)
    ;; Control Flow
    (#.+op-jump+         "JUMP"         :branch)
    (#.+op-jump-if-nil+  "JUMP_IF_NIL"  :imm)
    (#.+op-jump-if-true+ "JUMP_IF_TRUE" :imm)
    (#.+op-call+         "CALL"         :3op)
    (#.+op-tail-call+    "TAIL_CALL"    :3op)
    (#.+op-return+       "RETURN"       :2op)
    (#.+op-return-nil+   "RETURN_NIL"   :special)
    ;; Closure & Upvalue
    (#.+op-make-closure+  "MAKE_CLOSURE"  :3op)
    (#.+op-get-upvalue+   "GET_UPVALUE"   :2op)
    (#.+op-set-upvalue+   "SET_UPVALUE"   :2op)
    (#.+op-close-upvalue+ "CLOSE_UPVALUE" :2op)
    ;; Object Access
    (#.+op-get-slot+      "GET_SLOT"      :3op)
    (#.+op-set-slot+      "SET_SLOT"      :3op)
    (#.+op-get-global+    "GET_GLOBAL"    :2op)
    (#.+op-set-global+    "SET_GLOBAL"    :2op)
    (#.+op-make-instance+ "MAKE_INSTANCE" :3op)
    ;; Collections
    (#.+op-cons+          "CONS"          :3op)
    (#.+op-car+           "CAR"           :2op)
    (#.+op-cdr+           "CDR"           :2op)
    (#.+op-make-vector+   "MAKE_VECTOR"   :3op)
    (#.+op-vector-ref+    "VECTOR_REF"    :3op)
    (#.+op-vector-set+    "VECTOR_SET"    :3op)
    (#.+op-make-hash+     "MAKE_HASH"     :2op)
    (#.+op-hash-ref+      "HASH_REF"      :3op)
    (#.+op-hash-set+      "HASH_SET"      :3op)
    ;; Type Check
    (#.+op-type-check+    "TYPE_CHECK"    :3op)
    (#.+op-fixnump+       "FIXNUMP"       :2op)
    (#.+op-consp+         "CONSP"         :2op)
    (#.+op-symbolp+       "SYMBOLP"       :2op)
    (#.+op-functionp+     "FUNCTIONP"     :2op)
    (#.+op-stringp+       "STRINGP"       :2op)
    ;; Multiple Values
    (#.+op-values+        "VALUES"        :2op)
    (#.+op-recv-values+   "RECV_VALUES"   :2op)
    ;; Exception Handling
    (#.+op-push-handler+  "PUSH_HANDLER"  :imm)
    (#.+op-pop-handler+   "POP_HANDLER"   :special)
    (#.+op-signal+        "SIGNAL"        :2op)
    (#.+op-push-unwind+   "PUSH_UNWIND"   :branch)
    (#.+op-pop-unwind+    "POP_UNWIND"    :2op)
    ;; Wide prefix
    (#.+op-wide+          "WIDE"          :special))
  "Table of (opcode-byte mnemonic-string instruction-format) for all known opcodes.")

(defvar *opcode-names*
  (loop with ht = (make-hash-table :test #'eql)
        for (code name) in *opcode-info-data*
        do (setf (gethash code ht) name)
        finally (return ht))
  "Hash table mapping opcode byte value to its mnemonic string.")

(defvar *opcode-formats*
  (loop with ht = (make-hash-table :test #'eql)
        for (code nil format) in *opcode-info-data*
        do (setf (gethash code ht) format)
        finally (return ht))
  "Hash table mapping opcode byte value to its instruction-format keyword.")

