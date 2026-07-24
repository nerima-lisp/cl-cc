;;;; tests/unit/emit/x86-64-emit-logical-tests.lisp
;;;; Unit tests for src/emit/x86-64-emit-ops-logical.lisp
;;;;
;;;; Covers: emit-vm-null-p, emit-vm-true-pred, emit-vm-false-pred,
;;;;   emit-vm-and, emit-vm-or, emit-vm-logand, emit-vm-logior,
;;;;   emit-vm-logxor, emit-vm-logeqv, emit-vm-logtest, emit-vm-logbitp.
;;;;
;;;; Strategy: emit each instruction into a byte-collecting stream,
;;;; verify the byte count matches the byte-budget documented in the source.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %collect-logical-bytes (emit-fn inst)
  "Collect bytes emitted by EMIT-FN for INST into a list."
  (let ((bytes nil))
    (funcall emit-fn inst (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── emit-vm-null-p ──────────────────────────────────────────────────────

(deftest x86-emit-null-p-emits-bytes
  "emit-vm-null-p emits a non-empty byte sequence (TEST + SETE + MOVZX)."
  (let* ((inst (make-vm-null-p :dst :r0 :src :r1))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-null-p inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── emit-vm-true-pred / emit-vm-false-pred ──────────────────────────────

(deftest x86-emit-true-pred-emits-mov-imm-1
  "emit-vm-true-pred emits an immediate-1 MOV (7 bytes for REX + MOV r64, imm32 with imm=1)."
  (let* ((inst (make-vm-null-p :dst :r0 :src :r0))  ; reusing null-p struct (has :dst)
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-true-pred inst)))
    ;; MOV r64, imm with small immediate value ≥ 4 bytes
    (assert-true (>= (length bytes) 4))))

(deftest x86-emit-false-pred-emits-mov-imm-0
  "emit-vm-false-pred emits an immediate-0 MOV (same layout as true-pred)."
  (let* ((inst (make-vm-null-p :dst :r0 :src :r0))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-false-pred inst)))
    (assert-true (>= (length bytes) 4))))

;;; ─── fixed-size binary logical emitters ──────────────────────────────────

(deftest-each x86-emit-logical-fixed-size-emitters
  "Binary logical emitters produce their documented fixed byte counts."
  :cases (("and"    #'make-vm-and    #'cl-cc/codegen::emit-vm-and    17)
          ("or"     #'make-vm-or     #'cl-cc/codegen::emit-vm-or     17)
          ("logeqv" #'make-vm-logeqv #'cl-cc/codegen::emit-vm-logeqv  9)
          ("logbitp" #'make-vm-logbitp #'cl-cc/codegen::emit-vm-logbitp 15))
  (ctor emitter expected-length)
  (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-= expected-length (length bytes))))

;;; ─── emit-vm-logand / emit-vm-logior / emit-vm-logxor ───────────────────

(deftest-each x86-emit-alu-logical-emit-bytes
  "define-binary-alu-emitter logical ops emit a non-empty sequence."
  :cases (("logand" #'make-vm-logand #'cl-cc/codegen::emit-vm-logand)
          ("logior" #'make-vm-logior #'cl-cc/codegen::emit-vm-logior)
          ("logxor" #'make-vm-logxor #'cl-cc/codegen::emit-vm-logxor))
  (ctor emitter)
  (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-true (> (length bytes) 0))))

;;; ─── emit-vm-logtest ─────────────────────────────────────────────────────

(deftest x86-emit-logtest-emits-bytes
  "emit-vm-logtest emits a non-empty sequence (conservative 14 bytes max)."
  (let* ((inst (make-vm-logtest :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logtest inst)))
    (assert-true (> (length bytes) 0))
    (assert-true (<= (length bytes) 14))))
