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

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %collect-logical-bytes (emit-fn inst)
  "Collect bytes emitted by EMIT-FN for INST into a list."
  (let ((bytes nil))
    (funcall emit-fn inst (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── emit-vm-null-p ──────────────────────────────────────────────────────

(it-sequential "x86-emit-null-p-emits-bytes"
  (let* ((inst (make-vm-null-p :dst :r0 :src :r1))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-null-p inst)))
    (expect (> (length bytes) 0) :to-be-truthy)))

;;; ─── emit-vm-true-pred / emit-vm-false-pred ──────────────────────────────

(it-sequential "x86-emit-true-pred-emits-mov-imm-1"
  (let* ((inst (make-vm-null-p :dst :r0 :src :r0))  ; reusing null-p struct (has :dst)
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-true-pred inst)))
    ;; MOV r64, imm with small immediate value ≥ 4 bytes
    (expect (>= (length bytes) 4) :to-be-truthy)))

(it-sequential "x86-emit-false-pred-emits-mov-imm-0"
  (let* ((inst (make-vm-null-p :dst :r0 :src :r0))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-false-pred inst)))
    (expect (>= (length bytes) 4) :to-be-truthy)))

;;; ─── emit-vm-and / emit-vm-or ────────────────────────────────────────────

(it-sequential "x86-emit-vm-and-emits-17-bytes"
  (let* ((inst (make-vm-and :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-and inst)))
    (expect (= 17 (length bytes)) :to-be-truthy)))

(it-sequential "x86-emit-vm-or-emits-17-bytes"
  (let* ((inst (make-vm-or :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-or inst)))
    (expect (= 17 (length bytes)) :to-be-truthy)))

;;; ─── emit-vm-logand / emit-vm-logior / emit-vm-logxor ───────────────────

(it-sequential "x86-emit-logand-emits-bytes"
  (let* ((inst (make-vm-logand :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logand inst)))
    (expect (> (length bytes) 0) :to-be-truthy)))

(it-sequential "x86-emit-logior-emits-bytes"
  (let* ((inst (make-vm-logior :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logior inst)))
    (expect (> (length bytes) 0) :to-be-truthy)))

(it-sequential "x86-emit-logxor-emits-bytes"
  (let* ((inst (make-vm-logxor :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logxor inst)))
    (expect (> (length bytes) 0) :to-be-truthy)))

;;; ─── emit-vm-logeqv ──────────────────────────────────────────────────────

(it-sequential "x86-emit-logeqv-emits-9-bytes"
  (let* ((inst (make-vm-logeqv :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logeqv inst)))
    (expect (= 9 (length bytes)) :to-be-truthy)))

;;; ─── emit-vm-logtest ─────────────────────────────────────────────────────

(it-sequential "x86-emit-logtest-emits-bytes"
  (let* ((inst (make-vm-logtest :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logtest inst)))
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (<= (length bytes) 14) :to-be-truthy)))

;;; ─── emit-vm-logbitp ─────────────────────────────────────────────────────

(it-sequential "x86-emit-logbitp-emits-15-bytes"
  (let* ((inst (make-vm-logbitp :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logbitp inst)))
    (expect (= 15 (length bytes)) :to-be-truthy)))
