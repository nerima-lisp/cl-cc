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

;;; ─── fixed-size binary logical emitters ──────────────────────────────────

(it-sequential "x86-emit-logical-fixed-size-emitters and"
  (destructuring-bind (ctor emitter expected-length) (list #'make-vm-and #'cl-cc/codegen::emit-vm-and 17)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (= expected-length (length bytes)) :to-be-truthy))))

(it-sequential "x86-emit-logical-fixed-size-emitters or"
  (destructuring-bind (ctor emitter expected-length) (list #'make-vm-or #'cl-cc/codegen::emit-vm-or 17)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (= expected-length (length bytes)) :to-be-truthy))))

(it-sequential "x86-emit-logical-fixed-size-emitters logeqv"
  (destructuring-bind (ctor emitter expected-length) (list #'make-vm-logeqv #'cl-cc/codegen::emit-vm-logeqv 9)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (= expected-length (length bytes)) :to-be-truthy))))

(it-sequential "x86-emit-logical-fixed-size-emitters logbitp"
  (destructuring-bind (ctor emitter expected-length) (list #'make-vm-logbitp #'cl-cc/codegen::emit-vm-logbitp 15)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (= expected-length (length bytes)) :to-be-truthy))))

;;; ─── emit-vm-logand / emit-vm-logior / emit-vm-logxor ───────────────────

(it-sequential "x86-emit-alu-logical-emit-bytes logand"
  (destructuring-bind (ctor emitter) (list #'make-vm-logand #'cl-cc/codegen::emit-vm-logand)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (> (length bytes) 0) :to-be-truthy))))

(it-sequential "x86-emit-alu-logical-emit-bytes logior"
  (destructuring-bind (ctor emitter) (list #'make-vm-logior #'cl-cc/codegen::emit-vm-logior)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (> (length bytes) 0) :to-be-truthy))))

(it-sequential "x86-emit-alu-logical-emit-bytes logxor"
  (destructuring-bind (ctor emitter) (list #'make-vm-logxor #'cl-cc/codegen::emit-vm-logxor)
    (let ((bytes (%collect-logical-bytes
                emitter (funcall ctor :dst :r0 :lhs :r1 :rhs :r2))))
    (expect (> (length bytes) 0) :to-be-truthy))))

;;; ─── emit-vm-logtest ─────────────────────────────────────────────────────

(it-sequential "x86-emit-logtest-emits-bytes"
  (let* ((inst (make-vm-logtest :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/codegen::emit-vm-logtest inst)))
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (<= (length bytes) 14) :to-be-truthy)))
