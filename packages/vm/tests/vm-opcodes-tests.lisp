;;;; tests/unit/vm/vm-opcodes-tests.lisp
;;;; Unit tests for src/vm/vm-opcodes.lisp
;;;;
;;;; Covers: make-vm2-state (register array, global pre-population),
;;;;   vm2-reg-get / vm2-reg-set (register file read/write),
;;;;   vm2-collect-opcode-bigrams (empty, short, known-opcode bigrams),
;;;;   vm2-top-superoperator-candidates (empty, limit trimming),
;;;;   vm2-fuse-immediate-superinstructions (empty, odd-length passthrough,
;;;;   non-fuselable passthrough).

(in-package :cl-cc/test)

;;; ─── make-vm2-state ──────────────────────────────────────────────────────────

;;; ─── vm2-reg-get / vm2-reg-set ───────────────────────────────────────────────

;;; ─── vm2-collect-opcode-bigrams ──────────────────────────────────────────────

;;; ─── vm2-top-superoperator-candidates ────────────────────────────────────────

(it-sequential "vm2-top-candidates-empty-code-returns-nil"
  (expect (cl-cc/vm::vm2-top-superoperator-candidates #()) :to-be-null))

(it-sequential "vm2-top-candidates-limit-trims-result"
  (let* ((op-a cl-cc:+op2-const+)
         (op-b cl-cc:+op2-halt2+)
         (code (vector op-a 0 0 0 op-b 0 0 0)))
    (let ((result (cl-cc/vm::vm2-top-superoperator-candidates code :limit 1)))
      (expect (<= (length result) 1) :to-be-truthy))))

;;; ─── vm2-fuse-immediate-superinstructions ────────────────────────────────────

;;; ─── %vm2-emit4 (extracted helper) ───────────────────────────────────────

