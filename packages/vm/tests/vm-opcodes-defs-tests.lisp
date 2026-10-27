;;;; tests/unit/vm/vm-opcodes-defs-tests.lisp
;;;; Unit tests for src/vm/vm-opcodes-defs.lisp public API.
;;;;
;;;; Covers:
;;;;   make-vm-state        — public constructor for canonical execution state
;;;;   vm-reg-get/vm-reg-set — polymorphic register accessors across state implementations
;;;;   vm-state-registers   — generic method on vm2-state
;;;;   vm-output-stream     — generic method on vm2-state
;;;;   vm-global-vars       — generic method on vm2-state
;;;;   run-vm-with-opcode-bigrams — bigram profiling variant of run-vm

(in-package :cl-cc/test)

;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun make-bytecode2 (&rest words)
  "Build a simple-vector from WORDS for vm2 bytecode tests."
  (coerce words 'simple-vector))

;;; ─── make-vm-state ──────────────────────────────────────────────────────────

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

;;; ─── vm-reg-get / vm-reg-set on public VM state ─────────────────────────────

;;; ─── vm2 low-level constructor remains available explicitly ────────────────

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

(it-sequential "vm-opcodes-defs-run-vm-bigrams-returns-halted-value"
  (let* ((code (make-bytecode2 cl-cc:+op2-const+ 0 99 nil
                                cl-cc:+op2-halt2+ 0 nil nil))
         (s    (cl-cc:make-vm2-state)))
    (expect (= 99 (cl-cc/vm::run-vm-with-opcode-bigrams code s)) :to-be-truthy)))

(it-sequential "vm-opcodes-defs-run-vm-bigrams-second-value-is-hash-table"
  (let* ((code (make-bytecode2 cl-cc:+op2-const+ 0 1 nil
                                cl-cc:+op2-halt2+ 0 nil nil))
         (s    (cl-cc:make-vm2-state)))
    (multiple-value-bind (result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams code s)
      (declare (ignore result))
      (expect (hash-table-p counts) :to-be-truthy))))
