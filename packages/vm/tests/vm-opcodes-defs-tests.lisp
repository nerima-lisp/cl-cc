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

(it-sequential "vm-opcodes-defs-make-vm-state-returns-vm-io-state"
  (let ((s (cl-cc/vm::make-vm-state)))
    (expect (typep s 'cl-cc/vm::vm-io-state) :to-be-truthy)))

(it-sequential "vm-opcodes-defs-make-vm-state-defaults-to-standard-output"
  (let ((s (cl-cc/vm::make-vm-state)))
    (expect (cl-cc/vm::vm-standard-output s) :to-be *standard-output*)))

(it-sequential "vm-opcodes-defs-make-vm-state-accepts-custom-output-stream"
  (let* ((str (make-string-output-stream))
         (s   (cl-cc/vm::make-vm-state :output-stream str)))
    (expect (cl-cc/vm::vm-standard-output s) :to-be str)))

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

(it-sequential "vm-opcodes-defs-vm-state-registers-returns-hash-table"
  (let* ((s    (cl-cc/vm::make-vm-state))
         (regs (cl-cc/vm::vm-state-registers s)))
    (expect (hash-table-p regs) :to-be-truthy)))

(it-sequential "vm-opcodes-defs-vm-standard-output-reflects-custom-stream"
  (let* ((str (make-string-output-stream))
         (s   (cl-cc/vm::make-vm-state :output-stream str)))
    (expect (cl-cc/vm::vm-standard-output s) :to-be str)))

(it-sequential "vm-opcodes-defs-vm-global-vars-returns-hash-table"
  (let ((s (cl-cc/vm::make-vm-state)))
    (expect (hash-table-p (cl-cc/vm::vm-global-vars s)) :to-be-truthy)))

;;; ─── vm-reg-get / vm-reg-set on public VM state ─────────────────────────────

(it-sequential "vm-opcodes-defs-reg-get-fresh-register-nil"
  (let ((s (cl-cc/vm::make-vm-state)))
    (expect (= 0 (cl-cc/vm::vm-reg-get s 0)) :to-be-truthy)
    (expect (= 0 (cl-cc/vm::vm-reg-get s 127)) :to-be-truthy)
    (expect (= 0 (cl-cc/vm::vm-reg-get s 255)) :to-be-truthy)))

(it-sequential "vm-opcodes-defs-reg-set-returns-stored-value"
  (let ((s (cl-cc/vm::make-vm-state)))
    (let ((ret (cl-cc/vm::vm-reg-set s 0 42)))
      (expect (= 42 ret) :to-be-truthy)
      (expect (= 42 (cl-cc/vm::vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-opcodes-defs-reg-set-overwrite-takes-last"
  (let ((s (cl-cc/vm::make-vm-state)))
    (cl-cc/vm::vm-reg-set s 3 :first)
    (cl-cc/vm::vm-reg-set s 3 :second)
    (expect (cl-cc/vm::vm-reg-get s 3) :to-be :second)))

(it-sequential "vm-opcodes-defs-reg-set-adjacent-registers-independent"
  (let ((s (cl-cc/vm::make-vm-state)))
    (cl-cc/vm::vm-reg-set s 10 'alpha)
    (cl-cc/vm::vm-reg-set s 11 'beta)
    (expect (cl-cc/vm::vm-reg-get s 10) :to-be 'alpha)
    (expect (cl-cc/vm::vm-reg-get s 11) :to-be 'beta)))

(it-sequential "vm-opcodes-defs-reg-roundtrip-all-slots"
  (let ((s (cl-cc/vm::make-vm-state)))
    (dotimes (i 64)
      (cl-cc/vm::vm-reg-set s i i))
    (dotimes (i 64)
      (expect (= i (cl-cc/vm::vm-reg-get s i)) :to-be-truthy))))

;;; ─── vm2 low-level constructor remains available explicitly ────────────────

(it-sequential "vm-opcodes-defs-make-vm2-state-returns-vm2-state"
  (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc:vm2-state-p s) :to-be-truthy)))

(it-sequential "vm-opcodes-defs-make-vm2-state-accepts-custom-output-stream"
  (let* ((str (make-string-output-stream))
         (s   (cl-cc:make-vm2-state :output-stream str)))
    (expect (cl-cc:vm2-state-output-stream s) :to-be str)))

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
