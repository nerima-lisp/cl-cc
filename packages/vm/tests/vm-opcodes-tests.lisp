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

(it-sequential "vm2-state-creation-register-array-and-globals"
  (let ((s (cl-cc:make-vm2-state)))
    (expect (simple-vector-p (cl-cc:vm2-state-registers s)) :to-be-truthy)
    (expect (= cl-cc/vm::+vm-register-count+ (length (cl-cc:vm2-state-registers s))) :to-be-truthy)
    (expect (svref (cl-cc:vm2-state-registers s) 0) :to-be-null)
    (expect (svref (cl-cc:vm2-state-registers s) 255) :to-be-null)
    (expect (hash-table-p (cl-cc:vm2-state-global-vars s)) :to-be-truthy)
    (expect (nth-value 1 (gethash '*features* (cl-cc:vm2-state-global-vars s))) :to-be-truthy)
    (expect (cl-cc:vm2-state-output-stream s) :to-be *standard-output*)))

;;; ─── vm2-reg-get / vm2-reg-set ───────────────────────────────────────────────

(it-sequential "vm2-reg-get-fresh-registers-are-nil"
  (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::vm2-reg-get s 0) :to-be-null)
    (expect (cl-cc/vm::vm2-reg-get s 128) :to-be-null)
    (expect (cl-cc/vm::vm2-reg-get s 255) :to-be-null)))

(it-sequential "vm2-reg-set-returns-value-and-stores-it"
  (let ((s (cl-cc:make-vm2-state)))
    (let ((ret (cl-cc/vm::vm2-reg-set s 0 42)))
      (expect (= 42 ret) :to-be-truthy)
      (expect (= 42 (cl-cc/vm::vm2-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm2-reg-set-adjacent-registers-are-independent"
  (let ((s (cl-cc:make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 5 :foo)
    (cl-cc/vm::vm2-reg-set s 6 :bar)
    (expect (cl-cc/vm::vm2-reg-get s 5) :to-be :foo)
    (expect (cl-cc/vm::vm2-reg-get s 6) :to-be :bar)))

(it-sequential "vm2-reg-set-overwrite-takes-last"
  (let ((s (cl-cc:make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 10 'first)
    (cl-cc/vm::vm2-reg-set s 10 'second)
    (expect (cl-cc/vm::vm2-reg-get s 10) :to-be 'second)))

;;; ─── vm2-collect-opcode-bigrams ──────────────────────────────────────────────

(it-sequential "vm2-collect-bigrams-empty-vector-returns-empty-table"
  (let ((result (cl-cc/vm::vm2-collect-opcode-bigrams #())))
    (expect (hash-table-p result) :to-be-truthy)
    (expect (= 0 (hash-table-count result)) :to-be-truthy)))

(it-sequential "vm2-collect-bigrams-single-instruction-yields-no-pairs"
  (let ((result (cl-cc/vm::vm2-collect-opcode-bigrams #(0 0 0 0))))
    (expect (= 0 (hash-table-count result)) :to-be-truthy)))

(it-sequential "vm2-collect-bigrams-known-opcode-pair-counted"
  (let* ((op-a cl-cc:+op2-const+)
         (op-b cl-cc:+op2-halt2+)
         (code (vector op-a 0 0 0 op-b 0 0 0))
         (result (cl-cc/vm::vm2-collect-opcode-bigrams code)))
    ;; The pair (CONST HALT2) should appear with count 1
    (let ((pair-name-a (aref cl-cc/vm::*opcode-name-table* op-a))
          (pair-name-b (aref cl-cc/vm::*opcode-name-table* op-b)))
      (when (and pair-name-a pair-name-b)
        (expect (= 1 (gethash (list pair-name-a pair-name-b) result 0)) :to-be-truthy)))))

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

(it-sequential "vm2-fuse-empty-returns-empty"
  (let ((result (cl-cc/vm::vm2-fuse-immediate-superinstructions #())))
    (expect (vectorp result) :to-be-truthy)
    (expect (= 0 (length result)) :to-be-truthy)))

(it-sequential "vm2-fuse-single-instruction-passthrough"
  (let* ((op cl-cc:+op2-halt2+)
         (code (vector op 0 0 0))
         (result (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (expect (vectorp result) :to-be-truthy)
    (expect (= 4 (length result)) :to-be-truthy)
    (expect (= op (svref result 0)) :to-be-truthy)))

(it-sequential "vm2-fuse-const-halt-fused-to-const-halt2"
  (let* ((op-const cl-cc:+op2-const+)
         (op-halt  cl-cc:+op2-halt2+)
         (op-fused cl-cc:+op2-const-halt2+)
         (code (vector op-const 7 99 nil op-halt 7 nil nil))
         (result (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    ;; Fused → 4 words, first word is the superinstruction opcode
    (expect (= 4 (length result)) :to-be-truthy)
    (expect (= op-fused (svref result 0)) :to-be-truthy)))

;;; ─── %vm2-emit4 (extracted helper) ───────────────────────────────────────

(it-sequential "vm2-emit4-appends-four-elements"
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (cl-cc/vm::%vm2-emit4 out 10 20 30 40)
    (expect (= 4 (length out)) :to-be-truthy)
    (expect (= 10 (aref out 0)) :to-be-truthy)
    (expect (= 20 (aref out 1)) :to-be-truthy)
    (expect (= 30 (aref out 2)) :to-be-truthy)
    (expect (= 40 (aref out 3)) :to-be-truthy)))

(it-sequential "vm2-emit4-multiple-calls-accumulate"
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (cl-cc/vm::%vm2-emit4 out 1 2 3 4)
    (cl-cc/vm::%vm2-emit4 out 5 6 7 8)
    (expect (= 8 (length out)) :to-be-truthy)
    (expect (= 5 (aref out 4)) :to-be-truthy)
    (expect (= 8 (aref out 7)) :to-be-truthy)))
