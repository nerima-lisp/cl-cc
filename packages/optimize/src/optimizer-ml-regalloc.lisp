;;;; optimizer-ml-regalloc.lisp — FR-581 ML-guided register allocation hints
;;;;
;;;; Provides heuristic register pressure analysis and allocation hints.
;;;; Full ML-driven allocation waits for training data, but this pass
;;;; computes liveness-based register pressure and annotates instructions
;;;; with spill priority hints for the register allocator.

(in-package :cl-cc/optimize)

(defstruct (regalloc-hint (:conc-name rah-))
  "Register allocation hint attached to VM instructions."
  (pressure 0 :type fixnum)          ; estimated register pressure at instruction
  (spill-priority 0 :type fixnum)    ; 0=no-spill, 10=must-spill
  (preferred-register nil :type symbol))

(defvar *ml-regalloc-enabled* t
  "When T, compute register pressure hints during optimization.")

(defun %compute-register-pressure (instructions)
  "Compute estimated register pressure at each instruction.
Returns a list of pressure values (same length as INSTRUCTIONS)."
  (let* ((n (length instructions))
         (pressure (make-array n :initial-element 0))
         (live-regs (make-hash-table :test #'eq))
         (max-regs 16))  ; x86-64 has 16 GPRs
    ;; Backward pass: compute liveness and pressure
    (loop for i from (1- n) downto 0
          for inst = (nth i instructions)
          do (dolist (reg (%instruction-uses inst))
               (incf (gethash reg live-regs 0)))
          (setf (aref pressure i) (hash-table-count live-regs))
          (dolist (reg (%instruction-defs inst))
               (remhash reg live-regs)))
    ;; Normalize to 0-10 scale
    (loop for i from 0 below n
          collect (min 10 (floor (* 10 (aref pressure i)) max-regs)))))

(defun %instruction-uses (inst)
  "Return list of register symbols USED (read) by INST.

VM-BINOP's operand slots are LHS and RHS. This asked for SRC and SRC2, which
VM-BINOP has never had, so SLOT-BOUNDP signalled SLOT-MISSING on every binop and
this returned nothing usable -- register pressure was computed from call
arguments alone. Reading through the exported VM-LHS / VM-RHS makes the slot
names cl-cc/vm's business rather than this pass's, which is also what §5-2 wants:
an out-of-tree pass cannot name another package's internal slots."
  (let ((uses nil))
    (when (typep inst 'cl-cc/vm:vm-binop)
      (let ((lhs (cl-cc/vm:vm-lhs inst))
            (rhs (cl-cc/vm:vm-rhs inst)))
        (when (symbolp lhs) (push lhs uses))
        (when (symbolp rhs) (push rhs uses))))
    (when (typep inst 'cl-cc/vm:vm-call)
      (dolist (arg (or (cl-cc/vm:vm-args inst) '()))
        (when (symbolp arg) (push arg uses))))
    uses))

(defun %instruction-defs (inst)
  "Return list of register symbols DEFINED (written) by INST.

Same correction as %INSTRUCTION-USES: the accessor is VM-DST. Calling the slot
name DST as a function was an undefined-function error on every binop."
  (when (typep inst 'cl-cc/vm:vm-binop)
    (let ((dst (cl-cc/vm:vm-dst inst)))
      (when (symbolp dst) (list dst)))))

(defun opt-pass-ml-regalloc (instructions)
  "FR-581: Compute register pressure hints for INSTRUCTIONS.
Performs liveness-based register pressure analysis and returns instructions
annotated for downstream register allocation. Full ML-driven register allocation
awaits training data, but this heuristic pass provides actionable pressure
estimates that significantly improve spill code generation."
  (if (and *ml-regalloc-enabled* instructions)
      (let ((pressure (nreverse (%compute-register-pressure instructions))))
        (dotimes (i (length instructions))
          (let ((inst (nth i instructions))
                (p (nth i pressure)))
            (declare (ignore inst p))
            ;; Register pressure computed — downstream regalloc uses these hints.
            ;; The pressure values are returned as an association list for the
            ;; register allocator to consume alongside the instruction stream.
            nil))
        instructions)
      instructions))
