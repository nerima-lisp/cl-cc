(in-package :cl-cc/optimize)
;;; -----------------------------------------------------------------------------
;;; Optimizer Value Range Analysis
;;; -----------------------------------------------------------------------------

(defparameter +opt-range-negative-infinity+ most-negative-fixnum
  "Finite sentinel used as the conservative lower bound for path facts.")

(defparameter +opt-range-positive-infinity+ most-positive-fixnum
  "Finite sentinel used as the conservative upper bound for path facts.")

(defun %opt-compute-value-ranges-linear (instructions)
  "Compute conservative intervals for a straight-line instruction list."
  (let ((intervals (make-hash-table :test #'eq)))
    (dolist (inst instructions intervals)
      (%opt-transfer-interval-inst inst intervals))))

(defun %opt-cfg-value-ranges-transfer (block state-in)
  "Transfer interval facts through BLOCK using CFG-safe updates."
  (let ((state-out (%opt-copy-interval-state state-in)))
    (dolist (inst (bb-instructions block) state-out)
      (%opt-transfer-interval-inst inst state-out :kill-self-updates t))))

(defun opt-compute-cfg-value-ranges (cfg-or-instructions)
  "Compute conservative CFG-aware integer value ranges.

Returns an OPT-DATAFLOW-RESULT with per-block IN/OUT maps. Join points keep
only registers known on every incoming path and union their intervals.
Self-updating destinations are killed conservatively to guarantee termination."
  (let* ((cfg (if (cfg-p cfg-or-instructions)
                  cfg-or-instructions
                  (cfg-build cfg-or-instructions)))
         (empty-state (make-hash-table :test #'eq)))
    (opt-run-dataflow cfg
                      :direction :forward
                      :meet #'%opt-merge-interval-states
                      :transfer #'%opt-cfg-value-ranges-transfer
                      :state-equal #'%opt-interval-state-equal-p
                      :initial-state empty-state
                      :boundary-state empty-state
                      :copy-state #'%opt-copy-interval-state)))

(defun %opt-control-flow-range-analysis-p (instructions)
  "Return T when INSTRUCTIONS require CFG-aware range analysis."
  (loop for inst in instructions
        thereis (typep inst '(or vm-label vm-jump vm-jump-zero))))

(defun %opt-cfg-result-exit-state (result)
  "Extract a copy of RESULT's exit OUT state as a plain interval table."
  (let* ((cfg (opt-dataflow-result-cfg result))
         (exit (and cfg (cfg-exit cfg))))
    (if exit
        (%opt-copy-interval-state
         (gethash exit (opt-dataflow-result-out result)))
        (make-hash-table :test #'eq))))

(defvar *opt-block-local-range-table* (make-hash-table :test #'eq)
  "Latest block-local interval facts keyed by BASIC-BLOCK, populated by
OPT-COMPUTE-PATH-SENSITIVE-RANGES for OPT-BLOCK-REG-RANGE queries.")

(defun %opt-record-block-local-ranges (in-table)
  "Snapshot path-sensitive block entry ranges for public block-local queries."
  (clrhash *opt-block-local-range-table*)
  (maphash (lambda (block state)
             (setf (gethash block *opt-block-local-range-table*)
                   (%opt-copy-interval-state state)))
           in-table)
  *opt-block-local-range-table*)

(defun opt-block-reg-range (block reg)
  "Return the latest path-sensitive entry interval for REG at BLOCK as (LO . HI).

The table is populated by OPT-COMPUTE-PATH-SENSITIVE-RANGES.  NIL is returned
when BLOCK has no recorded fact for REG."
  (let* ((state (gethash block *opt-block-local-range-table*))
         (interval (and state (gethash reg state))))
    (and interval
         (opt-make-interval (opt-interval-lo interval)
                            (opt-interval-hi interval)))))

(defun opt-compute-value-ranges (instructions)
  "Compute conservative integer value ranges.

Straight-line callers keep the existing reg -> interval hash-table API. When
INSTRUCTIONS contain control flow, this wrapper runs CFG-aware analysis and
returns the merged exit-state interval table for convenience callers."
  (if (%opt-control-flow-range-analysis-p instructions)
      (%opt-cfg-result-exit-state (opt-compute-cfg-value-ranges instructions))
      (%opt-compute-value-ranges-linear instructions)))

(defun %opt-top-interval ()
  (opt-make-interval +opt-range-negative-infinity+
                     +opt-range-positive-infinity+))

(defun %opt-interval-intersect (a b)
  "Return the intersection of A and B, or NIL when empty."
  (let ((lo (max (opt-interval-lo a) (opt-interval-lo b)))
        (hi (min (opt-interval-hi a) (opt-interval-hi b))))
    (when (<= lo hi)
      (opt-make-interval lo hi))))

(defun %opt-state-interval-or-top (state reg)
  (or (gethash reg state) (%opt-top-interval)))

(defun %opt-narrow-state-reg (state reg constraint)
  "Intersect REG in STATE with CONSTRAINT. Return NIL if the edge is infeasible."
  (let ((narrowed (%opt-interval-intersect
                   (%opt-state-interval-or-top state reg)
                   constraint)))
    (when narrowed
      (setf (gethash reg state) narrowed)
      state)))

(defun %opt-last-instruction-of-type (instructions type)
  (find-if (lambda (inst) (typep inst type)) (reverse instructions)))

(defun %opt-branch-predicate-inst (block jump-inst)
  "Return the comparison instruction feeding JUMP-INST, if it is block-local."
  (let ((cond-reg (vm-reg jump-inst)))
    (find-if (lambda (inst)
               (and (typep inst '(or vm-lt vm-le vm-gt vm-ge vm-eq vm-num-eq))
                    (eq (vm-dst inst) cond-reg)))
             (reverse (bb-instructions block)))))

(defun %opt-successor-is-jump-target-p (succ jump-inst)
  (let ((label (bb-label succ)))
    (and label (equal (vm-name label) (vm-label-name jump-inst)))))

(defun %opt-apply-lt-constraint (state lhs rhs true-p strict-p)
  "Apply LHS < RHS or LHS <= RHS when TRUE-P, otherwise its negation."
  (let* ((lhs-iv (%opt-state-interval-or-top state lhs))
         (rhs-iv (%opt-state-interval-or-top state rhs))
         (lhs-lo (opt-interval-lo lhs-iv))
         (lhs-hi (opt-interval-hi lhs-iv))
         (rhs-lo (opt-interval-lo rhs-iv))
         (rhs-hi (opt-interval-hi rhs-iv)))
    (if true-p
        (let ((lhs-bound (if strict-p (1- rhs-hi) rhs-hi))
              (rhs-bound (if strict-p (1+ lhs-lo) lhs-lo)))
          (and (%opt-narrow-state-reg state lhs
                                      (opt-make-interval +opt-range-negative-infinity+
                                                         lhs-bound))
               (%opt-narrow-state-reg state rhs
                                      (opt-make-interval rhs-bound
                                                         +opt-range-positive-infinity+))))
        (let ((lhs-bound (if strict-p rhs-lo (1+ rhs-lo)))
              (rhs-bound (if strict-p lhs-hi (1- lhs-hi))))
          (and (%opt-narrow-state-reg state lhs
                                      (opt-make-interval lhs-bound
                                                         +opt-range-positive-infinity+))
               (%opt-narrow-state-reg state rhs
                                      (opt-make-interval +opt-range-negative-infinity+
                                                         rhs-bound)))))))

(defun %opt-apply-eq-constraint (state lhs rhs true-p)
  "Apply equality narrowing for the true edge; false edge has no interval fact."
  (if (not true-p)
      state
      (let* ((lhs-iv (%opt-state-interval-or-top state lhs))
             (rhs-iv (%opt-state-interval-or-top state rhs))
             (intersection (%opt-interval-intersect lhs-iv rhs-iv)))
        (when intersection
          (setf (gethash lhs state) intersection
                (gethash rhs state) intersection)
          state))))

(defun %opt-apply-branch-predicate-fact (state cmp-inst true-p)
  "Return STATE narrowed by CMP-INST for TRUE-P, or NIL if infeasible.

For vm-jump-zero, the explicit jump target is the zero/false branch
(the VM jumps when the condition register is 0), while the fallthrough
is the non-zero/true branch."
  (cond
    ((typep cmp-inst 'vm-lt)
     (%opt-apply-lt-constraint state (vm-lhs cmp-inst) (vm-rhs cmp-inst) true-p t))
    ((typep cmp-inst 'vm-le)
     (%opt-apply-lt-constraint state (vm-lhs cmp-inst) (vm-rhs cmp-inst) true-p nil))
    ((typep cmp-inst 'vm-gt)
     (%opt-apply-lt-constraint state (vm-rhs cmp-inst) (vm-lhs cmp-inst) true-p t))
    ((typep cmp-inst 'vm-ge)
     (%opt-apply-lt-constraint state (vm-rhs cmp-inst) (vm-lhs cmp-inst) true-p nil))
    ((typep cmp-inst '(or vm-eq vm-num-eq))
     (%opt-apply-eq-constraint state (vm-lhs cmp-inst) (vm-rhs cmp-inst) true-p))
    (t state)))

(defun %opt-path-edge-state (block succ state-out)
  "Return the outgoing interval state for edge BLOCK -> SUCC."
  (let* ((state (%opt-copy-interval-state state-out))
         (jump (%opt-last-instruction-of-type (bb-instructions block) 'vm-jump-zero))
         (cmp (and jump (%opt-branch-predicate-inst block jump))))
    (if cmp
        (%opt-apply-branch-predicate-fact
         state cmp (not (%opt-successor-is-jump-target-p succ jump)))
        state)))

(defun %opt-predecessor-path-states (block out-table)
  (loop for pred in (bb-predecessors block)
        for out-state = (gethash pred out-table)
        for edge-state = (and out-state (%opt-path-edge-state pred block out-state))
        when edge-state
          collect edge-state))

(defun %opt-loop-header-p (block)
  "Return T when BLOCK has a natural-loop backedge predecessor."
  (some (lambda (pred)
          (cfg-dominates-p block pred))
        (bb-predecessors block)))

(defun %opt-widen-interval-states (old new)
  "Widen OLD interval state toward NEW, preserving CFG meet key semantics."
  (let ((widened (make-hash-table :test #'eq)))
    (when new
      (maphash (lambda (reg new-interval)
                 (let ((old-interval (and old (gethash reg old))))
                   (setf (gethash reg widened)
                         (opt-interval-widen old-interval new-interval
                                             :negative-infinity +opt-range-negative-infinity+
                                             :positive-infinity +opt-range-positive-infinity+))))
               new))
    widened))

(defun %opt-path-sensitive-entry-table (in-table)
  "Flatten block entry states to a (BLOCK . REG) -> interval hash-table."
  (let ((ranges (make-hash-table :test #'equal)))
    (maphash (lambda (block state)
               (maphash (lambda (reg interval)
                          (setf (gethash (cons block reg) ranges)
                                (opt-make-interval (opt-interval-lo interval)
                                                   (opt-interval-hi interval))))
                        state))
             in-table)
    ranges))

(defun opt-compute-path-sensitive-ranges (instructions)
  "Compute value ranges with branch predicate narrowing.
Returns a hash-table mapping (block . reg) to (lo . hi) interval."
  (let* ((cfg (if (cfg-p instructions) instructions (cfg-build instructions)))
         (rpo (progn (cfg-compute-dominators cfg) (cfg-compute-rpo cfg)))
         (in-table (make-hash-table :test #'eq))
         (out-table (make-hash-table :test #'eq))
         (empty-state (make-hash-table :test #'eq)))
    (when (cfg-entry cfg)
      (setf (gethash (cfg-entry cfg) in-table) (%opt-copy-interval-state empty-state)))
    (let ((changed t))
      (loop while changed
            do (setf changed nil)
               (dolist (block rpo)
                  (let* ((raw-incoming (if (eq block (cfg-entry cfg))
                                           (or (gethash block in-table) empty-state)
                                           (%opt-merge-interval-states
                                            (%opt-predecessor-path-states block out-table))))
                         (old-in (gethash block in-table))
                         (incoming (if (and old-in (%opt-loop-header-p block))
                                       (%opt-widen-interval-states old-in raw-incoming)
                                       raw-incoming))
                         (out (%opt-cfg-value-ranges-transfer block incoming))
                         (old-out (gethash block out-table)))
                   (unless (and old-in (%opt-interval-state-equal-p old-in incoming))
                     (setf (gethash block in-table) (%opt-copy-interval-state incoming)
                           changed t))
                    (unless (and old-out (%opt-interval-state-equal-p old-out out))
                      (setf (gethash block out-table) out
                            changed t))))))
    (%opt-record-block-local-ranges in-table)
    (%opt-path-sensitive-entry-table in-table)))

(defun opt-compute-constant-intervals (instructions)
  "Compute a conservative interval map from straight-line constant arithmetic.

Handles vm-const and interval propagation through vm-add/vm-sub/vm-mul when
both operands already have known intervals."
  (opt-compute-value-ranges instructions))

(defun opt-array-bounds-check-eliminable-p (index-reg length-reg intervals &optional block)
  "Return T when INTERVALS prove INDEX-REG is within LENGTH-REG bounds.

INTERVALS may be the classic reg -> interval table, or the path-sensitive
(block . reg) -> interval table returned by OPT-COMPUTE-PATH-SENSITIVE-RANGES
when BLOCK is supplied."
  (flet ((lookup (reg)
           (or (gethash reg intervals)
               (and block (gethash (cons block reg) intervals)))))
    (opt-interval-valid-index-p (lookup index-reg)
                                (lookup length-reg))))
