(in-package :cl-cc/optimize)
;;; -----------------------------------------------------------------------------
;;; Optimizer Induction Variables
;;; -----------------------------------------------------------------------------

(defstruct (opt-induction-var (:conc-name opt-iv-))
  "Minimal scalar-evolution summary for a single affine induction variable."
  reg
  init
  step
  update-inst
  (kind :affine)
  multiplier
  offset
  base-reg)

(defun %opt-constant-reg-value (reg constants)
  "Return (values VALUE T) when REG has a known integer constant in CONSTANTS."
  (multiple-value-bind (value found-p) (gethash reg constants)
    (if (and found-p (integerp value))
        (values value t)
        (values nil nil))))

(defun %opt-simple-induction-step (inst constants)
  "Return (values REG STEP T) when INST updates REG by a constant step."
  (let ((dst (opt-inst-dst inst)))
    (cond
      ((and dst (typep inst 'vm-add))
       (cond
         ((eq dst (vm-lhs inst))
          (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
            (when (and ok (not (zerop c))) (values dst c t))))
         ((eq dst (vm-rhs inst))
          (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-lhs inst) constants)
            (when (and ok (not (zerop c))) (values dst c t))))))
      ((and dst (typep inst 'vm-sub) (eq dst (vm-lhs inst)))
       (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
         (when (and ok (not (zerop c))) (values dst (- c) t))))
      ((and dst (typep inst 'vm-inc) (eq dst (vm-src inst)))
       (values dst 1 t))
      ((and dst (typep inst 'vm-dec) (eq dst (vm-src inst)))
       (values dst -1 t)))))

(defun %opt-simple-scaled-expr (inst constants)
  "Return (values BASE MULTIPLIER T) when INST computes BASE * constant."
  (when (typep inst 'vm-mul)
    (let ((dst (opt-inst-dst inst)))
      (when dst
        (cond
          ((eq dst (vm-lhs inst))
           (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
             (when (and ok (not (zerop c))) (values dst c t))))
          ((eq dst (vm-rhs inst))
           (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-lhs inst) constants)
             (when (and ok (not (zerop c))) (values dst c t))))
          (t
           (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
             (when (and ok (not (zerop c)))
               (return-from %opt-simple-scaled-expr (values (vm-lhs inst) c t))))
           (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-lhs inst) constants)
             (when (and ok (not (zerop c)))
               (values (vm-rhs inst) c t)))))))))

(defun %opt-simple-add-expr (inst constants)
  "Return (values BASE OFFSET T) when INST computes BASE + integer constant."
  (when (typep inst '(or vm-add vm-sub))
    (let ((dst (opt-inst-dst inst)))
      (when dst
        (cond
          ((typep inst 'vm-add)
           (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
             (if ok
                 (values (vm-lhs inst) c t)
                 (multiple-value-bind (c2 ok2) (%opt-constant-reg-value (vm-lhs inst) constants)
                   (when ok2 (values (vm-rhs inst) c2 t))))))
          ((typep inst 'vm-sub)
           (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
             (when ok (values (vm-lhs inst) (- c) t)))))))))

(defun %opt-copy-constant-fact (inst constants)
  "Propagate a constant fact through a vm-move, or kill the destination fact."
  (multiple-value-bind (value found-p) (gethash (vm-src inst) constants)
    (if found-p
        (setf (gethash (vm-dst inst) constants) value)
        (remhash (vm-dst inst) constants))))

(defun %opt-compute-simple-inductions-with-constants (instructions constants)
  "Return simple induction summaries for INSTRUCTIONS seeded by CONSTANTS."
  (let ((constants (%opt-copy-constant-table constants))
        (inductions (make-hash-table :test #'eq))
        (exprs (make-hash-table :test #'eq)))
    (dolist (inst instructions inductions)
      (cond
        ((typep inst 'vm-const)
         (remhash (vm-dst inst) inductions)
         (remhash (vm-dst inst) exprs)
         (if (integerp (vm-value inst))
             (setf (gethash (vm-dst inst) constants) (vm-value inst))
             (remhash (vm-dst inst) constants)))
        ((typep inst 'vm-move)
         (remhash (vm-dst inst) inductions)
         (remhash (vm-dst inst) exprs)
         (%opt-copy-constant-fact inst constants))
        (t
         (let ((dst (opt-inst-dst inst))
               (handled nil))
           (multiple-value-bind (reg step ok)
               (%opt-simple-induction-step inst constants)
             (when ok
               (setf handled t)
               (multiple-value-bind (init found-p) (gethash reg constants)
                 (if found-p
                     (setf (gethash reg inductions)
                           (make-opt-induction-var :reg reg
                                                   :init init
                                                   :step step
                                                   :update-inst inst
                                                   :kind :affine
                                                   :offset step))
                     (remhash reg inductions))
                 (remhash reg constants)
                 (remhash reg exprs))))
           (unless handled
             (multiple-value-bind (base multiplier ok)
                 (%opt-simple-scaled-expr inst constants)
               (when ok
                 (setf handled t)
                 (if (eq dst base)
                     (multiple-value-bind (init found-p) (gethash base constants)
                       (if found-p
                           (setf (gethash base inductions)
                                 (make-opt-induction-var :reg base
                                                         :init init
                                                         :step nil
                                                         :update-inst inst
                                                         :kind :geometric
                                                         :multiplier multiplier))
                           (remhash base inductions))
                       (remhash base constants)
                       (remhash base exprs))
                     (progn
                       (setf (gethash dst exprs) (list :mul base multiplier))
                       (remhash dst constants)
                       (remhash dst inductions))))))
           (unless handled
             (multiple-value-bind (base offset ok)
                 (%opt-simple-add-expr inst constants)
               (when ok
                 (let ((base-iv (gethash base inductions))
                       (base-expr (gethash base exprs)))
                   (cond
                     (base-iv
                      (setf handled t)
                      (setf (gethash dst inductions)
                            (make-opt-induction-var :reg dst
                                                    :init (+ (opt-iv-init base-iv) offset)
                                                    :step (opt-iv-step base-iv)
                                                    :update-inst inst
                                                    :kind :derived
                                                    :offset offset
                                                    :base-reg base))
                      (remhash dst constants)
                      (remhash dst exprs))
                     ((and base-expr (eq (second base-expr) dst))
                      (setf handled t)
                      (multiple-value-bind (init found-p) (gethash dst constants)
                        (if found-p
                            (setf (gethash dst inductions)
                                  (make-opt-induction-var :reg dst
                                                          :init init
                                                          :step nil
                                                          :update-inst inst
                                                          :kind :affine-recurrence
                                                          :multiplier (third base-expr)
                                                          :offset offset))
                            (remhash dst inductions))
                        (remhash dst constants)
                        (remhash dst exprs)))
                     ((and dst (not (eq dst base)))
                      (setf handled t)
                      (setf (gethash dst exprs) (list :add base offset))
                      (remhash dst constants)
                      (remhash dst inductions)))))))
           (unless handled
             (when dst
               (remhash dst constants)
               (remhash dst inductions)
               (remhash dst exprs)))))))))

(defun opt-compute-simple-inductions (instructions)
  "Return reg -> opt-induction-var summaries for simple affine updates.

  Recognized patterns are intentionally conservative: affine self-updates,
geometric self-updates `(mul dst dst const)`, two-instruction affine recurrences
`dst = dst * c + d`, and derived variables `j = i + c` where `i` is already an
induction variable. Existing affine callers continue to read OPT-IV-STEP."
  (%opt-compute-simple-inductions-with-constants
   instructions
   (make-hash-table :test #'eq)))

(defun %opt-copy-constant-table (constants)
  "Return an EQ copy of a reg -> integer constant table."
  (let ((copy (make-hash-table :test #'eq)))
    (when constants
      (maphash (lambda (reg value)
                 (setf (gethash reg copy) value))
               constants))
    copy))

(defun %opt-constant-transfer-inst (inst constants)
  "Conservatively update CONSTANTS for simple constant propagation."
  (cond
    ((typep inst 'vm-const)
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) constants) (vm-value inst))
         (remhash (vm-dst inst) constants)))
    ((typep inst 'vm-move)
     (%opt-copy-constant-fact inst constants))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst (remhash dst constants)))))
  constants)

(defun %opt-loop-member-table (blocks)
  (let ((members (make-hash-table :test #'eq)))
    (dolist (block blocks members)
      (setf (gethash block members) t))))

(defun %opt-blocks-in-rpo-order (blocks)
  (sort (copy-list blocks) #'< :key #'bb-rpo-index))

(defun %opt-blocks-instructions (blocks)
  (loop for block in (%opt-blocks-in-rpo-order blocks)
        append (bb-instructions block)))

(defun %opt-loop-seed-constants (loop-blocks)
  "Collect constants from non-loop predecessors entering LOOP-BLOCKS."
  (let ((members (%opt-loop-member-table loop-blocks))
        (constants (make-hash-table :test #'eq)))
    (dolist (block loop-blocks constants)
      (dolist (pred (bb-predecessors block))
        (unless (gethash pred members)
          (dolist (inst (bb-instructions pred))
            (%opt-constant-transfer-inst inst constants)))))))

(defun opt-compute-loop-inductions (cfg-or-instructions)
  "Return header-block -> (reg -> opt-induction-var) for CFG natural loops.

The analysis uses CFG backedges (`tail -> header` where HEADER dominates TAIL)
and `cfg-collect-natural-loop` to keep induction facts scoped to each loop.
Constants from non-loop predecessor blocks seed the per-loop SCEV scan."
  (let* ((cfg (if (cfg-p cfg-or-instructions)
                  cfg-or-instructions
                  (cfg-build cfg-or-instructions)))
         (result (make-hash-table :test #'eq)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (cfg-compute-rpo cfg)
    (loop for tail across (cfg-blocks cfg)
          do (dolist (header (bb-successors tail))
               (when (cfg-dominates-p header tail)
                 (let* ((loop-blocks (cfg-collect-natural-loop header tail))
                        (seed (%opt-loop-seed-constants loop-blocks))
                        (ivs (%opt-compute-simple-inductions-with-constants
                              (%opt-blocks-instructions loop-blocks)
                              seed)))
                   (when (> (hash-table-count ivs) 0)
                     (setf (gethash header result) ivs))))))
    result))

(defun opt-induction-trip-count (init limit step &key inclusive-p predicate)
  "Return a conservative integer trip count for an affine induction variable.

  The loop condition is interpreted as `< limit` for positive STEP and `> limit`
  for negative STEP. With INCLUSIVE-P, the corresponding boundary is `<=` or `>=`.
PREDICATE may be a comparison instruction class/symbol such as `vm-le`, `vm-ge`,
or `vm-eq`; it overrides INCLUSIVE-P when supplied. Returns NIL when STEP is
zero except equality predicates, which are single-test guards."
  (let ((pred (and predicate
                   (if (symbolp predicate) predicate (type-of predicate)))))
    (cond
      ((member pred '(vm-le vm-ge) :test #'eq)
       (return-from opt-induction-trip-count
         (opt-induction-trip-count init limit step :inclusive-p t)))
      ((member pred '(vm-eq vm-num-eq) :test #'eq)
       (return-from opt-induction-trip-count
         (cond
           ((= init limit) 1)
           (t 0))))))
  (cond
    ((zerop step) nil)
    ((plusp step)
     (cond
       ((if inclusive-p (> init limit) (>= init limit)) 0)
       (inclusive-p (1+ (floor (- limit init) step)))
       (t (ceiling (- limit init) step))))
    (t
     (let ((magnitude (- step)))
       (cond
         ((if inclusive-p (< init limit) (<= init limit)) 0)
         (inclusive-p (1+ (floor (- init limit) magnitude)))
         (t (ceiling (- init limit) magnitude)))))))
