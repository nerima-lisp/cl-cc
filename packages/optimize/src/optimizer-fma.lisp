(in-package :cl-cc/optimize)
;;; -----------------------------------------------------------------------------
;;; Optimizer FMA Recognition
;;; -----------------------------------------------------------------------------

;;; FR-099: FMA (Fused Multiply-Add) Pattern Recognition
;;;
;;; Pattern: (vm-float-mul A B → T) followed by (vm-float-add C T → D) where T is used only once
;;; → replace with a single FMA instruction (vm-fma A B C → D).

(defun %opt-register-read-count (reg instructions)
  "Return how many times REG is read by INSTRUCTIONS."
  (loop for inst in instructions
        sum (count reg (opt-inst-read-regs inst) :test #'eq)))

(defun %opt-fma-block-boundary-p (inst)
  "Return T when INST delimits a basic block for local FMA recognition."
  (or (typep inst 'vm-label)
      (eq (vm-inst-effect-kind inst) :control)))

(defun %opt-fma-pure-p (inst)
  "Return T when INST is side-effect free enough for FMA replacement."
  (eq (vm-inst-effect-kind inst) :pure))

(defun %opt-fma-add-accumulator (mul add)
  "Return ADD's non-MUL accumulator operand, or NIL when ADD does not read MUL."
  (let ((tmp (vm-dst mul)))
    (cond
      ((eq tmp (vm-lhs add)) (vm-rhs add))
      ((eq tmp (vm-rhs add)) (vm-lhs add))
      (t nil))))

(defun %opt-inst-writes-reg-p (inst reg)
  "Return T when INST writes REG."
  (eq (opt-inst-dst inst) reg))

(defun %opt-fma-intervening-barrier-p (inst protected-regs)
  "Return T when INST prevents moving an earlier multiply to a later FMA site."
  (or (not (%opt-fma-pure-p inst))
      (some (lambda (reg) (%opt-inst-writes-reg-p inst reg)) protected-regs)))

(defun %opt-fma-replacement (mul add instructions)
  "Return a VM-FMA replacing MUL and ADD when the FR-099 guards hold."
  (let ((acc (%opt-fma-add-accumulator mul add)))
    (when (and acc
               (%opt-fma-pure-p mul)
               (%opt-fma-pure-p add)
               (= 1 (%opt-register-read-count (vm-dst mul) instructions)))
      (make-vm-fma :dst (vm-dst add)
                   :a (vm-lhs mul)
                   :b (vm-rhs mul)
                   :c acc))))

(defun %opt-fuse-fma-in-block (block instructions)
  "Recognize FR-099 FMA patterns inside one basic block."
  (let* ((n (length block))
         (insts (coerce block 'vector))
         (removed (make-array n :initial-element nil))
         (replacements (make-hash-table :test #'eql)))
    (loop for mul-index from 0 below n
          for mul = (aref insts mul-index)
          when (and (not (aref removed mul-index))
                    (typep mul 'vm-float-mul)
                    (%opt-fma-pure-p mul)
                    (= 1 (%opt-register-read-count (vm-dst mul) instructions)))
            do (let ((protected-regs (remove-duplicates
                                      (list (vm-dst mul) (vm-lhs mul) (vm-rhs mul))
                                      :test #'eq)))
                 (loop for add-index from (1+ mul-index) below n
                       for candidate = (aref insts add-index)
                       do (cond
                            ((and (not (aref removed add-index))
                                  (typep candidate 'vm-float-add))
                             (let ((replacement (%opt-fma-replacement mul candidate instructions)))
                               (when replacement
                                 (setf (aref removed mul-index) t)
                                 (setf (gethash add-index replacements) replacement)
                                 (return))))
                            ((%opt-fma-intervening-barrier-p candidate protected-regs)
                             (return))))))
    (loop for i from 0 below n
          unless (aref removed i)
            collect (or (gethash i replacements) (aref insts i)))))

(defun opt-pass-fma-recognition (instructions)
  "FR-099: Recognize scalar floating FMA in flat VM instruction streams.

Within each basic block, detects (vm-float-mul A B → T) feeding exactly one
(vm-float-add T C → D) or (vm-float-add C T → D), then replaces the pair with
(vm-fma D A B C).  The pass refuses to cross labels/control-flow boundaries,
requires both arithmetic instructions to be pure, and preserves operand values by
not fusing across intervening writes to the multiply destination or operands."
  (let ((result nil)
        (block nil))
    (labels ((flush-block ()
               (when block
                 (setf result (nconc result (%opt-fuse-fma-in-block (nreverse block) instructions)))
                 (setf block nil))))
      (dolist (inst instructions)
        (if (%opt-fma-block-boundary-p inst)
            (progn
              (flush-block)
              (setf result (nconc result (list inst))))
            (push inst block)))
      (flush-block)
      result)))
