(in-package :cl-cc/regalloc)
;;; ------------------------------------------------------------------------
;;; Regalloc - Allocation Policy and Register Preferences
;;;
;;; Owns allocator selection, ML-style spill cost scoring, ABI-aware register
;;; preference strategies, and target register-class policy. Allocation passes
;;; depend on this file; it must not depend on color, spill, or LSA internals.
;;; ------------------------------------------------------------------------

(defvar *current-allocation-policy* nil
  "Hint-derived allocation policy plist bound during allocation.
Recognized keys:
  :prefer-callee-saved-p
  :prefer-caller-saved-p")

(defparameter *regalloc-allocation-strategy* :linear-scan
  "Default register allocation strategy.

Valid values are :LINEAR-SCAN and :COLOR.  Linear scan remains the default;
graph coloring can be selected by binding this variable or by passing
:ALLOCATOR :COLOR in ALLOCATION-POLICY to ALLOCATE-REGISTERS.")

(defparameter *ml-regalloc-enabled* nil
  "When non-NIL, use FR-581 ML-style spill-cost prediction for spill choices.")

(defvar *current-regalloc-loop-depths* nil
  "Position -> loop-depth table bound while FR-581 spill decisions are made.")

(defun regalloc-loop-depths (instructions)
  "Return a position -> loop-depth table from backward branches in INSTRUCTIONS."
  (let ((label-pos (make-hash-table :test #'equal))
        (depths (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) label-pos) i))
    (loop for inst in instructions
          for i from 0
          when (typep inst '(or vm-jump vm-jump-zero))
            do (let ((target (gethash (vm-label-name inst) label-pos)))
                 (when (and target (< target i))
                   (loop for j from target to i do (incf (gethash j depths 0))))))
    depths))

(defun regalloc-ml-spill-cost (interval &optional (loop-depths *current-regalloc-loop-depths*))
  "Predict spill cost for INTERVAL using frequency × loop-depth weighted uses.
Higher scores mean the interval should be kept in a register when possible."
  (let ((weighted-uses
          (loop for pos in (interval-use-positions interval)
                sum (+ 1 (* 8 (if loop-depths (gethash pos loop-depths 0) 0))))))
    (+ weighted-uses
       (if (interval-crosses-call-p interval) 2 0)
       (if (interval-return-value-p interval) 4 0)
       (if (interval-remat-const interval) -6 0)
       (if (interval-remat-inst interval) -3 0))))

(defun %interval-next-use-after (interval position)
  "Return the next use position of INTERVAL after POSITION, or NIL."
  (find-if (lambda (use-pos) (> use-pos position))
           (interval-use-positions interval)))

(defun %return-value-preferred-reg (interval cc free-regs)
  "Strategy: prefer the ABI return register for return-value intervals."
  (let ((preferred (and (interval-return-value-p interval)
                        (if (interval-fp-p interval)
                            (target-fp-ret-reg cc)
                            (target-ret-reg cc)))))
    (and preferred (member preferred free-regs) preferred)))

(defun %call-crossing-preferred-reg (interval cc free-regs)
  "Strategy: prefer a callee-saved register for intervals crossing a call."
  (when (and (interval-crosses-call-p interval) (not (interval-fp-p interval)))
    (find-if (lambda (reg) (member reg free-regs :test #'eq))
             (target-callee-saved cc))))

(defun %param-preferred-reg (interval cc free-regs)
  "Strategy: prefer the ABI argument register matching the parameter position."
  (let* ((param-index (interval-parameter-index interval))
         (arg-regs (if (interval-fp-p interval) (target-fp-arg-regs cc) (target-arg-regs cc)))
         (preferred (and (integerp param-index)
                          (< param-index (length arg-regs))
                          (nth param-index arg-regs))))
    (and preferred (member preferred free-regs) preferred)))

(defun %hint-policy-preferred-reg (interval cc free-regs)
  "Strategy: when policy prefers caller-saved regs, bias allocation accordingly.
Safety guard: do not force caller-saved for call-crossing intervals."
  (when (and (getf *current-allocation-policy* :prefer-caller-saved-p)
             (not (interval-crosses-call-p interval))
             (not (interval-fp-p interval)))
    (find-if (lambda (reg) (member reg free-regs :test #'eq))
             (target-caller-saved cc))))

(defparameter *preferred-register-strategies*
  (list #'%hint-policy-preferred-reg
        #'%return-value-preferred-reg
        #'%call-crossing-preferred-reg
        #'%param-preferred-reg)
  "Ordered list of preferred-register strategies tried left-to-right; first truthy result wins.")

(defun regalloc-target-fp-registers (cc)
  "Return the allocatable floating-point register pool for CC."
  (case (target-name cc)
    (:x86-64 '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
               :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))
    (:aarch64 '(:v0 :v1 :v2 :v3 :v4 :v5 :v6 :v7
                :v8 :v9 :v10 :v11 :v12 :v13 :v14 :v15
                :v16 :v17 :v18 :v19 :v20 :v21 :v22 :v23
                :v24 :v25 :v26 :v27 :v28 :v29 :v30 :v31))
    (otherwise
     (remove-duplicates
      (append (copy-list (target-fp-arg-regs cc))
              (list (target-fp-ret-reg cc)))
      :test #'eq))))

(defun %derive-single-function-policy (instructions)
  "Best-effort default policy derivation for single-function instruction streams."
  (let* ((bodies (regalloc-collect-linear-functions instructions))
         (labels nil))
    (maphash (lambda (label body)
               (declare (ignore body))
               (push label labels))
             bodies)
    (when (= (length labels) 1)
      (let* ((label (first labels))
             (hints (regalloc-compute-interprocedural-hints instructions)))
         (regalloc-build-allocation-policy-from-hints hints label)))))

(defun %allocation-strategy (allocation-policy)
  "Return the requested allocation strategy, defaulting to linear scan."
  (or (getf allocation-policy :allocator)
      (getf allocation-policy :register-allocator)
      *regalloc-allocation-strategy*))

(defun %preferred-register-for-interval (interval cc free-regs)
  "Return a preferred free physical register for INTERVAL, or NIL."
  (some (lambda (strategy) (funcall strategy interval cc free-regs))
        *preferred-register-strategies*))
