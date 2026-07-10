(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Regalloc - Linear Scan Allocation and Public API
;;;
;;; Contains: linear-scan state, LSA helpers, linear-scan-allocate,
;;; allocate-registers (public API), and regalloc-lookup. Allocation policy
;;; lives in regalloc-policy.lisp; graph coloring lives in regalloc-color.lisp;
;;; spill insertion and live-range splitting live in regalloc-spill.lisp.
;;;
;;; Data structures (live-interval, regalloc-result), def/use analysis, and
;;; liveness computation are in regalloc.lisp (loads before this file).
;;;
;;; Load order: after regalloc-policy.lisp, regalloc-color.lisp, and regalloc-spill.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Linear Scan Allocation State

(defstruct (lsa-state (:conc-name lsa-))
  "Mutable state shared by the linear-scan allocation helpers."
  (assignment   (make-hash-table :test 'eq) :type hash-table)
  (spill-map    (make-hash-table :test 'eq) :type hash-table)
  (spill-count  0                           :type integer)
  (spill-offset 0                           :type integer)
  (free-regs    nil                         :type list)
  (free-fp-regs nil                         :type list)
  (active       nil                         :type list)
  (interval-map (make-hash-table :test 'eq) :type hash-table))

;;; Allocation policy and register preference helpers live in regalloc-policy.lisp.

(defun %lsa-interval-pool (state interval)
  "Return the free-register pool for INTERVAL's register class."
  (if (interval-fp-p interval) (lsa-free-fp-regs state) (lsa-free-regs state)))

(defun %lsa-set-interval-pool (state interval new-pool)
  "Replace the free-register pool for INTERVAL's register class."
  (if (interval-fp-p interval)
      (setf (lsa-free-fp-regs state) new-pool)
      (setf (lsa-free-regs state) new-pool)))

(defun %lsa-expire-old (state interval)
  "Return expired intervals to their register pools; remove from active list."
  (setf (lsa-active state)
        (remove-if (lambda (a)
                     (when (< (interval-end a) (interval-start interval))
                       (%lsa-set-interval-pool state a
                                               (cons (interval-phys-reg a)
                                                     (%lsa-interval-pool state a)))
                       t))
                   (lsa-active state))))

(defun %lsa-spill-current (state interval)
  "Assign a provisional spill slot to INTERVAL and record it in spill-map."
  (incf (lsa-spill-count state))
  (let ((slot (+ (lsa-spill-offset state) (lsa-spill-count state))))
    (setf (interval-spill-slot interval) slot)
    (setf (gethash (interval-vreg interval) (lsa-spill-map state)) slot)))

(defun %lsa-best-spill-candidate (state interval)
  "Return the active interval (or INTERVAL itself) with the farthest next use."
  (let ((same-class (remove-if-not (lambda (cand)
                                      (eq (interval-fp-p cand) (interval-fp-p interval)))
                                    (lsa-active state))))
    (if *ml-regalloc-enabled*
        (reduce (lambda (best candidate)
                  (let ((best-cost (regalloc-ml-spill-cost best))
                        (cand-cost (regalloc-ml-spill-cost candidate)))
                    (cond ((< cand-cost best-cost) candidate)
                          ((and (= cand-cost best-cost)
                                (< (interval-end candidate) (interval-end best)))
                           candidate)
                          (t best))))
                same-class
                :initial-value interval)
        (reduce (lambda (best candidate)
                  (let ((best-next (%interval-next-use-after best (interval-start interval)))
                        (cand-next (%interval-next-use-after candidate (interval-start interval))))
                    (cond ((null best) candidate)
                          ((null cand-next) candidate)
                          ((null best-next) best)
                          ((> cand-next best-next) candidate)
                          (t best))))
                same-class
                :initial-value interval))))

;;; Linear Scan Allocation — named helpers

(defun %lsa-assign (state interval phys)
  "Assign PHYS to INTERVAL and insert it into the active set (sorted by end)."
  (setf (interval-phys-reg interval) phys
        (gethash (interval-vreg interval) (lsa-assignment state)) phys
        (lsa-active state) (merge 'list (list interval) (lsa-active state) #'< :key #'interval-end)))

(defun %lsa-try-coalesce (state interval)
  "Attempt register coalescing for INTERVAL.  Returns T on success."
  (let* ((src-vreg (interval-coalesce-with interval))
         (src-int (and src-vreg (gethash src-vreg (lsa-interval-map state)))))
    (when (and src-int
               (eq (interval-fp-p src-int) (interval-fp-p interval))
               (interval-phys-reg src-int)
               (<= (interval-end src-int) (interval-start interval)))
      (let ((phys (interval-phys-reg src-int)))
        (setf (lsa-active state) (remove src-int (lsa-active state) :test #'eq))
        (%lsa-assign state interval phys)
        t))))

(defun %lsa-allocate-from-pool (state interval cc pool)
  "Pick a physical register from POOL (preferred first) and assign it."
  (let* ((preferred (%preferred-register-for-interval interval cc pool))
         (phys (or preferred (car pool))))
    (%lsa-set-interval-pool state interval (remove phys pool :count 1 :test #'eq))
    (%lsa-assign state interval phys)))

(defun %lsa-evict-and-assign (state interval)
  "Spill the worst active interval and reassign its register to INTERVAL."
  (let ((spill-candidate (%lsa-best-spill-candidate state interval)))
    (if (eq spill-candidate interval)
        (%lsa-spill-current state interval)
        (let ((freed-reg (interval-phys-reg spill-candidate)))
          (%lsa-spill-current state spill-candidate)
          (remhash (interval-vreg spill-candidate) (lsa-assignment state))
          (setf (interval-phys-reg spill-candidate) nil
                (lsa-active state) (remove spill-candidate (lsa-active state)))
          (%lsa-assign state interval freed-reg)))))

(defun linear-scan-allocate (intervals cc &optional (spill-slot-offset 0))
  "Perform linear scan register allocation.
   INTERVALS: sorted list of live-interval objects.
   CC: target-desc object.
   Returns (values assignment-ht spill-ht spill-count)."
  (let ((state (make-lsa-state
                :spill-offset spill-slot-offset
                :free-regs (remove (first (target-scratch-regs cc)) (copy-list (target-allocatable-regs cc)))
                :free-fp-regs (regalloc-target-fp-registers cc))))
    (dolist (int intervals)
      (setf (gethash (interval-vreg int) (lsa-interval-map state)) int))
    (dolist (interval intervals)
      (%lsa-expire-old state interval)
      (unless (%lsa-try-coalesce state interval)
        (let ((pool (%lsa-interval-pool state interval)))
          (if pool
              (%lsa-allocate-from-pool state interval cc pool)
              (%lsa-evict-and-assign state interval)))))
    ;; FR-199: Apply spill slot sharing / stack coloring.
    ;; Compute the actual spill count from the colored map's max slot index.
    (let ((colored-map (%maybe-color-spill-slots intervals (lsa-spill-map state) spill-slot-offset))
          (max-slot spill-slot-offset))
      (maphash (lambda (vreg slot)
                  (declare (ignore vreg))
                  (setf max-slot (max max-slot slot)))
                colored-map)
       ;; Slots are 1-indexed, so the maximum assigned slot is the frame slot count.
       (values (lsa-assignment state) colored-map max-slot))))

;;; Graph coloring and spill-slot coloring live in regalloc-color.lisp.
;;; Spill instructions, live-range splitting, and insertion live in regalloc-spill.lisp.

;;; Public API

(defun allocate-registers (instructions cc &optional float-vregs allocation-policy)
  "Run register allocation on VM instruction list.
   CC is a target-desc object.
   ALLOCATION-POLICY is an optional plist (e.g. from
   REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS) used to bias preferred registers.
   Returns a regalloc-result."
  (let* ((raw-intervals (compute-live-intervals instructions float-vregs))
         (split-instructions instructions)
         (split-slot-count 0)
         (intervals raw-intervals)
           (effective-policy (or allocation-policy
                                 (%derive-single-function-policy instructions)))
           (*current-allocation-policy* effective-policy)
           (*current-regalloc-loop-depths* (and *ml-regalloc-enabled*
                                                (regalloc-loop-depths instructions))))
    (multiple-value-setq (split-instructions intervals split-slot-count float-vregs)
      (split-live-ranges instructions raw-intervals float-vregs))
    (multiple-value-bind (assignment spill-map spill-count)
        (case (%allocation-strategy effective-policy)
          (:color (color-allocate-for-target intervals cc split-slot-count))
          (:linear-scan (linear-scan-allocate intervals cc split-slot-count))
          (otherwise (linear-scan-allocate intervals cc split-slot-count)))
      (let* ((remat-map (let ((ht (make-hash-table :test #'eq)))
                           (dolist (interval intervals ht)
                             (cond
                               ((interval-remat-const interval)
                                (setf (gethash (interval-vreg interval) ht)
                                      (list :const (interval-remat-const interval))))
                               ((interval-remat-inst interval)
                                (setf (gethash (interval-vreg interval) ht)
                                      (list :inst (interval-remat-inst interval))))))))
              (final-instructions
               (%finalize-split-spill-registers
                (if (plusp (hash-table-count spill-map))
                    (insert-spill-code split-instructions assignment spill-map cc remat-map float-vregs)
                    split-instructions)
                assignment)))
        (make-regalloc-result
         :assignment assignment
         :spill-map spill-map
         :spill-count spill-count
         :gpr-pressure (regalloc-register-pressure intervals :fp-p nil)
         :fp-pressure (regalloc-register-pressure intervals :fp-p t)
         :instructions final-instructions)))))

(defun regalloc-lookup (result vreg)
  "Look up physical register for VREG in allocation result.
   Returns the physical register keyword or NIL if spilled."
  (gethash vreg (regalloc-assignment result)))
