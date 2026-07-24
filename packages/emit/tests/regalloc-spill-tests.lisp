;;;; tests/regalloc-spill-tests.lisp - Live-Range Splitting and Spill Insertion
;;;
;;; Covers regalloc-spill.lisp: split-live-ranges (no-split / single-split),
;;; %collect-live-range-splits, %assign-live-range-split-slots,
;;; %boundaries-by-position, %finalize-split-spill-registers,
;;; regalloc-ml-spill-cost, regalloc-loop-depths.
;;;
;;; Migrated from the unregistered packages/regalloc/tests/regalloc-spill-tests.lisp
;;; into the cl-cc/test suite convention.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-spill-test-interval (vreg start end &key use-positions fp-p)
  "Construct a live-interval for spill tests."
  (cl-cc/regalloc::make-live-interval
   :vreg vreg
   :start start
   :end end
   :use-positions (or use-positions (list start end))
   :fp-p fp-p))

;;; ─── %collect-live-range-splits ───────────────────────────────────────────

(deftest spill-collect-live-range-splits-no-split-below-minimum
  "%collect-live-range-splits produces no boundaries when the gap between uses is smaller than the minimum hole size."
  ;; Gap of 5 between uses 2 and 7 is below minimum hole size of 8.
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(2 7)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list interval) minimum-hole-size)
      (assert-= 1 (length child-groups))
      (assert-null boundaries))))

(deftest spill-collect-live-range-splits-boundary-when-hole-exceeds-minimum
  "%collect-live-range-splits emits a split boundary when the use gap exceeds the minimum hole size."
  ;; Gap of 15 between uses 2 and 17 exceeds minimum of 8.
  (let* ((interval (make-spill-test-interval :v 0 20 :use-positions '(2 17)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list interval) minimum-hole-size)
      (assert-= 1 (length child-groups))
      (assert-= 1 (length boundaries))
      (assert-= 2 (cl-cc/regalloc::split-boundary-after-position (first boundaries)))
      (assert-= 17 (cl-cc/regalloc::split-boundary-before-position (first boundaries))))))

(deftest spill-collect-live-range-splits-separate-groups-per-interval
  "%collect-live-range-splits produces one child group per input interval."
  (let* ((int-a (make-spill-test-interval :a 0 10 :use-positions '(1 2)))
         (int-b (make-spill-test-interval :b 5 20 :use-positions '(5 6)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list int-a int-b) minimum-hole-size)
      (declare (ignore boundaries))
      (assert-= 2 (length child-groups)))))

;;; ─── %assign-live-range-split-slots ──────────────────────────────────────

(deftest spill-assign-live-range-split-slots-sequential-1-indexed
  "%assign-live-range-split-slots assigns sequential 1-indexed slot numbers to split boundaries."
  (let* ((interval (make-spill-test-interval :v 0 30 :use-positions '(2 20)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list interval) minimum-hole-size)
      (declare (ignore child-groups))
      (let ((slot-count (cl-cc/regalloc::%assign-live-range-split-slots boundaries)))
        (when (plusp (length boundaries))
          (assert-= 1 (cl-cc/regalloc::split-boundary-slot (first boundaries)))
          (assert-= 1 slot-count))))))

(deftest spill-assign-live-range-split-slots-zero-when-no-boundaries
  "%assign-live-range-split-slots returns zero when the boundary list is empty."
  (assert-= 0 (cl-cc/regalloc::%assign-live-range-split-slots '())))

;;; ─── %boundaries-by-position ──────────────────────────────────────────────

(deftest spill-boundaries-by-position-indexes-by-keyfn
  "%boundaries-by-position builds a hash table indexing each boundary by the value of the key function."
  (let* ((b1 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 3 :before-position 10
              :from-vreg :a :to-vreg :a/split1 :slot 1))
         (b2 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 5 :before-position 12
              :from-vreg :b :to-vreg :b/split1 :slot 2))
         (table (cl-cc/regalloc::%boundaries-by-position
                 (list b1 b2) #'cl-cc/regalloc::split-boundary-after-position)))
    (assert-true (member b1 (gethash 3 table) :test #'eq))
    (assert-true (member b2 (gethash 5 table) :test #'eq))
    (assert-null (gethash 99 table))))

(deftest spill-boundaries-by-position-groups-same-position
  "%boundaries-by-position collects all boundaries that share the same key value into a single list."
  (let* ((b1 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 5 :before-position 10
              :from-vreg :a :to-vreg :a/s1 :slot 1))
         (b2 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 5 :before-position 15
              :from-vreg :b :to-vreg :b/s1 :slot 2))
         (table (cl-cc/regalloc::%boundaries-by-position
                 (list b1 b2) #'cl-cc/regalloc::split-boundary-after-position)))
    (assert-= 2 (length (gethash 5 table)))))

;;; ─── split-live-ranges ────────────────────────────────────────────────────

(deftest spill-split-live-ranges-no-split-returns-originals
  "split-live-ranges returns the original instructions and intervals unchanged when no holes are large enough to split."
  (let* ((interval (make-spill-test-interval :v 0 5 :use-positions '(1 2 3 4)))
         (instructions (list (make-vm-const :dst :v :value 42)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions new-intervals split-count new-float)
        (cl-cc/regalloc::split-live-ranges instructions (list interval) nil minimum-hole-size)
      (assert-equal instructions new-instructions)
      (assert-= 1 (length new-intervals))
      (assert-= 0 split-count)
      (assert-null new-float))))

(deftest spill-split-live-ranges-single-split-inserts-load-and-store
  "split-live-ranges inserts vm-spill-load and vm-spill-store instructions when a live range is split."
  ;; Uses at 0 and 20 with a 20-unit gap trigger a split: a spill-load appears
  ;; before position 20 and a spill-store after position 0.
  (let* ((interval (make-spill-test-interval :v 0 25 :use-positions '(0 20)))
         (instructions (loop for i from 0 to 24
                             collect (make-vm-const :dst (intern (format nil "X~D" i) :keyword)
                                                    :value i)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions new-intervals split-count new-float)
        (cl-cc/regalloc::split-live-ranges instructions (list interval) nil minimum-hole-size)
      (declare (ignore new-intervals new-float))
      (assert-true (plusp split-count))
      (assert-true (some (lambda (inst) (typep inst 'cl-cc/regalloc::vm-spill-load)) new-instructions))
      (assert-true (some (lambda (inst) (typep inst 'cl-cc/regalloc::vm-spill-store)) new-instructions)))))

;;; ─── %finalize-split-spill-registers ─────────────────────────────────────

(deftest-each spill-finalize-split-spill-registers
  "%finalize-split-spill-registers rewrites spill instruction virtual regs to physical, or passes non-spill instructions through."
  :cases (("store-rewrite"    :store :v0 :rax 1)
          ("load-rewrite"     :load  :v1 :rbx 2)
          ("passthrough"      :const nil nil   nil)
          ("unassigned-store" :store :unassigned nil 3))
  (kind vreg phys slot)
  (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (cl-cc/regalloc::make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (cl-cc/regalloc::make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (cl-cc/regalloc::%finalize-split-spill-registers (list inst) assignment)))
      (assert-equal 1 (length result))
      (cond
        ((and (eq kind :store) phys)
         (assert-true (eq phys (cl-cc/regalloc::vm-spill-src (first result))))
         (assert-equal slot (cl-cc/regalloc::vm-spill-slot (first result))))
        ((and (eq kind :load) phys)
         (assert-true (eq phys (cl-cc/regalloc::vm-spill-dst (first result))))
         (assert-equal slot (cl-cc/regalloc::vm-spill-slot (first result))))
        (t
         (assert-true (eq inst (first result))))))))

;;; ─── regalloc-loop-depths ─────────────────────────────────────────────────

(deftest spill-loop-depths-empty-for-straight-line-code
  "regalloc-loop-depths returns an empty hash table when the instruction stream contains no backward branches."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)))
         (depths (cl-cc/regalloc::regalloc-loop-depths instructions)))
    (assert-= 0 (hash-table-count depths))))

(deftest spill-loop-depths-increments-for-backward-branch-targets
  "regalloc-loop-depths assigns a loop depth of at least 1 to positions covered by a backward branch."
  ;; A backward branch from position 3 to the label at position 1 marks
  ;; positions 1, 2, 3 with depth >= 1.
  (let* ((instructions (list (make-vm-const :dst :r0 :value 0)     ; 0
                             (make-vm-label :name "loop-head")     ; 1
                             (make-vm-const :dst :r1 :value 1)     ; 2
                             (make-vm-jump :label "loop-head")))   ; 3
         (depths (cl-cc/regalloc::regalloc-loop-depths instructions)))
    (assert-true (>= (gethash 1 depths 0) 1))
    (assert-true (>= (gethash 2 depths 0) 1))
    (assert-true (>= (gethash 3 depths 0) 1))))

;;; ─── regalloc-ml-spill-cost ───────────────────────────────────────────────

(deftest spill-ml-cost-positive-for-nontrivial-interval
  "regalloc-ml-spill-cost returns a positive score for an interval with multiple use positions."
  (let* ((cl-cc/regalloc::*ml-regalloc-enabled* t)
         (interval (make-spill-test-interval :v 0 10 :use-positions '(1 3 7))))
    (assert-true (> (cl-cc/regalloc::regalloc-ml-spill-cost interval nil) 0))))

(deftest spill-ml-cost-adds-bonus-for-call-crossing
  "regalloc-ml-spill-cost produces a higher cost for an interval that crosses a call than one that does not."
  (let* ((no-call (cl-cc/regalloc::make-live-interval :vreg :nc :start 0 :end 10
                                                      :use-positions '(2) :crosses-call-p nil
                                                      :return-value-p nil))
         (crosses (cl-cc/regalloc::make-live-interval :vreg :cc :start 0 :end 10
                                                      :use-positions '(2) :crosses-call-p t
                                                      :return-value-p nil)))
    (assert-true (> (cl-cc/regalloc::regalloc-ml-spill-cost crosses nil)
                    (cl-cc/regalloc::regalloc-ml-spill-cost no-call nil)))))

(deftest spill-ml-cost-adds-bonus-for-return-value
  "regalloc-ml-spill-cost produces a higher cost for a return-value interval than a plain interval."
  (let* ((plain  (cl-cc/regalloc::make-live-interval :vreg :p :start 0 :end 10
                                                     :use-positions '(2) :crosses-call-p nil
                                                     :return-value-p nil))
         (retval (cl-cc/regalloc::make-live-interval :vreg :r :start 0 :end 10
                                                     :use-positions '(2) :crosses-call-p nil
                                                     :return-value-p t)))
    (assert-true (> (cl-cc/regalloc::regalloc-ml-spill-cost retval nil)
                    (cl-cc/regalloc::regalloc-ml-spill-cost plain nil)))))

(deftest spill-ml-cost-reduces-for-rematerializable-const
  "regalloc-ml-spill-cost produces a lower cost for an interval with a remat-const than a plain interval."
  (let* ((plain (cl-cc/regalloc::make-live-interval :vreg :p :start 0 :end 10
                                                    :use-positions '(2) :remat-const nil))
         (remat (cl-cc/regalloc::make-live-interval :vreg :r :start 0 :end 10
                                                    :use-positions '(2) :remat-const 42)))
    (assert-true (< (cl-cc/regalloc::regalloc-ml-spill-cost remat nil)
                    (cl-cc/regalloc::regalloc-ml-spill-cost plain nil)))))

(deftest spill-ml-cost-weights-loop-body-uses
  "regalloc-ml-spill-cost produces a higher cost for uses inside a loop than uses at depth 0."
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(5)))
         (depths (let ((ht (make-hash-table :test #'eql)))
                   (setf (gethash 5 ht) 2)
                   ht))
         (shallow-cost (cl-cc/regalloc::regalloc-ml-spill-cost interval nil))
         (loop-cost    (cl-cc/regalloc::regalloc-ml-spill-cost interval depths)))
    (assert-true (> loop-cost shallow-cost))))
