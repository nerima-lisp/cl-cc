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

(it-sequential "spill-collect-live-range-splits-no-split-below-minimum"
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(2 7)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list interval) minimum-hole-size)
      (expect (= 1 (length child-groups)) :to-be-truthy)
      (expect boundaries :to-be-null))))

(it-sequential "spill-collect-live-range-splits-boundary-when-hole-exceeds-minimum"
  (let* ((interval (make-spill-test-interval :v 0 20 :use-positions '(2 17)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list interval) minimum-hole-size)
      (expect (= 1 (length child-groups)) :to-be-truthy)
      (expect (= 1 (length boundaries)) :to-be-truthy)
      (expect (= 2 (cl-cc/regalloc::split-boundary-after-position (first boundaries))) :to-be-truthy)
      (expect (= 17 (cl-cc/regalloc::split-boundary-before-position (first boundaries))) :to-be-truthy))))

(it-sequential "spill-collect-live-range-splits-separate-groups-per-interval"
  (let* ((int-a (make-spill-test-interval :a 0 10 :use-positions '(1 2)))
         (int-b (make-spill-test-interval :b 5 20 :use-positions '(5 6)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list int-a int-b) minimum-hole-size)
      (declare (ignore boundaries))
      (expect (= 2 (length child-groups)) :to-be-truthy))))

;;; ─── %assign-live-range-split-slots ──────────────────────────────────────

(it-sequential "spill-assign-live-range-split-slots-sequential-1-indexed"
  (let* ((interval (make-spill-test-interval :v 0 30 :use-positions '(2 20)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (cl-cc/regalloc::%collect-live-range-splits (list interval) minimum-hole-size)
      (declare (ignore child-groups))
      (let ((slot-count (cl-cc/regalloc::%assign-live-range-split-slots boundaries)))
        (when (plusp (length boundaries))
          (expect (= 1 (cl-cc/regalloc::split-boundary-slot (first boundaries))) :to-be-truthy)
          (expect (= 1 slot-count) :to-be-truthy))))))

(it-sequential "spill-assign-live-range-split-slots-zero-when-no-boundaries"
  (expect (= 0 (cl-cc/regalloc::%assign-live-range-split-slots '())) :to-be-truthy))

;;; ─── %boundaries-by-position ──────────────────────────────────────────────

(it-sequential "spill-boundaries-by-position-indexes-by-keyfn"
  (let* ((b1 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 3 :before-position 10
              :from-vreg :a :to-vreg :a/split1 :slot 1))
         (b2 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 5 :before-position 12
              :from-vreg :b :to-vreg :b/split1 :slot 2))
         (table (cl-cc/regalloc::%boundaries-by-position
                 (list b1 b2) #'cl-cc/regalloc::split-boundary-after-position)))
    (expect (member b1 (gethash 3 table) :test #'eq) :to-be-truthy)
    (expect (member b2 (gethash 5 table) :test #'eq) :to-be-truthy)
    (expect (gethash 99 table) :to-be-null)))

(it-sequential "spill-boundaries-by-position-groups-same-position"
  (let* ((b1 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 5 :before-position 10
              :from-vreg :a :to-vreg :a/s1 :slot 1))
         (b2 (cl-cc/regalloc::make-live-range-split-boundary
              :after-position 5 :before-position 15
              :from-vreg :b :to-vreg :b/s1 :slot 2))
         (table (cl-cc/regalloc::%boundaries-by-position
                 (list b1 b2) #'cl-cc/regalloc::split-boundary-after-position)))
    (expect (= 2 (length (gethash 5 table))) :to-be-truthy)))

;;; ─── split-live-ranges ────────────────────────────────────────────────────

(it-sequential "spill-split-live-ranges-no-split-returns-originals"
  (let* ((interval (make-spill-test-interval :v 0 5 :use-positions '(1 2 3 4)))
         (instructions (list (make-vm-const :dst :v :value 42)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions new-intervals split-count new-float)
        (cl-cc/regalloc::split-live-ranges instructions (list interval) nil minimum-hole-size)
      (expect new-instructions :to-equal instructions)
      (expect (= 1 (length new-intervals)) :to-be-truthy)
      (expect (= 0 split-count) :to-be-truthy)
      (expect new-float :to-be-null))))

(it-sequential "spill-split-live-ranges-single-split-inserts-load-and-store"
  (let* ((interval (make-spill-test-interval :v 0 25 :use-positions '(0 20)))
         (instructions (loop for i from 0 to 24
                             collect (make-vm-const :dst (intern (format nil "X~D" i) :keyword)
                                                    :value i)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions new-intervals split-count new-float)
        (cl-cc/regalloc::split-live-ranges instructions (list interval) nil minimum-hole-size)
      (declare (ignore new-intervals new-float))
      (expect (plusp split-count) :to-be-truthy)
      (expect (some (lambda (inst) (typep inst 'cl-cc/regalloc::vm-spill-load)) new-instructions) :to-be-truthy)
      (expect (some (lambda (inst) (typep inst 'cl-cc/regalloc::vm-spill-store)) new-instructions) :to-be-truthy))))

;;; ─── %finalize-split-spill-registers ─────────────────────────────────────

(it-sequential "spill-finalize-split-spill-registers store-rewrite" (destructuring-bind (kind vreg phys slot) (list :store :v0 :rax 1)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (cl-cc/regalloc::make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (cl-cc/regalloc::make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (cl-cc/regalloc::%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-src (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-dst (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

(it-sequential "spill-finalize-split-spill-registers load-rewrite"
  (destructuring-bind (kind vreg phys slot) (list :load :v1 :rbx 2)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (cl-cc/regalloc::make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (cl-cc/regalloc::make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (cl-cc/regalloc::%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-src (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-dst (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

(it-sequential "spill-finalize-split-spill-registers passthrough"
  (destructuring-bind (kind vreg phys slot) (list :const nil nil nil)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (cl-cc/regalloc::make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (cl-cc/regalloc::make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (cl-cc/regalloc::%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-src (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-dst (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

(it-sequential "spill-finalize-split-spill-registers unassigned-store"
  (destructuring-bind (kind vreg phys slot) (list :store :unassigned nil 3)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (cl-cc/regalloc::make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (cl-cc/regalloc::make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (cl-cc/regalloc::%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-src (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys (cl-cc/regalloc::vm-spill-dst (first result))) :to-be-truthy)
         (expect (cl-cc/regalloc::vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

;;; ─── regalloc-loop-depths ─────────────────────────────────────────────────

(it-sequential "spill-loop-depths-empty-for-straight-line-code"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)))
         (depths (cl-cc/regalloc::regalloc-loop-depths instructions)))
    (expect (= 0 (hash-table-count depths)) :to-be-truthy)))

(it-sequential "spill-loop-depths-increments-for-backward-branch-targets"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 0)     ; 0
                             (make-vm-label :name "loop-head")     ; 1
                             (make-vm-const :dst :r1 :value 1)     ; 2
                             (make-vm-jump :label "loop-head")))   ; 3
         (depths (cl-cc/regalloc::regalloc-loop-depths instructions)))
    (expect (>= (gethash 1 depths 0) 1) :to-be-truthy)
    (expect (>= (gethash 2 depths 0) 1) :to-be-truthy)
    (expect (>= (gethash 3 depths 0) 1) :to-be-truthy)))

;;; ─── regalloc-ml-spill-cost ───────────────────────────────────────────────

(it-sequential "spill-ml-cost-positive-for-nontrivial-interval"
  (let* ((cl-cc/regalloc::*ml-regalloc-enabled* t)
         (interval (make-spill-test-interval :v 0 10 :use-positions '(1 3 7))))
    (expect (> (cl-cc/regalloc::regalloc-ml-spill-cost interval nil) 0) :to-be-truthy)))

(it-sequential "spill-ml-cost-adds-bonus-for-call-crossing"
  (let* ((no-call (cl-cc/regalloc::make-live-interval :vreg :nc :start 0 :end 10
                                                      :use-positions '(2) :crosses-call-p nil
                                                      :return-value-p nil))
         (crosses (cl-cc/regalloc::make-live-interval :vreg :cc :start 0 :end 10
                                                      :use-positions '(2) :crosses-call-p t
                                                      :return-value-p nil)))
    (expect (> (cl-cc/regalloc::regalloc-ml-spill-cost crosses nil)
                    (cl-cc/regalloc::regalloc-ml-spill-cost no-call nil)) :to-be-truthy)))

(it-sequential "spill-ml-cost-adds-bonus-for-return-value"
  (let* ((plain  (cl-cc/regalloc::make-live-interval :vreg :p :start 0 :end 10
                                                     :use-positions '(2) :crosses-call-p nil
                                                     :return-value-p nil))
         (retval (cl-cc/regalloc::make-live-interval :vreg :r :start 0 :end 10
                                                     :use-positions '(2) :crosses-call-p nil
                                                     :return-value-p t)))
    (expect (> (cl-cc/regalloc::regalloc-ml-spill-cost retval nil)
                    (cl-cc/regalloc::regalloc-ml-spill-cost plain nil)) :to-be-truthy)))

(it-sequential "spill-ml-cost-reduces-for-rematerializable-const"
  (let* ((plain (cl-cc/regalloc::make-live-interval :vreg :p :start 0 :end 10
                                                    :use-positions '(2) :remat-const nil))
         (remat (cl-cc/regalloc::make-live-interval :vreg :r :start 0 :end 10
                                                    :use-positions '(2) :remat-const 42)))
    (expect (< (cl-cc/regalloc::regalloc-ml-spill-cost remat nil)
                    (cl-cc/regalloc::regalloc-ml-spill-cost plain nil)) :to-be-truthy)))

(it-sequential "spill-ml-cost-weights-loop-body-uses"
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(5)))
         (depths (let ((ht (make-hash-table :test #'eql)))
                   (setf (gethash 5 ht) 2)
                   ht))
         (shallow-cost (cl-cc/regalloc::regalloc-ml-spill-cost interval nil))
         (loop-cost    (cl-cc/regalloc::regalloc-ml-spill-cost interval depths)))
    (expect (> loop-cost shallow-cost) :to-be-truthy)))
