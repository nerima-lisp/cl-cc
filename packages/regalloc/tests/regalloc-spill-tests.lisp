(in-package :cl-cc/regalloc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-weave:it-sequential cl-weave:it-sequential-each cl-weave:expect
            cl-weave:signals cl-weave:it-todo)
          :cl-cc/regalloc))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tests for regalloc-spill.lisp — Live-Range Splitting and Spill Insertion
;;;
;;; Covers: split-live-ranges (no-split, single-split, multi-split),
;;;         %collect-live-range-splits, %assign-live-range-split-slots,
;;;         %boundaries-by-position, %finalize-split-spill-registers,
;;;         regalloc-ml-spill-cost, regalloc-loop-depths
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-spill-test-interval (vreg start end &key use-positions fp-p)
  "Construct a live-interval for spill tests."
  (make-live-interval :vreg vreg
                      :start start
                      :end end
                      :use-positions (or use-positions (list start end))
                      :fp-p fp-p))

;;; ─── %collect-live-range-splits ───────────────────────────────────────────

(it-sequential "%collect-live-range-splits no splits when hole is smaller than minimum"
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(2 7)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      ;; One group per original interval; no boundaries when no split occurs.
      (expect (= 1 (length child-groups)) :to-be-truthy)
      (expect (null boundaries) :to-be-truthy))))

(it-sequential "%collect-live-range-splits produces boundary when hole exceeds minimum"
  (let* ((interval (make-spill-test-interval :v 0 20 :use-positions '(2 17)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      (expect (= 1 (length child-groups)) :to-be-truthy)
      ;; Exactly one boundary should be emitted for the single split.
      (expect (= 1 (length boundaries)) :to-be-truthy)
      (expect (= 2 (split-boundary-after-position (first boundaries))) :to-be-truthy)
      (expect (= 17 (split-boundary-before-position (first boundaries))) :to-be-truthy))))

(it-sequential "%collect-live-range-splits multiple intervals produce separate groups"
  (let* ((int-a (make-spill-test-interval :a 0 10 :use-positions '(1 2)))
         (int-b (make-spill-test-interval :b 5 20 :use-positions '(5 6)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups _)
        (%collect-live-range-splits (list int-a int-b) minimum-hole-size)
      (declare (ignore _))
      (expect (= 2 (length child-groups)) :to-be-truthy))))

;;; ─── %assign-live-range-split-slots ──────────────────────────────────────

(it-sequential "%assign-live-range-split-slots assigns sequential 1-indexed slots"
  (let* ((interval (make-spill-test-interval :v 0 30 :use-positions '(2 20)))
         (minimum-hole-size 8))
    (multiple-value-bind (child-groups boundaries)
        (%collect-live-range-splits (list interval) minimum-hole-size)
      (declare (ignore child-groups))
      (let ((slot-count (%assign-live-range-split-slots boundaries)))
        ;; One boundary → one slot assigned; count == 1.
        (when (plusp (length boundaries))
          (expect (= 1 (split-boundary-slot (first boundaries))) :to-be-truthy)
          (expect (= 1 slot-count) :to-be-truthy))))))

(it-sequential "%assign-live-range-split-slots returns zero when no boundaries"
  (let ((slot-count (%assign-live-range-split-slots '())))
    (expect (= 0 slot-count) :to-be-truthy)))

;;; ─── %boundaries-by-position ──────────────────────────────────────────────

(it-sequential "%boundaries-by-position indexes boundaries by keyfn result"
  (let* ((b1 (make-live-range-split-boundary :after-position 3 :before-position 10
                                              :from-vreg :a :to-vreg :a/split1
                                              :slot 1))
         (b2 (make-live-range-split-boundary :after-position 5 :before-position 12
                                              :from-vreg :b :to-vreg :b/split1
                                              :slot 2))
         (table (%boundaries-by-position (list b1 b2) #'split-boundary-after-position)))
    (expect (member b1 (gethash 3 table) :test #'eq) :to-be-truthy)
    (expect (member b2 (gethash 5 table) :test #'eq) :to-be-truthy)
    (expect (null (gethash 99 table)) :to-be-truthy)))

(it-sequential "%boundaries-by-position groups multiple boundaries at same position"
  (let* ((b1 (make-live-range-split-boundary :after-position 5 :before-position 10
                                              :from-vreg :a :to-vreg :a/s1 :slot 1))
         (b2 (make-live-range-split-boundary :after-position 5 :before-position 15
                                              :from-vreg :b :to-vreg :b/s1 :slot 2))
         (table (%boundaries-by-position (list b1 b2) #'split-boundary-after-position)))
    (expect (= 2 (length (gethash 5 table))) :to-be-truthy)))

;;; ─── split-live-ranges ────────────────────────────────────────────────────

(it-sequential "split-live-ranges no-split path returns originals unchanged"
  (let* ((interval (make-spill-test-interval :v 0 5 :use-positions '(1 2 3 4)))
         (instructions (list (make-vm-const :dst :v :value 42)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions new-intervals split-count new-float)
        (split-live-ranges instructions (list interval) nil minimum-hole-size)
      (expect (equal instructions new-instructions) :to-be-truthy)
      (expect (= 1 (length new-intervals)) :to-be-truthy)
      (expect (= 0 split-count) :to-be-truthy)
      (expect (null new-float) :to-be-truthy))))

(it-sequential "split-live-ranges single-split path inserts spill load and store"
  (let* ((interval (make-spill-test-interval :v 0 25
                     :use-positions '(0 20)))
         ;; Build an instruction stream with 25 no-op const instructions
         ;; at positions 0..24.
         (instructions (loop for i from 0 to 24
                             collect (make-vm-const :dst (intern (format nil "X~D" i) :keyword)
                                                    :value i)))
         (minimum-hole-size 8))
    (multiple-value-bind (new-instructions _intervals split-count _float)
        (split-live-ranges instructions (list interval) nil minimum-hole-size)
      (declare (ignore _intervals _float))
      ;; At least one spill load and one spill store should be inserted.
      (expect (plusp split-count) :to-be-truthy)
      (expect (some (lambda (inst) (typep inst 'vm-spill-load)) new-instructions) :to-be-truthy)
      (expect (some (lambda (inst) (typep inst 'vm-spill-store)) new-instructions) :to-be-truthy))))

;;; ─── %finalize-split-spill-registers ─────────────────────────────────────

(it-sequential "%finalize-split-spill-registers-cases store-rewrite"
  (destructuring-bind (kind vreg phys slot) (list :store :v0 :rax 1)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys  (vm-spill-src (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys  (vm-spill-dst (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

(it-sequential "%finalize-split-spill-registers-cases load-rewrite"
  (destructuring-bind (kind vreg phys slot) (list :load :v1 :rbx 2)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys  (vm-spill-src (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys  (vm-spill-dst (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

(it-sequential "%finalize-split-spill-registers-cases passthrough"
  (destructuring-bind (kind vreg phys slot) (list :const nil nil nil)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys  (vm-spill-src (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys  (vm-spill-dst (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

(it-sequential "%finalize-split-spill-registers-cases unassigned-store"
  (destructuring-bind (kind vreg phys slot) (list :store :unassigned nil 3)
    (let* ((assignment (make-hash-table :test #'eq))
         (inst (cond
                 ((eq kind :store) (make-vm-spill-store :src-reg vreg :slot slot))
                 ((eq kind :load)  (make-vm-spill-load  :dst-reg vreg :slot slot))
                 (t                (make-vm-const :dst :r0 :value 99)))))
    (when phys (setf (gethash vreg assignment) phys))
    (let ((result (%finalize-split-spill-registers (list inst) assignment)))
      (expect (length result) :to-equal 1)
      (cond
        ((and (eq kind :store) phys)
         (expect (eq phys  (vm-spill-src (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        ((and (eq kind :load) phys)
         (expect (eq phys  (vm-spill-dst (first result))) :to-be-truthy)
         (expect (vm-spill-slot (first result)) :to-equal slot))
        (t
         (expect (eq inst (first result)) :to-be-truthy)))))))

;;; ─── regalloc-loop-depths ─────────────────────────────────────────────────

(it-sequential "regalloc-loop-depths returns empty table for straight-line code"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                              (make-vm-const :dst :r1 :value 2)))
         (depths (regalloc-loop-depths instructions)))
    (expect (= 0 (hash-table-count depths)) :to-be-truthy)))

(it-sequential "regalloc-loop-depths increments depth for backward branch targets"
  (let* ((instructions (list (make-vm-const :dst :r0 :value 0)         ; 0
                              (make-vm-label :name "loop-head")         ; 1
                              (make-vm-const :dst :r1 :value 1)         ; 2
                              (make-vm-jump :label-name "loop-head")))  ; 3
         (depths (regalloc-loop-depths instructions)))
    (expect (>= (gethash 1 depths 0) 1) :to-be-truthy)
    (expect (>= (gethash 2 depths 0) 1) :to-be-truthy)
    (expect (>= (gethash 3 depths 0) 1) :to-be-truthy)))

;;; ─── regalloc-ml-spill-cost ───────────────────────────────────────────────

(it-sequential "regalloc-ml-spill-cost returns positive score for non-trivial interval"
  (let* ((*ml-regalloc-enabled* t)
         (interval (make-spill-test-interval :v 0 10 :use-positions '(1 3 7))))
    (expect (> (regalloc-ml-spill-cost interval nil) 0) :to-be-truthy)))

(it-sequential "regalloc-ml-spill-cost adds bonus for call-crossing interval"
  (let* ((no-call  (make-live-interval :vreg :nc :start 0 :end 10
                                       :use-positions '(2) :crosses-call-p nil
                                       :return-value-p nil))
         (crosses  (make-live-interval :vreg :cc :start 0 :end 10
                                       :use-positions '(2) :crosses-call-p t
                                       :return-value-p nil)))
    (expect (> (regalloc-ml-spill-cost crosses nil)
           (regalloc-ml-spill-cost no-call nil)) :to-be-truthy)))

(it-sequential "regalloc-ml-spill-cost adds bonus for return-value interval"
  (let* ((plain  (make-live-interval :vreg :p :start 0 :end 10
                                     :use-positions '(2) :crosses-call-p nil
                                     :return-value-p nil))
         (retval (make-live-interval :vreg :r :start 0 :end 10
                                     :use-positions '(2) :crosses-call-p nil
                                     :return-value-p t)))
    (expect (> (regalloc-ml-spill-cost retval nil)
           (regalloc-ml-spill-cost plain nil)) :to-be-truthy)))

(it-sequential "regalloc-ml-spill-cost reduces cost for rematerializable const interval"
  (let* ((plain (make-live-interval :vreg :p :start 0 :end 10
                                    :use-positions '(2) :remat-const nil))
         (remat (make-live-interval :vreg :r :start 0 :end 10
                                    :use-positions '(2) :remat-const 42)))
    (expect (< (regalloc-ml-spill-cost remat nil)
           (regalloc-ml-spill-cost plain nil)) :to-be-truthy)))

(it-sequential "regalloc-ml-spill-cost uses loop-depths to weight loop-body uses"
  (let* ((interval (make-spill-test-interval :v 0 10 :use-positions '(5)))
         (depths (let ((ht (make-hash-table :test #'eql)))
                   (setf (gethash 5 ht) 2)
                   ht))
         (shallow-cost (regalloc-ml-spill-cost interval nil))
         (loop-cost    (regalloc-ml-spill-cost interval depths)))
    ;; Use at depth 2 should yield a higher score than use at depth 0.
    (expect (> loop-cost shallow-cost) :to-be-truthy)))
