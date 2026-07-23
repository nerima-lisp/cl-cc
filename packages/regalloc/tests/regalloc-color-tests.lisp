(in-package :cl-cc/regalloc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-weave:it-sequential cl-weave:it-sequential-each cl-weave:expect
            cl-weave:signals cl-weave:it-todo)
          :cl-cc/regalloc))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tests for regalloc-color.lisp — Graph Coloring Allocation (FR-061)
;;;                                  and Spill Slot Sharing (FR-199)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-test-interval (vreg start end &key use-positions crosses-call-p return-value-p)
  "Construct a live-interval for use in tests."
  (make-live-interval :vreg vreg
                      :start start
                      :end end
                      :use-positions (or use-positions (list start end))
                      :crosses-call-p crosses-call-p
                      :return-value-p return-value-p))

;;; ─── %intervals-overlap-p ─────────────────────────────────────────────────

(it-sequential "%intervals-overlap-p overlap :exact-overlap"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 0 10 :b 0 10 t)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :partial-left"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 0 8 :b 5 12 t)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :partial-right"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 5 12 :b 0 8 t)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :contained"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 0 20 :b 5 10 t)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :touching-end"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 0 10 :b 10 20 t)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :touching-start"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 10 20 :b 0 10 t)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :disjoint-ab"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 0 5 :b 6 10 nil)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

(it-sequential "%intervals-overlap-p overlap :disjoint-ba"
  (destructuring-bind (vreg-a start-a end-a vreg-b start-b end-b expected-p) (list :a 6 10 :b 0 5 nil)
    (let ((a (make-test-interval vreg-a start-a end-a))
        (b (make-test-interval vreg-b start-b end-b)))
    (expect (eq expected-p (%intervals-overlap-p a b)) :to-be-truthy))))

;;; ─── %color-build-interference-graph ─────────────────────────────────────

(it-sequential "color-build-interference-graph-overlapping"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (graph (%color-build-interference-graph (list a b))))
    (expect (member :b (gethash :a graph) :test #'eq) :to-be-truthy)
    (expect (member :a (gethash :b graph) :test #'eq) :to-be-truthy)))

(it-sequential "color-build-interference-graph-disjoint"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (graph (%color-build-interference-graph (list a b))))
    (expect (null (gethash :a graph)) :to-be-truthy)
    (expect (null (gethash :b graph)) :to-be-truthy)))

(it-sequential "color-build-interference-graph-no-vreg-skipped"
  (let* ((a (make-test-interval nil 0 10))
         (b (make-test-interval :b 0 10))
         (graph (%color-build-interference-graph (list a b))))
    (expect (= 1 (hash-table-count graph)) :to-be-truthy)
    (expect (null (gethash nil graph)) :to-be-truthy)))

;;; ─── %color-simplify ──────────────────────────────────────────────────────

(it-sequential "color-simplify-stack-ordering"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (graph (%color-build-interference-graph (list a b)))
         (imap (%interval-map (list a b)))
         (stack (%color-simplify graph imap 2)))
    (expect (= 2 (length stack)) :to-be-truthy)
    ;; All entries should have spill-p = NIL (degree < k=2 after simplification).
    (expect (every (lambda (entry) (not (cdr entry))) stack) :to-be-truthy)))

(it-sequential "color-simplify-spill-candidate-when-pressure"
  (let* ((a (make-test-interval :a 0 20 :use-positions '(0 5 10 15)))
         (b (make-test-interval :b 0 20 :use-positions '(1 6 11 16)))
         (c (make-test-interval :c 0 20 :use-positions '(2)))
         (graph (%color-build-interference-graph (list a b c)))
         (imap (%interval-map (list a b c)))
         (stack (%color-simplify graph imap 2)))
    (expect (= 3 (length stack)) :to-be-truthy)
    ;; At least one node on the stack should be a potential spill (cdr = T).
    (expect (some #'cdr stack) :to-be-truthy)))

;;; ─── color-allocate ───────────────────────────────────────────────────────

(it-sequential "color-allocate-assigns-registers"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 12 20))
         (available '(:r0 :r1)))
    (multiple-value-bind (assignment spill-map spill-count)
        (color-allocate (list a b) available)
      (expect (gethash :a assignment) :to-be-truthy)
      (expect (gethash :b assignment) :to-be-truthy)
      (expect (= 0 (hash-table-count spill-map)) :to-be-truthy)
      (expect (= 0 spill-count) :to-be-truthy))))

(it-sequential "color-allocate-spills-when-k-exceeded"
  (let* ((a (make-test-interval :a 0 20))
         (b (make-test-interval :b 0 20))
         (c (make-test-interval :c 0 20))
         (available '(:r0 :r1)))
    (multiple-value-bind (assignment spill-map spill-count)
        (color-allocate (list a b c) available)
      (declare (ignore assignment))
      (expect (= 1 (hash-table-count spill-map)) :to-be-truthy)
      (expect (>= spill-count 1) :to-be-truthy))))

(it-sequential "color-allocate-empty-intervals"
  (multiple-value-bind (assignment spill-map spill-count)
      (color-allocate '() '(:r0 :r1))
    (expect (= 0 (hash-table-count assignment)) :to-be-truthy)
    (expect (= 0 (hash-table-count spill-map)) :to-be-truthy)
    (expect (= 0 spill-count) :to-be-truthy)))

;;; ─── color-spill-slots ────────────────────────────────────────────────────

(it-sequential "color-spill-slots-non-overlapping-share-slot"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (color-map (color-spill-slots (list a b) 0)))
    (expect (= (gethash :a color-map) (gethash :b color-map)) :to-be-truthy)))

(it-sequential "color-spill-slots-overlapping-use-distinct-slots"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (color-map (color-spill-slots (list a b) 0)))
    (expect (/= (gethash :a color-map) (gethash :b color-map)) :to-be-truthy)))

(it-sequential "color-spill-slots-offset-respected"
  (let* ((a (make-test-interval :a 0 5))
         (color-map (color-spill-slots (list a) 3)))
    (expect (>= (gethash :a color-map) 4) :to-be-truthy)))

;;; ─── regalloc-color-spill-slots ───────────────────────────────────────────

(it-sequential "regalloc-color-spill-slots-passthrough-when-empty"
  (let* ((a (make-test-interval :a 0 10))
         (original-map (make-hash-table :test #'eq))
         (result (regalloc-color-spill-slots (list a) original-map 0)))
    (expect (eq original-map result) :to-be-truthy)))

(it-sequential "regalloc-color-spill-slots-applies-coloring"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (spill-map (let ((ht (make-hash-table :test #'eq)))
                      (setf (gethash :a ht) 1
                            (gethash :b ht) 2)
                      ht))
         (result (regalloc-color-spill-slots (list a b) spill-map 0)))
    (expect (= (gethash :a result) (gethash :b result)) :to-be-truthy)))

;;; ─── %color-spill-priority ────────────────────────────────────────────────

(it-sequential "%color-spill-priority ml-disabled :uses-length-weighted-formula"
  (destructuring-bind (start end use-positions crosses-call return-value expected) (list 0 10 '(1 2 3 4 5) nil nil 1/2)
    (let* ((*ml-regalloc-enabled* nil)
         (interval (make-test-interval :v start end
                                       :use-positions use-positions
                                       :crosses-call-p crosses-call
                                       :return-value-p return-value))
         (score (%color-spill-priority interval)))
    (expect (= score expected) :to-be-truthy))))

(it-sequential "%color-spill-priority ml-disabled :adds-call-cross-bonus"
  (destructuring-bind (start end use-positions crosses-call return-value expected) (list 0 10 '(1) t nil 11/10)
    (let* ((*ml-regalloc-enabled* nil)
         (interval (make-test-interval :v start end
                                       :use-positions use-positions
                                       :crosses-call-p crosses-call
                                       :return-value-p return-value))
         (score (%color-spill-priority interval)))
    (expect (= score expected) :to-be-truthy))))

(it-sequential "%color-spill-priority ml-disabled :adds-return-value-bonus"
  (destructuring-bind (start end use-positions crosses-call return-value expected) (list 0 10 '(1) nil t 11/10)
    (let* ((*ml-regalloc-enabled* nil)
         (interval (make-test-interval :v start end
                                       :use-positions use-positions
                                       :crosses-call-p crosses-call
                                       :return-value-p return-value))
         (score (%color-spill-priority interval)))
    (expect (= score expected) :to-be-truthy))))

(it-sequential "%color-spill-priority ml-disabled :length-clamp-at-one"
  (destructuring-bind (start end use-positions crosses-call return-value expected) (list 5 5 '(5) nil nil 1)
    (let* ((*ml-regalloc-enabled* nil)
         (interval (make-test-interval :v start end
                                       :use-positions use-positions
                                       :crosses-call-p crosses-call
                                       :return-value-p return-value))
         (score (%color-spill-priority interval)))
    (expect (= score expected) :to-be-truthy))))

;;; ─── %color-spill-candidate ───────────────────────────────────────────────

(it-sequential "%color-spill-candidate picks lowest-priority interval"
  (let* ((*ml-regalloc-enabled* nil)
         ;; :low-cost: 1 use over 20 positions → score ≈ 0.05
         (low  (make-test-interval :low-cost  0 20 :use-positions '(1)))
         ;; :high-cost: 10 uses over 10 positions → score = 1.0
         (high (make-test-interval :high-cost 0 10 :use-positions '(0 1 2 3 4 5 6 7 8 9 10)))
         (graph (%color-build-interference-graph (list low high)))
         (imap  (%interval-map (list low high)))
         (candidate (%color-spill-candidate graph imap)))
    (expect (eq :low-cost candidate) :to-be-truthy)))

(it-sequential "%color-spill-candidate tie-breaks by name order"
  (let* ((*ml-regalloc-enabled* nil)
         (a (make-test-interval :aaa 0 10 :use-positions '(0)))
         (z (make-test-interval :zzz 0 10 :use-positions '(0)))
         (graph (%color-build-interference-graph (list a z)))
         (imap  (%interval-map (list a z)))
         (candidate (%color-spill-candidate graph imap)))
    (expect (eq :aaa candidate) :to-be-truthy)))

;;; ─── %color-select-register ───────────────────────────────────────────────

(it-sequential "%color-select-register picks first non-conflicting register"
  (let* ((a      (make-test-interval :a 0 10))
         (b      (make-test-interval :b 0 10))
         (graph  (%color-build-interference-graph (list a b)))
         (assign (make-hash-table :test #'eq)))
    ;; Color :b with :r0; :a should receive :r1.
    (setf (gethash :b assign) :r0)
    (let ((result (%color-select-register :a graph assign '(:r0 :r1))))
      (expect (eq :r1 result) :to-be-truthy))))

(it-sequential "%color-select-register returns nil when all registers used by neighbors"
  (let* ((a      (make-test-interval :a 0 10))
         (b      (make-test-interval :b 0 10))
         (graph  (%color-build-interference-graph (list a b)))
         (assign (make-hash-table :test #'eq)))
    ;; Only one register, already given to :b.
    (setf (gethash :b assign) :r0)
    (let ((result (%color-select-register :a graph assign '(:r0))))
      (expect (null result) :to-be-truthy))))

;;; ─── %color-ordered-registers-for-interval ────────────────────────────────

(it-sequential "%color-ordered-registers-for-interval no-cc returns list unchanged"
  (let* ((interval (make-test-interval :v 0 10))
         (regs     '(:r0 :r1 :r2))
         (result   (%color-ordered-registers-for-interval interval nil regs)))
    ;; cc=nil means no preference can be computed.
    (expect (equal regs result) :to-be-truthy)))

;;; ─── %color-assign-spill-slots ────────────────────────────────────────────

(it-sequential "%color-assign-spill-slots assigns increasing slots for overlapping intervals"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15)))
    (multiple-value-bind (spill-map max-slot)
        (%color-assign-spill-slots (list a b) 0)
      (expect (/= (gethash :a spill-map) (gethash :b spill-map)) :to-be-truthy)
      (expect (>= (gethash :a spill-map) 1) :to-be-truthy)
      (expect (>= (gethash :b spill-map) 1) :to-be-truthy)
      (expect (= max-slot (max (gethash :a spill-map) (gethash :b spill-map))) :to-be-truthy))))

(it-sequential "%color-assign-spill-slots non-overlapping intervals share a slot"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10)))
    (multiple-value-bind (spill-map _)
        (%color-assign-spill-slots (list a b) 0)
      (declare (ignore _))
      (expect (= (gethash :a spill-map) (gethash :b spill-map)) :to-be-truthy))))

(it-sequential "%color-assign-spill-slots respects spill-slot-offset"
  (let* ((a (make-test-interval :a 0 5)))
    (multiple-value-bind (spill-map max-slot)
        (%color-assign-spill-slots (list a) 4)
      (expect (>= (gethash :a spill-map) 5) :to-be-truthy)
      (expect (= max-slot (gethash :a spill-map)) :to-be-truthy))))

;;; ─── %interval-map ────────────────────────────────────────────────────────

(it-sequential "%interval-map builds vreg->interval hash correctly"
  (let* ((a   (make-test-interval :a 0 10))
         (b   (make-test-interval :b 5 15))
         (imap (%interval-map (list a b))))
    (expect (eq a (gethash :a imap)) :to-be-truthy)
    (expect (eq b (gethash :b imap)) :to-be-truthy)
    (expect (= 2 (hash-table-count imap)) :to-be-truthy)))

(it-sequential "%interval-map skips nil-vreg intervals"
  (let* ((no-vreg (make-test-interval nil 0 10))
         (named   (make-test-interval :v  0 10))
         (imap    (%interval-map (list no-vreg named))))
    (expect (= 1 (hash-table-count imap)) :to-be-truthy)
    (expect (eq named (gethash :v imap)) :to-be-truthy)))

;;; ─── %copy-hash-into ──────────────────────────────────────────────────────

(it-sequential "%copy-hash-into copies all entries from source to destination"
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :a from) 1
          (gethash :b from) 2)
    (%copy-hash-into from to)
    (expect (= 1 (gethash :a to)) :to-be-truthy)
    (expect (= 2 (gethash :b to)) :to-be-truthy)))

(it-sequential "%copy-hash-into returns the destination table"
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :x from) :y)
    (let ((result (%copy-hash-into from to)))
      (expect (eq to result) :to-be-truthy))))

(it-sequential "%copy-hash-into does not mutate source"
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :k from) 99)
    (%copy-hash-into from to)
    (expect (= 1 (hash-table-count from)) :to-be-truthy)))

;;; ─── %spill-weight ────────────────────────────────────────────────────────

(it-sequential "%spill-weight scoring :plain"
  (destructuring-bind (vreg start end use-positions crosses-call return-value expected) (list 0 10 '(1 2) nil nil 2)
    (let ((interval (make-test-interval vreg start end
                                      :use-positions use-positions
                                      :crosses-call-p crosses-call
                                      :return-value-p return-value)))
    (expect (= expected (%spill-weight interval)) :to-be-truthy))))

(it-sequential "%spill-weight scoring :call-cross"
  (destructuring-bind (vreg start end use-positions crosses-call return-value expected) (list 0 10 '(1) t nil 2)
    (let ((interval (make-test-interval vreg start end
                                      :use-positions use-positions
                                      :crosses-call-p crosses-call
                                      :return-value-p return-value)))
    (expect (= expected (%spill-weight interval)) :to-be-truthy))))

(it-sequential "%spill-weight scoring :return-val"
  (destructuring-bind (vreg start end use-positions crosses-call return-value expected) (list 0 10 '(1) nil t 2)
    (let ((interval (make-test-interval vreg start end
                                      :use-positions use-positions
                                      :crosses-call-p crosses-call
                                      :return-value-p return-value)))
    (expect (= expected (%spill-weight interval)) :to-be-truthy))))

(it-sequential "%spill-weight scoring :call+return"
  (destructuring-bind (vreg start end use-positions crosses-call return-value expected) (list 0 10 '(1) t t 3)
    (let ((interval (make-test-interval vreg start end
                                      :use-positions use-positions
                                      :crosses-call-p crosses-call
                                      :return-value-p return-value)))
    (expect (= expected (%spill-weight interval)) :to-be-truthy))))

(it-sequential "%spill-weight scoring :no-uses"
  (destructuring-bind (vreg start end use-positions crosses-call return-value expected) (list 0 10 '() nil nil 0)
    (let ((interval (make-test-interval vreg start end
                                      :use-positions use-positions
                                      :crosses-call-p crosses-call
                                      :return-value-p return-value)))
    (expect (= expected (%spill-weight interval)) :to-be-truthy))))

;;; ─── %build-spill-interference-matrix ────────────────────────────────────

(it-sequential "%build-spill-interference-matrix overlapping intervals interfere"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 5 15))
         (matrix (%build-spill-interference-matrix (list a b))))
    (expect (member :b (gethash :a matrix) :test #'eq) :to-be-truthy)
    (expect (member :a (gethash :b matrix) :test #'eq) :to-be-truthy)))

(it-sequential "%build-spill-interference-matrix non-overlapping intervals do not interfere"
  (let* ((a (make-test-interval :a 0 5))
         (b (make-test-interval :b 6 10))
         (matrix (%build-spill-interference-matrix (list a b))))
    (expect (null (gethash :a matrix)) :to-be-truthy)
    (expect (null (gethash :b matrix)) :to-be-truthy)))

(it-sequential "%build-spill-interference-matrix symmetric edges"
  (let* ((a (make-test-interval :a 0 10))
         (b (make-test-interval :b 3  8))
         (c (make-test-interval :c 0 10))
         (matrix (%build-spill-interference-matrix (list a b c))))
    (dolist (vreg '(:a :b :c))
      (dolist (neighbor (gethash vreg matrix))
        (expect (member vreg (gethash neighbor matrix) :test #'eq) :to-be-truthy)))))

;;; ─── color-allocate-for-target (GPR+FP class split) ──────────────────────

(it-sequential "color-allocate-for-target separates gpr and fp classes"
  (let* ((gpr-int (make-live-interval :vreg :gpr :start 0 :end 10
                                      :use-positions '(0 10)
                                      :crosses-call-p nil
                                      :return-value-p nil
                                      :fp-p nil))
         (fp-int  (make-live-interval :vreg :fpr :start 0 :end 10
                                      :use-positions '(0 10)
                                      :crosses-call-p nil
                                      :return-value-p nil
                                      :fp-p t))
         ;; Use an x86-64 target so regalloc-target-fp-registers returns the
         ;; standard xmm pool and target-allocatable-regs derives from gpr-names.
         (cc      (make-target-desc
                    :name        :x86-64
                    :gpr-names   #(:rdi :rsi :rdx :rcx :r8 :r9 :rbx :r12)
                    :arg-regs    '(:rdi :rsi :rdx :rcx :r8 :r9)
                    :ret-reg     :rax
                    :fp-arg-regs '(:xmm0 :xmm1 :xmm2 :xmm3)
                    :fp-ret-reg  :xmm0
                    :callee-saved '(:rbx :r12)
                    :scratch-regs nil)))
    (multiple-value-bind (assignment spill-map spill-count)
        (color-allocate-for-target (list gpr-int fp-int) cc)
      (expect (gethash :gpr assignment) :to-be-truthy)
      (expect (gethash :fpr assignment) :to-be-truthy)
      (expect (= 0 (hash-table-count spill-map)) :to-be-truthy)
      (expect (= 0 spill-count) :to-be-truthy))))
