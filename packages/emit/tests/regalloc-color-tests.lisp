;;;; tests/regalloc-color-tests.lisp - Graph-Coloring Allocation Internals
;;;
;;; Covers the regalloc-color.lisp helpers NOT already exercised by the color
;;; section of regalloc-tests.lisp: %color-spill-candidate tie-breaking,
;;; %color-ordered-registers-for-interval, %color-assign-spill-slots (the
;;; internal two-value form), %interval-map, %copy-hash-into, %spill-weight,
;;; and %build-spill-interference-matrix.
;;;
;;; Migrated from the unregistered packages/regalloc/tests/regalloc-color-tests.lisp
;;; into the cl-cc/test suite convention; redundant coverage already present in
;;; regalloc-tests.lisp is intentionally omitted.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-color-test-interval (vreg start end &key (use-positions nil use-positions-p)
                                                     crosses-call-p return-value-p)
  "Construct a live-interval for graph-coloring tests.
An explicitly supplied USE-POSITIONS (even nil/empty) is honored; only an
absent USE-POSITIONS defaults to the interval's endpoints."
  (cl-cc/regalloc::make-live-interval
   :vreg vreg
   :start start
   :end end
   :use-positions (if use-positions-p use-positions (list start end))
   :crosses-call-p crosses-call-p
   :return-value-p return-value-p))

;;; ─── %color-spill-candidate (tie-break) ───────────────────────────────────

(deftest color-spill-candidate-tie-breaks-by-name-order
  "%color-spill-candidate breaks priority ties by selecting the interval whose vreg name sorts first."
  ;; Equal priority: :aaa should come before :zzz alphabetically.
  (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
         (a (make-color-test-interval :aaa 0 10 :use-positions '(0)))
         (z (make-color-test-interval :zzz 0 10 :use-positions '(0)))
         (graph (cl-cc/regalloc::%color-build-interference-graph (list a z)))
         (imap  (cl-cc/regalloc::%interval-map (list a z)))
         (candidate (cl-cc/regalloc::%color-spill-candidate graph imap)))
    (assert-eq :aaa candidate)))

;;; ─── %color-ordered-registers-for-interval ────────────────────────────────

(deftest color-ordered-registers-for-interval-no-cc-unchanged
  "%color-ordered-registers-for-interval returns the register list unchanged when no calling convention is provided."
  (let* ((interval (make-color-test-interval :v 0 10))
         (regs     '(:r0 :r1 :r2))
         (result   (cl-cc/regalloc::%color-ordered-registers-for-interval interval nil regs)))
    (assert-equal regs result)))

;;; ─── %color-assign-spill-slots ────────────────────────────────────────────

(deftest color-assign-spill-slots-distinct-for-overlapping
  "%color-assign-spill-slots assigns distinct increasing slot numbers to overlapping intervals."
  (let* ((a (make-color-test-interval :a 0 10))
         (b (make-color-test-interval :b 5 15)))
    (multiple-value-bind (spill-map max-slot)
        (cl-cc/regalloc::%color-assign-spill-slots (list a b) 0)
      (assert-true (/= (gethash :a spill-map) (gethash :b spill-map)))
      (assert-true (>= (gethash :a spill-map) 1))
      (assert-true (>= (gethash :b spill-map) 1))
      (assert-= max-slot (max (gethash :a spill-map) (gethash :b spill-map))))))

(deftest color-assign-spill-slots-shared-for-non-overlapping
  "%color-assign-spill-slots assigns the same slot number to non-overlapping intervals."
  (let* ((a (make-color-test-interval :a 0 5))
         (b (make-color-test-interval :b 6 10)))
    (multiple-value-bind (spill-map max-slot)
        (cl-cc/regalloc::%color-assign-spill-slots (list a b) 0)
      (declare (ignore max-slot))
      (assert-= (gethash :a spill-map) (gethash :b spill-map)))))

(deftest color-assign-spill-slots-respects-offset
  "%color-assign-spill-slots ensures all assigned slot numbers exceed the given spill-slot-offset."
  (let* ((a (make-color-test-interval :a 0 5)))
    (multiple-value-bind (spill-map max-slot)
        (cl-cc/regalloc::%color-assign-spill-slots (list a) 4)
      (assert-true (>= (gethash :a spill-map) 5))
      (assert-= max-slot (gethash :a spill-map)))))

;;; ─── %interval-map ────────────────────────────────────────────────────────

(deftest color-interval-map-builds-vreg-to-interval-hash
  "%interval-map builds a hash table mapping each vreg keyword to its live-interval struct."
  (let* ((a   (make-color-test-interval :a 0 10))
         (b   (make-color-test-interval :b 5 15))
         (imap (cl-cc/regalloc::%interval-map (list a b))))
    (assert-eq a (gethash :a imap))
    (assert-eq b (gethash :b imap))
    (assert-= 2 (hash-table-count imap))))

(deftest color-interval-map-skips-nil-vreg
  "%interval-map excludes intervals whose vreg is nil from the resulting hash table."
  (let* ((no-vreg (make-color-test-interval nil 0 10))
         (named   (make-color-test-interval :v  0 10))
         (imap    (cl-cc/regalloc::%interval-map (list no-vreg named))))
    (assert-= 1 (hash-table-count imap))
    (assert-eq named (gethash :v imap))))

;;; ─── %copy-hash-into ──────────────────────────────────────────────────────

(deftest color-copy-hash-into-copies-all-entries
  "%copy-hash-into transfers all key-value pairs from the source hash table into the destination."
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :a from) 1
          (gethash :b from) 2)
    (cl-cc/regalloc::%copy-hash-into from to)
    (assert-= 1 (gethash :a to))
    (assert-= 2 (gethash :b to))))

(deftest color-copy-hash-into-returns-destination
  "%copy-hash-into returns the destination hash table as its result."
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :x from) :y)
    (assert-eq to (cl-cc/regalloc::%copy-hash-into from to))))

(deftest color-copy-hash-into-does-not-mutate-source
  "%copy-hash-into leaves the source hash table unmodified after the copy."
  (let ((from (make-hash-table :test #'eq))
        (to   (make-hash-table :test #'eq)))
    (setf (gethash :k from) 99)
    (cl-cc/regalloc::%copy-hash-into from to)
    (assert-= 1 (hash-table-count from))))

;;; ─── %spill-weight ────────────────────────────────────────────────────────

(deftest-each color-spill-weight-scoring
  "%spill-weight counts use positions and adds one point each for call-crossing and return-value intervals."
  :cases (("plain"        0 10 '(1 2) nil nil 2)
          ("call-cross"   0 10 '(1)   t   nil 2)
          ("return-val"   0 10 '(1)   nil t   2)
          ("call+return"  0 10 '(1)   t   t   3)
          ("no-uses"      0 10 '()    nil nil 0))
  (start end use-positions crosses-call return-value expected)
  (let ((interval (make-color-test-interval :v start end
                                            :use-positions use-positions
                                            :crosses-call-p crosses-call
                                            :return-value-p return-value)))
    (assert-= expected (cl-cc/regalloc::%spill-weight interval))))

;;; ─── %build-spill-interference-matrix ─────────────────────────────────────

(deftest color-build-spill-interference-matrix-overlapping-interfere
  "%build-spill-interference-matrix records mutual interference for overlapping intervals."
  (let* ((a (make-color-test-interval :a 0 10))
         (b (make-color-test-interval :b 5 15))
         (matrix (cl-cc/regalloc::%build-spill-interference-matrix (list a b))))
    (assert-true (member :b (gethash :a matrix) :test #'eq))
    (assert-true (member :a (gethash :b matrix) :test #'eq))))

(deftest color-build-spill-interference-matrix-non-overlapping-clear
  "%build-spill-interference-matrix produces no interference entries for non-overlapping intervals."
  (let* ((a (make-color-test-interval :a 0 5))
         (b (make-color-test-interval :b 6 10))
         (matrix (cl-cc/regalloc::%build-spill-interference-matrix (list a b))))
    (assert-null (gethash :a matrix))
    (assert-null (gethash :b matrix))))

(deftest color-build-spill-interference-matrix-symmetric
  "%build-spill-interference-matrix produces a symmetric undirected interference matrix."
  (let* ((a (make-color-test-interval :a 0 10))
         (b (make-color-test-interval :b 3  8))
         (c (make-color-test-interval :c 0 10))
         (matrix (cl-cc/regalloc::%build-spill-interference-matrix (list a b c))))
    (dolist (vreg '(:a :b :c))
      (dolist (neighbor (gethash vreg matrix))
        (assert-true (member vreg (gethash neighbor matrix) :test #'eq))))))
