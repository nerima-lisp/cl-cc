;;;; tests/gc-stats-tests.lisp — GC Statistics Tests
;;;;
;;;; Continuation of gc-tests.lisp.
;;;; Tests for rt-gc-stats plist structure and correctness.

(in-package :cl-cc/test)


;;; ------------------------------------------------------------
;;; Test 8: gc-stats
;;; ------------------------------------------------------------

(it-sequential "gc-stats-returns-plist"
  (let* ((heap (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (expect (getf stats :minor-gc-count) :to-be-truthy)
    (expect (listp stats) :to-be-truthy)
    (expect (member :minor-gc-count  stats) :to-be-truthy)
    (expect (member :major-gc-count  stats) :to-be-truthy)
    (expect (member :words-collected stats) :to-be-truthy)
    (expect (member :words-promoted  stats) :to-be-truthy)
    (expect (member :young-used      stats) :to-be-truthy)
    (expect (member :young-total     stats) :to-be-truthy)
    (expect (member :old-used        stats) :to-be-truthy)
    (expect (member :old-total       stats) :to-be-truthy)
     (expect (member :heap-occupancy-pct stats) :to-be-truthy)
     (expect (member :free-list-count stats) :to-be-truthy)))

(it-sequential "gc-stats-public-facade-returns-fr-356-keys"
  (let* ((heap (%make-small-heap))
         (stats (cl-cc/runtime:gc-stats heap)))
    (expect (member :minor-gcs stats) :to-be-truthy)
    (expect (member :major-gcs stats) :to-be-truthy)
    (expect (member :total-collected-bytes stats) :to-be-truthy)
    (expect (member :pause-ms-p99 stats) :to-be-truthy)
    (expect (= 0 (getf stats :minor-gcs)) :to-be-truthy)
    (expect (= 0 (getf stats :major-gcs)) :to-be-truthy)
    (expect (= 0 (getf stats :total-collected-bytes)) :to-be-truthy)))

(it-sequential "gc-stats-minor-gc-count-increments"
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (expect (= 1 (getf (cl-cc/runtime:rt-gc-stats heap) :minor-gc-count)) :to-be-truthy)
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

(it-sequential "gc-stats-totals-match-heap-structure"
  (let* ((heap  (%make-small-heap))
         (stats (cl-cc/runtime:rt-gc-stats heap)))
    (expect (= (cl-cc/runtime:rt-heap-young-semi-size heap) (getf stats :young-total)) :to-be-truthy)
    (expect (= (cl-cc/runtime:rt-heap-old-size heap) (getf stats :old-total)) :to-be-truthy)))

(it-sequential "gc-stats-young-used-after-alloc"
  (let* ((heap (%make-small-heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)
    (expect (= 6 (getf (cl-cc/runtime:rt-gc-stats heap) :young-used)) :to-be-truthy)))

(it-sequential "rt-heap-occupancy-pct-combines-young-and-old-usage"
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 40 :old-size 30))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 5)
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 10))
    (expect (= 30.0d0 (cl-cc/runtime:rt-heap-occupancy-pct heap)) :to-be-truthy)
    (expect (= 30.0d0 (getf (cl-cc/runtime:rt-gc-stats heap)
                           :heap-occupancy-pct)) :to-be-truthy)))

(it-sequential "rt-gc-profile-samples-allocation-sites"
  (let ((heap (%make-small-heap))
        (cl-cc/runtime:*gc-profile-enabled* t)
        (cl-cc/runtime:*gc-profile-interval* 16)
        (cl-cc/runtime::*gc-profile-bytes-since-sample* 0)
        (cl-cc/runtime::*gc-profile-samples* (make-hash-table :test #'equal))
        (cl-cc/runtime::*gc-profile-current-function* :profile-test))
    (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 2)
    (let ((report (cl-cc/runtime:rt-gc-profile-report)))
      (expect (getf report :enabled-p) :to-be-truthy)
      (expect (getf report :hot-spots) :to-equal (list (list :function :profile-test :count 1))))))
