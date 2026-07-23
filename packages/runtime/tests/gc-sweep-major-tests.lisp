;;;; tests/gc-sweep-major-tests.lisp - GC Sweep and Major Collection Tests
;;;
;;; Tests for %gc-sweep-old-space and rt-gc-major-collect:
;;; - Sweep reclaims dead (unmarked) old-space objects to the free-list
;;; - Sweep clears mark bits on live objects
;;; - Major GC increments counter and restores gc-state
;;; - Major GC reclaims unreachable old-space objects
;;; - Major GC preserves rooted old-space objects

(in-package :cl-cc/test)


;;; ------------------------------------------------------------
;;; Test 9: %gc-sweep-old-space
;;; ------------------------------------------------------------

(it-sequential "gc-sweep-old-space-reclaims-dead-objects"
  (let* ((heap     (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64))
         (old-base (cl-cc/runtime:rt-heap-old-base heap))
         ;; Place two 3-word objects in old space
         (live-addr old-base)
         (dead-addr (+ old-base 3)))
    ;; Write headers: live object is marked; dead object is not
    (cl-cc/runtime:rt-heap-set-header
     heap live-addr
     (cl-cc/runtime:header-set-mark
      (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0)))
    (cl-cc/runtime:rt-heap-set-header
     heap dead-addr
     (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0))
    ;; Advance old-free past both objects
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
    ;; Sweep
    (cl-cc/runtime::%gc-sweep-old-space heap)
    ;; Dead object (3 words) should be reclaimed
    (expect (>= (cl-cc/runtime:rt-heap-words-collected heap) 3) :to-be-truthy)
    ;; Free-list should contain the dead object
    (expect (not (null (cl-cc/runtime:rt-heap-free-list heap))) :to-be-truthy)
    ;; Live object's mark bit should be cleared after sweep
    (let ((live-hdr (cl-cc/runtime:rt-heap-object-header heap live-addr)))
      (expect (cl-cc/runtime:header-marked-p live-hdr) :to-be-falsy))))

(it-sequential "gc-sweep-old-space-all-live-no-reclaim"
  (let* ((heap     (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    ;; Two marked objects
    (cl-cc/runtime:rt-heap-set-header
     heap old-base
     (cl-cc/runtime:header-set-mark
      (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0)))
    (cl-cc/runtime:rt-heap-set-header
     heap (+ old-base 3)
     (cl-cc/runtime:header-set-mark
      (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0)))
    (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
    (cl-cc/runtime::%gc-sweep-old-space heap)
    (expect (= 0 (cl-cc/runtime:rt-heap-words-collected heap)) :to-be-truthy)
    (expect (null (cl-cc/runtime:rt-heap-free-list heap)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Test 10: rt-gc-major-collect
;;; ------------------------------------------------------------

(it-sequential "gc-major-collect-increments-counter"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal))
        (cl-cc/runtime::*rt-global-var-registry* (make-hash-table :test #'eq)))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
      (expect (= 0 (cl-cc/runtime:rt-heap-major-gc-count heap)) :to-be-truthy)
      (cl-cc/runtime:rt-gc-major-collect heap)
      (expect (= 1 (cl-cc/runtime:rt-heap-major-gc-count heap)) :to-be-truthy)
      (expect (cl-cc/runtime:rt-heap-gc-state heap) :to-be :normal))))

(it-sequential "gc-major-collect-gc-state-restored-on-empty-heap"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal))
        (cl-cc/runtime::*rt-global-var-registry* (make-hash-table :test #'eq)))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128)))
      (cl-cc/runtime:rt-gc-major-collect heap)
      (expect (cl-cc/runtime:rt-heap-gc-state heap) :to-be :normal))))

(it-sequential "gc-major-collect-reclaims-unreachable-old-object"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal))
        (cl-cc/runtime::*rt-global-var-registry* (make-hash-table :test #'eq)))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128))
           ;; Promote an object by bumping it into old space manually
           (old-base (cl-cc/runtime:rt-heap-old-base heap)))
      ;; Place an unmarked object in old space with no root
      (cl-cc/runtime:rt-heap-set-header
       heap old-base
       (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0))
      (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 3))
      ;; Run major GC — object is unreachable, should be collected
      (cl-cc/runtime:rt-gc-major-collect heap)
      (expect (>= (cl-cc/runtime:rt-heap-words-collected heap) 3) :to-be-truthy))))

(it-sequential "gc-major-collect-preserves-rooted-old-object"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal))
        (cl-cc/runtime::*rt-global-var-registry* (make-hash-table :test #'eq)))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128))
           (old-base (cl-cc/runtime:rt-heap-old-base heap)))
      ;; Place live object in old space
      (cl-cc/runtime:rt-heap-set-header
       heap old-base
       (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0))
      (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 3))
      ;; Register root pointing directly into old space
      (let ((root (cons nil old-base)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-major-collect heap)
        ;; Live object must still have a readable header
        (let ((hdr (cl-cc/runtime:rt-heap-object-header heap old-base)))
          (expect (integerp hdr) :to-be-truthy)
          (expect (= 3 (cl-cc/runtime:rt-header-size hdr)) :to-be-truthy))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

(it-sequential "gc-major-collect-stats-major-count"
  (let ((cl-cc/runtime:*rt-concurrent-gc-enabled-p* nil)
        (cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal))
        (cl-cc/runtime::*rt-global-var-registry* (make-hash-table :test #'eq)))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128)))
      (cl-cc/runtime:rt-gc-major-collect heap)
      (cl-cc/runtime:rt-gc-major-collect heap)
      (expect (= 2 (getf (cl-cc/runtime:rt-gc-stats heap) :major-gc-count)) :to-be-truthy))))

(it-sequential "gc-configure-concurrent-mode-updates-runtime-flags"
  (let ((cl-cc/runtime:*rt-concurrent-gc-enabled-p* nil)
        (cl-cc/runtime:*rt-concurrent-gc-write-barrier-mode* :satb)
        (cl-cc/runtime:*rt-concurrent-gc-stw-phases* nil)
        (cl-cc/runtime:*rt-concurrent-gc-mutator-assist-p* nil))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128)))
      (cl-cc/runtime:rt-gc-configure-concurrent-mode
       :enabled-p t
       :write-barrier :satb
       :stw-phases '(:initial-mark :final-remark)
       :mutator-assist-p t)
      (let ((stats (cl-cc/runtime:rt-gc-stats heap)))
        (expect (getf stats :concurrent-gc-enabled-p) :to-be-truthy)
        (expect (getf stats :concurrent-gc-write-barrier) :to-be :satb)
        (expect (getf stats :concurrent-gc-stw-phases) :to-equal '(:initial-mark :final-remark))
        (expect (getf stats :concurrent-gc-mutator-assist-p) :to-be-truthy)))))

(it-sequential "gc-major-collect-enters-concurrent-state-when-enabled"
  (let ((cl-cc/runtime:*rt-concurrent-gc-enabled-p* nil)
        (cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal))
        (cl-cc/runtime::*rt-global-var-registry* (make-hash-table :test #'eq)))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128)))
      (cl-cc/runtime:rt-gc-configure-concurrent-mode :enabled-p t)
      (cl-cc/runtime:rt-gc-major-collect heap)
      (expect (cl-cc/runtime:rt-heap-gc-state heap) :to-be :normal)
      (expect (getf (cl-cc/runtime:rt-gc-stats heap) :concurrent-gc-enabled-p) :to-be-truthy))))

(it-sequential "gc-concurrent-assist-marks-satb-old-pointers-with-budget"
  (let ((cl-cc/runtime:*rt-concurrent-gc-enabled-p* nil)
        (cl-cc/runtime:*rt-concurrent-gc-mutator-assist-p* nil))
    (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 128))
           (old-base (cl-cc/runtime:rt-heap-old-base heap))
           (a old-base)
           (b (+ old-base 3)))
      (cl-cc/runtime:rt-heap-set-header
       heap a (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0))
      (cl-cc/runtime:rt-heap-set-header
       heap b (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0))
      (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
      (setf (cl-cc/runtime:rt-heap-satb-queue heap) (list a b))
      (cl-cc/runtime:rt-gc-configure-concurrent-mode
       :enabled-p t
       :mutator-assist-p t)
      (setf (cl-cc/runtime:rt-heap-gc-state heap) :major-gc-concurrent)
      (expect (= 1 (cl-cc/runtime:rt-gc-concurrent-assist heap :budget 1)) :to-be-truthy)
      (expect (or (cl-cc/runtime:header-marked-p (cl-cc/runtime:rt-heap-object-header heap a))
           (cl-cc/runtime:header-marked-p (cl-cc/runtime:rt-heap-object-header heap b))) :to-be-truthy)
      ;; SATB queue is fully drained to the grey queue before any budgeted
      ;; incremental marking occurs; expect the queue to be empty.
      (expect (= 0 (length (cl-cc/runtime:rt-heap-satb-queue heap))) :to-be-truthy))))
