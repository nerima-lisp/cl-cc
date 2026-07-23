;;;; tests/gc-tests.lisp - Generational GC Tests
;;;
;;; Tests for the cl-cc/runtime generational GC:
;;; - Object header encoding/decoding
;;; - Heap creation and layout
;;; - Bump-pointer allocation
;;; - Minor GC: garbage collection, live-object preservation, promotion
;;; - Write barrier and card table
;;; - GC statistics

(in-package :cl-cc/test)

;;; Import GC symbols from the runtime package.
;;; We alias them locally via a helper rather than polluting the test package
;;; — all calls below are fully qualified as cl-cc/runtime:*.

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-small-heap ()
  "Create a minimal heap: 32-word young space (16 per semi), 32-word old space."
  (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))

(defun %write-header (heap addr size tag &optional (age 0))
  "Write a freshly made header at ADDR."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-rt-header size tag :gc-bits age)))

;;; ------------------------------------------------------------
;;; Test 1: gc-header-basics
;;; ------------------------------------------------------------

(it-sequential "gc-header-field-roundtrip size"
  (destructuring-bind (h accessor expected) (list (cl-cc/runtime:make-rt-header 7 1 :gc-bits 0) #'cl-cc/runtime:rt-header-size 7)
    (expect (= expected (funcall accessor h)) :to-be-truthy)))

(it-sequential "gc-header-field-roundtrip tag"
  (destructuring-bind (h accessor expected) (list (cl-cc/runtime:make-rt-header 3 5 :gc-bits 0) #'cl-cc/runtime:rt-header-type-tag 5)
    (expect (= expected (funcall accessor h)) :to-be-truthy)))

(it-sequential "gc-header-field-roundtrip age"
  (destructuring-bind (h accessor expected) (list (cl-cc/runtime:make-rt-header 3 1 :gc-bits 2) #'cl-cc/runtime:rt-header-age 2)
    (expect (= expected (funcall accessor h)) :to-be-truthy)))

(it-sequential "gc-header-bit-toggles"
  (let ((h (cl-cc/runtime:make-rt-header 3 1 :gc-bits 0)))
    ;; mark bit
    (let* ((hm (cl-cc/runtime:header-set-mark h))
           (hu (cl-cc/runtime:header-clear-mark hm)))
      (expect (cl-cc/runtime:header-marked-p hm) :to-be-truthy)
      (expect (cl-cc/runtime:header-marked-p h) :to-be-falsy)
      (expect (cl-cc/runtime:header-marked-p hu) :to-be-falsy))
    ;; gray bit
    (let* ((hg (cl-cc/runtime:header-set-gray h))
           (hu (cl-cc/runtime:header-clear-gray hg)))
      (expect (cl-cc/runtime:header-gray-p hg) :to-be-truthy)
      (expect (cl-cc/runtime:header-gray-p h) :to-be-falsy)
      (expect (cl-cc/runtime:header-gray-p hu) :to-be-falsy))))

(it-sequential "gc-header-forwarding-cases not-forwarding"
  (destructuring-bind (expect-fwd fwd-ptr plain-header target-addr) (list nil nil (cl-cc/runtime:make-rt-header 3 1 :gc-bits 0) nil)
    (if expect-fwd
      (let ((fwd (cl-cc/runtime:header-make-forwarding-ptr target-addr)))
        (expect (cl-cc/runtime:header-forwarding-p fwd) :to-be-truthy)
        (expect (= target-addr (cl-cc/runtime:header-forwarding-ptr fwd)) :to-be-truthy))
      (expect (cl-cc/runtime:header-forwarding-p plain-header) :to-be-falsy))))

(it-sequential "gc-header-forwarding-cases forwarding-ptr"
  (destructuring-bind (expect-fwd fwd-ptr plain-header target-addr) (list t nil nil 42)
    (if expect-fwd
      (let ((fwd (cl-cc/runtime:header-make-forwarding-ptr target-addr)))
        (expect (cl-cc/runtime:header-forwarding-p fwd) :to-be-truthy)
        (expect (= target-addr (cl-cc/runtime:header-forwarding-ptr fwd)) :to-be-truthy))
      (expect (cl-cc/runtime:header-forwarding-p plain-header) :to-be-falsy))))

(it-sequential "rt-header-increment-age increment"
  (destructuring-bind (start-age expected) (list 1 2)
    (let* ((h  (cl-cc/runtime:make-rt-header 3 1 :gc-bits start-age))
         (h2 (cl-cc/runtime:rt-header-increment-age h)))
    (expect (= expected (cl-cc/runtime:rt-header-age h2)) :to-be-truthy))))

(it-sequential "rt-header-increment-age cap-at-3"
  (destructuring-bind (start-age expected) (list 3 3)
    (let* ((h  (cl-cc/runtime:make-rt-header 3 1 :gc-bits start-age))
         (h2 (cl-cc/runtime:rt-header-increment-age h)))
    (expect (= expected (cl-cc/runtime:rt-header-age h2)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; Test 2: gc-heap-creation
;;; ------------------------------------------------------------

(it-sequential "gc-heap-creation-layout 32-word-young"
  (destructuring-bind (heap expected-from expected-to expected-old) (list (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32) 0 16 32)
    (expect (= expected-from (cl-cc/runtime:rt-heap-young-from-base heap)) :to-be-truthy) (expect (= expected-to (cl-cc/runtime:rt-heap-young-to-base heap)) :to-be-truthy) (expect (= expected-old (cl-cc/runtime:rt-heap-old-base heap)) :to-be-truthy)))

(it-sequential "gc-heap-creation-layout 16-word-young"
  (destructuring-bind (heap expected-from expected-to expected-old) (list (cl-cc/runtime:make-rt-heap :young-size 16 :old-size 16) 0 8 16)
    (expect (= expected-from (cl-cc/runtime:rt-heap-young-from-base heap)) :to-be-truthy) (expect (= expected-to (cl-cc/runtime:rt-heap-young-to-base heap)) :to-be-truthy) (expect (= expected-old (cl-cc/runtime:rt-heap-old-base heap)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Test 3: gc-alloc-basic
;;; ------------------------------------------------------------

(it-sequential "gc-alloc-sequential-addresses-and-free-pointer"
  (let* ((heap (%make-small-heap))
         (addr1 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (expect (= 0 addr1) :to-be-truthy)
    (expect (= 3 (cl-cc/runtime:rt-heap-young-free heap)) :to-be-truthy)
    (let ((addr2 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (expect (= 3 addr2) :to-be-truthy))))

(it-sequential "rt-alloc-header-size-readable"
  (let* ((heap (%make-small-heap))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (expect (= 3 (cl-cc/runtime:rt-heap-object-size heap addr)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Test 4: gc-minor-gc-collects-garbage
;;; ------------------------------------------------------------

(it-sequential "gc-minor-gc-collects-and-updates-root"
  (let* ((heap (%make-small-heap)))
    (let ((addr1 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
          (addr2 (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr1 3 cl-cc/runtime:+rt-tag-cons+)
      (%write-header heap addr2 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Only register addr1 as a root; addr2 is unreachable
      (let ((root (cons nil addr1)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (expect (= 1 (cl-cc/runtime:rt-heap-minor-gc-count heap)) :to-be-truthy)
        (expect (>= (cl-cc/runtime:rt-heap-words-collected heap) 3) :to-be-truthy)
        (expect (cl-cc/runtime:rt-young-addr-p heap (cdr root)) :to-be-truthy)
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 5: gc-minor-gc-preserves-live-objects
;;; ------------------------------------------------------------

(it-sequential "gc-minor-gc-preserves-live-object-data"
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      ;; Write slot values (non-pointer integers, safe for this test)
      (cl-cc/runtime:rt-heap-set heap (+ addr 1) 111)
      (cl-cc/runtime:rt-heap-set heap (+ addr 2) 222)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (let ((new-addr (cdr root)))
          (expect (= 111 (cl-cc/runtime:rt-heap-ref heap (+ new-addr 1))) :to-be-truthy)
          (expect (= 222 (cl-cc/runtime:rt-heap-ref heap (+ new-addr 2))) :to-be-truthy))
        (cl-cc/runtime:rt-gc-remove-root heap root))))
  (let* ((heap (%make-small-heap)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-string+ 3)))
      (%write-header heap addr 3 cl-cc/runtime:+rt-tag-string+)
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        (let* ((new-addr (cdr root))
               (new-hdr  (cl-cc/runtime:rt-heap-object-header heap new-addr)))
          (expect (= cl-cc/runtime:+rt-tag-string+ (cl-cc/runtime:rt-header-type-tag new-hdr)) :to-be-truthy))
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 6: gc-promotion-threshold
;;; ------------------------------------------------------------

(it-sequential "gc-promotion-promotes-old-object"
  (let* ((cl-cc/runtime:*gc-tenuring-threshold* 3)
         (heap (cl-cc/runtime:make-rt-heap :young-size 128 :old-size 64)))
    (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      ;; Write header with age already at the tenuring threshold so the
      ;; very next minor GC will promote it.
      (cl-cc/runtime:rt-heap-set-header
       heap addr
       (cl-cc/runtime:make-rt-header 3 cl-cc/runtime:+rt-tag-cons+ :gc-bits 3))
      (let ((root (cons nil addr)))
        (cl-cc/runtime:rt-gc-add-root heap root)
        (cl-cc/runtime:rt-gc-minor-collect heap)
        ;; words-promoted must be > 0
        (expect (> (cl-cc/runtime:rt-heap-words-promoted heap) 0) :to-be-truthy)
        ;; The object must now live in old space
        (expect (cl-cc/runtime:rt-old-addr-p heap (cdr root)) :to-be-truthy)
        (cl-cc/runtime:rt-gc-remove-root heap root)))))

;;; ------------------------------------------------------------
;;; Test 7: gc-write-barrier-card-dirty
;;; ------------------------------------------------------------

(it-sequential "gc-write-barrier-card-dirty-behavior"
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (let ((young-addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%write-header heap young-addr 3 cl-cc/runtime:+rt-tag-cons+)
      (let* ((old-addr (cl-cc/runtime:rt-heap-old-base heap)))
        (%write-header heap old-addr 3 cl-cc/runtime:+tag-other+)
        (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-addr 3))
        (expect (cl-cc/runtime:rt-card-dirty-p heap old-addr) :to-be-falsy)
        (cl-cc/runtime:rt-gc-write-barrier heap old-addr 1 young-addr)
        (expect (cl-cc/runtime:rt-card-dirty-p heap old-addr) :to-be-truthy)
        (expect (= young-addr (cl-cc/runtime:rt-heap-ref heap (+ old-addr 1))) :to-be-truthy))))
  (let* ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (let* ((old-base (cl-cc/runtime:rt-heap-old-base heap))
           (obj1     old-base)
           (obj2     (+ old-base 3)))
      (%write-header heap obj1 3 cl-cc/runtime:+tag-other+)
      (%write-header heap obj2 3 cl-cc/runtime:+tag-other+)
      (setf (cl-cc/runtime:rt-heap-old-free heap) (+ old-base 6))
      (cl-cc/runtime:rt-gc-write-barrier heap obj1 1 obj2)
      (expect (cl-cc/runtime:rt-card-dirty-p heap obj1) :to-be-falsy))))

;;; ------------------------------------------------------------
;;; Section 4: GC minor helper unit tests
;;; ------------------------------------------------------------

(it-sequential "gc-update-promoted-empty-list-is-noop"
  (let ((heap (%make-small-heap)))
    (cl-cc/runtime::%gc-update-promoted heap '())
    ;; No assertion needed — just verify no error is raised.
    (expect t :to-be-truthy)))

(it-sequential "gc-cheney-scan-empty-to-space-terminates"
  (let* ((heap         (%make-small-heap))
         (base         (cl-cc/runtime:rt-heap-young-to-base heap))
         (to-free-cell (cons base base))
         (promoted     (list nil))
         (in-source-p  (lambda (addr) (declare (ignore addr)) nil)))
    ;; scan == (cdr to-free-cell) so the loop body never runs
    (cl-cc/runtime::%gc-cheney-scan heap base to-free-cell promoted in-source-p)
    (expect (= base (cdr to-free-cell)) :to-be-truthy)))

(it-sequential "gc-scan-dirty-cards-clean-heap-is-noop"
  (let* ((heap         (%make-small-heap))
         (to-free-cell (cons (cl-cc/runtime:rt-heap-young-to-base heap)
                             (cl-cc/runtime:rt-heap-young-to-base heap)))
         (promoted     (list nil))
         (in-source-p  (lambda (addr) (declare (ignore addr)) nil))
         (initial-free (cdr to-free-cell)))
    (cl-cc/runtime::%gc-scan-dirty-cards heap to-free-cell promoted in-source-p)
    ;; No dirty cards → to-free not advanced
    (expect (= initial-free (cdr to-free-cell)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Test: rt-gc-auto-configure-heap
;;; ------------------------------------------------------------

(it-sequential "gc-auto-configure-small-memory-within-clamp-bounds"
  (let ((saved-young cl-cc/runtime::*gc-young-size-words*)
        (saved-old   cl-cc/runtime::*gc-old-size-words*))
    (unwind-protect
        (let ((result (cl-cc/runtime::rt-gc-auto-configure-heap :memory-bytes (* 4 1024 1024))))
          ;; young clamp: [1MB/8 .. 4MB/8] words; small input should hit lower bound
          (expect (>= cl-cc/runtime::*gc-young-size-words* (floor (* 1 1024 1024) 8)) :to-be-truthy)
          (expect (<= cl-cc/runtime::*gc-young-size-words* (floor (* 4 1024 1024) 8)) :to-be-truthy)
          ;; old clamp: [4MB/8 .. 16MB/8] words; small input should hit lower bound
          (expect (>= cl-cc/runtime::*gc-old-size-words* (floor (* 4 1024 1024) 8)) :to-be-truthy)
          (expect (<= cl-cc/runtime::*gc-old-size-words* (floor (* 16 1024 1024) 8)) :to-be-truthy)
          ;; result plist includes the expected keys
          (expect (getf result :young-size-words) :to-be-truthy)
          (expect (getf result :old-size-words) :to-be-truthy))
      (setf cl-cc/runtime::*gc-young-size-words* saved-young
            cl-cc/runtime::*gc-old-size-words*   saved-old))))

(it-sequential "gc-auto-configure-large-memory-capped-at-maximums"
  (let ((saved-young cl-cc/runtime::*gc-young-size-words*)
        (saved-old   cl-cc/runtime::*gc-old-size-words*))
    (unwind-protect
        (let ((result (cl-cc/runtime::rt-gc-auto-configure-heap
                       :memory-bytes (* 64 1024 1024 1024)))) ; 64 GB
          ;; young cap: 4MB/8 = 524288 words
          (expect (<= cl-cc/runtime::*gc-young-size-words* (floor (* 4 1024 1024) 8)) :to-be-truthy)
          ;; old cap: 16MB/8 = 2097152 words
          (expect (<= cl-cc/runtime::*gc-old-size-words* (floor (* 16 1024 1024) 8)) :to-be-truthy)
          ;; result plist must report the capped values
          (expect (= cl-cc/runtime::*gc-young-size-words* (getf result :young-size-words)) :to-be-truthy)
          (expect (= cl-cc/runtime::*gc-old-size-words* (getf result :old-size-words)) :to-be-truthy))
      (setf cl-cc/runtime::*gc-young-size-words* saved-young
            cl-cc/runtime::*gc-old-size-words*   saved-old))))

(it-sequential "gc-auto-configure-result-plist-keys-present"
  (let ((saved-young cl-cc/runtime::*gc-young-size-words*)
        (saved-old   cl-cc/runtime::*gc-old-size-words*))
    (unwind-protect
        (let ((result (cl-cc/runtime::rt-gc-auto-configure-heap :memory-bytes (* 256 1024 1024))))
          (expect (member :memory-bytes result) :to-be-truthy)
          (expect (member :young-size-words result) :to-be-truthy)
          (expect (member :old-size-words result) :to-be-truthy))
      (setf cl-cc/runtime::*gc-young-size-words* saved-young
            cl-cc/runtime::*gc-old-size-words*   saved-old))))
