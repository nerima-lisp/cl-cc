;;;; tests/gc-fr-tests.lisp - GC Feature Requirement Evidence Tests
;;;;
;;;; Tests for FR implementations in the cl-cc/runtime generational GC:
;;;; - FR-331: Old-Space Free-List Reuse
;;;; - FR-333: Nursery Sizing Heuristics
;;;; - FR-335: Write Barrier Young-to-Young Elision
;;;; - FR-336: GC-NaN-Boxing Integration
;;;; - FR-341: GC Pause Time Goals / SLO

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-small-heap-fr ()
  "Create a minimal heap: 64-word young space (32 per semi), 128-word old space."
  (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 128))

(defun %fr-write-object (heap addr size tag &optional (age 0))
  "Write a header at ADDR and return the address."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-rt-header size tag :gc-bits age))
  addr)

(defun %fr-assert-free-list-blocks (heap expected-blocks)
  (let ((blocks (cl-cc/runtime::rt-heap-free-list-blocks heap)))
    (expect (listp blocks) :to-be-truthy)
    (dolist (block expected-blocks)
      (expect (member block blocks :test #'equal) :to-be-truthy))))

(defun %fr-assert-pointer-tags (expected-tags)
  (dolist (case expected-tags)
    (destructuring-bind (addr tag) case
      (expect (= tag (cl-cc/runtime:pointer-tag
                 (cl-cc/runtime:encode-pointer addr tag))) :to-be-truthy))))

(defun %fr-assert-header-fields (header expected-size expected-tag expected-age)
  (expect (typep header '(unsigned-byte 64)) :to-be-truthy)
  (expect (= expected-size (cl-cc/runtime:rt-header-size header)) :to-be-truthy)
  (expect (= expected-tag (cl-cc/runtime:rt-header-type-tag header)) :to-be-truthy)
  (expect (= expected-age (cl-cc/runtime:rt-header-age header)) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-331: Old-Space Free-List Allocation Reuse
;;; ------------------------------------------------------------

(it-sequential "fr-331-free-list-exists"
  (let ((heap (%make-small-heap-fr)))
    (let ((bins (cl-cc/runtime::rt-heap-free-bins heap)))
      (expect (vectorp bins) :to-be-truthy)
      (expect (= 16 (length bins)) :to-be-truthy))))

(it-sequential "fr-331-free-list-insert-find-and-split"
  (let ((heap (%make-small-heap-fr)))
    (let ((insert-bin (cl-cc/runtime::rt-free-list-insert heap 8 1000)))
      (expect (integerp insert-bin) :to-be-truthy)
      (multiple-value-bind (bin addr) (cl-cc/runtime::rt-free-list-find heap 4)
        (expect (= insert-bin bin) :to-be-truthy)
        (expect (= 1000 addr) :to-be-truthy)
        (%fr-assert-free-list-blocks heap (list (cons 4 1004)))))))

(it-sequential "fr-331-free-list-blocks-enumeration"
  (let ((heap (%make-small-heap-fr)))
    (cl-cc/runtime::rt-free-list-insert heap 8 1000)
    (cl-cc/runtime::rt-free-list-insert heap 16 2000)
    (%fr-assert-free-list-blocks heap (list (cons 8 1000)
                                            (cons 16 2000)))))

(it-sequential "fr-331-free-list-bin-index"
  (let ((bin-3  (cl-cc/runtime::rt-free-list-bin-index 3))
        (bin-4  (cl-cc/runtime::rt-free-list-bin-index 4))
        (bin-8  (cl-cc/runtime::rt-free-list-bin-index 8))
        (bin-32 (cl-cc/runtime::rt-free-list-bin-index 32))
        (bin-64 (cl-cc/runtime::rt-free-list-bin-index 64)))
    (expect (= bin-3 bin-4) :to-be-truthy)
    (expect (>= bin-32 bin-8) :to-be-truthy)
    (expect (>= bin-64 bin-32) :to-be-truthy)))

(it-sequential "fr-156-size-class-segregated-allocator-evidence"
  (let ((heap (%make-small-heap-fr)))
    (cl-cc/runtime::rt-free-list-insert heap 16 100)
    (cl-cc/runtime::rt-free-list-insert heap 64 200)
    (multiple-value-bind (bin addr) (cl-cc/runtime::rt-free-list-find heap 12)
      (expect (= (cl-cc/runtime::rt-free-list-bin-index 16) bin) :to-be-truthy)
      (expect (= 100 addr) :to-be-truthy)
      (%fr-assert-free-list-blocks heap (list (cons 4 112))))))

;;; ------------------------------------------------------------
;;; FR-333: Nursery Sizing Heuristics
;;; ------------------------------------------------------------

(it-sequential "fr-333-high-promotion-grows-default-nursery"
  (let ((cl-cc/runtime:*gc-young-size-words* 65536)
        (cl-cc/runtime::*rt-minor-gc-window-start* nil)
        (cl-cc/runtime::*rt-low-promotion-cycles* 0))
    (cl-cc/runtime::%rt-gc-tune-nursery 0.9d0)
    (expect (= 131072 cl-cc/runtime:*gc-young-size-words*) :to-be-truthy)))

(it-sequential "fr-333-low-promotion-shrinks-default-nursery-after-stable-cycles"
  (let ((cl-cc/runtime:*gc-young-size-words* 65536)
        (cl-cc/runtime::*rt-minor-gc-window-start*
          (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
        (cl-cc/runtime::*rt-low-promotion-cycles* 0))
    (cl-cc/runtime::%rt-gc-tune-nursery 0.01d0)
    (expect (= 65536 cl-cc/runtime:*gc-young-size-words*) :to-be-truthy)
    (cl-cc/runtime::%rt-gc-tune-nursery 0.01d0)
    (expect (= 65536 cl-cc/runtime:*gc-young-size-words*) :to-be-truthy)
    (cl-cc/runtime::%rt-gc-tune-nursery 0.01d0)
    (expect (= 32768 cl-cc/runtime:*gc-young-size-words*) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; FR-335: Write Barrier Young-to-Young Elision
;;; ------------------------------------------------------------

(it-sequential "fr-335-write-barrier-young-to-young-fast-path"
  (let ((heap (%make-small-heap-fr)))
    (let ((obj-addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
          (target   (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
      (%fr-write-object heap obj-addr 3 cl-cc/runtime:+rt-tag-cons+)
      (%fr-write-object heap target 3 cl-cc/runtime:+rt-tag-cons+)
      (setf (cl-cc/runtime::rt-heap-gc-state heap) :major-gc)
      (cl-cc/runtime:rt-gc-write-barrier heap obj-addr 1 target)
      (expect (= target (cl-cc/runtime:rt-heap-ref heap (+ obj-addr 1))) :to-be-truthy)
      (expect (find-if-not #'zerop (cl-cc/runtime:rt-heap-card-table heap)) :to-be-falsy)
      (expect (= 0 (length (cl-cc/runtime::rt-heap-satb-queue heap))) :to-be-truthy))))

(it-sequential "fr-335-barrier-buffer-flush"
  (let ((heap (%make-small-heap-fr)))
    (let ((old-addr (cl-cc/runtime:rt-heap-old-base heap)))
      (push old-addr (cl-cc/runtime:rt-heap-barrier-buffer heap))
      (expect (cl-cc/runtime:rt-card-dirty-p heap old-addr) :to-be-falsy)
      (cl-cc/runtime:rt-gc-flush-barrier-buffer heap)
      (expect (cl-cc/runtime:rt-card-dirty-p heap old-addr) :to-be-truthy)
      (expect (= 0 (length (cl-cc/runtime:rt-heap-barrier-buffer heap))) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; FR-336: GC-NaN-Boxing Integration
;;; ------------------------------------------------------------

(it-sequential "fr-336-val-pointer-p-detection"
  (expect (cl-cc/runtime:val-pointer-p
                 (cl-cc/runtime:encode-fixnum 42)) :to-be-falsy)
  (expect (cl-cc/runtime:val-pointer-p cl-cc/runtime:+val-t+) :to-be-falsy)
  (expect (cl-cc/runtime:val-pointer-p cl-cc/runtime:+val-nil+) :to-be-falsy))

(it-sequential "fr-336-decode-pointer-roundtrip"
  (let* ((addr #xABCD)
         (encoded (cl-cc/runtime:encode-pointer addr cl-cc/runtime:+tag-object+)))
    (expect (cl-cc/runtime:val-pointer-p encoded) :to-be-truthy)
    (expect (= addr (cl-cc/runtime:decode-pointer encoded)) :to-be-truthy)))

(it-sequential "fr-336-pointer-tag-extraction"
  (%fr-assert-pointer-tags
   `((#x100 ,cl-cc/runtime:+tag-object+)
     (#x200 ,cl-cc/runtime:+tag-cons+)
     (#x300 ,cl-cc/runtime:+tag-symbol+))))

(it-sequential "fr-336-val-cons-p-detection"
  (let ((cons-ptr (cl-cc/runtime:encode-pointer #x500 cl-cc/runtime:+tag-cons+))
        (obj-ptr (cl-cc/runtime:encode-pointer #x600 cl-cc/runtime:+tag-object+)))
    (expect (cl-cc/runtime:val-cons-p cons-ptr) :to-be-truthy)
    (expect (cl-cc/runtime:val-cons-p obj-ptr) :to-be-falsy)))

(it-sequential "fr-140-symbol-immediates-roundtrip-common-symbols"
  (expect (= cl-cc/runtime:+val-nil+ (cl-cc/runtime:cl-value->val nil)) :to-be-truthy)
  (expect (= cl-cc/runtime:+val-t+ (cl-cc/runtime:cl-value->val t)) :to-be-truthy)
  (let ((encoded (cl-cc/runtime:cl-value->val :key)))
    (expect (cl-cc/runtime::val-immediate-symbol-p encoded) :to-be-truthy)
    (expect (cl-cc/runtime:val-pointer-p encoded) :to-be-falsy)
    (expect (cl-cc/runtime:val->cl-value encoded) :to-be :key)))

(it-sequential "fr-264-compressed-pointer-roundtrip"
  (let ((cl-cc/runtime:*compressed-pointers-enabled* t)
        (cl-cc/runtime:*heap-base-address* #x10000000))
    (let ((encoded (cl-cc/runtime:encode-pointer #x10001000 cl-cc/runtime:+tag-object+)))
      (expect (cl-cc/runtime:val-compressed-pointer-p encoded) :to-be-truthy)
      (expect (= #x10001000 (cl-cc/runtime:decode-pointer encoded)) :to-be-truthy))))

(it-sequential "fr-265-small-string-optimization-roundtrip"
  (let ((encoded (cl-cc/runtime:cl-value->val "abc")))
    (expect (cl-cc/runtime:val-sso-string-p encoded) :to-be-truthy)
    (expect (cl-cc/runtime:val->cl-value encoded) :to-equal "abc")))

(it-sequential "fr-266-compressed-object-header-is-one-word"
  (let ((header (cl-cc/runtime:make-rt-header 42 cl-cc/runtime:+rt-tag-cons+ :gc-bits 2)))
    (%fr-assert-header-fields header 42 cl-cc/runtime:+rt-tag-cons+ 2)))

(it-sequential "fr-184-weak-reference-and-finalizer-evidence"
  (let* ((heap (%make-small-heap-fr))
         (marked (make-hash-table :test #'eql))
         (referent (cl-cc/runtime:encode-pointer (cl-cc/runtime:rt-heap-old-base heap)
                                                  cl-cc/runtime:+tag-object+))
         (weak (cl-cc/runtime:rt-make-weak-ref referent))
         (object-addr (cl-cc/runtime:rt-heap-old-base heap))
         (finalized nil)
         (cl-cc/runtime::*rt-reference-registry* (list weak))
         (cl-cc/runtime::*rt-finalizer-registry* nil)
         (cl-cc/runtime::*rt-finalization-queue* nil))
    (cl-cc/runtime:rt-heap-set-header
     heap (cl-cc/runtime:rt-heap-old-base heap)
     (cl-cc/runtime:make-rt-header 1 cl-cc/runtime:+rt-tag-cons+ :gc-bits 0))
    (cl-cc/runtime::%rt-gc-process-weak-references heap marked)
    (expect (cl-cc/runtime:rt-ref-get weak) :to-be-null)
    (cl-cc/runtime:register-finalizer object-addr (lambda (obj) (setf finalized obj)))
    (cl-cc/runtime::%rt-gc-process-finalizers heap marked)
    ;; After GC processing, object is queued but not yet executed
    (expect cl-cc/runtime::*rt-finalization-queue* :to-equal (list object-addr))
    ;; Running pending finalizers executes them and clears the queue
    (cl-cc/runtime::rt-run-pending-finalizers)
    (expect (= object-addr finalized) :to-be-truthy)))

(it-sequential "fr-300-runtime-condition-restart-stacks"
  (let ((handled nil))
    (multiple-value-bind (value foundp)
        (cl-cc/runtime:rt-establish-handler
         'error
         (lambda (condition) (setf handled condition) :handled)
         (lambda () (cl-cc/runtime:rt-dispatch-signal
                     (make-condition 'simple-error :format-control "x"))))
      (expect foundp :to-be-truthy)
      (expect value :to-be :handled)
      (expect (typep handled 'simple-error) :to-be-truthy)))
  (multiple-value-bind (value foundp)
      (cl-cc/runtime:rt-establish-restart
       'use-value
       (lambda (x) x)
       (lambda () (cl-cc/runtime:rt-dispatch-restart 'use-value '(42))))
    (expect foundp :to-be-truthy)
    (expect (= 42 value) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; FR-341: GC Pause Time Goals / SLO
;;; ------------------------------------------------------------

(it-sequential "fr-341-pause-max-ms-configured"
  (expect (numberp cl-cc/runtime::*gc-max-pause-ms*) :to-be-truthy)
  (expect (>= cl-cc/runtime::*gc-max-pause-ms* 1) :to-be-truthy))

(it-sequential "fr-341-pause-accounting-increments-on-budget-exceed"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-max-pause-ms* 0))
    (let ((exceeded-before (cl-cc/runtime::rt-heap-pause-exceeded-count heap))
          (budget-before (cl-cc/runtime::rt-heap-incremental-work-budget heap)))
      (cl-cc/runtime::%rt-gc-note-pause
       heap
       (- (get-internal-real-time) internal-time-units-per-second))
      (expect (= (1+ exceeded-before) (cl-cc/runtime::rt-heap-pause-exceeded-count heap)) :to-be-truthy)
      (expect (< (cl-cc/runtime::rt-heap-incremental-work-budget heap)
                      budget-before) :to-be-truthy))))

(it-sequential "fr-341-throughput-target"
  (expect (numberp cl-cc/runtime::*gc-throughput-target*) :to-be-truthy)
  (expect (>= cl-cc/runtime::*gc-throughput-target* 0.0d0) :to-be-truthy)
  (expect (<= cl-cc/runtime::*gc-throughput-target* 1.0d0) :to-be-truthy))

;;; ------------------------------------------------------------
;;; Integrated FR Tests
;;; ------------------------------------------------------------

(it-sequential "fr-integrated-alloc-and-gc"
  (let ((heap (%make-small-heap-fr)))
    (loop repeat 10
          for i from 1
          do (let ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
               (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
               (cl-cc/runtime:rt-heap-set heap (+ addr 1)
                                          (cl-cc/runtime:encode-fixnum i))
               (cl-cc/runtime:rt-heap-set heap (+ addr 2)
                                          (cl-cc/runtime:encode-fixnum (+ i 100)))
               (expect (integerp addr) :to-be-truthy)
                (expect (>= addr 0) :to-be-truthy)))
    (cl-cc/runtime:rt-gc-minor-collect heap)
    (expect (cl-cc/runtime:rt-gc-verify-heap heap) :to-be-truthy)))

(it-sequential "fr-700-heap-profiler-report-has-stable-output-format"
  (let ((cl-cc/runtime:*gc-profile-enabled* t)
        (cl-cc/runtime::*gc-profile-interval* 16)
        (cl-cc/runtime::*gc-profile-bytes-since-sample* 0)
        (cl-cc/runtime::*gc-profile-samples* (make-hash-table :test #'equal))
        (cl-cc/runtime::*gc-profile-current-function* 'test-allocation-site))
    (cl-cc/runtime:rt-gc-profile-sample 48)
    (let ((report (cl-cc/runtime:rt-gc-profile-report)))
      (expect (getf report :enabled-p) :to-be t)
      (expect (= 16 (getf report :interval-bytes)) :to-be-truthy)
      (expect (listp (getf report :hot-spots)) :to-be-truthy)
      (let ((spot (first (getf report :hot-spots))))
        (expect (getf spot :function) :to-be 'test-allocation-site)
        (expect (= 3 (getf spot :count)) :to-be-truthy)))))

;;; ------------------------------------------------------------
;;; FR-730..734: GC lifecycle references, finalizers, pinning
;;; ------------------------------------------------------------

(defun %gc-fr-marked-set (&rest addrs)
  (let ((marked (make-hash-table :test #'eql)))
    (dolist (addr addrs marked)
      (setf (gethash addr marked) t))))

(it-sequential "fr-730-weak-pointer-clears-unmarked-heap-referent"
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (weak (cl-cc/runtime:rt-make-weak-pointer addr))
         (cl-cc/runtime::*rt-reference-registry* (list weak)))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime::%rt-gc-process-weak-references heap (%gc-fr-marked-set))
    (expect (cl-cc/runtime:rt-weak-pointer-value weak) :to-be-falsy)))

(it-sequential "fr-730-weak-pointer-keeps-marked-referent"
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (weak (cl-cc/runtime:rt-make-weak-pointer addr))
         (cl-cc/runtime::*rt-reference-registry* (list weak)))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime::%rt-gc-process-weak-references heap (%gc-fr-marked-set addr))
    (expect (= addr (cl-cc/runtime:rt-weak-pointer-value weak)) :to-be-truthy)))

(it-sequential "fr-731-ephemeron-marks-value-when-key-is-live"
  (let* ((heap (%make-small-heap-fr))
         (key (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (value (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (marked (%gc-fr-marked-set key))
         (cl-cc/runtime:*rt-ephemeron-registry* nil))
    (%fr-write-object heap key 3 cl-cc/runtime:+rt-tag-cons+)
    (%fr-write-object heap value 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:rt-make-ephemeron key value)
    (cl-cc/runtime::%rt-gc-process-ephemerons heap marked)
    (expect (gethash value marked) :to-be-truthy)))

(it-sequential "fr-732-finalizer-is-scheduled-after-gc-and-runs-outside-gc-pause"
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (seen nil)
         (cl-cc/runtime:*rt-finalizer-registry* nil)
         (cl-cc/runtime:*pending-finalizers* nil)
         (cl-cc/runtime:*rt-finalization-queue* nil))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:register-finalizer addr (lambda (obj) (push obj seen)))
    (cl-cc/runtime::%rt-gc-process-finalizers heap (%gc-fr-marked-set))
    (expect cl-cc/runtime:*rt-finalization-queue* :to-equal (list addr))
    (expect (= 1 (cl-cc/runtime:rt-run-pending-finalizers)) :to-be-truthy)
    (expect seen :to-equal (list addr))))

(it-sequential "fr-733-pinning-registers-and-cleans-up-relocation-barriers"
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3)))
    (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:with-pinned-objects ((pinned heap addr))
      (expect (= addr pinned) :to-be-truthy)
      (expect (cl-cc/runtime:rt-object-pinned-p heap addr) :to-be-truthy))
    (expect (cl-cc/runtime:rt-object-pinned-p heap addr) :to-be-falsy)))

(it-sequential "fr-734-weak-hash-table-removes-dead-weak-keys"
  (let* ((heap (%make-small-heap-fr))
         (key (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (value :payload)
         (ht (cl-cc/runtime:rt-make-hash-table :weakness :key))
         (cl-cc/runtime::*rt-weak-hash-table-registry* (list ht)))
    (%fr-write-object heap key 3 cl-cc/runtime:+rt-tag-cons+)
    (cl-cc/runtime:rt-sethash key ht value)
    (expect (= 1 (cl-cc/runtime:rt-hash-count ht)) :to-be-truthy)
    (cl-cc/runtime::%rt-gc-process-weak-hash-tables heap (%gc-fr-marked-set))
    (expect (= 0 (cl-cc/runtime:rt-hash-count ht)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; FR-373: Heap ASLR (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(it-sequential "fr-gc-interface-function-exists rt-heap-randomize-base"
  (destructuring-bind (sym) (list 'cl-cc/runtime:rt-heap-randomize-base)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-gc-interface-function-exists rt-install-stack-guard"
  (destructuring-bind (sym) (list 'cl-cc/runtime:rt-install-stack-guard)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-373-heap-randomize-returns-value"
  (let ((offset (cl-cc/runtime:rt-heap-randomize-base)))
    (expect (integerp offset) :to-be-truthy)
    (expect (>= offset 0) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; FR-376: Guard Pages for Stack Overflow (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(it-sequential "fr-376-stack-guard-requires-native-backend"
  (signals error (cl-cc/runtime:rt-install-stack-guard 0 4096)))

(it-sequential "fr-376-madvise-requires-native-backend"
  (let ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (signals error (cl-cc/runtime::rt-heap-madvise-sequential heap 0 8))
    (signals error (cl-cc/runtime::rt-heap-madvise-willneed heap 0 8))
    (signals error (cl-cc/runtime::rt-heap-madvise-hugepage heap)))
  (signals error (cl-cc/runtime:mmap-advice nil :sequential)))

(it-sequential "fr-623-huge-pages-require-native-backend"
  (signals error (cl-cc/runtime:rt-huge-page-mmap nil 4096
                                     (logior cl-cc/runtime::+rt-prot-read+
                                             cl-cc/runtime::+rt-prot-write+)
                                     cl-cc/runtime::+rt-map-anonymous+
                                     nil 0))
  (expect (cl-cc/runtime:try-enable-huge-pages) :to-be-falsy)
  (expect (cl-cc/runtime:huge-pages-enabled-p) :to-be-falsy))
(it-sequential
  "fr-772-xom-requires-native-support"
  (let ((cl-cc/runtime::*xom-enabled* t))
    (if (cl-cc/runtime::rt-xom-supported-p) (expect
        (cl-cc/runtime::rt-xom-effective-prot)
        :to-equal
        cl-cc/runtime::+rt-prot-exec+)
      (expect
        (cl-cc/runtime::rt-xom-effective-prot)
        :to-equal
        (logior cl-cc/runtime::+rt-prot-read+ cl-cc/runtime::+rt-prot-exec+)))))

;;; ------------------------------------------------------------
;;; FR-377: Immortal / Permanent Objects (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(it-sequential "fr-377-immortal-functions-exist"
  (expect (fboundp 'cl-cc/runtime:rt-make-immortal) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime:rt-immortal-p) :to-be-truthy))

(it-sequential "fr-377-make-immortal-roundtrip"
  (let* ((handle (cl-cc/runtime:rt-make-immortal cl-cc/runtime:+rt-tag-cons+ 1)))
    (expect handle :to-be-truthy)
    (expect (cl-cc/runtime:rt-immortal-p handle) :to-be-truthy)
    (expect t :to-be-truthy)))

(it-sequential "fr-377-immortal-count-increases"
  (let ((before (hash-table-count cl-cc/runtime::*rt-immortal-registry*)))
    (cl-cc/runtime:rt-make-immortal cl-cc/runtime:+rt-tag-cons+ 1)
    (let ((after (hash-table-count cl-cc/runtime::*rt-immortal-registry*)))
      (expect (> after before) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; FR-371: GC Safepoints (⚠️ Pure CL interface)
;;; ------------------------------------------------------------

(it-sequential "fr-371-gc-safepoint-interface-exists"
  (expect (or (multiple-value-bind (symbol status)
                       (find-symbol "RT-GC-REQUEST-STOP" "CL-CC/RUNTIME")
                     (and status (fboundp symbol)))
                   (and (find-symbol "RT-GC-ENTER-SAFE-REGION" "CL-CC/RUNTIME")
                        (multiple-value-bind (symbol status)
                            (find-symbol "RT-GC-ENTER-SAFE-REGION" "CL-CC/RUNTIME")
                          (and status (fboundp symbol))))) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-338: Parallel GC worker interface (⚠️ Pure CL/SB-THREAD interface)
;;; ------------------------------------------------------------

(it-sequential "fr-338-parallel-root-scan-sequential-fallback"
  (let ((heap (%make-small-heap-fr)))
    (let* ((addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
           (root (cons nil addr)))
      (%fr-write-object heap addr 3 cl-cc/runtime:+rt-tag-cons+)
      (cl-cc/runtime:rt-gc-add-root heap root)
      (let ((roots (cl-cc/runtime:rt-gc-parallel-root-scan heap 1)))
        (expect (member addr roots :test #'eql) :to-be-truthy))
      (cl-cc/runtime:rt-gc-remove-root heap root))))

(it-sequential "fr-338-worker-count-detection-is-non-negative"
  (expect (integerp (cl-cc/runtime:rt-gc-detect-worker-count)) :to-be-truthy)
  (expect (>= (cl-cc/runtime:rt-gc-detect-worker-count) 0) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-343..345: TLAB and zero-fill interface (⚠️ Pure CL implementation)
;;; ------------------------------------------------------------

(it-sequential "fr-343-tlab-alloc-bumps-private-buffer"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-tlab-size-words* 8)
        (cl-cc/runtime::*rt-thread-local-heaps* nil))
    (let* ((addr (cl-cc/runtime:rt-gc-tlab-alloc heap :worker-a 3))
           (tlab (cl-cc/runtime::%rt-gc-tlab-for heap :worker-a)))
      (expect (= addr (cl-cc/runtime:rt-tlab-base tlab)) :to-be-truthy)
      (expect (= (+ addr 3) (cl-cc/runtime:rt-tlab-free tlab)) :to-be-truthy))))

(it-sequential "fr-344-tlab-retire-records-waste-and-dummy-header"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-tlab-size-words* 8)
        (cl-cc/runtime::*gc-tlab-retire-fill* t)
        (cl-cc/runtime::*rt-thread-local-heaps* nil))
    (let* ((addr (cl-cc/runtime:rt-gc-tlab-alloc heap :worker-b 3))
           (tlab (cl-cc/runtime::%rt-gc-tlab-for heap :worker-b))
           (free-before (cl-cc/runtime:rt-tlab-free tlab)))
      (declare (ignore addr))
      (cl-cc/runtime:rt-gc-tlab-retire-all heap)
      (expect (cl-cc/runtime:rt-tlab-retired-p tlab) :to-be-truthy)
      (expect (> (cl-cc/runtime:rt-tlab-waste-bytes tlab) 0) :to-be-truthy)
      (expect (= 5 (cl-cc/runtime:rt-header-size
                   (cl-cc/runtime:rt-heap-object-header heap free-before))) :to-be-truthy))))

(it-sequential "fr-345-tlab-allocation-returns-zero-filled-words"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime::*gc-tlab-size-words* 8)
        (cl-cc/runtime::*rt-thread-local-heaps* nil))
    (let ((addr (cl-cc/runtime:rt-gc-tlab-alloc heap :worker-c 3)))
      (expect (= 0 (cl-cc/runtime:rt-heap-ref heap addr)) :to-be-truthy)
      (expect (= 0 (cl-cc/runtime:rt-heap-ref heap (+ addr 1))) :to-be-truthy)
      (expect (= 0 (cl-cc/runtime:rt-heap-ref heap (+ addr 2))) :to-be-truthy))))

(it-sequential "fr-345-simd-zero-fill-returns-addr-and-is-fboundp"
  (let ((heap (%make-small-heap-fr)))
    (expect (fboundp 'cl-cc/runtime:rt-gc-simd-zero-fill) :to-be-truthy)
    (let ((result (cl-cc/runtime:rt-gc-simd-zero-fill heap 64 4)))
      (expect (= 64 result) :to-be-truthy))
    ;; Validate argument type checks reject negative inputs
    (signals error (cl-cc/runtime:rt-gc-simd-zero-fill heap -1 4))
    (signals error (cl-cc/runtime:rt-gc-simd-zero-fill heap 0 -1))))

;;; ------------------------------------------------------------
;;; FR-347: Compressed object references (⚠️ Pure CL offset codec)
;;; ------------------------------------------------------------

(it-sequential "fr-347-compressed-reference-roundtrip"
  (let* ((heap (%make-small-heap-fr))
         (addr (cl-cc/runtime:rt-gc-alloc heap cl-cc/runtime:+rt-tag-cons+ 3))
         (offset (cl-cc/runtime:rt-compress-object-ref heap addr)))
    (expect (<= 0 offset #xffffffff) :to-be-truthy)
    (expect (= addr (cl-cc/runtime:rt-decompress-object-ref heap offset)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; FR-363..365: NUMA allocation, local GC schedule, and interleaving metadata
;;; ------------------------------------------------------------

(it-sequential "fr-363-numa-local-alloc-records-node-metadata"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime:*rt-numa-enabled* t))
    (let ((addr (cl-cc/runtime:rt-numa-local-alloc heap :thread-1 3)))
      (expect (integerp addr) :to-be-truthy)
      (expect (= 0 (gethash addr (cl-cc/runtime::rt-heap-numa-node-map heap))) :to-be-truthy))))

(it-sequential "fr-364-numa-gc-affinity-records-worker-schedule"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime:*gc-worker-count* 2))
    (let ((schedule (cl-cc/runtime:rt-gc-numa-affinity heap 0)))
      (expect (= 2 (length schedule)) :to-be-truthy)
      (expect (cl-cc/runtime::rt-heap-numa-gc-schedule heap) :to-equal schedule))))

(it-sequential "fr-365-heap-interleave-records-shared-region"
  (let ((heap (%make-small-heap-fr)))
    (let ((region (cl-cc/runtime:rt-heap-interleave heap 4 8)))
      (expect (getf region :policy) :to-equal :interleave)
      (expect (member region (cl-cc/runtime::rt-heap-interleaved-regions heap)
                           :test #'equal) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; FR-367 / FR-371 / FR-375 / FR-391 / FR-392 additional warning-FR evidence
;;; ------------------------------------------------------------

(it-sequential "fr-367-gc-probes-log-when-enabled"
  (let ((cl-cc/runtime:*gc-probes-enabled* t))
    (let ((output (with-output-to-string (*trace-output*)
                    (cl-cc/runtime:rt-gc-probe-alloc 7)
                    (cl-cc/runtime:rt-gc-probe-gc-start :minor)
                    (cl-cc/runtime:rt-gc-probe-gc-end :minor))))
      (expect (search "GC-PROBE-ALLOC 7" output) :to-be-truthy)
      (expect (search "GC-PROBE-GC-START :MINOR" output) :to-be-truthy)
      (expect (search "GC-PROBE-GC-END :MINOR" output) :to-be-truthy))))

(it-sequential "fr-371-safe-region-depth-roundtrip"
  (let ((thread-id :fr-371-thread))
    (expect (= 1 (cl-cc/runtime:rt-gc-enter-safe-region thread-id)) :to-be-truthy)
    (expect (= 0 (cl-cc/runtime:rt-gc-leave-safe-region thread-id)) :to-be-truthy)))

(it-sequential "fr-375-asan-shadow-memory-poisoning"
  (let ((heap (%make-small-heap-fr))
        (cl-cc/runtime:*rt-asan-enabled* t))
    (cl-cc/runtime:rt-sanitizer-reset-state)
    (cl-cc/runtime:rt-sanitizer-poison-address 0)
    (signals error (cl-cc/runtime:rt-heap-ref heap 0))
    (cl-cc/runtime:rt-sanitizer-unpoison-address 0)
    (expect (= 0 (cl-cc/runtime:rt-heap-ref heap 0)) :to-be-truthy)))

(it-sequential "fr-391-heap-growth-policy-expands-old-space"
  (let ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (setf (cl-cc/runtime:rt-heap-young-free heap)
          (+ (cl-cc/runtime::rt-heap-young-from-base heap)
             (cl-cc/runtime::rt-heap-young-semi-size heap))
          (cl-cc/runtime::rt-heap-old-free heap)
          (+ (cl-cc/runtime:rt-heap-old-base heap)
             (cl-cc/runtime::rt-heap-old-size heap)))
    (let ((old-size-before (cl-cc/runtime::rt-heap-old-size heap)))
      (expect (cl-cc/runtime:rt-heap-maybe-grow heap) :to-be-truthy)
      (expect (> (cl-cc/runtime::rt-heap-old-size heap) old-size-before) :to-be-truthy))))

(it-sequential "fr-392-heap-shrink-policy-reduces-grown-heap"
  (let ((heap (cl-cc/runtime:make-rt-heap :young-size 64 :old-size 64)))
    (setf (cl-cc/runtime:rt-heap-young-free heap)
          (+ (cl-cc/runtime::rt-heap-young-from-base heap)
             (cl-cc/runtime::rt-heap-young-semi-size heap))
          (cl-cc/runtime::rt-heap-old-free heap)
          (+ (cl-cc/runtime:rt-heap-old-base heap)
             (cl-cc/runtime::rt-heap-old-size heap)))
    (cl-cc/runtime:rt-heap-maybe-grow heap)
    (setf (cl-cc/runtime:rt-heap-young-free heap)
          (cl-cc/runtime::rt-heap-young-from-base heap)
          (cl-cc/runtime::rt-heap-old-free heap)
          (cl-cc/runtime:rt-heap-old-base heap))
    (let ((words-before (length (cl-cc/runtime::rt-heap-words heap))))
      (cl-cc/runtime:rt-heap-maybe-shrink heap)
      (cl-cc/runtime:rt-heap-maybe-shrink heap)
      (expect (cl-cc/runtime:rt-heap-maybe-shrink heap) :to-be-truthy)
      (expect (< (length (cl-cc/runtime::rt-heap-words heap)) words-before) :to-be-truthy))))
