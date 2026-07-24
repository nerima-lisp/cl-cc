(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; rt-heap Structure
;;; ------------------------------------------------------------

(defstruct (rt-heap (:constructor %make-rt-heap)
                    (:conc-name rt-heap-))
  (words           nil  :type simple-vector)
  ;;; FR-087: rt-heap Field Reordering — young-free co-located with young-from-base/young-to-base/young-semi-size in same cache line
  (young-from-base 0    :type fixnum)
  (young-to-base   0    :type fixnum)
  (young-semi-size 0    :type fixnum)
  (young-free      0    :type fixnum)
  (old-base        0    :type fixnum)
  (old-size        0    :type fixnum)
  (old-free        0    :type fixnum)
  (minor-gc-count  0    :type fixnum)
  (major-gc-count  0    :type fixnum)
  (words-collected 0    :type fixnum)
  (words-promoted  0    :type fixnum)
  (card-table      nil  :type (simple-array (unsigned-byte 8) (*)))
  (card-summary    nil  :type simple-vector)
  (num-cards       0    :type fixnum)
  (roots           nil  :type list)
  (satb-queue      nil  :type list)
  (barrier-buffer  nil  :type list)
  (free-list       nil  :type list)
  (free-bins       nil  :type simple-vector)
  (slab-pools      nil  :type hash-table)
  (lazy-sweep-cursor 0  :type fixnum)
  (lazy-sweep-limit 0   :type fixnum)
  (incremental-work-budget 64 :type fixnum)
  (pause-exceeded-count 0 :type fixnum)
  (access-bits     nil  :type hash-table)
  (recent-promotions nil :type hash-table)
  (gc-state        :normal :type keyword)
  (total-alloc-words 0  :type fixnum)
  (age-hist        nil  :type simple-vector)
  ;;; FR-086: Large Object Space (LOS) — objects exceeding threshold bypass nursery; allocated directly in large-object space
  (large-obj-threshold 8192 :type fixnum)
  (large-obj-base  0    :type fixnum)
  (large-obj-size  0    :type fixnum)
  (large-obj-free  0    :type fixnum)
  (gc-pause-total  0.0d0 :type double-float)
  (gc-pause-max    0.0d0 :type double-float)
  (gc-wall-start-tick 0 :type integer)
  (allocation-rate-words-per-sec 0.0d0 :type double-float)
  (allocation-rate-last-tick 0 :type integer)
  (allocation-rate-last-total 0 :type integer)
  (pressure-hooks  nil  :type list)
  (pressure-threshold-high 80.0d0 :type double-float)
  (pressure-threshold-low 20.0d0 :type double-float)
  (max-heap-words  0    :type fixnum)
  (initial-heap-words 0 :type fixnum)
  (initial-old-size 0 :type fixnum)
  (initial-large-obj-size 0 :type fixnum)
  (shrink-threshold 0.25d0 :type double-float)
  (shrink-counter 0 :type fixnum)
  (compaction-trigger-fraction 0.5d0 :type double-float)
  (forwarding-table nil :type hash-table)
  (numa-node-map nil :type hash-table)
  (numa-gc-schedule nil :type list)
  (interleaved-regions nil :type list)
  (co-location-hints nil :type hash-table)
  (gc-inhibit      nil  :type boolean)
  (gc-pending      nil  :type boolean))

(defun rt-heap-detect-container-memory-limit ()
  "Return the cgroup memory limit in bytes if running in a container,
    or NIL if not containerized or limit cannot be determined.
   
    Checks cgroup v2 first (/sys/fs/cgroup/memory.max), then v1
    (/sys/fs/cgroup/memory/memory.limit_in_bytes).
    Returns NIL on failure (non-container environment or permission error)."
  ;; FR-423: Container-Aware Heap Sizing — reads cgroup v1/v2 memory limits for Docker/Kubernetes compatibility
  (or (ignore-errors
        (with-open-file (f "/sys/fs/cgroup/memory.max"
                           :direction :input
                           :if-does-not-exist nil)
          (when f
            (let ((line (read-line f nil nil)))
              (when line
                (let ((limit (parse-integer line :junk-allowed t)))
                  (unless (string= line "max")
                    limit)))))))
      (ignore-errors
        (with-open-file (f "/sys/fs/cgroup/memory/memory.limit_in_bytes"
                           :direction :input
                           :if-does-not-exist nil)
          (when f
            (let ((line (read-line f nil nil)))
              (when line
                (let ((limit (parse-integer line :junk-allowed t)))
                  ;; A very large value means "no limit"
                  (unless (> limit (expt 2 50))
                    limit)))))))))

(defun %rt-system-memory-bytes-fallback ()
  "Return the compile-time default system memory constant.
This is a portable fallback stub; it does not query /proc/meminfo or sysctl.
The name reflects that it is a fallback, not a real detection routine."
  +rt-default-system-memory-bytes+)

(defun %rt-clamp (value min-value max-value)
  "Return VALUE clamped to [MIN-VALUE, MAX-VALUE]."
  (min max-value (max min-value value)))

(defun rt-gc-auto-configure-heap (&key memory-bytes)
  "Auto-configure *GC-YOUNG-SIZE-WORDS* and *GC-OLD-SIZE-WORDS*.

When a container memory limit is detected, the ergonomic budget is capped at
75% of that limit so the runtime leaves headroom for code, stacks, native
allocations, and host implementation overhead.  Explicit MEMORY-BYTES remains
an upper input but is also bounded by the container cap when present."
  (let* ((container-limit (rt-heap-detect-container-memory-limit))
         (container-budget (and container-limit (floor (* container-limit 3) 4)))
         (detected-bytes (or memory-bytes (%rt-system-memory-bytes-fallback)))
         (available-bytes (if container-budget
                              (min detected-bytes container-budget)
                              detected-bytes))
         (young-bytes (%rt-clamp (floor available-bytes 128)
                                  (* 1 1024 1024)
                                  (* 4 1024 1024)))
         (old-bytes (%rt-clamp (floor available-bytes 32)
                               (* 4 1024 1024)
                               (* 16 1024 1024)))
         (young-words (max 2 (floor young-bytes 8)))
         (old-words (max 1 (floor old-bytes 8))))
    (when (oddp young-words)
      (incf young-words))
    (setf *gc-young-size-words* young-words
          *gc-old-size-words* old-words)
    (list :memory-bytes available-bytes
           :container-limit-bytes container-limit
           :container-heap-cap-bytes container-budget
            :young-size-words young-words
            :old-size-words old-words)))

(defparameter *rt-heap-randomize* nil
  "When T, apply logical address-space randomization to heap bases (FR-373).
   Default NIL for deterministic test behaviour; set to T for production.")

;;; FR-373: Address Space Layout Randomization for Heap
(defun rt-heap-randomize-base (&optional (size #x1000000 size-supplied-p))
  "Return a randomized base offset for heap allocation.

Pure CL implements heap ASLR as a logical word offset into the managed
SIMPLE-VECTOR.  Native backends can map the same interface to
mmap(MAP_FIXED_NOREPLACE).  Implicit calls return 0 unless
*RT-HEAP-RANDOMIZE* is true; explicit SIZE calls return a fresh Pure CL random
offset in [0, SIZE)."
  (let ((max-offset (max 1 (min size #x1000000))))
    (if (or size-supplied-p *rt-heap-randomize*)
        (random max-offset)
        0)))

;;; FR-376: Guard Pages for Stack Overflow Detection
(defun rt-install-stack-guard (stack-base stack-size)
  "Install a guard page at the end of the stack."
  (check-type stack-base integer)
  (check-type stack-size integer)
  (error "RT-INSTALL-STACK-GUARD requires a native mprotect/signal backend."))
