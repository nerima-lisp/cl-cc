(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; make-rt-heap Factory
;;; ------------------------------------------------------------

(defun make-rt-heap (&key (young-size *gc-young-size-words* young-size-p)
                           (old-size   *gc-old-size-words* old-size-p))
  "Allocate and initialize a fresh managed heap.

   YOUNG-SIZE is the total young generation size in words; it is split
   evenly into two semi-spaces of YOUNG-SIZE/2 words each.
   OLD-SIZE is the old generation size in words.

   Layout of the flat word vector:
     [0 .. semi-size-1]                from-space
     [semi-size .. 2*semi-size-1]      to-space
     [2*semi-size .. 2*semi-size+old-1] old space"
  (unless (or young-size-p old-size-p)
    (rt-gc-auto-configure-heap)
    (setf young-size *gc-young-size-words*
          old-size *gc-old-size-words*))
  (let* (;; --- Layout arithmetic ---
         (semi-size           (floor young-size 2))
         (large-obj-size      old-size)
         (managed-words       (+ (* 2 semi-size) old-size large-obj-size))
         (heap-base           (if *rt-heap-randomize*
                                  (rt-heap-randomize-base managed-words)
                                  (rt-heap-randomize-base)))
         (young-from-base     heap-base)
         (young-to-base       (+ young-from-base semi-size))
         (old-base            (+ young-to-base semi-size))
         (large-obj-base      (+ old-base old-size))
         (total-size          (+ heap-base managed-words))
         ;; --- Storage allocation ---
         (words               (make-array total-size :initial-element 0))
         (num-cards           (ceiling old-size +gc-card-size-words+))
         (card-table          (make-array num-cards
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0))
         (card-summary        (make-array (ceiling num-cards +gc-card-summary-block-size+)
                                          :initial-element nil))
         (age-hist            (make-array 16 :initial-element 0))
         (free-bins           (make-array +rt-free-list-bin-count+ :initial-element nil))
         (slab-pools          (make-hash-table :test #'eql))
         ;; --- Ancillary hash tables ---
         (access-bits         (make-hash-table :test #'eql))
         (recent-promotions   (make-hash-table :test #'eql))
         (forwarding-table    (make-hash-table :test #'eql))
         (numa-node-map       (make-hash-table :test #'eql))
         (co-location-hints   (make-hash-table :test #'eql))
         ;; --- Growth / cap policy ---
         ;; FR-391: 0 means unlimited dynamic growth; positive *GC-MAX-HEAP-WORDS*
         ;; caps RT-HEAP-MAYBE-GROW.
         (max-heap-words      (if (plusp *gc-max-heap-words*) *gc-max-heap-words* 0))
         ;; --- Wall-clock timestamp ---
         (now                 (get-internal-real-time)))
    (when (and *compressed-pointers-enabled*
               (> managed-words +compressed-heap-region-words+))
      (error "cl-cc/runtime: compressed pointers require heap <= 4GB (~D words requested, max ~D)"
             managed-words +compressed-heap-region-words+))
    (when *compressed-pointers-enabled*
      (setf *heap-base-address* young-from-base))
    (%make-rt-heap
     :words                          words
     :young-from-base                young-from-base
     :young-to-base                  young-to-base
     :young-semi-size                semi-size
     :young-free                     young-from-base
     :old-base                       old-base
     :old-size                       old-size
     :old-free                       old-base
     :minor-gc-count                 0
     :major-gc-count                 0
     :words-collected                0
     :words-promoted                 0
     :card-table                     card-table
     :card-summary                   card-summary
     :num-cards                      num-cards
     :roots                          nil
     :satb-queue                     nil
     :barrier-buffer                 nil
     :free-list                      nil
     :free-bins                      free-bins
     :slab-pools                     slab-pools
     :lazy-sweep-cursor              old-base
     :lazy-sweep-limit               old-base
     :incremental-work-budget        64
     :pause-exceeded-count           0
     :access-bits                    access-bits
     :recent-promotions              recent-promotions
     :gc-state                       :normal
     :total-alloc-words              0
     :age-hist                       age-hist
     :large-obj-threshold            8192
     :large-obj-base                 large-obj-base
     :large-obj-size                 large-obj-size
     :large-obj-free                 large-obj-base
     :gc-pause-total                 0.0d0
     :gc-pause-max                   0.0d0
     :gc-wall-start-tick             now
     :allocation-rate-words-per-sec  0.0d0
     :allocation-rate-last-tick      now
     :allocation-rate-last-total     0
     :pressure-hooks                 nil
     :pressure-threshold-high        80.0d0
     :pressure-threshold-low         20.0d0
     :max-heap-words                 max-heap-words
     :initial-heap-words             total-size
     :initial-old-size               old-size
     :initial-large-obj-size         large-obj-size
     :shrink-threshold               0.25d0
     :shrink-counter                 0
     :compaction-trigger-fraction    0.5d0
     :forwarding-table               forwarding-table
     :numa-node-map                  numa-node-map
     :numa-gc-schedule               nil
     :interleaved-regions            nil
     :co-location-hints              co-location-hints
     :gc-inhibit                     nil
     :gc-pending                     nil)))

;;; ------------------------------------------------------------
;;; Heap Word Access
;;; ------------------------------------------------------------

(defun rt-gc-forward-object (heap old-addr new-addr)
  "Record an OLD-ADDR -> NEW-ADDR forwarding entry for concurrent relocation.

This is the public FR-342 forwarding table hook used by future compactors.  The
Pure CL implementation does not move payload words concurrently yet; it records
the relocation mapping and relies on RT-HEAP-REF's relocation barrier to heal
stale slots lazily as mutators read them."
  (check-type heap rt-heap)
  (check-type old-addr integer)
  (check-type new-addr integer)
  (setf (gethash old-addr (rt-heap-forwarding-table heap)) new-addr)
  new-addr)

(defun rt-gc-clear-forwarding-table (heap)
  "Clear all transient concurrent-relocation forwarding entries for HEAP."
  (check-type heap rt-heap)
  (clrhash (rt-heap-forwarding-table heap))
  heap)

;;; ------------------------------------------------------------
;;; FR-347: Compressed Object References (portable offset codec)
;;; ------------------------------------------------------------

(defun rt-compress-object-ref (heap addr)
  "Return ADDR as a 32-bit heap-relative offset for FR-347.

The Pure CL heap stores ordinary word addresses, but this helper exposes the
same contract a native backend would use for compressed object references:
OFFSET = ADDR - HEAP-BASE.  The offset is validated to fit in 32 bits so callers
can test density-sensitive paths without native pointer compression."
  (check-type heap rt-heap)
  (check-type addr integer)
  (let* ((base (rt-heap-young-from-base heap))
          (offset (- addr base)))
    (unless (<= 0 offset #xffffffff)
      (error "cl-cc/runtime: address ~D cannot be compressed relative to heap base ~D"
             addr base))
    offset))

(defun rt-decompress-object-ref (heap offset)
  "Expand a 32-bit heap-relative OFFSET back into an absolute heap word address."
  (check-type heap rt-heap)
  (check-type offset (integer 0 #xffffffff))
  (+ (rt-heap-young-from-base heap) offset))

(defun %rt-ensure-compressed-pointer-range (addr size-words)
  "Signal if [ADDR, ADDR+SIZE-WORDS) cannot be represented as a compressed pointer."
  (when *compressed-pointers-enabled*
    (let ((end-offset (- (+ addr size-words) *heap-base-address*)))
      (unless (<= 0 end-offset +compressed-heap-region-words+)
        (error "cl-cc/runtime: allocation [~D, ~D) escapes compressed 4GB heap region based at ~D"
               addr (+ addr size-words) *heap-base-address*)))))

;;; ------------------------------------------------------------
;;; FR-288: Large Page Support (portable implementation)
;;;   On Linux with MAP_HUGETLB, native runtime can enable huge pages.
;;;   Portable CL implementation: tracks *rt-heap-hugepage-enabled* toggle
;;;   and provides os-advise-hugepage for transparent hugepage hinting.
;;;   Native backends should wire mmap(MAP_HUGETLB) or MADV_HUGEPAGE.
;;;
;;; FR-363: NUMA-Aware Heap Allocation (portable implementation)
;;;   Interface: *rt-numa-enabled*, rt-numa-local-alloc.
;;;   Portable implementation records per-thread NUMA node mappings
;;;   and delegates to GC bump allocator. Linux libnuma integration
;;;   available via the topology module (detect-numa-topology).
;;;
;;; FR-364: NUMA-Local GC (portable implementation)
;;;   Interface: rt-gc-numa-affinity.
;;;   Portable implementation generates deterministic round-robin
;;;   worker schedules. Native runtimes translate to sched_setaffinity.
;;;
;;; FR-365: Memory Interleaving (portable implementation)
;;;   Interface: rt-heap-interleave.
;;;   Portable implementation records interleave policy metadata.
;;;   Linux backends use mbind(MPOL_INTERLEAVE) when available.
;;; ------------------------------------------------------------

(defun rt-numa-node-of-thread (thread-id)
  "Return the NUMA node for THREAD-ID.

Pure CL has no portable NUMA discovery, so every thread maps to node 0. Native
Linux runtimes should query sched_getcpu/get_mempolicy or libnuma; Windows
runtimes can use GetNumaProcessorNodeEx."
  (declare (ignore thread-id))
  0)

(defun rt-numa-local-alloc (heap thread-id size-words)
  "Allocate SIZE-WORDS words from THREAD-ID's local NUMA node.

This portable stub records node metadata and delegates to the existing GC bump
allocator. Linux integrations should use numa_alloc_local/mbind for backing VM
pages; Windows integrations should use VirtualAllocExNuma."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let* ((node (if *rt-numa-enabled* (rt-numa-node-of-thread thread-id) 0))
         (addr (rt-gc-alloc heap 0 size-words)))
    (setf (gethash addr (rt-heap-numa-node-map heap)) node)
    addr))

(defun rt-gc-numa-affinity (heap node)
  "Return a portable NUMA-local GC worker schedule for NODE.

GC workers should be scheduled on the same NUMA node as the objects they scan.
Pure CL cannot set affinity, so this records a deterministic round-robin worker
assignment that native runtimes can translate to OS scheduler calls."
  (check-type heap rt-heap)
  (check-type node integer)
  (let* ((workers (max 1 *gc-worker-count*))
         (schedule (loop for worker from 0 below workers
                         collect (list :worker worker :node (mod (+ node worker) workers)))))
    (setf (rt-heap-numa-gc-schedule heap) schedule)
    schedule))

(defun rt-heap-interleave (heap addr size)
  "Mark [ADDR, ADDR+SIZE) as interleaved shared data.

FR-365: Pure CL implementation records interleaved region metadata in
RT-HEAP-INTERLEAVED-REGIONS.  The NUMA-aware GC worker scheduler
(rt-gc-numa-affinity) uses this metadata for deterministic work distribution.

Deferred to Tier 6 (memory-gc.md): native Linux mbind(MPOL_INTERLEAVE) call
for global shared data pages requires OS-level mmap/mbind integration."
  (check-type heap rt-heap)
  (check-type addr integer)
  (check-type size (integer 0 *))
  (let ((region (list :start addr :end (+ addr size) :policy :interleave :portable t)))
    (push region (rt-heap-interleaved-regions heap))
    region))

(defun rt-heap-ref (heap index)
  "Read word at absolute INDEX from HEAP.

FR-342 relocation barrier: during concurrent relocation, pointer-like values are
checked against RT-HEAP-FORWARDING-TABLE. A forwarded slot is self-healed in
place and the updated value is returned, amortizing stale-pointer repair."
  (%rt-ubsan-check-access heap index :read)
  (when *rt-asan-enabled*
    (%rt-asan-check-address heap index :read))
  (%rt-hwasan-check-address index 0)
  (%rt-tsan-check-access index :read)
  (%rt-msan-check-read index)
  (when (and (rt-heap-access-bits heap)
             (integerp index)
             (or (and (>= index (rt-heap-young-from-base heap))
                      (< index (+ (rt-heap-young-from-base heap)
                                  (rt-heap-young-semi-size heap))))
                 (and (>= index (rt-heap-old-base heap))
                      (< index (+ (rt-heap-old-base heap)
                                  (rt-heap-old-size heap))))
                 (and (>= index (rt-heap-large-obj-base heap))
                      (< index (+ (rt-heap-large-obj-base heap)
                                  (rt-heap-large-obj-size heap))))))
    (setf (gethash index (rt-heap-access-bits heap))
          (rt-heap-minor-gc-count heap)))
  (let* ((value (svref (rt-heap-words heap) index))
         (table (rt-heap-forwarding-table heap))
         (old-addr (cond
                     ((and (integerp value) (val-pointer-p value))
                      (decode-pointer value))
                     ((integerp value) value)
                     (t nil)))
         (new-addr (and table old-addr (gethash old-addr table))))
    (if new-addr
        (let ((healed (if (and (integerp value) (val-pointer-p value))
                          (encode-pointer new-addr (pointer-tag value))
                          new-addr)))
          (setf (svref (rt-heap-words heap) index) healed)
          healed)
        value)))

(defun rt-heap-set (heap index value)
  "Write VALUE at absolute INDEX in HEAP."
  (%rt-ubsan-check-access heap index :write)
  (when *rt-asan-enabled*
    (%rt-asan-check-address heap index :write))
  (%rt-hwasan-check-address index 0)
  (%rt-tsan-check-access index :write)
  (%rt-with-sanitizer-map-lock ()
    (setf (gethash index *rt-heap-init-map*) t))
  (setf (svref (rt-heap-words heap) index) value))

(defun rt-heap-object-header (heap addr)
  "Read the header word of the object at absolute word address ADDR."
  (rt-heap-ref heap addr))

(defun rt-heap-set-header (heap addr new-header)
  "Write NEW-HEADER as the header of the object at absolute word address ADDR."
  (rt-heap-set heap addr new-header))

(defun rt-heap-object-size (heap addr)
  "Return the size in words of the object at absolute word address ADDR."
  (rt-header-size (rt-heap-object-header heap addr)))

(defun rt-heap-occupancy-pct (heap)
  "Return total young+old heap occupancy as a percentage.

The numerator is the active young from-space cursor plus the old-generation
allocation cursor.  The denominator is one young semi-space plus the old-space
capacity, matching the capacities reported by RT-GC-STATS."
  (let* ((young-used (- (rt-heap-young-free heap)
                        (rt-heap-young-from-base heap)))
         (old-used (- (rt-heap-old-free heap)
                      (rt-heap-old-base heap)))
         (used (+ young-used old-used))
         (total (+ (rt-heap-young-semi-size heap)
                   (rt-heap-old-size heap))))
    (if (plusp total)
        (* 100.0d0 (/ used total))
        0.0d0)))

(defun rt-heap-register-pressure-hook (heap hook)
  "Register HOOK to be called with (HEAP LEVEL OCCUPANCY-PCT) after GC pressure checks."
  (check-type hook function)
  (pushnew hook (rt-heap-pressure-hooks heap) :test #'eq)
  hook)

(defun rt-heap-fragmentation-pct (heap)
  "Return old-space fragmentation as free-list words divided by old-space size."
  (let ((free-words (loop for (size . nil) in (rt-heap-free-list-blocks heap) sum size))
        (old-size (rt-heap-old-size heap)))
    (if (plusp old-size)
        (/ (float free-words 1.0d0) old-size)
        0.0d0)))

(defun rt-heap-should-compact-p (heap)
  "Return true when fragmentation exceeds the configured compaction trigger."
  (> (rt-heap-fragmentation-pct heap)
     (rt-heap-compaction-trigger-fraction heap)))

(defun %rt-heap-live-used-words (heap)
  "Return the current used words in young, old, and large-object spaces."
  (+ (- (rt-heap-young-free heap) (rt-heap-young-from-base heap))
     (- (rt-heap-old-free heap) (rt-heap-old-base heap))
     (- (rt-heap-large-obj-free heap) (rt-heap-large-obj-base heap))))
