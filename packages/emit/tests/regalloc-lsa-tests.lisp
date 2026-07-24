;;;; tests/regalloc-lsa-tests.lisp - Linear Scan Allocator Internals Tests
;;;
;;; Covers the linear-scan allocation path in regalloc-allocate.lisp /
;;; regalloc-policy.lisp: %lsa-assign, %lsa-try-coalesce, %lsa-allocate-from-pool,
;;; %lsa-evict-and-assign, %lsa-expire-old, %lsa-best-spill-candidate,
;;; %interval-next-use-after, the preferred-register strategy helpers,
;;; regalloc-target-fp-registers, %allocation-strategy,
;;; %derive-single-function-policy, and the linear-scan-allocate driver.
;;;
;;; Migrated from the unregistered packages/regalloc/tests/regalloc-lsa-tests.lisp
;;; into the cl-cc/test suite convention. Tests that merely duplicate the
;;; lsa-state coverage already in regalloc-tests.lisp are intentionally omitted.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-lsa-test-interval (vreg start end &key fp-p coalesce-with
                                                   (use-positions nil use-positions-p)
                                                   crosses-call-p)
  "Construct a live-interval for linear-scan allocator tests.
An explicitly supplied USE-POSITIONS (even nil/empty) is honored; only an
absent USE-POSITIONS defaults to the interval's endpoints."
  (cl-cc/regalloc::make-live-interval
   :vreg vreg
   :start start
   :end end
   :fp-p fp-p
   :coalesce-with coalesce-with
   :use-positions (if use-positions-p use-positions (list start end))
   :crosses-call-p crosses-call-p))

(defun make-test-lsa-state (&key (free-regs '(:r0 :r1 :r2))
                                 (free-fp-regs '(:xmm0 :xmm1))
                                 active)
  "Construct a minimal lsa-state for testing."
  (cl-cc/regalloc::make-lsa-state :free-regs free-regs
                                  :free-fp-regs free-fp-regs
                                  :active (or active nil)))

(defun make-lsa-minimal-cc (&key (name :x86-64)
                                 (arg-regs '(:rdi :rsi :rdx))
                                 (ret-reg :rax)
                                 (fp-arg-regs '(:xmm0 :xmm1))
                                 (fp-ret-reg :xmm0)
                                 (callee-saved '(:rbx :r12))
                                 (caller-saved '(:rax :rcx :rdx :rsi :rdi)))
  "Build a minimal target-desc fixture for preferred-register strategy tests."
  (cl-cc/target::make-target-desc
   :name         name
   :gpr-names    (coerce (append arg-regs callee-saved caller-saved) 'vector)
   :arg-regs     arg-regs
   :ret-reg      ret-reg
   :fp-arg-regs  fp-arg-regs
   :fp-ret-reg   fp-ret-reg
   :callee-saved callee-saved
   :scratch-regs nil))

;;; ─── %lsa-assign ──────────────────────────────────────────────────────────

(deftest lsa-assign-records-assignment-and-inserts-into-active
  "%lsa-assign sets the interval's physical register and adds it to the active list."
  (let* ((state (make-test-lsa-state))
         (interval (make-lsa-test-interval :v 0 10)))
    (cl-cc/regalloc::%lsa-assign state interval :r0)
    (assert-eq :r0 (cl-cc/regalloc::interval-phys-reg interval))
    (assert-eq :r0 (gethash :v (cl-cc/regalloc::lsa-assignment state)))
    (assert-true (member interval (cl-cc/regalloc::lsa-active state) :test #'eq))))

(deftest lsa-assign-keeps-active-list-sorted-by-interval-end
  "%lsa-assign maintains the active list in ascending order of interval end positions."
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 20))
         (int-b (make-lsa-test-interval :b 0  5))
         (int-c (make-lsa-test-interval :c 0 12)))
    (cl-cc/regalloc::%lsa-assign state int-a :r0)
    (cl-cc/regalloc::%lsa-assign state int-b :r1)
    (cl-cc/regalloc::%lsa-assign state int-c :r2)
    (let ((ends (mapcar #'interval-end (cl-cc/regalloc::lsa-active state))))
      (assert-equal ends (sort (copy-list ends) #'<)))))

;;; ─── %lsa-try-coalesce ────────────────────────────────────────────────────

(deftest lsa-try-coalesce-succeeds-when-source-ends-at-current-start
  "%lsa-try-coalesce assigns the source's register to the new interval when the source ends exactly at the new interval's start."
  ;; Source interval ends at 5; new interval starts at 5 → coalesce allowed.
  (let* ((state (make-test-lsa-state :free-regs '(:r0 :r1)))
         (src-int (make-lsa-test-interval :src 0 5))
         (new-int (make-lsa-test-interval :new 5 10 :coalesce-with :src)))
    (setf (cl-cc/regalloc::interval-phys-reg src-int) :r0)
    (setf (gethash :src (cl-cc/regalloc::lsa-interval-map state)) src-int)
    (setf (cl-cc/regalloc::lsa-active state) (list src-int))
    (let ((result (cl-cc/regalloc::%lsa-try-coalesce state new-int)))
      (assert-true result)
      (assert-eq :r0 (cl-cc/regalloc::interval-phys-reg new-int))
      ;; Source should have been removed from active and replaced by new-int.
      (assert-false (member src-int (cl-cc/regalloc::lsa-active state) :test #'eq))
      (assert-true (member new-int (cl-cc/regalloc::lsa-active state) :test #'eq)))))

(deftest-each lsa-try-coalesce-fails
  "%lsa-try-coalesce returns nil for a still-live source, a missing coalesce hint, or an FP class mismatch."
  :cases (("source-still-live" 10 5 nil :src t)
          ("no-coalesce-hint"   5 5 nil nil nil)
          ("fp-class-mismatch"  5 5 t   :src t))
  (src-end new-start new-fp-p coalesce-with setup-src-p)
  (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (cl-cc/regalloc::interval-phys-reg src-int) :r0)
      (setf (gethash :src (cl-cc/regalloc::lsa-interval-map state)) src-int)
      (setf (cl-cc/regalloc::lsa-active state) (list src-int)))
    (assert-null (cl-cc/regalloc::%lsa-try-coalesce state new-int))))

;;; ─── %lsa-allocate-from-pool ──────────────────────────────────────────────

(deftest lsa-allocate-from-pool-assigns-and-removes-from-pool
  "%lsa-allocate-from-pool assigns a physical register to the interval and removes it from the free register pool."
  (let* ((state    (make-test-lsa-state :free-regs '(:r0 :r1 :r2)))
         (interval (make-lsa-test-interval :v 0 10))
         (cc       (cl-cc/target::make-target-desc
                    :name        :x86-64
                    :gpr-names   #(:rdi :rsi :rdx :rcx)
                    :arg-regs    '(:rdi :rsi)
                    :ret-reg     :rax
                    :fp-arg-regs '(:xmm0)
                    :fp-ret-reg  :xmm0
                    :callee-saved '()
                    :scratch-regs nil)))
    (cl-cc/regalloc::%lsa-allocate-from-pool state interval cc '(:r0 :r1 :r2))
    (assert-false (null (cl-cc/regalloc::interval-phys-reg interval)))
    (assert-true (member (cl-cc/regalloc::interval-phys-reg interval) '(:r0 :r1 :r2) :test #'eq))
    (assert-false (member (cl-cc/regalloc::interval-phys-reg interval)
                          (cl-cc/regalloc::lsa-free-regs state) :test #'eq))))

;;; ─── %lsa-evict-and-assign ────────────────────────────────────────────────

(deftest lsa-evict-and-assign-spills-current-when-worst-candidate
  "%lsa-evict-and-assign spills the current interval when it is the worst eviction candidate."
  ;; When *ml-regalloc-enabled* is nil and interval has no active candidates
  ;; with a farther next use, the current interval itself is spilled.
  (let* ((state (make-test-lsa-state :free-regs nil))
         (interval (make-lsa-test-interval :v 0 10 :use-positions '(2))))
    (setf (cl-cc/regalloc::lsa-active state) nil)
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
      (cl-cc/regalloc::%lsa-evict-and-assign state interval))
    (assert-false (null (cl-cc/regalloc::interval-spill-slot interval)))
    (assert-true (> (cl-cc/regalloc::lsa-spill-count state) 0))))

(deftest lsa-evict-and-assign-frees-candidate-and-assigns
  "%lsa-evict-and-assign evicts the active candidate and assigns its freed register to the new interval."
  ;; Active candidate has a nearer next use than the new interval, so the new
  ;; interval evicts it and steals its register.
  (let* ((state (make-test-lsa-state :free-regs nil))
         (candidate (make-lsa-test-interval :cand 0 20 :use-positions '(3)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(15))))
    (setf (cl-cc/regalloc::interval-phys-reg candidate) :r0)
    (setf (gethash :cand (cl-cc/regalloc::lsa-assignment state)) :r0)
    (setf (cl-cc/regalloc::lsa-active state) (list candidate))
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
      (cl-cc/regalloc::%lsa-evict-and-assign state interval))
    (assert-false (null (cl-cc/regalloc::interval-spill-slot candidate)))
    (assert-null (cl-cc/regalloc::interval-phys-reg candidate))
    (assert-eq :r0 (cl-cc/regalloc::interval-phys-reg interval))))

;;; ─── %lsa-expire-old (boundary + FP-pool cases only) ──────────────────────

(deftest lsa-expire-old-keeps-interval-ending-at-current-start
  "%lsa-expire-old does not remove an interval whose end equals the current interval's start position."
  ;; Linear scan rule: an interval ending at I is still live at I (5 < 5 is false).
  (let* ((touching (make-lsa-test-interval :t 0 5))
         (current  (make-lsa-test-interval :c 5 10))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::interval-phys-reg touching) :r0)
    (setf (cl-cc/regalloc::lsa-active state) (list touching))
    (cl-cc/regalloc::%lsa-expire-old state current)
    (assert-true (member touching (cl-cc/regalloc::lsa-active state) :test #'eq))))

(deftest lsa-expire-old-returns-fp-register-to-fp-pool
  "%lsa-expire-old returns an expired FP interval's register to the FP free pool, not the GPR pool."
  (let* ((fp-expired (make-lsa-test-interval :fpe 0 2 :fp-p t))
         (current    (make-lsa-test-interval :c   5 10))
         (state      (make-test-lsa-state :free-regs nil :free-fp-regs nil)))
    (setf (cl-cc/regalloc::interval-phys-reg fp-expired) :xmm5)
    (setf (cl-cc/regalloc::lsa-active state) (list fp-expired))
    (cl-cc/regalloc::%lsa-expire-old state current)
    (assert-true (member :xmm5 (cl-cc/regalloc::lsa-free-fp-regs state) :test #'eq))
    (assert-false (member :xmm5 (cl-cc/regalloc::lsa-free-regs state) :test #'eq))))

;;; ─── %lsa-spill-current (distinct-slot case only) ─────────────────────────

(deftest lsa-spill-current-assigns-distinct-slots
  "%lsa-spill-current assigns a different spill slot to each successive spilled interval."
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 10))
         (int-b (make-lsa-test-interval :b 5 15)))
    (cl-cc/regalloc::%lsa-spill-current state int-a)
    (cl-cc/regalloc::%lsa-spill-current state int-b)
    (assert-true (/= (cl-cc/regalloc::interval-spill-slot int-a)
                     (cl-cc/regalloc::interval-spill-slot int-b)))))

;;; ─── %lsa-best-spill-candidate ────────────────────────────────────────────

(deftest lsa-best-spill-candidate-returns-farthest-next-use
  "%lsa-best-spill-candidate selects the active interval with the farthest next use as the eviction target."
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(20)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(8)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::lsa-active state) (list candidate))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      (assert-eq candidate best))))

(deftest lsa-best-spill-candidate-returns-self-when-no-farther-use
  "%lsa-best-spill-candidate returns the current interval itself when no active interval has a farther next use."
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(6)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(20)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::lsa-active state) (list candidate))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      (assert-eq interval best))))

(deftest lsa-best-spill-candidate-ml-enabled-returns-lowest-cost
  "%lsa-best-spill-candidate in ML mode selects the interval with the lowest ML spill cost."
  ;; ML mode uses regalloc-ml-spill-cost. The remat-const interval has a
  ;; negative cost adjustment, making it cheaper to spill.
  (let* ((no-remat   (make-lsa-test-interval :nr 0 10 :use-positions '(1 2 3)))
         (with-remat (make-lsa-test-interval :wr 0 10 :use-positions '(1 2 3)))
         (interval   (make-lsa-test-interval :new 5 15 :use-positions '(6)))
         (state      (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::interval-remat-const with-remat) 42)
    (setf (cl-cc/regalloc::lsa-active state) (list no-remat with-remat))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* t)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      (assert-eq with-remat best))))

(deftest lsa-best-spill-candidate-ignores-cross-class-intervals
  "%lsa-best-spill-candidate only considers active intervals of the same register class as the new interval."
  (let* ((fp-cand  (make-lsa-test-interval :fp 0 30 :fp-p t :use-positions '(20)))
         (interval (make-lsa-test-interval :new 5 25 :fp-p nil :use-positions '(8)))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::lsa-active state) (list fp-cand))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      ;; fp-cand is filtered out; interval is the only candidate → returns itself.
      (assert-eq interval best))))

;;; ─── %interval-next-use-after ─────────────────────────────────────────────

(deftest-each lsa-interval-next-use-after
  "%interval-next-use-after returns the first use position strictly greater than the query position, or nil."
  :cases (("finds-first-after" '(2 5 8 12) 4 5)
          ("returns-nil-none"  '(1 2 3)    5 nil)
          ("exact-boundary"    '(5 10)     4 5)
          ("nil-on-equal-pos"  '(5 10)     5 10)
          ("empty-list"        nil         0 nil))
  (use-positions position expected)
  (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (assert-equal expected (cl-cc/regalloc::%interval-next-use-after interval position))))

;;; ─── %return-value-preferred-reg ──────────────────────────────────────────

(deftest-each lsa-return-value-preferred-reg
  "%return-value-preferred-reg returns the ABI return register only when the interval is a return value and that register is free."
  :cases (("gpr-rv-in-pool"      nil t   :rax :xmm0 '(:rax :rbx :rcx) :rax)
          ("not-return-value"    nil nil :rax :xmm0 '(:rax :rbx)       nil)
          ("ret-reg-not-in-pool" nil t   :rax :xmm0 '(:rbx :rcx)       nil)
          ("fp-rv-in-pool"       t   t   :rax :xmm0 '(:xmm0 :xmm1)     :xmm0))
  (fp-p return-value-p ret-reg fp-ret-reg free-regs expected)
  (let* ((cc       (make-lsa-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) return-value-p)
    (assert-eq expected (cl-cc/regalloc::%return-value-preferred-reg interval cc free-regs))))

;;; ─── %call-crossing-preferred-reg ─────────────────────────────────────────

(deftest lsa-call-crossing-preferred-reg-prefers-callee-saved
  "%call-crossing-preferred-reg returns a callee-saved register for a GPR interval that crosses a call."
  (let* ((cc        (make-lsa-minimal-cc :callee-saved '(:rbx :r12)))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rdi :rbx :r12)))
    (let ((result (cl-cc/regalloc::%call-crossing-preferred-reg interval cc free-regs)))
      (assert-true (member result '(:rbx :r12) :test #'eq)))))

(deftest lsa-call-crossing-preferred-reg-nil-for-non-call-crossing
  "%call-crossing-preferred-reg returns nil when the interval does not cross a call."
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rdi :rbx)))
    (assert-null (cl-cc/regalloc::%call-crossing-preferred-reg interval cc free-regs))))

(deftest lsa-call-crossing-preferred-reg-nil-for-fp
  "%call-crossing-preferred-reg returns nil for FP intervals regardless of call-crossing status."
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p t :fp-p t))
         (free-regs '(:xmm0)))
    (assert-null (cl-cc/regalloc::%call-crossing-preferred-reg interval cc free-regs))))

;;; ─── %param-preferred-reg ─────────────────────────────────────────────────

(deftest lsa-param-preferred-reg-returns-matching-arg-reg
  "%param-preferred-reg returns the argument register corresponding to the interval's parameter index."
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rdx)))
    (setf (cl-cc/regalloc::interval-parameter-index interval) 1)
    (assert-eq :rsi (cl-cc/regalloc::%param-preferred-reg interval cc free-regs))))

(deftest lsa-param-preferred-reg-nil-when-index-out-of-range
  "%param-preferred-reg returns nil when the parameter index exceeds the available argument registers."
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi)))
    (setf (cl-cc/regalloc::interval-parameter-index interval) 5)
    (assert-null (cl-cc/regalloc::%param-preferred-reg interval cc free-regs))))

(deftest lsa-param-preferred-reg-nil-when-not-in-free-pool
  "%param-preferred-reg returns nil when the matching argument register is not in the free register pool."
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rsi :rdx)))       ; :rdi not available
    (setf (cl-cc/regalloc::interval-parameter-index interval) 0)
    (assert-null (cl-cc/regalloc::%param-preferred-reg interval cc free-regs))))

;;; ─── %hint-policy-preferred-reg ───────────────────────────────────────────

(deftest lsa-hint-policy-preferred-reg-returns-caller-saved-when-preferred
  "%hint-policy-preferred-reg returns a caller-saved register when the allocation policy prefers caller-saved."
  (let* ((cc        (make-lsa-minimal-cc :callee-saved '(:rbx) :caller-saved '(:rax :rcx)))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rax :rbx :rcx))
         (cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (let ((result (cl-cc/regalloc::%hint-policy-preferred-reg interval cc free-regs)))
      (assert-false (null result))
      (assert-true (member result (cl-cc/target::target-caller-saved cc) :test #'eq)))))

(deftest lsa-hint-policy-preferred-reg-nil-when-not-preferred
  "%hint-policy-preferred-reg returns nil when the allocation policy does not prefer caller-saved registers."
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx))
         (cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p nil)))
    (assert-null (cl-cc/regalloc::%hint-policy-preferred-reg interval cc free-regs))))

(deftest lsa-hint-policy-preferred-reg-nil-for-call-crossing
  "%hint-policy-preferred-reg returns nil for call-crossing intervals even when the policy prefers caller-saved."
  ;; Safety guard: do not force caller-saved for call-crossing intervals.
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rax :rbx))
         (cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (assert-null (cl-cc/regalloc::%hint-policy-preferred-reg interval cc free-regs))))

;;; ─── regalloc-target-fp-registers ─────────────────────────────────────────

(deftest lsa-target-fp-registers-x86-64-returns-16-xmm
  "regalloc-target-fp-registers returns all 16 XMM registers for the x86-64 target."
  (let* ((cc      (make-lsa-minimal-cc :name :x86-64))
         (fp-regs (cl-cc/regalloc::regalloc-target-fp-registers cc)))
    (assert-= 16 (length fp-regs))
    (assert-true (every (lambda (r) (member r fp-regs :test #'eq))
                        '(:xmm0 :xmm7 :xmm15)))))

(deftest lsa-target-fp-registers-aarch64-returns-32-v
  "regalloc-target-fp-registers returns all 32 V registers for the AArch64 target."
  (let* ((cc      (make-lsa-minimal-cc :name :aarch64))
         (fp-regs (cl-cc/regalloc::regalloc-target-fp-registers cc)))
    (assert-= 32 (length fp-regs))
    (assert-true (every (lambda (r) (member r fp-regs :test #'eq))
                        '(:v0 :v15 :v31)))))

(deftest lsa-target-fp-registers-unknown-falls-back-to-union
  "regalloc-target-fp-registers falls back to the union of fp-arg-regs and fp-ret-reg for unknown targets."
  ;; Duplicates (:f0 in both fp-arg-regs and fp-ret-reg) must be removed.
  (let* ((cc (cl-cc/target::make-target-desc
              :name        :unknown-arch
              :gpr-names   #(:r0)
              :arg-regs    '(:r0)
              :ret-reg     :r0
              :fp-arg-regs '(:f0 :f1)
              :fp-ret-reg  :f0
              :callee-saved '()
              :scratch-regs nil))
         (fp-regs (cl-cc/regalloc::regalloc-target-fp-registers cc)))
    (assert-= 2 (length fp-regs))
    (assert-true (member :f0 fp-regs :test #'eq))
    (assert-true (member :f1 fp-regs :test #'eq))))

;;; ─── %preferred-register-for-interval strategy priority order ─────────────

(deftest lsa-preferred-register-hint-policy-wins
  "%preferred-register-for-interval applies the hint-policy strategy before all other strategies."
  ;; Return-value and param-index are both eligible, but hint-policy is first
  ;; in the strategy list and picks a caller-saved register, so it wins.
  (let* ((cc (cl-cc/target::make-target-desc
              :name        :x86-64
              :gpr-names   #(:rdi :rax :rbx)
              :arg-regs    '(:rdi)
              :ret-reg     :rax
              :fp-arg-regs '()
              :fp-ret-reg  nil
              :callee-saved '(:rbx)
              :scratch-regs nil))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rax :rbx)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) t)
    (setf (cl-cc/regalloc::interval-parameter-index interval) 0)
    (let ((cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p t)))
      (let ((result (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs)))
        (assert-false (null result))
        (assert-true (member result (cl-cc/target::target-caller-saved cc) :test #'eq))))))

(deftest lsa-preferred-register-return-value-wins-when-hint-nil
  "%preferred-register-for-interval applies the return-value strategy when hint-policy produces no result."
  (let* ((cc        (make-lsa-minimal-cc :ret-reg :rax))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) t)
    (let ((cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p nil)))
      (assert-eq :rax (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs)))))

(deftest lsa-preferred-register-param-wins-when-others-nil
  "%preferred-register-for-interval applies the parameter strategy when hint-policy and return-value both yield nil."
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rbx)))
    (setf (cl-cc/regalloc::interval-parameter-index interval) 0)
    (let ((cl-cc/regalloc::*current-allocation-policy* nil))
      (assert-eq :rdi (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs)))))

(deftest lsa-preferred-register-nil-when-no-strategy-fires
  "%preferred-register-for-interval returns nil when no preferred-register strategy produces a result."
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rbx)))
    (let ((cl-cc/regalloc::*current-allocation-policy* nil))
      (assert-null (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs)))))

;;; ─── %allocation-strategy ─────────────────────────────────────────────────

(deftest-each lsa-allocation-strategy-dispatch
  "%allocation-strategy resolves the strategy keyword from the policy plist, defaulting to the current strategy."
  :cases (("color-via-allocator"  '(:allocator :color)          :color)
          ("lscan-via-allocator"  '(:allocator :linear-scan)    :linear-scan)
          ("color-via-alt-key"    '(:register-allocator :color) :color)
          ("nil-policy-default"   nil                           :linear-scan))
  (policy expected-strategy)
  (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :linear-scan))
    (assert-eq expected-strategy (cl-cc/regalloc::%allocation-strategy policy))))

(deftest lsa-allocation-strategy-falls-back-to-default
  "%allocation-strategy returns the value of *regalloc-allocation-strategy* when the policy list is empty."
  (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :color))
    (assert-eq :color (cl-cc/regalloc::%allocation-strategy '()))))

;;; ─── %derive-single-function-policy ───────────────────────────────────────

(deftest lsa-derive-single-function-policy-nonnil-for-single-function
  "%derive-single-function-policy returns a non-nil policy plist for an instruction stream with exactly one function."
  (let ((instructions (list (make-vm-label :name "entry")
                            (make-vm-const :dst :r0 :value 42)
                            (make-vm-ret  :reg :r0))))
    (assert-false (null (cl-cc/regalloc::%derive-single-function-policy instructions)))))

(deftest lsa-derive-single-function-policy-nil-for-multi-function
  "%derive-single-function-policy returns nil when the instruction stream contains more than one function."
  (let ((instructions (list (make-vm-label :name "f1")
                            (make-vm-const :dst :r0 :value 1)
                            (make-vm-ret  :reg :r0)
                            (make-vm-label :name "f2")
                            (make-vm-const :dst :r0 :value 2)
                            (make-vm-ret  :reg :r0))))
    (assert-null (cl-cc/regalloc::%derive-single-function-policy instructions))))

;;; ─── linear-scan-allocate integration ─────────────────────────────────────

(deftest lsa-linear-scan-allocate-assigns-non-overlapping-intervals
  "linear-scan-allocate returns correct assignment and spill hash tables for non-overlapping intervals with no spills."
  (let* ((int-a (make-lsa-test-interval :a 0  5 :use-positions '(0 5)))
         (int-b (make-lsa-test-interval :b 6 10 :use-positions '(6 10)))
         (cc    (cl-cc/target::make-target-desc
                 :name        :x86-64
                 :gpr-names   #(:r0 :r1)
                 :arg-regs    '(:r0)
                 :ret-reg     :r0
                 :fp-arg-regs '()
                 :fp-ret-reg  nil
                 :callee-saved '()
                 :scratch-regs nil)))
    (multiple-value-bind (assignment spill-map spill-count)
        (cl-cc/regalloc::linear-scan-allocate (list int-a int-b) cc)
      (assert-true (hash-table-p assignment))
      (assert-true (hash-table-p spill-map))
      (assert-true (integerp spill-count))
      (assert-true (gethash :a assignment))
      (assert-true (gethash :b assignment))
      (assert-= 0 (hash-table-count spill-map)))))

(deftest lsa-linear-scan-allocate-spills-under-pressure
  "linear-scan-allocate spills at least one interval when mutually overlapping intervals exceed the register pool."
  ;; Three mutually overlapping intervals with only 2 registers: one must spill.
  (let* ((int-a (make-lsa-test-interval :a 0 20 :use-positions '(0 5 10)))
         (int-b (make-lsa-test-interval :b 0 20 :use-positions '(1 6 11)))
         (int-c (make-lsa-test-interval :c 0 20 :use-positions '(2 7 12)))
         (cc    (cl-cc/target::make-target-desc
                 :name        :x86-64
                 :gpr-names   #(:r0 :r1)
                 :arg-regs    '(:r0)
                 :ret-reg     :r0
                 :fp-arg-regs '()
                 :fp-ret-reg  nil
                 :callee-saved '()
                 :scratch-regs nil)))
    (multiple-value-bind (assignment spill-map spill-count)
        (cl-cc/regalloc::linear-scan-allocate (list int-a int-b int-c) cc)
      (declare (ignore assignment))
      (assert-true (>= (hash-table-count spill-map) 1))
      (assert-true (>= spill-count 1)))))
