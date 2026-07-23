(in-package :cl-cc/regalloc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-weave:it-sequential cl-weave:it-sequential-each cl-weave:expect
            cl-weave:signals cl-weave:it-todo)
          :cl-cc/regalloc))

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Tests for regalloc-allocate.lisp — Linear Scan Allocator Internals
;;;
;;; Covers: %lsa-try-coalesce, %lsa-assign, %lsa-allocate-from-pool,
;;;         %lsa-evict-and-assign
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun make-lsa-test-interval (vreg start end &key fp-p coalesce-with
                                               use-positions crosses-call-p)
  "Construct a live-interval for linear-scan allocator tests."
  (make-live-interval :vreg vreg
                      :start start
                      :end end
                      :fp-p fp-p
                      :coalesce-with coalesce-with
                      :use-positions (or use-positions (list start end))
                      :crosses-call-p crosses-call-p))

(defun make-test-lsa-state (&key (free-regs '(:r0 :r1 :r2))
                                  (free-fp-regs '(:xmm0 :xmm1))
                                  active)
  "Construct a minimal lsa-state for testing."
  (make-lsa-state :free-regs free-regs
                  :free-fp-regs free-fp-regs
                  :active (or active nil)))

;;; ─── %lsa-assign ──────────────────────────────────────────────────────────

(it-sequential "%lsa-assign records assignment and inserts into active list"
  (let* ((state (make-test-lsa-state))
         (interval (make-lsa-test-interval :v 0 10)))
    (%lsa-assign state interval :r0)
    (expect (eq :r0 (interval-phys-reg interval)) :to-be-truthy)
    (expect (eq :r0 (gethash :v (lsa-assignment state))) :to-be-truthy)
    (expect (member interval (lsa-active state) :test #'eq) :to-be-truthy)))

(it-sequential "%lsa-assign keeps active list sorted by interval end"
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 20))
         (int-b (make-lsa-test-interval :b 0  5))
         (int-c (make-lsa-test-interval :c 0 12)))
    (%lsa-assign state int-a :r0)
    (%lsa-assign state int-b :r1)
    (%lsa-assign state int-c :r2)
    ;; After insertion the active list should be ordered by interval-end ascending.
    (let ((ends (mapcar #'interval-end (lsa-active state))))
      (expect (equal ends (sort (copy-list ends) #'<)) :to-be-truthy))))

;;; ─── %lsa-try-coalesce ────────────────────────────────────────────────────

(it-sequential "%lsa-try-coalesce succeeds when source ends exactly at current start"
  (let* ((state (make-test-lsa-state :free-regs '(:r0 :r1)))
         (src-int (make-lsa-test-interval :src 0 5))
         (new-int (make-lsa-test-interval :new 5 10 :coalesce-with :src)))
    ;; Manually assign :r0 to the source and mark it active.
    (setf (interval-phys-reg src-int) :r0)
    (setf (gethash :src (lsa-interval-map state)) src-int)
    (setf (lsa-active state) (list src-int))
    (let ((result (%lsa-try-coalesce state new-int)))
      (expect (eq t result) :to-be-truthy)
      (expect (eq :r0 (interval-phys-reg new-int)) :to-be-truthy)
      ;; Source should have been removed from active and replaced by new-int.
      (expect (not (member src-int (lsa-active state) :test #'eq)) :to-be-truthy)
      (expect (member new-int (lsa-active state) :test #'eq) :to-be-truthy))))

(it-sequential "%lsa-try-coalesce fails (returns nil) :source-still-live"
  (destructuring-bind (src-end new-start new-fp-p coalesce-with setup-src-p) (list 10 5 nil :src t)
    (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (interval-phys-reg src-int) :r0)
      (setf (gethash :src (lsa-interval-map state)) src-int)
      (setf (lsa-active state) (list src-int)))
    (expect (null (%lsa-try-coalesce state new-int)) :to-be-truthy))))

(it-sequential "%lsa-try-coalesce fails (returns nil) :no-coalesce-hint"
  (destructuring-bind (src-end new-start new-fp-p coalesce-with setup-src-p) (list 5 5 nil nil nil)
    (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (interval-phys-reg src-int) :r0)
      (setf (gethash :src (lsa-interval-map state)) src-int)
      (setf (lsa-active state) (list src-int)))
    (expect (null (%lsa-try-coalesce state new-int)) :to-be-truthy))))

(it-sequential "%lsa-try-coalesce fails (returns nil) :fp-class-mismatch"
  (destructuring-bind (src-end new-start new-fp-p coalesce-with setup-src-p) (list 5 5 t :src t)
    (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (interval-phys-reg src-int) :r0)
      (setf (gethash :src (lsa-interval-map state)) src-int)
      (setf (lsa-active state) (list src-int)))
    (expect (null (%lsa-try-coalesce state new-int)) :to-be-truthy))))

;;; ─── %lsa-allocate-from-pool ──────────────────────────────────────────────

(it-sequential "%lsa-allocate-from-pool assigns register and removes it from pool"
  (let* ((state    (make-test-lsa-state :free-regs '(:r0 :r1 :r2)))
         (interval (make-lsa-test-interval :v 0 10))
         (cc       (make-target-desc
                     :name        :x86-64
                     :gpr-names   #(:rdi :rsi :rdx :rcx)
                     :arg-regs    '(:rdi :rsi)
                     :ret-reg     :rax
                     :fp-arg-regs '(:xmm0)
                     :fp-ret-reg  :xmm0
                     :callee-saved '()
                     :scratch-regs nil)))
    (%lsa-allocate-from-pool state interval cc '(:r0 :r1 :r2))
    (expect (not (null (interval-phys-reg interval))) :to-be-truthy)
    (expect (member (interval-phys-reg interval) '(:r0 :r1 :r2) :test #'eq) :to-be-truthy)
    (expect (not (member (interval-phys-reg interval)
                     (lsa-free-regs state) :test #'eq)) :to-be-truthy)))

;;; ─── %lsa-evict-and-assign ────────────────────────────────────────────────

(it-sequential "%lsa-evict-and-assign spills current when it is worst candidate"
  (let* ((state (make-test-lsa-state :free-regs nil))
         (interval (make-lsa-test-interval :v 0 10 :use-positions '(2))))
    (setf (lsa-active state) nil)
    (let ((*ml-regalloc-enabled* nil))
      (%lsa-evict-and-assign state interval))
    ;; The interval should be spilled (spill-slot set, count bumped).
    (expect (not (null (interval-spill-slot interval))) :to-be-truthy)
    (expect (> (lsa-spill-count state) 0) :to-be-truthy)))

(it-sequential "%lsa-evict-and-assign frees candidate register and assigns to interval"
  (let* ((state (make-test-lsa-state :free-regs nil))
         ;; Candidate: next use at position 3 (near).
         (candidate (make-lsa-test-interval :cand 0 20 :use-positions '(3)))
         ;; New interval: next use at position 15 (far from current start 5).
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(15))))
    (setf (interval-phys-reg candidate) :r0)
    (setf (gethash :cand (lsa-assignment state)) :r0)
    (setf (lsa-active state) (list candidate))
    (let ((*ml-regalloc-enabled* nil))
      (%lsa-evict-and-assign state interval))
    ;; Candidate should now be spilled.
    (expect (not (null (interval-spill-slot candidate))) :to-be-truthy)
    (expect (null (interval-phys-reg candidate)) :to-be-truthy)
    ;; New interval should have been assigned the freed register.
    (expect (eq :r0 (interval-phys-reg interval)) :to-be-truthy)))

;;; ─── %lsa-expire-old ──────────────────────────────────────────────────────

(it-sequential "%lsa-expire-old removes intervals that end before current start"
  (let* ((expired  (make-lsa-test-interval :expired  0  3))
         (alive    (make-lsa-test-interval :alive    0 10))
         (current  (make-lsa-test-interval :curr     5 15))
         (state    (make-test-lsa-state :free-regs '())))
    (setf (interval-phys-reg expired) :r0)
    (setf (interval-phys-reg alive)   :r1)
    (setf (lsa-active state) (list expired alive))
    (%lsa-expire-old state current)
    ;; expired should have been removed from active
    (expect (not (member expired (lsa-active state) :test #'eq)) :to-be-truthy)
    ;; alive should still be in active
    (expect (member alive (lsa-active state) :test #'eq) :to-be-truthy)))

(it-sequential "%lsa-expire-old returns register of expired interval to pool"
  (let* ((expired (make-lsa-test-interval :e 0 3))
         (current (make-lsa-test-interval :c 5 15))
         (state   (make-test-lsa-state :free-regs nil)))
    (setf (interval-phys-reg expired) :r7)
    (setf (lsa-active state) (list expired))
    (%lsa-expire-old state current)
    (expect (member :r7 (lsa-free-regs state) :test #'eq) :to-be-truthy)))

(it-sequential "%lsa-expire-old does not expire intervals ending at exactly current start"
  (let* ((touching (make-lsa-test-interval :t 0 5))
         (current  (make-lsa-test-interval :c 5 10))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (interval-phys-reg touching) :r0)
    (setf (lsa-active state) (list touching))
    (%lsa-expire-old state current)
    ;; The interval ends at 5; current starts at 5.  5 < 5 is false so it
    ;; should NOT be expired.
    (expect (member touching (lsa-active state) :test #'eq) :to-be-truthy)))

(it-sequential "%lsa-expire-old returns fp register to fp pool for fp intervals"
  (let* ((fp-expired (make-lsa-test-interval :fpe 0 2 :fp-p t))
         (current    (make-lsa-test-interval :c   5 10))
         (state      (make-test-lsa-state :free-regs nil :free-fp-regs nil)))
    (setf (interval-phys-reg fp-expired) :xmm5)
    (setf (lsa-active state) (list fp-expired))
    (%lsa-expire-old state current)
    (expect (member :xmm5 (lsa-free-fp-regs state) :test #'eq) :to-be-truthy)
    (expect (not (member :xmm5 (lsa-free-regs state) :test #'eq)) :to-be-truthy)))

;;; ─── %lsa-spill-current ───────────────────────────────────────────────────

(it-sequential "%lsa-spill-current sets spill-slot and increments count"
  (let* ((state    (make-test-lsa-state))
         (interval (make-lsa-test-interval :v 0 10))
         (before   (lsa-spill-count state)))
    (%lsa-spill-current state interval)
    (expect (not (null (interval-spill-slot interval))) :to-be-truthy)
    (expect (> (lsa-spill-count state) before) :to-be-truthy)))

(it-sequential "%lsa-spill-current records vreg in spill-map"
  (let* ((state    (make-test-lsa-state))
         (interval (make-lsa-test-interval :myvreg 0 10)))
    (%lsa-spill-current state interval)
    (expect (gethash :myvreg (lsa-spill-map state)) :to-be-truthy)))

(it-sequential "%lsa-spill-current assigns distinct slots for two intervals"
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 10))
         (int-b (make-lsa-test-interval :b 5 15)))
    (%lsa-spill-current state int-a)
    (%lsa-spill-current state int-b)
    (expect (/= (interval-spill-slot int-a) (interval-spill-slot int-b)) :to-be-truthy)))

;;; ─── %lsa-best-spill-candidate ────────────────────────────────────────────

(it-sequential "%lsa-best-spill-candidate ml-disabled: returns interval with farthest next use"
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(20)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(8)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (lsa-active state) (list candidate))
    (let* ((*ml-regalloc-enabled* nil)
           (best (%lsa-best-spill-candidate state interval)))
      (expect (eq candidate best) :to-be-truthy))))

(it-sequential "%lsa-best-spill-candidate ml-disabled: returns self when no active with farther use"
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(6)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(20)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (lsa-active state) (list candidate))
    (let* ((*ml-regalloc-enabled* nil)
           (best (%lsa-best-spill-candidate state interval)))
      (expect (eq interval best) :to-be-truthy))))

(it-sequential "%lsa-best-spill-candidate ml-enabled: returns lowest-cost interval"
  (let* ((no-remat  (make-lsa-test-interval :nr 0 10 :use-positions '(1 2 3)))
         (with-remat (make-lsa-test-interval :wr 0 10 :use-positions '(1 2 3)))
         (interval   (make-lsa-test-interval :new 5 15 :use-positions '(6)))
         (state      (make-test-lsa-state :free-regs nil)))
    (setf (interval-remat-const with-remat) 42)
    (setf (lsa-active state) (list no-remat with-remat))
    (let* ((*ml-regalloc-enabled* t)
           (best (%lsa-best-spill-candidate state interval)))
      ;; with-remat has lower ML cost (remat-const bonus = -6) so it should
      ;; be the preferred spill candidate.
      (expect (eq with-remat best) :to-be-truthy))))

(it-sequential "%lsa-best-spill-candidate ignores cross-class active intervals"
  (let* ((fp-cand (make-lsa-test-interval :fp 0 30 :fp-p t :use-positions '(20)))
         (interval (make-lsa-test-interval :new 5 25 :fp-p nil :use-positions '(8)))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (lsa-active state) (list fp-cand))
    (let* ((*ml-regalloc-enabled* nil)
           (best (%lsa-best-spill-candidate state interval)))
      ;; fp-cand is filtered out; interval is the only candidate → returns itself.
      (expect (eq interval best) :to-be-truthy))))

;;; ─── %interval-next-use-after ─────────────────────────────────────────────

(it-sequential "%interval-next-use-after :finds-first-after"
  (destructuring-bind (use-positions position expected) (list '(2 5 8 12) 4 5)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (equal expected (%interval-next-use-after interval position)) :to-be-truthy))))

(it-sequential "%interval-next-use-after :returns-nil-none"
  (destructuring-bind (use-positions position expected) (list '(1 2 3) 5 nil)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (equal expected (%interval-next-use-after interval position)) :to-be-truthy))))

(it-sequential "%interval-next-use-after :exact-boundary"
  (destructuring-bind (use-positions position expected) (list '(5 10) 4 5)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (equal expected (%interval-next-use-after interval position)) :to-be-truthy))))

(it-sequential "%interval-next-use-after :nil-on-equal-pos"
  (destructuring-bind (use-positions position expected) (list '(5 10) 5 10)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (equal expected (%interval-next-use-after interval position)) :to-be-truthy))))

(it-sequential "%interval-next-use-after :empty-list"
  (destructuring-bind (use-positions position expected) (list '() 0 nil)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (equal expected (%interval-next-use-after interval position)) :to-be-truthy))))

;;; ─── %lsa-interval-pool and %lsa-set-interval-pool ───────────────────────

(it-sequential "%lsa-interval-pool :gpr-returns-free-regs"
  (destructuring-bind (fp-p free-regs free-fp-regs expected) (list nil '(:r0 :r1) '(:xmm0) '(:r0 :r1))
    (let* ((state    (make-test-lsa-state :free-regs free-regs :free-fp-regs free-fp-regs))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (expect (equal expected (%lsa-interval-pool state interval)) :to-be-truthy))))

(it-sequential "%lsa-interval-pool :fp-returns-free-fp-regs"
  (destructuring-bind (fp-p free-regs free-fp-regs expected) (list t '(:r0) '(:xmm0 :xmm1) '(:xmm0 :xmm1))
    (let* ((state    (make-test-lsa-state :free-regs free-regs :free-fp-regs free-fp-regs))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (expect (equal expected (%lsa-interval-pool state interval)) :to-be-truthy))))

(it-sequential "%lsa-set-interval-pool :gpr-updates-free-regs"
  (destructuring-bind (fp-p new-pool expected-free-regs expected-free-fp-regs) (list nil '(:r0 :r1 :r2) '(:r0 :r1 :r2) '(:xmm0))
    (let* ((state    (make-test-lsa-state :free-regs '(:r0) :free-fp-regs '(:xmm0)))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (%lsa-set-interval-pool state interval new-pool)
    (expect (equal expected-free-regs    (lsa-free-regs state)) :to-be-truthy)
    (expect (equal expected-free-fp-regs (lsa-free-fp-regs state)) :to-be-truthy))))

(it-sequential "%lsa-set-interval-pool :fp-updates-free-fp-regs"
  (destructuring-bind (fp-p new-pool expected-free-regs expected-free-fp-regs) (list t '(:xmm0 :xmm1) '(:r0) '(:xmm0 :xmm1))
    (let* ((state    (make-test-lsa-state :free-regs '(:r0) :free-fp-regs '(:xmm0)))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (%lsa-set-interval-pool state interval new-pool)
    (expect (equal expected-free-regs    (lsa-free-regs state)) :to-be-truthy)
    (expect (equal expected-free-fp-regs (lsa-free-fp-regs state)) :to-be-truthy))))

;;; ─── preferred-register strategy helpers ─────────────────────────────────

(defun make-minimal-cc (&key (name :x86-64)
                              (arg-regs '(:rdi :rsi :rdx))
                              (ret-reg :rax)
                              (fp-arg-regs '(:xmm0 :xmm1))
                              (fp-ret-reg :xmm0)
                              (callee-saved '(:rbx :r12))
                              (caller-saved '(:rax :rcx :rdx :rsi :rdi)))
  "Build a minimal target-desc fixture for preferred-register strategy tests."
  (make-target-desc
    :name         name
    :gpr-names    (coerce (append arg-regs callee-saved caller-saved) 'vector)
    :arg-regs     arg-regs
    :ret-reg      ret-reg
    :fp-arg-regs  fp-arg-regs
    :fp-ret-reg   fp-ret-reg
    :callee-saved callee-saved
    :scratch-regs nil))

(it-sequential "%return-value-preferred-reg :gpr-rv-in-pool"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list nil t :rax :xmm0 '(:rax :rbx :rcx) :rax)
    (let* ((cc       (make-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (interval-return-value-p interval) return-value-p)
    (expect (eq expected (%return-value-preferred-reg interval cc free-regs)) :to-be-truthy))))

(it-sequential "%return-value-preferred-reg :not-return-value"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list nil nil :rax :xmm0 '(:rax :rbx) nil)
    (let* ((cc       (make-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (interval-return-value-p interval) return-value-p)
    (expect (eq expected (%return-value-preferred-reg interval cc free-regs)) :to-be-truthy))))

(it-sequential "%return-value-preferred-reg :ret-reg-not-in-pool"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list nil t :rax :xmm0 '(:rbx :rcx) nil)
    (let* ((cc       (make-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (interval-return-value-p interval) return-value-p)
    (expect (eq expected (%return-value-preferred-reg interval cc free-regs)) :to-be-truthy))))

(it-sequential "%return-value-preferred-reg :fp-rv-in-pool"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list t t :rax :xmm0 '(:xmm0 :xmm1) :xmm0)
    (let* ((cc       (make-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (interval-return-value-p interval) return-value-p)
    (expect (eq expected (%return-value-preferred-reg interval cc free-regs)) :to-be-truthy))))

(it-sequential "%call-crossing-preferred-reg prefers callee-saved for call-crossing gpr interval"
  (let* ((cc       (make-minimal-cc :callee-saved '(:rbx :r12)))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rdi :rbx :r12)))
    (let ((result (%call-crossing-preferred-reg interval cc free-regs)))
      (expect (member result '(:rbx :r12) :test #'eq) :to-be-truthy))))

(it-sequential "%call-crossing-preferred-reg returns nil for non-call-crossing interval"
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rdi :rbx)))
    (expect (null (%call-crossing-preferred-reg interval cc free-regs)) :to-be-truthy)))

(it-sequential "%call-crossing-preferred-reg returns nil for fp interval"
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p t :fp-p t))
         (free-regs '(:xmm0)))
    (expect (null (%call-crossing-preferred-reg interval cc free-regs)) :to-be-truthy)))

(it-sequential "%param-preferred-reg returns arg-reg matching parameter-index"
  (let* ((cc       (make-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rdx)))
    (setf (interval-parameter-index interval) 1)
    (expect (eq :rsi (%param-preferred-reg interval cc free-regs)) :to-be-truthy)))

(it-sequential "%param-preferred-reg returns nil when param-index is out of range"
  (let* ((cc       (make-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi)))
    (setf (interval-parameter-index interval) 5)
    (expect (null (%param-preferred-reg interval cc free-regs)) :to-be-truthy)))

(it-sequential "%param-preferred-reg returns nil when preferred reg not in free pool"
  (let* ((cc       (make-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rsi :rdx)))  ; :rdi not available
    (setf (interval-parameter-index interval) 0)
    (expect (null (%param-preferred-reg interval cc free-regs)) :to-be-truthy)))

(it-sequential "%hint-policy-preferred-reg returns caller-saved when policy prefers it"
  (let* ((cc       (make-minimal-cc :callee-saved '(:rbx) :caller-saved '(:rax :rcx)))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rax :rbx :rcx))
         (*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (let ((result (%hint-policy-preferred-reg interval cc free-regs)))
      (expect (not (null result)) :to-be-truthy)
      ;; The result must be a caller-saved register.
      (expect (member result (target-caller-saved cc) :test #'eq) :to-be-truthy))))

(it-sequential "%hint-policy-preferred-reg returns nil when policy does not prefer caller-saved"
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx))
         (*current-allocation-policy* '(:prefer-caller-saved-p nil)))
    (expect (null (%hint-policy-preferred-reg interval cc free-regs)) :to-be-truthy)))

(it-sequential "%hint-policy-preferred-reg returns nil for call-crossing intervals even with policy"
  (let* ((cc       (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rax :rbx))
         (*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (expect (null (%hint-policy-preferred-reg interval cc free-regs)) :to-be-truthy)))

;;; ─── regalloc-target-fp-registers ────────────────────────────────────────

(it-sequential "regalloc-target-fp-registers x86-64 returns 16 xmm registers"
  (let* ((cc (make-minimal-cc :name :x86-64))
         (fp-regs (regalloc-target-fp-registers cc)))
    (expect (= 16 (length fp-regs)) :to-be-truthy)
    (expect (every (lambda (r) (member r fp-regs :test #'eq))
               '(:xmm0 :xmm7 :xmm15)) :to-be-truthy)))

(it-sequential "regalloc-target-fp-registers aarch64 returns 32 v registers"
  (let* ((cc (make-minimal-cc :name :aarch64))
         (fp-regs (regalloc-target-fp-registers cc)))
    (expect (= 32 (length fp-regs)) :to-be-truthy)
    (expect (every (lambda (r) (member r fp-regs :test #'eq))
               '(:v0 :v15 :v31)) :to-be-truthy)))

(it-sequential "regalloc-target-fp-registers unknown target falls back to fp-arg-regs union fp-ret-reg"
  (let* ((cc (make-target-desc
               :name        :unknown-arch
               :gpr-names   #(:r0)
               :arg-regs    '(:r0)
               :ret-reg     :r0
               :fp-arg-regs '(:f0 :f1)
               :fp-ret-reg  :f0
               :callee-saved '()
               :scratch-regs nil))
         (fp-regs (regalloc-target-fp-registers cc)))
    ;; Duplicates (:f0 in both fp-arg-regs and fp-ret-reg) must be removed.
    (expect (= 2 (length fp-regs)) :to-be-truthy)
    (expect (member :f0 fp-regs :test #'eq) :to-be-truthy)
    (expect (member :f1 fp-regs :test #'eq) :to-be-truthy)))

;;; ─── %preferred-register-for-interval strategy priority order ────────────

(it-sequential "%preferred-register-for-interval: hint-policy wins over all others"
  (let* ((cc (make-target-desc
               :name        :x86-64
               :gpr-names   #(:rdi :rax :rbx)
               :arg-regs    '(:rdi)
               :ret-reg     :rax
               :fp-arg-regs '()
               :fp-ret-reg  nil
               :callee-saved '(:rbx)
               :scratch-regs nil))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rax :rbx)))
    ;; Make interval eligible for multiple strategies.
    (setf (interval-return-value-p interval) t)
    (setf (interval-parameter-index interval) 0)
    (let ((*current-allocation-policy* '(:prefer-caller-saved-p t)))
      ;; hint-policy prefers caller-saved (:rdi, :rax); those appear before :rbx.
      ;; return-value prefers :rax.  Both are in free pool.
      ;; hint-policy must win (first in strategy list).
      (let ((result (%preferred-register-for-interval interval cc free-regs)))
        (expect (not (null result)) :to-be-truthy)
        ;; The winning strategy is hint-policy which returns a caller-saved reg.
        (expect (member result (target-caller-saved cc) :test #'eq) :to-be-truthy)))))

(it-sequential "%preferred-register-for-interval: return-value wins when hint-policy returns nil"
  (let* ((cc (make-minimal-cc :ret-reg :rax))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx)))
    (setf (interval-return-value-p interval) t)
    (let ((*current-allocation-policy* '(:prefer-caller-saved-p nil)))
      (expect (eq :rax (%preferred-register-for-interval interval cc free-regs)) :to-be-truthy))))

(it-sequential "%preferred-register-for-interval: param wins when neither hint nor return-value fires"
  (let* ((cc (make-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rbx)))
    (setf (interval-parameter-index interval) 0)
    (let ((*current-allocation-policy* nil))
      (expect (eq :rdi (%preferred-register-for-interval interval cc free-regs)) :to-be-truthy))))

(it-sequential "%preferred-register-for-interval: returns nil when no strategy fires"
  (let* ((cc (make-minimal-cc))
         (interval (make-lsa-test-interval :v 0 10))
         (free-regs '(:rbx)))
    ;; No return-value, no param-index, no call-crossing, no policy hint.
    (let ((*current-allocation-policy* nil))
      (expect (null (%preferred-register-for-interval interval cc free-regs)) :to-be-truthy))))

;;; ─── %allocation-strategy ────────────────────────────────────────────────

(it-sequential "%allocation-strategy dispatch :color-via-allocator"
  (destructuring-bind (policy expected-strategy) (list '(:allocator :color) :color)
    (let ((*regalloc-allocation-strategy* :linear-scan))
    (expect (eq expected-strategy (%allocation-strategy policy)) :to-be-truthy))))

(it-sequential "%allocation-strategy dispatch :lscan-via-allocator"
  (destructuring-bind (policy expected-strategy) (list '(:allocator :linear-scan) :linear-scan)
    (let ((*regalloc-allocation-strategy* :linear-scan))
    (expect (eq expected-strategy (%allocation-strategy policy)) :to-be-truthy))))

(it-sequential "%allocation-strategy dispatch :color-via-alt-key"
  (destructuring-bind (policy expected-strategy) (list '(:register-allocator :color) :color)
    (let ((*regalloc-allocation-strategy* :linear-scan))
    (expect (eq expected-strategy (%allocation-strategy policy)) :to-be-truthy))))

(it-sequential "%allocation-strategy dispatch :nil-policy-default"
  (destructuring-bind (policy expected-strategy) (list nil :linear-scan)
    (let ((*regalloc-allocation-strategy* :linear-scan))
    (expect (eq expected-strategy (%allocation-strategy policy)) :to-be-truthy))))

(it-sequential "%allocation-strategy falls back to *regalloc-allocation-strategy* when policy is empty"
  (let ((*regalloc-allocation-strategy* :color))
    (expect (eq :color (%allocation-strategy '())) :to-be-truthy)))

;;; ─── %derive-single-function-policy ─────────────────────────────────────

(it-sequential "%derive-single-function-policy returns non-nil policy for single-function stream"
  (let* ((instructions
           (list (make-vm-label :name "entry")
                 (make-vm-const :dst :r0 :value 42)
                 (make-vm-ret  :reg :r0))))
    (let ((policy (%derive-single-function-policy instructions)))
      ;; Single function → policy plist should be non-nil.
      (expect (not (null policy)) :to-be-truthy))))

(it-sequential "%derive-single-function-policy returns nil for multi-function stream"
  (let* ((instructions
           (list (make-vm-label :name "f1")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret  :reg :r0)
                 (make-vm-label :name "f2")
                 (make-vm-const :dst :r0 :value 2)
                 (make-vm-ret  :reg :r0))))
    (let ((policy (%derive-single-function-policy instructions)))
      (expect (null policy) :to-be-truthy))))

;;; ─── linear-scan-allocate integration test ───────────────────────────────

(it-sequential "linear-scan-allocate returns assignment-ht spill-ht spill-count for hand-crafted intervals"
  (let* ((int-a (make-lsa-test-interval :a 0  5 :use-positions '(0 5)))
         (int-b (make-lsa-test-interval :b 6 10 :use-positions '(6 10)))
         (cc    (make-target-desc
                  :name        :x86-64
                  :gpr-names   #(:r0 :r1)
                  :arg-regs    '(:r0)
                  :ret-reg     :r0
                  :fp-arg-regs '()
                  :fp-ret-reg  nil
                  :callee-saved '()
                  :scratch-regs nil))
         (intervals (list int-a int-b)))
    (multiple-value-bind (assignment spill-map spill-count)
        (linear-scan-allocate intervals cc)
      (expect (hash-table-p assignment) :to-be-truthy)
      (expect (hash-table-p spill-map) :to-be-truthy)
      (expect (integerp spill-count) :to-be-truthy)
      ;; Both intervals should have been assigned a physical register.
      (expect (gethash :a assignment) :to-be-truthy)
      (expect (gethash :b assignment) :to-be-truthy)
      ;; No spills expected when two registers are available for two non-overlapping intervals.
      (expect (= 0 (hash-table-count spill-map)) :to-be-truthy))))

(it-sequential "linear-scan-allocate spills when register pressure exceeds pool"
  (let* ((int-a (make-lsa-test-interval :a 0 20 :use-positions '(0 5 10)))
         (int-b (make-lsa-test-interval :b 0 20 :use-positions '(1 6 11)))
         (int-c (make-lsa-test-interval :c 0 20 :use-positions '(2 7 12)))
         (cc    (make-target-desc
                  :name        :x86-64
                  :gpr-names   #(:r0 :r1)
                  :arg-regs    '(:r0)
                  :ret-reg     :r0
                  :fp-arg-regs '()
                  :fp-ret-reg  nil
                  :callee-saved '()
                  :scratch-regs nil))
         (intervals (list int-a int-b int-c)))
    (multiple-value-bind (assignment spill-map spill-count)
        (linear-scan-allocate intervals cc)
      (declare (ignore assignment))
      ;; At least one vreg must have been spilled.
      (expect (>= (hash-table-count spill-map) 1) :to-be-truthy)
      (expect (>= spill-count 1) :to-be-truthy))))
