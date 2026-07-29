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

(it-sequential "lsa-assign-records-assignment-and-inserts-into-active"
  (let* ((state (make-test-lsa-state))
         (interval (make-lsa-test-interval :v 0 10)))
    (cl-cc/regalloc::%lsa-assign state interval :r0)
    (expect (cl-cc/regalloc::interval-phys-reg interval) :to-be :r0)
    (expect (gethash :v (cl-cc/regalloc::lsa-assignment state)) :to-be :r0)
    (expect (member interval (cl-cc/regalloc::lsa-active state) :test #'eq) :to-be-truthy)))

(it-sequential "lsa-assign-keeps-active-list-sorted-by-interval-end"
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 20))
         (int-b (make-lsa-test-interval :b 0  5))
         (int-c (make-lsa-test-interval :c 0 12)))
    (cl-cc/regalloc::%lsa-assign state int-a :r0)
    (cl-cc/regalloc::%lsa-assign state int-b :r1)
    (cl-cc/regalloc::%lsa-assign state int-c :r2)
    (let ((ends (mapcar #'interval-end (cl-cc/regalloc::lsa-active state))))
      (expect (sort (copy-list ends) #'<) :to-equal ends))))

;;; ─── %lsa-try-coalesce ────────────────────────────────────────────────────

(it-sequential "lsa-try-coalesce-succeeds-when-source-ends-at-current-start"
  (let* ((state (make-test-lsa-state :free-regs '(:r0 :r1)))
         (src-int (make-lsa-test-interval :src 0 5))
         (new-int (make-lsa-test-interval :new 5 10 :coalesce-with :src)))
    (setf (cl-cc/regalloc::interval-phys-reg src-int) :r0)
    (setf (gethash :src (cl-cc/regalloc::lsa-interval-map state)) src-int)
    (setf (cl-cc/regalloc::lsa-active state) (list src-int))
    (let ((result (cl-cc/regalloc::%lsa-try-coalesce state new-int)))
      (expect result :to-be-truthy)
      (expect (cl-cc/regalloc::interval-phys-reg new-int) :to-be :r0)
      ;; Source should have been removed from active and replaced by new-int.
      (expect (member src-int (cl-cc/regalloc::lsa-active state) :test #'eq) :to-be-falsy)
      (expect (member new-int (cl-cc/regalloc::lsa-active state) :test #'eq) :to-be-truthy))))

(it-sequential "lsa-try-coalesce-fails source-still-live" (destructuring-bind (src-end new-start new-fp-p coalesce-with setup-src-p) (list 10 5 nil :src t)
    (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (cl-cc/regalloc::interval-phys-reg src-int) :r0)
      (setf (gethash :src (cl-cc/regalloc::lsa-interval-map state)) src-int)
      (setf (cl-cc/regalloc::lsa-active state) (list src-int)))
    (expect (cl-cc/regalloc::%lsa-try-coalesce state new-int) :to-be-null))))

(it-sequential "lsa-try-coalesce-fails no-coalesce-hint"
  (destructuring-bind (src-end new-start new-fp-p coalesce-with setup-src-p) (list 5 5 nil nil nil)
    (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (cl-cc/regalloc::interval-phys-reg src-int) :r0)
      (setf (gethash :src (cl-cc/regalloc::lsa-interval-map state)) src-int)
      (setf (cl-cc/regalloc::lsa-active state) (list src-int)))
    (expect (cl-cc/regalloc::%lsa-try-coalesce state new-int) :to-be-null))))

(it-sequential "lsa-try-coalesce-fails fp-class-mismatch"
  (destructuring-bind (src-end new-start new-fp-p coalesce-with setup-src-p) (list 5 5 t :src t)
    (let* ((state   (make-test-lsa-state))
         (src-int (make-lsa-test-interval :src 0 src-end :fp-p nil))
         (new-int (make-lsa-test-interval :new new-start 15
                                          :coalesce-with coalesce-with
                                          :fp-p new-fp-p)))
    (when setup-src-p
      (setf (cl-cc/regalloc::interval-phys-reg src-int) :r0)
      (setf (gethash :src (cl-cc/regalloc::lsa-interval-map state)) src-int)
      (setf (cl-cc/regalloc::lsa-active state) (list src-int)))
    (expect (cl-cc/regalloc::%lsa-try-coalesce state new-int) :to-be-null))))

;;; ─── %lsa-allocate-from-pool ──────────────────────────────────────────────

(it-sequential "lsa-allocate-from-pool-assigns-and-removes-from-pool"
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
    (expect (null (cl-cc/regalloc::interval-phys-reg interval)) :to-be-falsy)
    (expect (member (cl-cc/regalloc::interval-phys-reg interval) '(:r0 :r1 :r2) :test #'eq) :to-be-truthy)
    (expect (member (cl-cc/regalloc::interval-phys-reg interval)
                          (cl-cc/regalloc::lsa-free-regs state) :test #'eq) :to-be-falsy)))

;;; ─── %lsa-evict-and-assign ────────────────────────────────────────────────

(it-sequential "lsa-evict-and-assign-spills-current-when-worst-candidate"
  (let* ((state (make-test-lsa-state :free-regs nil))
         (interval (make-lsa-test-interval :v 0 10 :use-positions '(2))))
    (setf (cl-cc/regalloc::lsa-active state) nil)
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
      (cl-cc/regalloc::%lsa-evict-and-assign state interval))
    (expect (null (cl-cc/regalloc::interval-spill-slot interval)) :to-be-falsy)
    (expect (> (cl-cc/regalloc::lsa-spill-count state) 0) :to-be-truthy)))

(it-sequential "lsa-evict-and-assign-frees-candidate-and-assigns"
  (let* ((state (make-test-lsa-state :free-regs nil))
         (candidate (make-lsa-test-interval :cand 0 20 :use-positions '(3)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(15))))
    (setf (cl-cc/regalloc::interval-phys-reg candidate) :r0)
    (setf (gethash :cand (cl-cc/regalloc::lsa-assignment state)) :r0)
    (setf (cl-cc/regalloc::lsa-active state) (list candidate))
    (let ((cl-cc/regalloc::*ml-regalloc-enabled* nil))
      (cl-cc/regalloc::%lsa-evict-and-assign state interval))
    (expect (null (cl-cc/regalloc::interval-spill-slot candidate)) :to-be-falsy)
    (expect (cl-cc/regalloc::interval-phys-reg candidate) :to-be-null)
    (expect (cl-cc/regalloc::interval-phys-reg interval) :to-be :r0)))

;;; ─── %lsa-expire-old (boundary + FP-pool cases only) ──────────────────────

(it-sequential "lsa-expire-old-keeps-interval-ending-at-current-start"
  (let* ((touching (make-lsa-test-interval :t 0 5))
         (current  (make-lsa-test-interval :c 5 10))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::interval-phys-reg touching) :r0)
    (setf (cl-cc/regalloc::lsa-active state) (list touching))
    (cl-cc/regalloc::%lsa-expire-old state current)
    (expect (member touching (cl-cc/regalloc::lsa-active state) :test #'eq) :to-be-truthy)))

(it-sequential "lsa-expire-old-returns-fp-register-to-fp-pool"
  (let* ((fp-expired (make-lsa-test-interval :fpe 0 2 :fp-p t))
         (current    (make-lsa-test-interval :c   5 10))
         (state      (make-test-lsa-state :free-regs nil :free-fp-regs nil)))
    (setf (cl-cc/regalloc::interval-phys-reg fp-expired) :xmm5)
    (setf (cl-cc/regalloc::lsa-active state) (list fp-expired))
    (cl-cc/regalloc::%lsa-expire-old state current)
    (expect (member :xmm5 (cl-cc/regalloc::lsa-free-fp-regs state) :test #'eq) :to-be-truthy)
    (expect (member :xmm5 (cl-cc/regalloc::lsa-free-regs state) :test #'eq) :to-be-falsy)))

;;; ─── %lsa-spill-current (distinct-slot case only) ─────────────────────────

(it-sequential "lsa-spill-current-assigns-distinct-slots"
  (let* ((state (make-test-lsa-state))
         (int-a (make-lsa-test-interval :a 0 10))
         (int-b (make-lsa-test-interval :b 5 15)))
    (cl-cc/regalloc::%lsa-spill-current state int-a)
    (cl-cc/regalloc::%lsa-spill-current state int-b)
    (expect (/= (cl-cc/regalloc::interval-spill-slot int-a)
                     (cl-cc/regalloc::interval-spill-slot int-b)) :to-be-truthy)))

;;; ─── %lsa-best-spill-candidate ────────────────────────────────────────────

(it-sequential "lsa-best-spill-candidate-returns-farthest-next-use"
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(20)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(8)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::lsa-active state) (list candidate))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      (expect best :to-be candidate))))

(it-sequential "lsa-best-spill-candidate-returns-self-when-no-farther-use"
  (let* ((candidate (make-lsa-test-interval :cand 0 30 :use-positions '(6)))
         (interval  (make-lsa-test-interval :new  5 25 :use-positions '(20)))
         (state     (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::lsa-active state) (list candidate))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      (expect best :to-be interval))))

(it-sequential "lsa-best-spill-candidate-ml-enabled-returns-lowest-cost"
  (let* ((no-remat   (make-lsa-test-interval :nr 0 10 :use-positions '(1 2 3)))
         (with-remat (make-lsa-test-interval :wr 0 10 :use-positions '(1 2 3)))
         (interval   (make-lsa-test-interval :new 5 15 :use-positions '(6)))
         (state      (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::interval-remat-const with-remat) 42)
    (setf (cl-cc/regalloc::lsa-active state) (list no-remat with-remat))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* t)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      (expect best :to-be with-remat))))

(it-sequential "lsa-best-spill-candidate-ignores-cross-class-intervals"
  (let* ((fp-cand  (make-lsa-test-interval :fp 0 30 :fp-p t :use-positions '(20)))
         (interval (make-lsa-test-interval :new 5 25 :fp-p nil :use-positions '(8)))
         (state    (make-test-lsa-state :free-regs nil)))
    (setf (cl-cc/regalloc::lsa-active state) (list fp-cand))
    (let* ((cl-cc/regalloc::*ml-regalloc-enabled* nil)
           (best (cl-cc/regalloc::%lsa-best-spill-candidate state interval)))
      ;; fp-cand is filtered out; interval is the only candidate → returns itself.
      (expect best :to-be interval))))

;;; ─── %interval-next-use-after ─────────────────────────────────────────────

(it-sequential "lsa-interval-next-use-after finds-first-after" (destructuring-bind (use-positions position expected) (list '(2 5 8 12) 4 5)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (cl-cc/regalloc::%interval-next-use-after interval position) :to-equal expected))))

(it-sequential "lsa-interval-next-use-after returns-nil-none"
  (destructuring-bind (use-positions position expected) (list '(1 2 3) 5 nil)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (cl-cc/regalloc::%interval-next-use-after interval position) :to-equal expected))))

(it-sequential "lsa-interval-next-use-after exact-boundary"
  (destructuring-bind (use-positions position expected) (list '(5 10) 4 5)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (cl-cc/regalloc::%interval-next-use-after interval position) :to-equal expected))))

(it-sequential "lsa-interval-next-use-after nil-on-equal-pos"
  (destructuring-bind (use-positions position expected) (list '(5 10) 5 10)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (cl-cc/regalloc::%interval-next-use-after interval position) :to-equal expected))))

(it-sequential "lsa-interval-next-use-after empty-list"
  (destructuring-bind (use-positions position expected) (list nil 0 nil)
    (let ((interval (make-lsa-test-interval :v 0 20 :use-positions use-positions)))
    (expect (cl-cc/regalloc::%interval-next-use-after interval position) :to-equal expected))))

;;; ─── %return-value-preferred-reg ──────────────────────────────────────────

(it-sequential "lsa-return-value-preferred-reg gpr-rv-in-pool" (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list nil t :rax :xmm0 '(:rax :rbx :rcx) :rax)
    (let* ((cc       (make-lsa-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) return-value-p)
    (expect (cl-cc/regalloc::%return-value-preferred-reg interval cc free-regs) :to-be expected))))

(it-sequential "lsa-return-value-preferred-reg not-return-value"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list nil nil :rax :xmm0 '(:rax :rbx) nil)
    (let* ((cc       (make-lsa-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) return-value-p)
    (expect (cl-cc/regalloc::%return-value-preferred-reg interval cc free-regs) :to-be expected))))

(it-sequential "lsa-return-value-preferred-reg ret-reg-not-in-pool"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list nil t :rax :xmm0 '(:rbx :rcx) nil)
    (let* ((cc       (make-lsa-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) return-value-p)
    (expect (cl-cc/regalloc::%return-value-preferred-reg interval cc free-regs) :to-be expected))))

(it-sequential "lsa-return-value-preferred-reg fp-rv-in-pool"
  (destructuring-bind (fp-p return-value-p ret-reg fp-ret-reg free-regs expected) (list t t :rax :xmm0 '(:xmm0 :xmm1) :xmm0)
    (let* ((cc       (make-lsa-minimal-cc :ret-reg ret-reg :fp-ret-reg fp-ret-reg))
         (interval (make-lsa-test-interval :v 0 10 :fp-p fp-p)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) return-value-p)
    (expect (cl-cc/regalloc::%return-value-preferred-reg interval cc free-regs) :to-be expected))))

;;; ─── %call-crossing-preferred-reg ─────────────────────────────────────────

(it-sequential "lsa-call-crossing-preferred-reg-prefers-callee-saved"
  (let* ((cc        (make-lsa-minimal-cc :callee-saved '(:rbx :r12)))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rdi :rbx :r12)))
    (let ((result (cl-cc/regalloc::%call-crossing-preferred-reg interval cc free-regs)))
      (expect (member result '(:rbx :r12) :test #'eq) :to-be-truthy))))

(it-sequential "lsa-call-crossing-preferred-reg-nil-for-non-call-crossing"
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rdi :rbx)))
    (expect (cl-cc/regalloc::%call-crossing-preferred-reg interval cc free-regs) :to-be-null)))

(it-sequential "lsa-call-crossing-preferred-reg-nil-for-fp"
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p t :fp-p t))
         (free-regs '(:xmm0)))
    (expect (cl-cc/regalloc::%call-crossing-preferred-reg interval cc free-regs) :to-be-null)))

;;; ─── %param-preferred-reg ─────────────────────────────────────────────────

(it-sequential "lsa-param-preferred-reg-returns-matching-arg-reg"
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rdx)))
    (setf (cl-cc/regalloc::interval-parameter-index interval) 1)
    (expect (cl-cc/regalloc::%param-preferred-reg interval cc free-regs) :to-be :rsi)))

(it-sequential "lsa-param-preferred-reg-nil-when-index-out-of-range"
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi)))
    (setf (cl-cc/regalloc::interval-parameter-index interval) 5)
    (expect (cl-cc/regalloc::%param-preferred-reg interval cc free-regs) :to-be-null)))

(it-sequential "lsa-param-preferred-reg-nil-when-not-in-free-pool"
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi :rdx)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rsi :rdx)))       ; :rdi not available
    (setf (cl-cc/regalloc::interval-parameter-index interval) 0)
    (expect (cl-cc/regalloc::%param-preferred-reg interval cc free-regs) :to-be-null)))

;;; ─── %hint-policy-preferred-reg ───────────────────────────────────────────

(it-sequential "lsa-hint-policy-preferred-reg-returns-caller-saved-when-preferred"
  (let* ((cc        (make-lsa-minimal-cc :callee-saved '(:rbx) :caller-saved '(:rax :rcx)))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p nil))
         (free-regs '(:rax :rbx :rcx))
         (cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (let ((result (cl-cc/regalloc::%hint-policy-preferred-reg interval cc free-regs)))
      (expect (null result) :to-be-falsy)
      (expect (member result (cl-cc/target::target-caller-saved cc) :test #'eq) :to-be-truthy))))

(it-sequential "lsa-hint-policy-preferred-reg-nil-when-not-preferred"
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx))
         (cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p nil)))
    (expect (cl-cc/regalloc::%hint-policy-preferred-reg interval cc free-regs) :to-be-null)))

(it-sequential "lsa-hint-policy-preferred-reg-nil-for-call-crossing"
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10 :crosses-call-p t))
         (free-regs '(:rax :rbx))
         (cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p t)))
    (expect (cl-cc/regalloc::%hint-policy-preferred-reg interval cc free-regs) :to-be-null)))

;;; ─── regalloc-target-fp-registers ─────────────────────────────────────────

(it-sequential "lsa-target-fp-registers-x86-64-returns-16-xmm"
  (let* ((cc      (make-lsa-minimal-cc :name :x86-64))
         (fp-regs (cl-cc/regalloc::regalloc-target-fp-registers cc)))
    (expect (= 16 (length fp-regs)) :to-be-truthy)
    (expect (every (lambda (r) (member r fp-regs :test #'eq))
                        '(:xmm0 :xmm7 :xmm15)) :to-be-truthy)))

(it-sequential "lsa-target-fp-registers-aarch64-returns-32-v"
  (let* ((cc      (make-lsa-minimal-cc :name :aarch64))
         (fp-regs (cl-cc/regalloc::regalloc-target-fp-registers cc)))
    (expect (= 32 (length fp-regs)) :to-be-truthy)
    (expect (every (lambda (r) (member r fp-regs :test #'eq))
                        '(:v0 :v15 :v31)) :to-be-truthy)))

(it-sequential "lsa-target-fp-registers-unknown-falls-back-to-union"
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
    (expect (= 2 (length fp-regs)) :to-be-truthy)
    (expect (member :f0 fp-regs :test #'eq) :to-be-truthy)
    (expect (member :f1 fp-regs :test #'eq) :to-be-truthy)))

;;; ─── %preferred-register-for-interval strategy priority order ─────────────

(it-sequential "lsa-preferred-register-hint-policy-wins"
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
        (expect (null result) :to-be-falsy)
        (expect (member result (cl-cc/target::target-caller-saved cc) :test #'eq) :to-be-truthy)))))

(it-sequential "lsa-preferred-register-return-value-wins-when-hint-nil"
  (let* ((cc        (make-lsa-minimal-cc :ret-reg :rax))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rax :rbx)))
    (setf (cl-cc/regalloc::interval-return-value-p interval) t)
    (let ((cl-cc/regalloc::*current-allocation-policy* '(:prefer-caller-saved-p nil)))
      (expect (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs) :to-be :rax))))

(it-sequential "lsa-preferred-register-param-wins-when-others-nil"
  (let* ((cc        (make-lsa-minimal-cc :arg-regs '(:rdi :rsi)))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rdi :rsi :rbx)))
    (setf (cl-cc/regalloc::interval-parameter-index interval) 0)
    (let ((cl-cc/regalloc::*current-allocation-policy* nil))
      (expect (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs) :to-be :rdi))))

(it-sequential "lsa-preferred-register-nil-when-no-strategy-fires"
  (let* ((cc        (make-lsa-minimal-cc))
         (interval  (make-lsa-test-interval :v 0 10))
         (free-regs '(:rbx)))
    (let ((cl-cc/regalloc::*current-allocation-policy* nil))
      (expect (cl-cc/regalloc::%preferred-register-for-interval interval cc free-regs) :to-be-null))))

;;; ─── %allocation-strategy ─────────────────────────────────────────────────

(it-sequential "lsa-allocation-strategy-dispatch color-via-allocator" (destructuring-bind (policy expected-strategy) (list '(:allocator :color) :color)
    (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :linear-scan))
    (expect (cl-cc/regalloc::%allocation-strategy policy) :to-be expected-strategy))))

(it-sequential "lsa-allocation-strategy-dispatch lscan-via-allocator"
  (destructuring-bind (policy expected-strategy) (list '(:allocator :linear-scan) :linear-scan)
    (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :linear-scan))
    (expect (cl-cc/regalloc::%allocation-strategy policy) :to-be expected-strategy))))

(it-sequential "lsa-allocation-strategy-dispatch color-via-alt-key"
  (destructuring-bind (policy expected-strategy) (list '(:register-allocator :color) :color)
    (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :linear-scan))
    (expect (cl-cc/regalloc::%allocation-strategy policy) :to-be expected-strategy))))

(it-sequential "lsa-allocation-strategy-dispatch nil-policy-default"
  (destructuring-bind (policy expected-strategy) (list nil :linear-scan)
    (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :linear-scan))
    (expect (cl-cc/regalloc::%allocation-strategy policy) :to-be expected-strategy))))

(it-sequential "lsa-allocation-strategy-falls-back-to-default"
  (let ((cl-cc/regalloc::*regalloc-allocation-strategy* :color))
    (expect (cl-cc/regalloc::%allocation-strategy '()) :to-be :color)))

;;; ─── %derive-single-function-policy ───────────────────────────────────────

(it-sequential "lsa-derive-single-function-policy-nonnil-for-single-function"
  (let ((instructions (list (make-vm-label :name "entry")
                            (make-vm-const :dst :r0 :value 42)
                            (make-vm-ret  :reg :r0))))
    (expect (null (cl-cc/regalloc::%derive-single-function-policy instructions)) :to-be-falsy)))

(it-sequential "lsa-derive-single-function-policy-nil-for-multi-function"
  (let ((instructions (list (make-vm-label :name "f1")
                            (make-vm-const :dst :r0 :value 1)
                            (make-vm-ret  :reg :r0)
                            (make-vm-label :name "f2")
                            (make-vm-const :dst :r0 :value 2)
                            (make-vm-ret  :reg :r0))))
    (expect (cl-cc/regalloc::%derive-single-function-policy instructions) :to-be-null)))

;;; ─── linear-scan-allocate integration ─────────────────────────────────────

(it-sequential "lsa-linear-scan-allocate-assigns-non-overlapping-intervals"
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
      (expect (hash-table-p assignment) :to-be-truthy)
      (expect (hash-table-p spill-map) :to-be-truthy)
      (expect (integerp spill-count) :to-be-truthy)
      (expect (gethash :a assignment) :to-be-truthy)
      (expect (gethash :b assignment) :to-be-truthy)
      (expect (= 0 (hash-table-count spill-map)) :to-be-truthy))))

(it-sequential "lsa-linear-scan-allocate-spills-under-pressure"
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
      (expect (>= (hash-table-count spill-map) 1) :to-be-truthy)
      (expect (>= spill-count 1) :to-be-truthy))))
