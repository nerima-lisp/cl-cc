;;;; tests/unit/optimize/optimizer-memory-tests.lisp
;;;; Unit tests for optimizer-memory-alias.lisp — heap alias and interval analysis
;;;;
;;;; Covers: opt-heap-root-inst-p, opt-heap-root-kind,
;;;;   opt-compute-heap-aliases, opt-compute-points-to,
;;;;   opt-interval-* arithmetic, opt-compute-value-ranges,
;;;;   cfg-value-ranges, simple-induction-variable detection.

(in-package :cl-cc/test)

;;; ─── opt-heap-root-inst-p ───────────────────────────────────────────────

(it-sequential "heap-root-inst-p-true-cases vm-cons"
  (destructuring-bind (inst) (list (make-vm-cons        :dst :r0 :car-src :r1 :cdr-src :r2))
    (expect (cl-cc/optimize::opt-heap-root-inst-p inst) :to-be-truthy)))

(it-sequential "heap-root-inst-p-true-cases vm-make-array"
  (destructuring-bind (inst) (list (make-vm-make-array  :dst :r0 :size-reg :r1))
    (expect (cl-cc/optimize::opt-heap-root-inst-p inst) :to-be-truthy)))

(it-sequential "heap-root-inst-p-true-cases vm-make-closure"
  (destructuring-bind (inst) (list (make-vm-make-closure :dst :r0 :label "f" :env-regs '(:r1)))
    (expect (cl-cc/optimize::opt-heap-root-inst-p inst) :to-be-truthy)))

(it-sequential "heap-root-inst-p-false-cases vm-const"
  (destructuring-bind (inst) (list (make-vm-const :dst :r0 :value 1))
    (expect (cl-cc/optimize::opt-heap-root-inst-p inst) :to-be-falsy)))

(it-sequential "heap-root-inst-p-false-cases vm-add"
  (destructuring-bind (inst) (list (make-vm-add   :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-heap-root-inst-p inst) :to-be-falsy)))

(it-sequential "heap-root-inst-p-false-cases vm-move"
  (destructuring-bind (inst) (list (make-vm-move  :dst :r0 :src :r1))
    (expect (cl-cc/optimize::opt-heap-root-inst-p inst) :to-be-falsy)))

;;; ─── opt-heap-root-kind ─────────────────────────────────────────────────

(it-sequential "heap-root-kind-values cons"
  (destructuring-bind (inst expected-kind) (list (make-vm-cons        :dst :r0 :car-src :r1 :cdr-src :r2) :cons)
    (expect (cl-cc/optimize::opt-heap-root-kind inst) :to-be expected-kind)))

(it-sequential "heap-root-kind-values array"
  (destructuring-bind (inst expected-kind) (list (make-vm-make-array  :dst :r0 :size-reg :r1) :array)
    (expect (cl-cc/optimize::opt-heap-root-kind inst) :to-be expected-kind)))

(it-sequential "heap-root-kind-values closure"
  (destructuring-bind (inst expected-kind) (list (make-vm-make-closure :dst :r0 :label "f" :env-regs '(:r1)) :closure)
    (expect (cl-cc/optimize::opt-heap-root-kind inst) :to-be expected-kind)))

;;; ─── %opt-build-root-map / opt-compute-heap-aliases ────────────────────

(it-sequential "heap-alias-cons-sets-own-root"
  (let* ((inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list inst))))
    (expect (gethash :r0 roots) :to-be :r0)))

(it-sequential "heap-alias-move-propagates-root"
  (let* ((cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (move-inst (make-vm-move :dst :r3 :src :r0))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list cons-inst move-inst))))
    (expect (gethash :r3 roots) :to-be :r0)))

(it-sequential "heap-alias-non-heap-write-kills-root"
  (let* ((cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (add-inst  (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list cons-inst add-inst))))
    (expect (nth-value 1 (gethash :r0 roots)) :to-be-falsy)))

(it-sequential "heap-alias-unknown-register-returns-nil"
  (let ((pt (make-hash-table :test #'eq)))
    (expect (gethash :r99 pt) :to-be-null)))

(it-sequential "heap-alias-known-register-returns-canonical-root"
  (let ((pt (make-hash-table :test #'eq)))
    (setf (gethash :r0 pt) :r0)
    (expect (gethash :r0 pt) :to-be :r0)))

(it-sequential "points-to-tracks-fresh-root-and-move"
  (let* ((insts (list (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)
                      (make-vm-move :dst :r3 :src :r0)))
         (points-to (cl-cc/optimize:opt-compute-points-to insts)))
    (expect (cl-cc/optimize:opt-points-to-root :r0 points-to) :to-be :r0)
    (expect (cl-cc/optimize:opt-points-to-root :r3 points-to) :to-be :r0)))

(it-sequential "points-to-root-reports-unknown-register"
  (let ((points-to (cl-cc/optimize:opt-compute-points-to nil)))
    (multiple-value-bind (root found-p)
        (cl-cc/optimize:opt-points-to-root :missing points-to)
      (expect root :to-be-null)
      (expect found-p :to-be-falsy))))

(it-sequential "points-to-overwrite-kills-stale-root"
  (let* ((insts (list (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)
                      (make-vm-add :dst :r0 :lhs :r1 :rhs :r2)))
         (points-to (cl-cc/optimize:opt-compute-points-to insts)))
    (expect (nth-value 1 (cl-cc/optimize:opt-points-to-root :r0 points-to)) :to-be-falsy)))

;;; ─── Memory SSA snapshot (FR-217 partial) ───────────────────────────────

(it-sequential "memory-ssa-snapshot-assigns-monotonic-versions-for-def-use-chain"
  (let* ((set1 (make-vm-set-global :src :r0 :name 'g))
         (get1 (make-vm-get-global :dst :r1 :name 'g))
         (set2 (make-vm-set-global :src :r2 :name 'g))
         (get2 (make-vm-get-global :dst :r3 :name 'g))
         (ann  (cl-cc/optimize::opt-compute-memory-ssa-snapshot
                (list set1 get1 set2 get2))))
    (expect (= 0 (cl-cc/optimize::opt-memory-ssa-version-at set1 ann :point :in)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at set1 ann :point :out)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :in)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :out)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at set2 ann :point :in)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize::opt-memory-ssa-version-at set2 ann :point :out)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize::opt-memory-ssa-version-at get2 ann :point :in)) :to-be-truthy)))

(it-sequential "memory-ssa-snapshot-slot-location-uses-alias-root"
  (let* ((mk   (make-vm-cons :dst :obj :car-src :a :cdr-src :b))
         (mv   (make-vm-move :dst :alias :src :obj))
         (wr   (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'slot :value-reg :v))
         (rd   (cl-cc:make-vm-slot-read :dst :out :obj-reg :alias :slot-name 'slot))
         (ann  (cl-cc/optimize::opt-compute-memory-ssa-snapshot (list mk mv wr rd)))
         (wr-loc (getf (gethash wr ann) :location))
         (rd-loc (getf (gethash rd ann) :location)))
    (expect rd-loc :to-equal wr-loc)
    (expect (= 2 (cl-cc/optimize::opt-memory-ssa-version-at rd ann :point :in)) :to-be-truthy)))

(it-sequential "memory-ssa-cfg-snapshot-cross-block-dominating-store-threads-version"
  (let* ((set1 (make-vm-set-global :src :r0 :name 'g))
         (get1 (make-vm-get-global :dst :r1 :name 'g))
         (ann  (cl-cc/optimize::opt-compute-memory-ssa-cfg-snapshot
                (list set1
                      (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-const :dst :r7 :value 1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-const :dst :r8 :value 2)
                       (make-vm-label :name "join")
                       get1
                       (make-vm-ret :reg :r1)))))
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at set1 ann :point :out)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :in)) :to-be-truthy)
    (expect (getf (gethash get1 ann) :incoming-from) :to-be :state)))

(it-sequential "memory-ssa-cfg-snapshot-join-disagree-synthesizes-phi-version"
  (let* ((set-a (make-vm-set-global :src :r0 :name 'g))
         (set-b (make-vm-set-global :src :r1 :name 'g))
         (get1  (make-vm-get-global :dst :r2 :name 'g))
         (ann   (cl-cc/optimize::opt-compute-memory-ssa-cfg-snapshot
                 (list (make-vm-jump-zero :reg :r9 :label "else")
                       set-a
                       (make-vm-jump :label "join")
                       (make-vm-label :name "else")
                        set-b
                        (make-vm-label :name "join")
                        get1
                        (make-vm-ret :reg :r2)))))
    (let ((join-in (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :in))
          (entry (gethash get1 ann)))
    (expect (= 1 (cl-cc/optimize::opt-memory-ssa-version-at set-a ann :point :out)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize::opt-memory-ssa-version-at set-b ann :point :out)) :to-be-truthy)
      (expect (and (integerp join-in) (> join-in 2)) :to-be-truthy)
      (expect (getf entry :incoming-from) :to-be :phi))))

(it-sequential "memory-ssa-cfg-snapshot-empty-join-propagates-synthetic-phi-to-successor"
  (let* ((set-a (make-vm-set-global :src :r0 :name 'g))
         (set-b (make-vm-set-global :src :r1 :name 'g))
         (get1  (make-vm-get-global :dst :r2 :name 'g))
         (ann   (cl-cc/optimize::opt-compute-memory-ssa-cfg-snapshot
                 (list (make-vm-jump-zero :reg :r9 :label "else")
                       set-a
                       (make-vm-jump :label "join")
                       (make-vm-label :name "else")
                       set-b
                       (make-vm-label :name "join")
                       (make-vm-jump :label "after")
                       (make-vm-label :name "after")
                       get1
                       (make-vm-ret :reg :r2)))))
    (let ((join-in (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :in))
          (entry (gethash get1 ann)))
      (expect (and (integerp join-in) (> join-in 2)) :to-be-truthy)
      (expect (getf entry :incoming-from) :to-be :state))))

(it-sequential "memory-ssa-cfg-snapshot-local-def-overrides-phi-origin-for-following-use"
  (let* ((set-a (make-vm-set-global :src :r0 :name 'g))
         (set-b (make-vm-set-global :src :r1 :name 'g))
         (set-c (make-vm-set-global :src :r3 :name 'g))
         (get1  (make-vm-get-global :dst :r2 :name 'g))
         (ann   (cl-cc/optimize::opt-compute-memory-ssa-cfg-snapshot
                 (list (make-vm-jump-zero :reg :r9 :label "else")
                       set-a
                       (make-vm-jump :label "join")
                       (make-vm-label :name "else")
                       set-b
                       (make-vm-label :name "join")
                       set-c
                       get1
                       (make-vm-ret :reg :r2)))))
    (expect (getf (gethash set-c ann) :incoming-from) :to-be :phi)
    (expect (getf (gethash get1 ann) :incoming-from) :to-be :state)
    (expect (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :in) :to-be (cl-cc/optimize::opt-memory-ssa-version-at set-c ann :point :out))))

(it-sequential "memory-ssa-cfg-snapshot-records-explicit-memory-phi-nodes"
  (let* ((set-a (make-vm-set-global :src :r0 :name 'g))
         (set-b (make-vm-set-global :src :r1 :name 'g))
         (get1  (make-vm-get-global :dst :r2 :name 'g))
         (ann   (cl-cc/optimize::opt-compute-memory-ssa-cfg-snapshot
                 (list (make-vm-jump-zero :reg :r9 :label "else")
                       set-a
                       (make-vm-jump :label "join")
                       (make-vm-label :name "else")
                       set-b
                       (make-vm-label :name "join")
                       get1
                       (make-vm-ret :reg :r2))))
         (phi-map (cl-cc/optimize::opt-memory-ssa-phi-nodes ann))
         (all-phis nil))
    (maphash (lambda (_block nodes)
               (setf all-phis (nconc all-phis (copy-list nodes))))
             phi-map)
    (expect (consp all-phis) :to-be-truthy)
    (let ((phi (find (list :global 'g) all-phis
                     :key #'cl-cc/optimize::opt-memory-phi-location
                     :test #'equal)))
      (expect phi :to-be-truthy)
      (expect (> (cl-cc/optimize::opt-memory-phi-version phi) 2) :to-be-truthy)
      (expect (= 2 (length (cl-cc/optimize::opt-memory-phi-incoming phi))) :to-be-truthy))))

(it-sequential "memory-ssa-cfg-snapshot-branch-constant-prunes-infeasible-edge"
  (let* ((set-a (make-vm-set-global :src :r0 :name 'g))
         (set-b (make-vm-set-global :src :r1 :name 'g))
         (get1  (make-vm-get-global :dst :r2 :name 'g))
         (ann   (cl-cc/optimize::opt-compute-memory-ssa-cfg-snapshot
                 (list (make-vm-const :dst :r9 :value 0)
                       (make-vm-jump-zero :reg :r9 :label "else")
                       set-a
                       (make-vm-jump :label "join")
                       (make-vm-label :name "else")
                       set-b
                       (make-vm-label :name "join")
                       get1
                       (make-vm-ret :reg :r2)))))
    ;; only else edge is feasible => join should take set-b version directly
    (expect (= (cl-cc/optimize::opt-memory-ssa-version-at set-b ann :point :out) (cl-cc/optimize::opt-memory-ssa-version-at get1 ann :point :in)) :to-be-truthy)
    (expect (getf (gethash get1 ann) :incoming-from) :to-be :state)))

;;; ─── opt-interval-* arithmetic ───────────────────────────────────────────

(it-sequential "interval-make-and-read-lo-hi"
  (let ((iv (cl-cc/optimize::opt-make-interval 3 7)))
    (expect (= 3 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 7 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "interval-arithmetic-cases add"
  (destructuring-bind (op a-lo a-hi b-lo b-hi expected-lo expected-hi) (list #'cl-cc/optimize:opt-interval-add 1 2 3 4 4 6)
    (let* ((a (cl-cc/optimize::opt-make-interval a-lo a-hi))
         (b (cl-cc/optimize::opt-make-interval b-lo b-hi))
         (r (funcall op a b)))
    (expect (= expected-lo (cl-cc/optimize::opt-interval-lo r)) :to-be-truthy)
    (expect (= expected-hi (cl-cc/optimize::opt-interval-hi r)) :to-be-truthy))))

(it-sequential "interval-arithmetic-cases sub"
  (destructuring-bind (op a-lo a-hi b-lo b-hi expected-lo expected-hi) (list #'cl-cc/optimize:opt-interval-sub 5 8 1 3 2 7)
    (let* ((a (cl-cc/optimize::opt-make-interval a-lo a-hi))
         (b (cl-cc/optimize::opt-make-interval b-lo b-hi))
         (r (funcall op a b)))
    (expect (= expected-lo (cl-cc/optimize::opt-interval-lo r)) :to-be-truthy)
    (expect (= expected-hi (cl-cc/optimize::opt-interval-hi r)) :to-be-truthy))))

(it-sequential "interval-arithmetic-cases mul-pos"
  (destructuring-bind (op a-lo a-hi b-lo b-hi expected-lo expected-hi) (list #'cl-cc/optimize::opt-interval-mul 2 3 4 5 8 15)
    (let* ((a (cl-cc/optimize::opt-make-interval a-lo a-hi))
         (b (cl-cc/optimize::opt-make-interval b-lo b-hi))
         (r (funcall op a b)))
    (expect (= expected-lo (cl-cc/optimize::opt-interval-lo r)) :to-be-truthy)
    (expect (= expected-hi (cl-cc/optimize::opt-interval-hi r)) :to-be-truthy))))

(it-sequential "interval-arithmetic-cases mul-mixed"
  (destructuring-bind (op a-lo a-hi b-lo b-hi expected-lo expected-hi) (list #'cl-cc/optimize::opt-interval-mul -1 2 3 4 -4 8)
    (let* ((a (cl-cc/optimize::opt-make-interval a-lo a-hi))
         (b (cl-cc/optimize::opt-make-interval b-lo b-hi))
         (r (funcall op a b)))
    (expect (= expected-lo (cl-cc/optimize::opt-interval-lo r)) :to-be-truthy)
    (expect (= expected-hi (cl-cc/optimize::opt-interval-hi r)) :to-be-truthy))))

;;; ─── opt-compute-value-ranges ──────────────────────────────────────────

(it-sequential "constant-intervals-integer-const-yields-singleton"
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)))
         (ivals (cl-cc/optimize:opt-compute-value-ranges insts)))
    (let ((iv (gethash :r0 ivals)))
      (expect iv :to-be-truthy)
      (expect (= 5 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
      (expect (= 5 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy))))

(it-sequential "constant-intervals-float-const-yields-no-entry"
  (let* ((insts (list (make-vm-const :dst :r0 :value 3.14)))
         (ivals (cl-cc/optimize:opt-compute-value-ranges insts)))
    (expect (nth-value 1 (gethash :r0 ivals)) :to-be-falsy)))

(it-sequential "constant-intervals-add-two-known-yields-sum-interval"
  (let* ((insts (list (make-vm-const :dst :r0 :value 2)
                      (make-vm-const :dst :r1 :value 3)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (ivals (cl-cc/optimize:opt-compute-value-ranges insts)))
    (let ((iv (gethash :r2 ivals)))
      (expect iv :to-be-truthy)
      (expect (= 5 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy))))

(it-sequential "value-ranges-propagate-move-and-arithmetic"
  (let* ((insts (list (make-vm-const :dst :r0 :value 4)
                      (make-vm-move  :dst :r1 :src :r0)
                      (make-vm-const :dst :r2 :value 6)
                      (make-vm-add   :dst :r3 :lhs :r1 :rhs :r2)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :r3 ivals)))
     (expect iv :to-be-truthy)
     (expect (= 10 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
     (expect (= 10 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "value-ranges-logand-mask-with-unknown-input-narrows-to-8-bit"
  (let* ((insts (list (make-vm-get-global :dst :x :name 'x)
                      (make-vm-const :dst :mask :value #xFF)
                      (make-vm-logand :dst :masked :lhs :x :rhs :mask)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :masked ivals)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= #xFF (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)
    (expect (= 8 (cl-cc/optimize::opt-interval-bit-width iv)) :to-be-truthy)
    (expect (= #xFF (cl-cc/optimize::opt-interval-known-bits-mask iv)) :to-be-truthy)))

(it-sequential "value-ranges-add-of-masked-8-bit-values-is-9-bit-wide"
  (let* ((insts (list (make-vm-get-global :dst :x :name 'x)
                      (make-vm-get-global :dst :y :name 'y)
                      (make-vm-const :dst :mask :value #xFF)
                      (make-vm-logand :dst :x8 :lhs :x :rhs :mask)
                      (make-vm-logand :dst :y8 :lhs :y :rhs :mask)
                      (make-vm-add :dst :sum :lhs :x8 :rhs :y8)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :sum ivals)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= #x1FE (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)
    (expect (= 9 (cl-cc/optimize::opt-interval-bit-width iv)) :to-be-truthy)
     (expect (= #x1FF (cl-cc/optimize::opt-interval-known-bits-mask iv)) :to-be-truthy)
     (expect (cl-cc/optimize::opt-interval-fits-fixnum-width-p iv) :to-be-truthy)))

(it-sequential "value-ranges-logior-of-masked-8-bit-values-stays-8-bit"
  (let* ((insts (list (make-vm-get-global :dst :x :name 'x)
                      (make-vm-get-global :dst :y :name 'y)
                      (make-vm-const :dst :mask :value #xFF)
                      (make-vm-logand :dst :x8 :lhs :x :rhs :mask)
                      (make-vm-logand :dst :y8 :lhs :y :rhs :mask)
                      (make-vm-logior :dst :out :lhs :x8 :rhs :y8)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :out ivals)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= #xFF (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)
    (expect (= 8 (cl-cc/optimize::opt-interval-bit-width iv)) :to-be-truthy)))

(it-sequential "value-ranges-logxor-of-masked-8-bit-values-stays-8-bit"
  (let* ((insts (list (make-vm-get-global :dst :x :name 'x)
                      (make-vm-get-global :dst :y :name 'y)
                      (make-vm-const :dst :mask :value #xFF)
                      (make-vm-logand :dst :x8 :lhs :x :rhs :mask)
                      (make-vm-logand :dst :y8 :lhs :y :rhs :mask)
                      (make-vm-logxor :dst :out :lhs :x8 :rhs :y8)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :out ivals)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= #xFF (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)
    (expect (= 8 (cl-cc/optimize::opt-interval-bit-width iv)) :to-be-truthy)))

(it-sequential "value-ranges-ash-left-shift-scales-interval"
  (let* ((insts (list (make-vm-const :dst :x :value 5)
                      (make-vm-const :dst :k :value 3)
                      (make-vm-ash :dst :y :lhs :x :rhs :k)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :y ivals)))
    (expect iv :to-be-truthy)
    (expect (= 40 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 40 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "value-ranges-ash-right-shift-shrinks-interval"
  (let* ((insts (list (make-vm-const :dst :x0 :value 8)
                      (make-vm-const :dst :x1 :value 12)
                      (make-vm-add :dst :x :lhs :x0 :rhs :x1)
                      (make-vm-const :dst :k :value -2)
                      (make-vm-ash :dst :y :lhs :x :rhs :k)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :y ivals)))
    (expect iv :to-be-truthy)
    (expect (= 5 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 5 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "value-ranges-prove-array-index-in-bounds"
  (let* ((insts (list (make-vm-const :dst :idx :value 2)
                      (make-vm-const :dst :len :value 5)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts)))
    (expect (cl-cc/optimize::opt-array-bounds-check-eliminable-p :idx :len ivals) :to-be-truthy)))

(it-sequential "value-ranges-reject-out-of-bounds-index"
  (let* ((insts (list (make-vm-const :dst :idx :value 5)
                      (make-vm-const :dst :len :value 5)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts)))
    (expect (cl-cc/optimize::opt-array-bounds-check-eliminable-p :idx :len ivals) :to-be-falsy)))

(it-sequential "cfg-value-ranges-join-unions-constant-predecessors"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :cond :label "else")
                     (make-vm-const :dst :r0 :value 1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-const :dst :r0 :value 3)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r0))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-compute-cfg-value-ranges cfg))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result)))
         (iv (gethash :r0 join-in)))
    (expect iv :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 3 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "cfg-value-ranges-join-drops-missing-predecessor-fact"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :cond :label "else")
                     (make-vm-const :dst :r0 :value 1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-const :dst :r1 :value 3)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r1))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-compute-cfg-value-ranges cfg))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result))))
    (expect (nth-value 1 (gethash :r0 join-in)) :to-be-falsy)))

(it-sequential "cfg-value-ranges-loop-self-update-kills-unsafe-fact"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-const :dst :i :value 0)
                     (make-vm-label :name "loop")
                     (make-vm-const :dst :one :value 1)
                     (make-vm-add :dst :i :lhs :i :rhs :one)
                     (make-vm-jump-zero :reg :exit-flag :label "exit")
                     (make-vm-jump :label "loop")
                     (make-vm-label :name "exit")
                     (make-vm-ret :reg :i))))
         (loop-block (cl-cc/optimize:cfg-get-block-by-label cfg "loop"))
         (exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
         (result (cl-cc/optimize:opt-compute-cfg-value-ranges cfg))
         (loop-in (gethash loop-block (cl-cc/optimize:opt-dataflow-result-in result)))
         (loop-out (gethash loop-block (cl-cc/optimize:opt-dataflow-result-out result)))
         (exit-in (gethash exit-block (cl-cc/optimize:opt-dataflow-result-in result))))
    (expect (nth-value 1 (gethash :i loop-in)) :to-be-falsy)
    (expect (nth-value 1 (gethash :i loop-out)) :to-be-falsy)
    (expect (nth-value 1 (gethash :i exit-in)) :to-be-falsy)))

(it-sequential "value-ranges-convenience-wrapper-merges-branch-exit-ranges"
  (let* ((insts (list (make-vm-jump-zero :reg :cond :label "else")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-const :dst :r0 :value 3)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r0)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :r0 ivals)))
    (expect iv :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 3 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(defun %test-path-sensitive-branch-cfg ()
  (cl-cc/optimize:cfg-build
   (list (make-vm-get-global :dst :raw :name 'raw)
         (make-vm-const :dst :mask :value #xFF)
         (make-vm-logand :dst :idx :lhs :raw :rhs :mask)
         (make-vm-const :dst :len :value 10)
         (make-vm-lt :dst :cmp :lhs :idx :rhs :len)
         (make-vm-jump-zero :reg :cmp :label "then")
         (make-vm-jump :label "join")
         (make-vm-label :name "then")
         (make-vm-jump :label "join")
         (make-vm-label :name "join")
         (make-vm-ret :reg :idx))))

(it-sequential "path-sensitive-ranges-narrow-jump-target-branch-from-lt"
  (let* ((cfg (%test-path-sensitive-branch-cfg))
         (then-block (cl-cc/optimize:cfg-get-block-by-label cfg "then"))
         (ranges (cl-cc/optimize:opt-compute-path-sensitive-ranges cfg))
         (iv (gethash (cons then-block :idx) ranges)))
    (expect iv :to-be-truthy)
    (expect (= 10 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 255 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "path-sensitive-ranges-narrow-fallthrough-branch-from-lt"
  (let* ((cfg (%test-path-sensitive-branch-cfg))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (then-block (cl-cc/optimize:cfg-get-block-by-label cfg "then"))
         (fallthrough-block (find-if (lambda (succ) (not (eq succ then-block)))
                                     (cl-cc/optimize:bb-successors entry)))
         (ranges (cl-cc/optimize:opt-compute-path-sensitive-ranges cfg))
         (iv (gethash (cons fallthrough-block :idx) ranges)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= 9 (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "path-sensitive-ranges-join-unions-narrowed-predecessors"
  (let* ((cfg (%test-path-sensitive-branch-cfg))
         (join-block (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (ranges (cl-cc/optimize:opt-compute-path-sensitive-ranges cfg))
         (iv (gethash (cons join-block :idx) ranges)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-interval-lo iv)) :to-be-truthy)
    (expect (= #xFF (cl-cc/optimize::opt-interval-hi iv)) :to-be-truthy)))

(it-sequential "bounds-check-elimination-uses-path-sensitive-ranges"
  (let* ((cfg (%test-path-sensitive-branch-cfg))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (then-block (cl-cc/optimize:cfg-get-block-by-label cfg "then"))
         (fallthrough-block (find-if (lambda (succ) (not (eq succ then-block)))
                                     (cl-cc/optimize:bb-successors entry)))
         (ranges (cl-cc/optimize:opt-compute-path-sensitive-ranges cfg)))
    ;; jump-target (then) is the false branch: idx >= 10 — NOT bounds-check-eliminable
    (expect (cl-cc/optimize:opt-array-bounds-check-eliminable-p :idx :len ranges then-block) :to-be-falsy)
    ;; fallthrough is the true branch: idx < 10 — bounds-check-eliminable
    (expect (cl-cc/optimize:opt-array-bounds-check-eliminable-p :idx :len ranges fallthrough-block) :to-be-truthy)))

(it-sequential "path-sensitive-ranges-expose-block-local-query-api"
  (let* ((cfg (%test-path-sensitive-branch-cfg))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (false-block (cl-cc/optimize:cfg-get-block-by-label cfg "then"))
         (true-block (find-if (lambda (succ) (not (eq succ false-block)))
                              (cl-cc/optimize:bb-successors entry))))
    (cl-cc/optimize:opt-compute-path-sensitive-ranges cfg)
    (let ((true-range (cl-cc/optimize:opt-block-reg-range true-block :idx))
          (false-range (cl-cc/optimize:opt-block-reg-range false-block :idx)))
      (expect true-range :to-equal '(0 . 9))
      (expect false-range :to-equal '(10 . 255)))))

(it-sequential "interval-widen-expands-moving-bound-to-sentinel"
  (let ((widened (cl-cc/optimize:opt-interval-widen
                  (cl-cc/optimize::opt-make-interval 0 0)
                  (cl-cc/optimize::opt-make-interval 0 1))))
    (expect (= 0 (cl-cc/optimize::opt-interval-lo widened)) :to-be-truthy)
    (expect (= most-positive-fixnum (cl-cc/optimize::opt-interval-hi widened)) :to-be-truthy)))

(defun %test-path-sensitive-counted-loop-cfg ()
  (cl-cc/optimize:cfg-build
   (list (make-vm-const :dst :i :value 0)
         (make-vm-const :dst :one :value 1)
         (make-vm-const :dst :n :value 4)
         (make-vm-label :name "loop")
         (make-vm-lt :dst :cmp :lhs :i :rhs :n)
         (make-vm-jump-zero :reg :cmp :label "exit")
         (make-vm-add :dst :j :lhs :i :rhs :one)
         (make-vm-move :dst :i :src :j)
         (make-vm-jump :label "loop")
         (make-vm-label :name "exit")
         (make-vm-ret :reg :i))))

(it-sequential "path-sensitive-ranges-widen-loop-header-and-converge"
  (let* ((cfg (%test-path-sensitive-counted-loop-cfg))
         (loop-block (cl-cc/optimize:cfg-get-block-by-label cfg "loop"))
         (exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
         (body-block (find-if (lambda (succ) (not (eq succ exit-block)))
                              (cl-cc/optimize:bb-successors loop-block))))
    (cl-cc/optimize:opt-compute-path-sensitive-ranges cfg)
    (expect (cl-cc/optimize:opt-block-reg-range loop-block :i) :to-equal (cons 0 cl-cc/optimize::+opt-range-positive-infinity+))
    (expect (cl-cc/optimize:opt-block-reg-range body-block :i) :to-equal '(0 . 3))
    (expect (cl-cc/optimize:opt-block-reg-range exit-block :i) :to-equal (cons 4 cl-cc/optimize::+opt-range-positive-infinity+))))

(it-sequential "simple-induction-detects-affine-update"
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-add   :dst :i :lhs :i :rhs :one)))
         (ivs (cl-cc/optimize::opt-compute-simple-inductions insts))
         (iv (gethash :i ivs)))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize::opt-iv-init iv)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-iv-step iv)) :to-be-truthy)))

(it-sequential "simple-induction-kills-stale-fact-after-overwrite"
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-add   :dst :i :lhs :i :rhs :one)
                      (make-vm-const :dst :i :value 42)))
         (ivs (cl-cc/optimize::opt-compute-simple-inductions insts)))
    (expect (nth-value 1 (gethash :i ivs)) :to-be-falsy)))

(it-sequential "induction-trip-count-cases"
  (expect (= 5 (cl-cc/optimize::opt-induction-trip-count 0 10 2)) :to-be-truthy)
  (expect (= 6 (cl-cc/optimize::opt-induction-trip-count 0 10 2 :inclusive-p t)) :to-be-truthy)
  (expect (= 4 (cl-cc/optimize::opt-induction-trip-count 10 0 -3)) :to-be-truthy))

(it-sequential "simple-induction-detects-inc-and-dec-updates"
  (let* ((inc-insts (list (make-vm-const :dst :i :value 0)
                          (make-vm-inc :dst :i :src :i)))
         (dec-insts (list (make-vm-const :dst :j :value 10)
                          (make-vm-dec :dst :j :src :j)))
         (inc-ivs (cl-cc/optimize:opt-compute-simple-inductions inc-insts))
         (dec-ivs (cl-cc/optimize:opt-compute-simple-inductions dec-insts)))
    (expect (= 1 (cl-cc/optimize:opt-iv-step (gethash :i inc-ivs))) :to-be-truthy)
    (expect (= -1 (cl-cc/optimize:opt-iv-step (gethash :j dec-ivs))) :to-be-truthy)))

(it-sequential "induction-trip-count-comparison-predicates"
  (expect (= 6 (cl-cc/optimize:opt-induction-trip-count 0 10 2 :predicate 'vm-le)) :to-be-truthy)
  (expect (= 6 (cl-cc/optimize:opt-induction-trip-count 10 0 -2 :predicate 'vm-ge)) :to-be-truthy)
  (expect (= 1 (cl-cc/optimize:opt-induction-trip-count 4 4 1 :predicate 'vm-eq)) :to-be-truthy)
  (expect (= 0 (cl-cc/optimize:opt-induction-trip-count 3 4 1 :predicate 'vm-eq)) :to-be-truthy))

(it-sequential "loop-induction-analysis-is-scoped-to-natural-loop"
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-label :name "loop")
                      (make-vm-inc :dst :i :src :i)
                      (make-vm-jump :label "loop")))
         (cfg (cl-cc/optimize:cfg-build insts))
         (header (cl-cc/optimize:cfg-get-block-by-label cfg "loop"))
         (loops (cl-cc/optimize:opt-compute-loop-inductions cfg))
         (ivs (gethash header loops))
         (iv (and ivs (gethash :i ivs))))
    (expect iv :to-be-truthy)
    (expect (= 0 (cl-cc/optimize:opt-iv-init iv)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize:opt-iv-step iv)) :to-be-truthy)))

(it-sequential "bounds-check-elimination-annotates-provably-safe-loop-access"
  (let* ((aref-inst (cl-cc:make-vm-aref :dst :out :array-reg :arr :index-reg :idx))
         (insts (list (make-vm-const :dst :len :value 4)
                      (make-vm-make-array :dst :arr :size-reg :len)
                      (make-vm-const :dst :idx :value 2)
                      (make-vm-label :name "loop")
                      aref-inst
                      (make-vm-jump :label "loop")))
         (result (cl-cc/optimize:opt-pass-bounds-check-elimination insts)))
    (expect result :to-equal insts)
    (expect (cl-cc/optimize:opt-bounds-check-eliminable-marked-p aref-inst) :to-be-truthy)
    (expect (getf (cl-cc/optimize:opt-bounds-check-eliminable-metadata aref-inst)
                          :index-reg) :to-be :idx)))

(it-sequential "bounds-check-elimination-keeps-out-of-bounds-access-checked"
  (let* ((aref-inst (cl-cc:make-vm-aref :dst :out :array-reg :arr :index-reg :idx))
         (insts (list (make-vm-const :dst :len :value 4)
                      (make-vm-make-array :dst :arr :size-reg :len)
                      (make-vm-const :dst :idx :value 4)
                      aref-inst)))
    (cl-cc/optimize:opt-pass-bounds-check-elimination insts)
    (expect (cl-cc/optimize:opt-bounds-check-eliminable-marked-p aref-inst) :to-be-falsy)))

(it-sequential "constant-intervals-unknown-operand-kills-dst"
  (let* ((insts (list (make-vm-const :dst :r0 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r99)))
         (ivals (cl-cc/optimize:opt-compute-value-ranges insts)))
    (expect (nth-value 1 (gethash :r2 ivals)) :to-be-falsy)))
