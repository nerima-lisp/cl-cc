;;;; tests/unit/optimize/ssa-tests.lisp — SSA Construction/Destruction Tests
;;;
;;; Tests for Phase 1: ssa-construct, ssa-round-trip, ssa-place-phis,
;;; and SSA destruction (phi elimination → parallel copies).

(in-package :cl-cc/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun count-type (insts type-sym)
  "Count instructions of TYPE-SYM in INSTS."
  (count-if (lambda (i) (typep i (find-symbol (symbol-name type-sym) :cl-cc)))
            insts))

(defun make-ssa-test-block (id)
  (cl-cc/optimize:make-basic-block :id id))

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

(it-sequential "ssa-versioned-reg-format version-3"
  (destructuring-bind (reg ver expected) (list :r5 3 "R5.3")
    (expect (symbol-name (cl-cc/optimize:ssa-versioned-reg reg ver)) :to-equal expected)))

(it-sequential "ssa-versioned-reg-format version-0"
  (destructuring-bind (reg ver expected) (list :r0 0 "R0.0")
    (expect (symbol-name (cl-cc/optimize:ssa-versioned-reg reg ver)) :to-equal expected)))

;;; ─── SSA Rename State ────────────────────────────────────────────────────

(it-sequential "ssa-rename-state-push-pop"
  (let ((s (cl-cc/optimize:make-ssa-rename-state)))
    (let ((v0 (cl-cc/optimize:ssr-push-new-version s :r0)))
      (expect (cl-cc/optimize:ssr-current-version s :r0) :to-be v0)
      (let ((v1 (cl-cc/optimize:ssr-push-new-version s :r0)))
        (expect (cl-cc/optimize:ssr-current-version s :r0) :to-be v1)
        (cl-cc/optimize:ssr-pop-version s :r0)
        (expect (cl-cc/optimize:ssr-current-version s :r0) :to-be v0)))))

(it-sequential "ssa-rename-state-unversioned"
  (let ((s (cl-cc/optimize:make-ssa-rename-state)))
    (expect (cl-cc/optimize:ssr-current-version s :r99) :to-be :r99)))

;;; ─── SSA Round-Trip ──────────────────────────────────────────────────────

(it-sequential "ssa-round-trip-cases linear-sequence"
  (destructuring-bind (insts assert-fn) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-const :dst :r1 :value 2)
                 (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                 (make-vm-ret   :reg :r2)) (lambda (result) (expect (>= (length result) 1) :to-be-truthy)))
    (let ((result (cl-cc/optimize:ssa-round-trip insts)))
    (funcall assert-fn result))))

(it-sequential "ssa-round-trip-cases preserves-types"
  (destructuring-bind (insts assert-fn) (list (list (make-vm-const :dst :r0 :value 42)
                 (make-vm-ret   :reg :r0)) (lambda (result)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ret)) result) :to-be-truthy)))
    (let ((result (cl-cc/optimize:ssa-round-trip insts)))
    (funcall assert-fn result))))

(it-sequential "ssa-round-trip-cases empty"
  (destructuring-bind (insts assert-fn) (list nil (lambda (result) (expect result :to-be-null)))
    (let ((result (cl-cc/optimize:ssa-round-trip insts)))
    (funcall assert-fn result))))

(it-sequential "ssa-round-trip-cases with-label"
  (destructuring-bind (insts assert-fn) (list (list (make-vm-const    :dst :r0 :value 1)
                 (make-vm-jump-zero :reg :r0 :label "L1")
                 (make-vm-const    :dst :r0 :value 2)
                 (make-vm-label    :name "L1")
                 (make-vm-ret      :reg :r0)) (lambda (result)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result) :to-be-truthy)))
    (let ((result (cl-cc/optimize:ssa-round-trip insts)))
    (funcall assert-fn result))))

;;; ─── Phi Placement ───────────────────────────────────────────────────────

(it-sequential "ssa-phi-placement-no-phis-linear"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      ;; No phi-nodes needed for linear code
      (let ((total-phis 0))
        (maphash (lambda (_b phis) (incf total-phis (length phis))) phi-map)
        (expect (= 0 total-phis) :to-be-truthy)))))

(it-sequential "ssa-phi-placement-merge-point"
  (let* ((insts (list (make-vm-const    :dst :r0 :value 0)
                      (make-vm-jump-zero :reg :r0 :label "merge")
                      (make-vm-const    :dst :r1 :value 42)
                      (make-vm-label    :name "merge")
                      (make-vm-ret      :reg :r1))))
    ;; Just verify construction succeeds without error
    (multiple-value-bind (cfg _phi _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore _phi _renamed))
      (expect (cl-cc/optimize:cfg-entry cfg) :to-be-truthy))))

(it-sequential "ssa-phi-placement-prunes-never-read-reg"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "then")
                      (make-vm-const :dst :r1 :value 10)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "then")
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-label :name "join")
                      ;; join never reads :r1
                      (make-vm-ret :reg :r0))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      (let ((total-phis 0)
            (has-r1-phi nil))
        (maphash (lambda (_b phis)
                   (declare (ignore _b))
                   (incf total-phis (length phis))
                   (when (find-if (lambda (p) (eq (cl-cc:phi-reg p) :r1)) phis)
                     (setf has-r1-phi t)))
                 phi-map)
        (expect has-r1-phi :to-be-falsy)
        (expect (= 0 total-phis) :to-be-truthy)))))

(it-sequential "ssa-phi-placement-prunes-local-only-reads"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "then")
                      (make-vm-const :dst :r1 :value 10)
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "then")
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-add :dst :r3 :lhs :r1 :rhs :r0)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r0))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      (let ((has-r1-phi nil))
        (maphash (lambda (_b phis)
                   (declare (ignore _b))
                   (when (find-if (lambda (p) (eq (cl-cc:phi-reg p) :r1)) phis)
                     (setf has-r1-phi t)))
                 phi-map)
        (expect has-r1-phi :to-be-falsy)))))

(it-sequential "ssa-phi-placement-keeps-live-in-join-phi"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "then")
                      (make-vm-const :dst :r1 :value 10)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "then")
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-ret :reg :r2))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      (let ((join (cl-cc/optimize:cfg-get-block-by-label cfg "join")))
        (expect (find-if (lambda (p) (eq (cl-cc:phi-reg p) :r1))
                  (gethash join phi-map)) :to-be-truthy)))))

(it-sequential "ssa-lcssa-inserts-exit-phi-for-loop-defined-value"
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-const :dst :lim :value 2)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :i)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (phi-map nil)
         (exit-block nil)
         (has-i-phi nil))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-dominance-frontiers cfg)
    (setf phi-map (cl-cc/optimize:ssa-place-phis cfg))
    (setf phi-map (cl-cc/optimize::ssa-place-lcssa-phis cfg phi-map))
    (setf exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "Lexit"))
    (setf has-i-phi
          (find-if (lambda (p) (eq (cl-cc:phi-reg p) :i))
                   (gethash exit-block phi-map)))
    (expect has-i-phi :to-be-truthy)))

(it-sequential "ssa-lcssa-places-phi-on-exit-not-later-use"
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-const :dst :lim :value 2)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-jump :label "Lpost")
                      (make-vm-label :name "Lpost")
                      (make-vm-ret :reg :i)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (phi-map nil)
         (exit-block nil)
         (post-block nil))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-dominance-frontiers cfg)
    (setf phi-map (cl-cc/optimize:ssa-place-phis cfg))
    (setf phi-map (cl-cc/optimize::ssa-place-lcssa-phis cfg phi-map))
    (setf exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "Lexit")
          post-block (cl-cc/optimize:cfg-get-block-by-label cfg "Lpost"))
    (expect (find-if (lambda (p) (eq (cl-cc:phi-reg p) :i))
              (gethash exit-block phi-map)) :to-be-truthy)
    (expect (find-if (lambda (p) (eq (cl-cc:phi-reg p) :i))
              (gethash post-block phi-map)) :to-be-falsy)))

(it-sequential "ssa-lcssa-construct-preserves-exit-phi"
  (let ((insts (list (make-vm-const :dst :i :value 0)
                     (make-vm-const :dst :one :value 1)
                     (make-vm-const :dst :lim :value 2)
                     (make-vm-label :name "Lh")
                     (make-vm-lt :dst :c :lhs :i :rhs :lim)
                     (make-vm-jump-zero :reg :c :label "Lexit")
                     (make-vm-add :dst :i :lhs :i :rhs :one)
                     (make-vm-jump :label "Lh")
                     (make-vm-label :name "Lexit")
                     (make-vm-jump :label "Lpost")
                     (make-vm-label :name "Lpost")
                     (make-vm-ret :reg :i))))
    (multiple-value-bind (cfg phi-map renamed)
        (cl-cc/optimize:ssa-construct insts)
      (let* ((exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "Lexit"))
             (post-block (cl-cc/optimize:cfg-get-block-by-label cfg "Lpost"))
             (exit-phi (find-if (lambda (p) (eq (cl-cc:phi-reg p) :i))
                                (gethash exit-block phi-map)))
             (post-reads (loop for inst in (gethash post-block renamed)
                               append (cl-cc/optimize::opt-inst-read-regs inst))))
        (expect exit-phi :to-be-truthy)
        (expect (cl-cc:phi-kind exit-phi) :to-be :lcssa)
        (expect (member (cl-cc:phi-dst exit-phi) post-reads :test #'eq) :to-be-truthy)
        (expect (find-if (lambda (p) (eq (cl-cc:phi-reg p) :i))
                  (gethash post-block phi-map)) :to-be-falsy)))))

(it-sequential "ssa-trivial-phi-elimination-rewrites-uses"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi    (cl-cc/optimize:make-ssa-phi
                  :dst (cl-cc/optimize:ssa-versioned-reg :r1 0)
                  :args (list (cons pred-a :r2)
                              (cons pred-b :r2))
                  :reg :r1))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst    (make-vm-add :dst :r3 :lhs (cl-cc:phi-dst phi) :rhs :r4)))
    (setf (gethash join phi-map) (list phi)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map new-renamed-map)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore new-phi-map))
      (let ((rewritten (first (gethash join new-renamed-map))))
        (expect (cl-cc/vm::vm-lhs rewritten) :to-be :r2)
        (expect (cl-cc/vm::vm-rhs rewritten) :to-be :r4)))))

(it-sequential "ssa-trivial-phi-elimination-shortcuts-phi-of-phi-chain"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi-b  (cl-cc/optimize:make-ssa-phi
                  :dst :b.0
                  :args (list (cons pred-a :b-from-1)
                              (cons pred-b :b-from-2))
                  :reg :b))
         (phi-a  (cl-cc/optimize:make-ssa-phi
                  :dst :a.0
                  :args (list (cons pred-b :b.0)
                              (cons pred-a :c.0))
                  :reg :a))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash pred-b phi-map) (list phi-b)
          (gethash join phi-map) (list phi-a)
          (gethash join renamed) (list (make-vm-ret :reg :a.0)))
    (multiple-value-bind (new-phi-map _new-renamed)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore _new-renamed))
      (let* ((new-phi-a (find-if (lambda (p) (eq (cl-cc:phi-dst p) :a.0))
                                 (gethash join new-phi-map)))
             (shortcut (assoc pred-b (cl-cc:phi-args new-phi-a) :test #'eq)))
        (expect new-phi-a :to-be-truthy)
        (expect (cdr shortcut) :to-be :b-from-2)))))

(it-sequential "ssa-trivial-phi-elimination-runs-all-passes-together"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi-b  (cl-cc/optimize:make-ssa-phi
                   :dst :b.0
                   :args (list (cons pred-a :x.0)
                               (cons pred-b :x.0))
                   :reg :b))
         (phi-a  (cl-cc/optimize:make-ssa-phi
                   :dst :a.0
                   :args (list (cons pred-b :b.0)
                               (cons pred-a :x.0))
                   :reg :a))
         (phi-same (cl-cc/optimize:make-ssa-phi
                     :dst :same.0
                     :args (list (cons pred-a :v.0)
                                 (cons pred-b :v.0))
                     :reg :same))
         (phi-dead (cl-cc/optimize:make-ssa-phi
                     :dst :dead.0
                     :args (list (cons pred-a :d1.0)
                                 (cons pred-b :d2.0))
                     :reg :dead))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r9 :lhs :a.0 :rhs :same.0)))
    (setf (gethash pred-b phi-map) (list phi-b)
          (gethash join phi-map) (list phi-a phi-same phi-dead)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map new-renamed-map)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (let ((total-phis 0)
            (rewritten (first (gethash join new-renamed-map))))
        (maphash (lambda (_b phis)
                   (declare (ignore _b))
                   (incf total-phis (length phis)))
                 new-phi-map)
        (expect (= 0 total-phis) :to-be-truthy)
        (expect (cl-cc/vm::vm-lhs rewritten) :to-be :x.0)
        (expect (cl-cc/vm::vm-rhs rewritten) :to-be :v.0)))))

;;; ─── Targeted Trivial Phi Elimination Tests ─────────────────────────────

(it-sequential "ssa-phi-elim-all-same-arg-multi-pred"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (pred-c (make-ssa-test-block 3))
         (join   (make-ssa-test-block 4))
         (phi    (cl-cc/optimize:make-ssa-phi
                  :dst (cl-cc/optimize:ssa-versioned-reg :r1 0)
                  :args (list (cons pred-a :r0)
                              (cons pred-b :r0)
                              (cons pred-c :r0))
                  :reg :r1))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst    (make-vm-add :dst :r2 :lhs (cl-cc:phi-dst phi) :rhs :r5)))
    (setf (gethash join phi-map) (list phi)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map new-renamed-map)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (let ((total-phis 0))
        (maphash (lambda (_b phis) (incf total-phis (length phis))) new-phi-map)
        (expect (= 0 total-phis) :to-be-truthy)
        (let ((rewritten (first (gethash join new-renamed-map))))
          (expect (cl-cc/vm::vm-lhs rewritten) :to-be :r0)
          (expect (cl-cc/vm::vm-rhs rewritten) :to-be :r5))))))

(it-sequential "ssa-phi-elim-phi-of-phi-chain-deep"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi-a  (cl-cc/optimize:make-ssa-phi
                  :dst :a.0
                  :args (list (cons pred-a :x.0) (cons pred-b :y.0))
                  :reg :a))
         (phi-b  (cl-cc/optimize:make-ssa-phi
                  :dst :b.0
                  :args (list (cons pred-a :a.0) (cons pred-b :z.0))
                  :reg :b))
         (phi-c  (cl-cc/optimize:make-ssa-phi
                  :dst :c.0
                  :args (list (cons pred-b :b.0) (cons pred-a :w.0))
                  :reg :c))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash pred-a phi-map) (list phi-a)
          (gethash pred-b phi-map) (list phi-b)
          (gethash join phi-map) (list phi-c)
          (gethash pred-a renamed) (list (make-vm-add :dst :r1 :lhs :a.0 :rhs :c.0))
          (gethash pred-b renamed) (list (make-vm-add :dst :r2 :lhs :b.0 :rhs :c.0))
          (gethash join renamed) (list (make-vm-add :dst :r3 :lhs :c.0 :rhs :c.0)))
    (multiple-value-bind (new-phi-map _new-renamed)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore _new-renamed))
      (let ((new-phi-c (find-if (lambda (p) (eq (cl-cc:phi-dst p) :c.0))
                                (gethash join new-phi-map)))
            (new-phi-b (find-if (lambda (p) (eq (cl-cc:phi-dst p) :b.0))
                                (gethash pred-b new-phi-map))))
        (expect new-phi-b :to-be-truthy)
        (expect (cdr (assoc pred-a (cl-cc:phi-args new-phi-b) :test #'eq)) :to-be :x.0)
        (expect new-phi-c :to-be-truthy)
        (expect (cdr (assoc pred-b (cl-cc:phi-args new-phi-c) :test #'eq)) :to-be :z.0)
        (expect (cdr (assoc pred-a (cl-cc:phi-args new-phi-c) :test #'eq)) :to-be :w.0)
        ;; phi-b arg pred-b is unchanged
        (expect (cdr (assoc pred-b (cl-cc:phi-args new-phi-b) :test #'eq)) :to-be :z.0)))))

(it-sequential "ssa-phi-elim-unused-phi"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi    (cl-cc/optimize:make-ssa-phi
                  :dst :unused.0
                  :args (list (cons pred-a :x.0) (cons pred-b :y.0))
                  :reg :unused))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst    (make-vm-ret :reg :r0)))
    (setf (gethash join phi-map) (list phi)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map _new-renamed)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore _new-renamed))
      (let ((total-phis 0))
        (maphash (lambda (_b phis) (incf total-phis (length phis))) new-phi-map)
        (expect (= 0 total-phis) :to-be-truthy)
        ;; Verify the renamed instruction is untouched
        (let ((original (first (gethash join renamed))))
          (expect (cl-cc/vm::vm-reg original) :to-be :r0))))))

(it-sequential "ssa-phi-elim-idempotent"
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi-a  (cl-cc/optimize:make-ssa-phi
                  :dst :a.0
                  :args (list (cons pred-a :x.0) (cons pred-b :y.0))
                  :reg :a))
         (phi-b  (cl-cc/optimize:make-ssa-phi
                  :dst :b.0
                  :args (list (cons pred-a :x.0) (cons pred-b :x.0))
                  :reg :b))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash join phi-map) (list phi-a phi-b)
          (gethash join renamed) (list (make-vm-add :dst :r2 :lhs :a.0 :rhs :x.0)))
    (multiple-value-bind (first-phi first-renamed)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (multiple-value-bind (second-phi second-renamed)
          (cl-cc/optimize:ssa-eliminate-trivial-phis first-phi first-renamed)
        ;; Same phi count
        (let ((count-1 0) (count-2 0))
          (maphash (lambda (_b phis) (incf count-1 (length phis))) first-phi)
          (maphash (lambda (_b phis) (incf count-2 (length phis))) second-phi)
          (expect (= count-1 count-2) :to-be-truthy))
        ;; Same instruction content (first pass should have eliminated phi-b (same-arg :x.0))
        (let ((inst-1 (first (gethash join first-renamed)))
              (inst-2 (first (gethash join second-renamed))))
          (expect (cl-cc/vm::vm-lhs inst-2) :to-be (cl-cc/vm::vm-lhs inst-1))
          (expect (cl-cc/vm::vm-rhs inst-2) :to-be (cl-cc/vm::vm-rhs inst-1)))
        ;; Verify phi-b was eliminated and phi-a remains
        (let ((phi-b-1 (find-if (lambda (p) (eq (cl-cc:phi-dst p) :b.0))
                                (gethash join first-phi)))
              (phi-b-2 (find-if (lambda (p) (eq (cl-cc:phi-dst p) :b.0))
                                (gethash join second-phi))))
          ;; phi-b is all-same-arg :x.0 → eliminated in both passes
          (expect phi-b-1 :to-be-falsy)
          (expect phi-b-2 :to-be-falsy))))))

;;; ─── Parallel Copy Sequentialization ─────────────────────────────────────

(it-sequential "ssa-seq-copies-behavior"
  (expect (cl-cc/optimize:ssa-sequentialize-copies nil) :to-be-null)
  (let* ((result (cl-cc/optimize:ssa-sequentialize-copies '((:r0 . :r1) (:r2 . :r3)))))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (every (lambda (i) (typep i 'cl-cc/vm::vm-move)) result) :to-be-truthy))
  (let* ((result (cl-cc/optimize:ssa-sequentialize-copies '((:r0 . :r1) (:r1 . :r0)))))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (every (lambda (i) (typep i 'cl-cc/vm::vm-logxor)) result) :to-be-truthy)))

(it-sequential "ssa-seq-copies-xor-swap-shape"
  (let ((result (cl-cc/optimize:ssa-sequentialize-copies '((:r0 . :r1) (:r1 . :r0)))))
    (expect (= 3 (length result)) :to-be-truthy)
    (destructuring-bind (first second third) result
      (expect (cl-cc/vm::vm-dst first) :to-be :r0)
      (expect (cl-cc/vm::vm-lhs first) :to-be :r0)
      (expect (cl-cc/vm::vm-rhs first) :to-be :r1)
      (expect (cl-cc/vm::vm-dst second) :to-be :r1)
      (expect (cl-cc/vm::vm-lhs second) :to-be :r0)
      (expect (cl-cc/vm::vm-rhs second) :to-be :r1)
      (expect (cl-cc/vm::vm-dst third) :to-be :r0)
      (expect (cl-cc/vm::vm-lhs third) :to-be :r0)
      (expect (cl-cc/vm::vm-rhs third) :to-be :r1))))

(it-sequential "ssa-seq-copies-three-cycle-still-uses-temp"
  (let* ((result (cl-cc/optimize:ssa-sequentialize-copies
                  '((:r0 . :r1) (:r1 . :r2) (:r2 . :r0))))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move)) result)))
    (expect (= 4 (length result)) :to-be-truthy)
    (expect (= 4 (length moves)) :to-be-truthy)
    (expect (some (lambda (i)
                         (let ((dst (cl-cc/vm::vm-dst i)))
                           (and (keywordp dst)
                                (search "SSATMP" (symbol-name dst)))))
                       moves) :to-be-truthy)))

(it-sequential "ssa-seq-copies-dag-emits-ready-leaves-first"
  (let ((result (cl-cc/optimize:ssa-sequentialize-copies
                 '((:r0 . :r1) (:r1 . :r2) (:r3 . :r1)))))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (every (lambda (i) (typep i 'cl-cc/vm::vm-move)) result) :to-be-truthy)
    (expect (some (lambda (i)
                          (search "SSATMP" (symbol-name (cl-cc/vm::vm-dst i))))
                        result) :to-be-falsy)
    ;; :r0 and :r3 both read the old :r1, so they must precede :r1 <- :r2.
    (let ((r1-write-pos (position-if (lambda (i) (eq :r1 (cl-cc/vm::vm-dst i))) result)))
      (expect r1-write-pos :to-be-truthy)
      (expect (every (lambda (i)
                            (or (eq :r1 (cl-cc/vm::vm-dst i))
                                (< (position i result) r1-write-pos)))
                          result) :to-be-truthy))))

(it-sequential "ssa-destroy-places-phi-copies-before-terminator"
  (let* ((insts (list (make-vm-label :name "pred")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r2)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (pred (cl-cc/optimize:cfg-get-block-by-label cfg "pred"))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash join phi-map)
          (list (cl-cc/optimize:make-ssa-phi
                 :dst :r2
                 :args (list (cons pred :r1))
                 :reg :r2))
          (gethash pred renamed) (list (make-vm-jump :label "join"))
          (gethash join renamed) (list (make-vm-ret :reg :r2)))
    (let* ((out (cl-cc/optimize:ssa-destroy cfg phi-map renamed))
           (move-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-move)) out))
           (jump-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out)))
      (expect move-pos :to-be-truthy)
      (expect jump-pos :to-be-truthy)
      (expect (< move-pos jump-pos) :to-be-truthy))))

(it-sequential "ssa-destroy-keeps-conditional-edge-phi-copy-on-target-edge"
  (let* ((insts (list (make-vm-label :name "pred")
                      (make-vm-jump-zero :reg :cond :label "join")
                      (make-vm-label :name "fallthrough")
                      (make-vm-ret :reg :rf)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r2)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (pred (cl-cc/optimize:cfg-get-block-by-label cfg "pred"))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash join phi-map)
          (list (cl-cc/optimize:make-ssa-phi
                 :dst :r2
                 :args (list (cons pred :r1))
                 :reg :r2))
          (gethash pred renamed) (list (make-vm-jump-zero :reg :cond :label "join"))
          (gethash join renamed) (list (make-vm-ret :reg :r2)))
    (let* ((out (cl-cc/optimize:ssa-destroy cfg phi-map renamed))
           (jump-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
           (jump-inst (nth jump-pos out))
           (pad-name (cl-cc/vm::vm-label-name jump-inst))
           (pad-pos (position-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-label)
                                        (string= pad-name (cl-cc/vm::vm-name i))))
                                 out))
           (move-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-move)) out)))
      (expect jump-pos :to-be-truthy)
      (expect (string= "join" pad-name) :to-be-falsy)
      (expect pad-pos :to-be-truthy)
      (expect move-pos :to-be-truthy)
      (expect (< jump-pos move-pos) :to-be-truthy)
      (expect (< pad-pos move-pos) :to-be-truthy)
      (expect (cl-cc/vm::vm-dst (nth (1+ pad-pos) out)) :to-be :r2)
      (expect (cl-cc/vm::vm-src (nth (1+ pad-pos) out)) :to-be :r1))))

;;; ─── %ssa-resolve-reg ────────────────────────────────────────────────────

(it-sequential "ssa-resolve-reg-follows-chain"
  (let ((r (make-hash-table :test #'eq)))
    (setf (gethash :r0 r) :r1
          (gethash :r1 r) :r2)
    (expect (cl-cc/optimize::%ssa-resolve-reg :r0 r) :to-be :r2)))

(it-sequential "ssa-resolve-reg-identity-when-not-mapped"
  (let ((r (make-hash-table :test #'eq)))
    (expect (cl-cc/optimize::%ssa-resolve-reg :r5 r) :to-be :r5)))

;;; ─── %ssa-rewrite-tree ───────────────────────────────────────────────────

(it-sequential "ssa-rewrite-tree-replaces-symbol"
  (let ((r (make-hash-table :test #'eq)))
    (setf (gethash :r0 r) :r9)
    (expect (cl-cc/optimize::%ssa-rewrite-tree :r0 r) :to-be :r9)))

(it-sequential "ssa-rewrite-tree-passthrough-unmapped"
  (let ((r (make-hash-table :test #'eq)))
    (expect (cl-cc/optimize::%ssa-rewrite-tree :r3 r) :to-be :r3)
    (expect (= 42 (cl-cc/optimize::%ssa-rewrite-tree 42  r)) :to-be-truthy)
    (expect (cl-cc/optimize::%ssa-rewrite-tree 'foo r) :to-be 'foo)))

(it-sequential "ssa-rewrite-tree-walks-cons"
  (let ((r (make-hash-table :test #'eq)))
    (setf (gethash :r0 r) :r1)
    (expect (cl-cc/optimize::%ssa-rewrite-tree '(:r0 :r2) r) :to-equal '(:r1 :r2))))

;;; ─── ssa-rewrite-dst ─────────────────────────────────────────────────────

(it-sequential "ssa-rewrite-dst-changes-destination"
  (let* ((inst   (make-vm-const :dst :r0 :value 42))
         (result (cl-cc/optimize::ssa-rewrite-dst inst :r0 :r9)))
    (expect (typep result 'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst result) :to-be :r9)
    (expect (= 42 (cl-cc/vm::vm-value result)) :to-be-truthy)))

(it-sequential "ssa-rewrite-dst-noop-when-no-match"
  (let* ((inst (make-vm-const :dst :r1 :value 7)))
    (expect (cl-cc/optimize::ssa-rewrite-dst inst :r99 :r0) :to-be inst)))

;;; ─── %ssa-collect-uses ───────────────────────────────────────────────────

(it-sequential "ssa-collect-uses-records-instruction-reads"
  (let* ((phi-map    (make-hash-table :test #'eq))
         (renamed    (make-hash-table :test #'eq))
         (blk        (make-ssa-test-block 1))
         (inst       (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (setf (gethash blk renamed) (list inst))
    (let ((uses (cl-cc/optimize::%ssa-collect-uses phi-map renamed)))
      (expect (gethash :r0 uses) :to-be-truthy)
      (expect (gethash :r1 uses) :to-be-truthy)
      (expect (gethash :r2 uses) :to-be-falsy))))
