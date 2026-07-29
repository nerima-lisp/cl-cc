(in-package :cl-cc/test)

;;; ── CSE (Common Subexpression Elimination) Unit Tests ──────────────────────

(it-sequential "cse-dedup same-order"
  (destructuring-bind (i4) (list (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
    (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (expect (cl-cc:vm-add-p (third out)) :to-be-truthy)
    (expect (cl-cc:vm-move-p (fourth out)) :to-be-truthy)
    (expect (cl-cc/vm::vm-src (fourth out)) :to-equal :R2))))

(it-sequential "cse-dedup swapped-order"
  (destructuring-bind (i4) (list (cl-cc:make-vm-add :dst :R3 :lhs :R1 :rhs :R0))
    (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (expect (cl-cc:vm-add-p (third out)) :to-be-truthy)
    (expect (cl-cc:vm-move-p (fourth out)) :to-be-truthy)
    (expect (cl-cc/vm::vm-src (fourth out)) :to-equal :R2))))

(it-sequential "cse-label-flushes"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
          (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
          (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
          (j  (cl-cc:make-vm-jump :label "L1"))
          (lbl (cl-cc:make-vm-label :name "L1"))
          (i4 (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
          (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 j lbl i4))))
    (expect (cl-cc:vm-add-p (third out)) :to-be-truthy)
    (expect (cl-cc:vm-jump-p (fourth out)) :to-be-truthy)
    (expect (cl-cc:vm-label-p (fifth out)) :to-be-truthy)
    (expect (cl-cc:vm-add-p (sixth out)) :to-be-truthy)))

(it-sequential "cse-fallthrough-label-preserves-state"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (lbl (cl-cc:make-vm-label :name "L1"))
         (i4 (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 lbl i4))))
    (expect (cl-cc:vm-label-p (fourth out)) :to-be-truthy)
    (expect (cl-cc:vm-move-p (fifth out)) :to-be-truthy)))

(it-sequential "cse-unary-dedup"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc:make-vm-neg :dst :R1 :src :R0))
         (i3 (cl-cc:make-vm-neg :dst :R2 :src :R0))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3))))
    (expect (cl-cc:vm-neg-p (second out)) :to-be-truthy)
    (expect (cl-cc:vm-move-p (third out)) :to-be-truthy)
    (expect (cl-cc/vm::vm-src (third out)) :to-equal :R1)))

(it-sequential "cse-const-no-move"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 42))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2))))
    (expect (cl-cc:vm-const-p (first out)) :to-be-truthy)
    (expect (cl-cc:vm-const-p (second out)) :to-be-truthy)))

(it-sequential "cse-different-ops-no-dedup"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (i4 (cl-cc:make-vm-sub :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (expect (cl-cc:vm-add-p (third out)) :to-be-truthy)
    (expect (cl-cc:vm-sub-p (fourth out)) :to-be-truthy)))

(it-sequential "optimizer-value-ordering-structural pair-ascending"
  (destructuring-bind (expected a b) (list t '(:r0 . 1) '(:r0 . 2))
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "optimizer-value-ordering-structural keyword-ascending"
  (destructuring-bind (expected a b) (list t :r0 :r1)
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "optimizer-value-ordering-structural integer-ascending"
  (destructuring-bind (expected a b) (list t 1 2)
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "optimizer-value-ordering-structural string-descending"
  (destructuring-bind (expected a b) (list nil "b" "a")
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "optimizer-gvn-dominates-branch"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (j  (cl-cc:make-vm-jump :label "L1"))
         (l  (cl-cc:make-vm-label :name "L1"))
         (i4 (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (r  (cl-cc:make-vm-ret :reg :R3))
         (out (cl-cc/optimize::opt-pass-gvn (list i1 i2 i3 j l i4 r))))
    (expect (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out) :to-equal 1)
     (expect (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-move)
                               (eq :R3 (cl-cc/vm::vm-dst i))))
                        out) :to-be-truthy)))
