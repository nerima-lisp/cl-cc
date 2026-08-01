;;;; tests/unit/optimize/optimizer-flow-block-tests.lisp
;;;; Unit tests for optimizer-flow-core.lisp — block/CFG helpers and correlation passes
;;;;
;;;; Covers: %block-mergeable-successor-p, %block-strip-merge-jump,
;;;;   cfg-block-temperature, opt-pass-branch-correlation,
;;;;   opt-pass-tail-merge (tail-duplication).

(in-package :cl-cc/test)

;;; ─── Straight-line preservation / empty-input ───────────────────────────

(it-sequential "opt-passes-preserve-straight-line dead-basic-blocks"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-dead-basic-blocks)
    (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (expect (listp result) :to-be-truthy))))

(it-sequential "opt-passes-preserve-straight-line nil-check-elim"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize:opt-pass-dominated-type-check-elim)
    (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (expect (listp result) :to-be-truthy))))

(it-sequential "opt-passes-preserve-straight-line block-merge"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-block-merge)
    (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (expect (listp result) :to-be-truthy))))

(it-sequential "opt-passes-preserve-straight-line tail-merge"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-tail-merge)
    (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (expect (listp result) :to-be-truthy))))

(it-sequential "opt-pass-returns-list-for-empty-input dead-basic-blocks"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-dead-basic-blocks)
    (expect (listp (funcall pass-fn nil)) :to-be-truthy)))

(it-sequential "opt-pass-returns-list-for-empty-input block-merge"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-block-merge)
    (expect (listp (funcall pass-fn nil)) :to-be-truthy)))

(it-sequential "opt-pass-returns-list-for-empty-input tail-merge"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-tail-merge)
    (expect (listp (funcall pass-fn nil)) :to-be-truthy)))

;;; ─── %type-check-elim helpers ───────────────────────────────────────────────

(it-sequential "type-check-elim-lookup-fact-cases match"
  (destructuring-bind (pred src should-match) (list 'p :r0 t)
    (let* ((fact  (list :pred 'p :src :r0 :dst :r1))
         (facts (list fact))
         (result (cl-cc/optimize::%type-check-elim-lookup-fact facts pred src)))
    (if should-match
        (expect result :to-be-truthy)
        (expect result :to-be-null)))))

(it-sequential "type-check-elim-lookup-fact-cases wrong-src"
  (destructuring-bind (pred src should-match) (list 'p :r9 nil)
    (let* ((fact  (list :pred 'p :src :r0 :dst :r1))
         (facts (list fact))
         (result (cl-cc/optimize::%type-check-elim-lookup-fact facts pred src)))
    (if should-match
        (expect result :to-be-truthy)
        (expect result :to-be-null)))))

(it-sequential "type-check-elim-lookup-fact-cases wrong-pred"
  (destructuring-bind (pred src should-match) (list 'q :r0 nil)
    (let* ((fact  (list :pred 'p :src :r0 :dst :r1))
         (facts (list fact))
         (result (cl-cc/optimize::%type-check-elim-lookup-fact facts pred src)))
    (if should-match
        (expect result :to-be-truthy)
        (expect result :to-be-null)))))

;;; ─── %block-mergeable-successor-p / %block-strip-merge-jump / cfg-block-temperature ──

(it-sequential "block-mergeable-successor-single-predecessor"
  (let* ((blk  (%make-test-basic-block))
         (succ (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-successors blk)   (list succ)
          (cl-cc/optimize:bb-predecessors succ) (list blk))
    (expect (cl-cc/optimize::%block-mergeable-successor-p blk) :to-be-truthy)))

(it-sequential "block-mergeable-successor-multiple-predecessors"
  (let* ((blk   (%make-test-basic-block))
         (other (%make-test-basic-block))
         (succ  (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-successors blk)   (list succ)
          (cl-cc/optimize:bb-predecessors succ) (list blk other))
    (expect (cl-cc/optimize::%block-mergeable-successor-p blk) :to-be-falsy)))

(it-sequential "block-mergeable-multiple-successors"
  (let* ((blk   (%make-test-basic-block))
         (succ1 (%make-test-basic-block))
         (succ2 (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-successors blk) (list succ1 succ2))
    (expect (cl-cc/optimize::%block-mergeable-successor-p blk) :to-be-falsy)))

(it-sequential "block-strip-merge-jump-cases matching"
  (destructuring-bind (insts target expected-len) (list (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "next")) "next" 1)
    (expect (= expected-len (length (cl-cc/optimize::%block-strip-merge-jump insts target))) :to-be-truthy)))

(it-sequential "block-strip-merge-jump-cases non-matching"
  (destructuring-bind (insts target expected-len) (list (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "other")) "next" 2)
    (expect (= expected-len (length (cl-cc/optimize::%block-strip-merge-jump insts target))) :to-be-truthy)))

(it-sequential "block-strip-merge-jump-cases empty"
  (destructuring-bind (insts target expected-len) (list nil "next" 0)
    (expect (= expected-len (length (cl-cc/optimize::%block-strip-merge-jump insts target))) :to-be-truthy)))

(it-sequential "cfg-block-cold-p-signal-error-and-ordinary signal-error"
  (destructuring-bind (instructions expect-cold) (list (list (make-vm-signal-error :error-reg :r0)) t)
    (let ((blk (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions blk) instructions)
    (if expect-cold
        (expect (cl-cc/optimize::%cfg-block-cold-p blk) :to-be-truthy)
        (expect (cl-cc/optimize::%cfg-block-cold-p blk) :to-be-falsy)))))

(it-sequential "cfg-block-cold-p-signal-error-and-ordinary ordinary"
  (destructuring-bind (instructions expect-cold) (list (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)) nil)
    (let ((blk (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions blk) instructions)
    (if expect-cold
        (expect (cl-cc/optimize::%cfg-block-cold-p blk) :to-be-truthy)
        (expect (cl-cc/optimize::%cfg-block-cold-p blk) :to-be-falsy)))))

(it-sequential "branch-correlation-propagates-through-forwarder"
  (let* ((pred   (make-vm-label :name "pred"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br     (make-vm-jump-zero :reg :r1 :label "mid"))
         (then   (make-vm-label :name "then"))
         (ret1   (make-vm-ret :reg :r1))
         (mid    (make-vm-label :name "mid"))
         (jmp    (make-vm-jump :label "join"))
         (join   (make-vm-label :name "join"))
         (p2     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred p1 br then ret1 mid jmp join p2 ret2))))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :r2)
                  (eql (cl-cc/vm::vm-value i) 0)))
           out) :to-be-truthy)))

(it-sequential "branch-correlation-forwarder-does-not-propagate-on-mismatch"
  (let* ((pred   (make-vm-label :name "pred"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br     (make-vm-jump-zero :reg :r1 :label "mid"))
         (then   (make-vm-label :name "then"))
         (ret1   (make-vm-ret :reg :r1))
         (mid    (make-vm-label :name "mid"))
         (jmp    (make-vm-jump :label "join"))
         (otherp (make-vm-label :name "otherp"))
         (p2     (cl-cc:make-vm-integer-p :dst :r3 :src :r4))
         (br2    (make-vm-jump-zero :reg :r3 :label "join"))
         (join   (make-vm-label :name "join"))
         (p3     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred p1 br then ret1 mid jmp otherp p2 br2 join p3 ret2))))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :r2)))
           out) :to-be-falsy)))

(it-sequential "tail-duplication-duplicates-small-shared-tail"
  (let* ((insts (list (make-vm-label :name "p1")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "p2")
                      (make-vm-label :name "p2")
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-jump :label "tail")
                      (make-vm-label :name "tail")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (ret-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-ret)) out)))
    (expect (= 1 jump-to-tail) :to-be-truthy)
    (expect (= 1 ret-count) :to-be-truthy)))

(it-sequential "tail-duplication-skips-large-tail"
  (let* ((large-tail (append (loop for i from 0 below 13
                                   collect (make-vm-const :dst (intern (format nil "R~D" i) :keyword)
                                                          :value i))
                             (list (make-vm-ret :reg :r12))))
         (insts (append (list (make-vm-jump-zero :reg :cond :label "p2")
                              (make-vm-label :name "p1")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "p2")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "tail"))
                        large-tail))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                  out)))
    (expect (= 2 jump-to-tail) :to-be-truthy)))

(it-sequential "tail-duplication-duplicates-larger-tail-up-to-threshold"
  (let* ((tail (append (loop for i from 0 below 11
                             collect (make-vm-const :dst (intern (format nil "T~D" i) :keyword)
                                                    :value i))
                       (list (make-vm-ret :reg :t10))))
         (insts (append (list (make-vm-jump-zero :reg :cond :label "p2")
                              (make-vm-label :name "p1")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "p2")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "tail"))
                        tail))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (ret-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-ret)) out)))
    (expect (= 0 jump-to-tail) :to-be-truthy)
    (expect (>= ret-count 2) :to-be-truthy)))

(it-sequential "tail-duplication-handles-conditional-predecessor-target"
  (let* ((insts (list (make-vm-label :name "p1")
                      (make-vm-jump-zero :reg :cond :label "tail")
                      (make-vm-label :name "p2")
                      (make-vm-jump :label "tail")
                      (make-vm-label :name "tail")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-zero-to-tail (count-if (lambda (i)
                                        (and (typep i 'cl-cc/vm::vm-jump-zero)
                                             (equal (cl-cc/vm::vm-label-name i) "tail")))
                                      out))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (add-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (expect (= 0 jump-zero-to-tail) :to-be-truthy)
    (expect (= 0 jump-to-tail) :to-be-truthy)
    (expect (>= add-count 2) :to-be-truthy)))
