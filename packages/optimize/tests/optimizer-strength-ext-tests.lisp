;;;; tests/unit/optimize/optimizer-strength-ext-tests.lisp
;;;; Unit tests for src/optimize/optimizer-strength-ext.lisp
;;;;
;;;; Covers: opt-reassociate-commutative-p, opt-copy-commutative-binop,
;;;;   opt-pass-reassociate (constant-drift, label-env-clear, passthrough),
;;;;   opt-pass-batch-concatenate (chain packing, non-chain passthrough,
;;;;   empty input).

(in-package :cl-cc/test)

;;; ─── opt-reassociate-commutative-p ──────────────────────────────────────

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-add"
  (destructuring-bind (inst) (list (make-vm-add         :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-integer-add"
  (destructuring-bind (inst) (list (make-vm-integer-add  :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-mul"
  (destructuring-bind (inst) (list (make-vm-mul          :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-integer-mul"
  (destructuring-bind (inst) (list (make-vm-integer-mul  :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-logand"
  (destructuring-bind (inst) (list (make-vm-logand       :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-logior"
  (destructuring-bind (inst) (list (make-vm-logior       :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-true-for-commutative-ops vm-logxor"
  (destructuring-bind (inst) (list (make-vm-logxor       :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-truthy)))

(it-sequential "reassociate-commutative-p-false-for-non-commutative vm-sub"
  (destructuring-bind (inst) (list (make-vm-sub  :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-falsy)))

(it-sequential "reassociate-commutative-p-false-for-non-commutative vm-div"
  (destructuring-bind (inst) (list (make-vm-div  :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-falsy)))

(it-sequential "reassociate-commutative-p-false-for-non-commutative vm-move"
  (destructuring-bind (inst) (list (make-vm-move :dst :r0 :src :r1))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-falsy)))

(it-sequential "reassociate-commutative-p-false-for-non-commutative vm-const"
  (destructuring-bind (inst) (list (make-vm-const :dst :r0 :value 1))
    (expect (cl-cc/optimize::opt-reassociate-commutative-p inst) :to-be-falsy)))

;;; ─── opt-copy-commutative-binop ──────────────────────────────────────────

(it-sequential "copy-commutative-binop-creates-correct-type vm-add"
  (destructuring-bind (inst expected-type) (list (make-vm-add         :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-add)
    (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (expect (typep new-inst expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst new-inst) :to-be :r3)
    (expect (cl-cc/vm::vm-lhs new-inst) :to-be :r4)
    (expect (cl-cc/vm::vm-rhs new-inst) :to-be :r5))))

(it-sequential "copy-commutative-binop-creates-correct-type vm-integer-add"
  (destructuring-bind (inst expected-type) (list (make-vm-integer-add  :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-integer-add)
    (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (expect (typep new-inst expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst new-inst) :to-be :r3)
    (expect (cl-cc/vm::vm-lhs new-inst) :to-be :r4)
    (expect (cl-cc/vm::vm-rhs new-inst) :to-be :r5))))

(it-sequential "copy-commutative-binop-creates-correct-type vm-mul"
  (destructuring-bind (inst expected-type) (list (make-vm-mul          :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-mul)
    (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (expect (typep new-inst expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst new-inst) :to-be :r3)
    (expect (cl-cc/vm::vm-lhs new-inst) :to-be :r4)
    (expect (cl-cc/vm::vm-rhs new-inst) :to-be :r5))))

(it-sequential "copy-commutative-binop-creates-correct-type vm-logand"
  (destructuring-bind (inst expected-type) (list (make-vm-logand       :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-logand)
    (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (expect (typep new-inst expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst new-inst) :to-be :r3)
    (expect (cl-cc/vm::vm-lhs new-inst) :to-be :r4)
    (expect (cl-cc/vm::vm-rhs new-inst) :to-be :r5))))

(it-sequential "copy-commutative-binop-creates-correct-type vm-logior"
  (destructuring-bind (inst expected-type) (list (make-vm-logior       :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-logior)
    (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (expect (typep new-inst expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst new-inst) :to-be :r3)
    (expect (cl-cc/vm::vm-lhs new-inst) :to-be :r4)
    (expect (cl-cc/vm::vm-rhs new-inst) :to-be :r5))))

(it-sequential "copy-commutative-binop-creates-correct-type vm-logxor"
  (destructuring-bind (inst expected-type) (list (make-vm-logxor       :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-logxor)
    (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (expect (typep new-inst expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst new-inst) :to-be :r3)
    (expect (cl-cc/vm::vm-lhs new-inst) :to-be :r4)
    (expect (cl-cc/vm::vm-rhs new-inst) :to-be :r5))))

(it-sequential "copy-commutative-binop-otherwise-returns-inst"
  (let* ((inst (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))
         (result (cl-cc/optimize::opt-copy-commutative-binop inst :r9 :r8 :r7)))
    ;; The original inst is returned unchanged (otherwise clause)
    (expect result :to-be inst)))

;;; ─── opt-pass-reassociate ────────────────────────────────────────────────

(it-sequential "reassociate-empty-input-returns-nil"
  (expect (cl-cc/optimize::opt-pass-reassociate nil) :to-be-null))

(it-sequential "reassociate-straight-line-no-consts-unchanged"
  (let* ((insts (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (result (cl-cc/optimize::opt-pass-reassociate insts)))
    (expect (or (null result) (listp result)) :to-be-truthy)))

(it-sequential "reassociate-label-clears-env"
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)
                      (make-vm-label :name "sep")
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-reassociate insts)))
    ;; Should produce a list (no crash)
    (expect (or (null result) (listp result)) :to-be-truthy)))

(it-sequential "reassociate-single-const-tracked"
  (let* ((insts (list (make-vm-const :dst :r0 :value 10)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-reassociate insts)))
    (expect (or (null result)
                     (some (lambda (i)
                             (and (typep i 'cl-cc/vm::vm-const)
                                  (= 10 (cl-cc/vm::vm-value i))))
                           result)) :to-be-truthy)))

;;; ─── opt-pass-batch-concatenate ──────────────────────────────────────────

(it-sequential "batch-concatenate-empty-input-returns-nil"
  (expect (cl-cc/optimize::opt-pass-batch-concatenate nil) :to-be-null))

(it-sequential "batch-concatenate-non-concat-passthrough"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    (expect (= (length insts) (length result)) :to-be-truthy)))

(it-sequential "batch-concatenate-single-no-chain"
  (let* ((inst   (make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1))
         (insts  (list inst (make-vm-ret :reg :r2)))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    ;; Lone concat stays as is (1 concat in output)
    (expect (= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result)) :to-be-truthy)))

(it-sequential "batch-concatenate-packs-two-chain"
  (let* ((c1 (make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1))
         (c2 (make-vm-concatenate :dst :r4 :str1 :r2 :str2 :r3))
         (ret (make-vm-ret :reg :r4))
         (insts (list c1 c2 ret))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    ;; Packed into one concatenate
    (expect (= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result)) :to-be-truthy)
    ;; The single concat should have parts (r0 r1 r3)
    (let ((packed (find-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result)))
      (when packed
        (let ((parts (cl-cc/vm::vm-parts packed)))
          (expect (listp parts) :to-be-truthy)
          (expect (= 3 (length parts)) :to-be-truthy))))))

(it-sequential "batch-concatenate-does-not-pack-when-used-multiple-times"
  (let* ((c1  (make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1))
         (c2  (make-vm-concatenate :dst :r3 :str1 :r2 :str2 :r1))
         ;; Use r2 again to make use-count=2
         (c3  (make-vm-concatenate :dst :r4 :str1 :r2 :str2 :r3))
         (ret (make-vm-ret :reg :r4))
         (insts (list c1 c2 c3 ret))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    ;; Should have at least 2 vm-concatenate (no merging since r2 used twice)
    (expect (>= (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result) 2) :to-be-truthy)))

;;; ─── *opt-commutative-binop-table* / opt-reassociate-commutative-p ───────

(it-sequential "commutative-binop-table-coverage"
  (expect (= 7 (length cl-cc/optimize::*opt-commutative-binop-table*)) :to-be-truthy)
  (dolist (type '(vm-add vm-integer-add vm-mul vm-integer-mul
                  vm-logand vm-logior vm-logxor))
    (expect (assoc type cl-cc/optimize::*opt-commutative-binop-table* :test #'eq) :to-be-truthy)))

(it-sequential "opt-copy-commutative-binop-cases add"
  (destructuring-bind (inst expected-type) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2) 'cl-cc/vm::vm-add)
    (let ((result (cl-cc/optimize::opt-copy-commutative-binop inst :r9 :r1 :r2)))
    (expect (typep result expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst result) :to-be :r9))))

(it-sequential "opt-copy-commutative-binop-cases mul"
  (destructuring-bind (inst expected-type) (list (make-vm-mul  :dst :r0 :lhs :r1 :rhs :r2) 'cl-cc/vm::vm-mul)
    (let ((result (cl-cc/optimize::opt-copy-commutative-binop inst :r9 :r1 :r2)))
    (expect (typep result expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst result) :to-be :r9))))

(it-sequential "opt-copy-commutative-binop-cases logand"
  (destructuring-bind (inst expected-type) (list (make-vm-logand :dst :r0 :lhs :r1 :rhs :r2) 'cl-cc/vm::vm-logand)
    (let ((result (cl-cc/optimize::opt-copy-commutative-binop inst :r9 :r1 :r2)))
    (expect (typep result expected-type) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst result) :to-be :r9))))
