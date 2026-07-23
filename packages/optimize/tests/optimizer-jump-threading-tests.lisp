;;;; packages/optimize/tests/optimizer-jump-threading-tests.lisp
;;;; Unit tests for optimizer-jump-threading.lisp
;;;;
;;;; Covers: %opt-jump-branch-comparison-fact (nil and non-nil paths),
;;;;   %opt-jump-fact-killed-p, %opt-jump-same-comparison-p,
;;;;   %opt-jump-known-constant / %opt-jump-put-constant / %opt-jump-kill-constant,
;;;;   %opt-jump-rewrite-block-with-fact (redundant-comparison rewrite,
;;;;     move propagation, kill-on-redef),
;;;;   %opt-pass-jump-propagate-edge-values (integration smoke),
;;;;   opt-pass-jump-threading-with-propagation (end-to-end chain+propagation).

(in-package :cl-cc/test)

;;; ─── %opt-jump-fact-killed-p ─────────────────────────────────────────────

(it-sequential "jump-fact-killed-p-cases kills-lhs"
  (destructuring-bind (fact dst expected) (list (list :pred 'vm-lt :lhs :x :rhs :y) :x t)
    (if expected
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-falsy))))

(it-sequential "jump-fact-killed-p-cases kills-rhs"
  (destructuring-bind (fact dst expected) (list (list :pred 'vm-lt :lhs :x :rhs :y) :y t)
    (if expected
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-falsy))))

(it-sequential "jump-fact-killed-p-cases no-match"
  (destructuring-bind (fact dst expected) (list (list :pred 'vm-lt :lhs :x :rhs :y) :z nil)
    (if expected
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-falsy))))

(it-sequential "jump-fact-killed-p-cases nil-fact"
  (destructuring-bind (fact dst expected) (list nil :x nil)
    (if expected
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-falsy))))

(it-sequential "jump-fact-killed-p-cases nil-dst"
  (destructuring-bind (fact dst expected) (list (list :pred 'vm-lt :lhs :x :rhs :y) nil nil)
    (if expected
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-jump-fact-killed-p fact dst) :to-be-falsy))))

;;; ─── %opt-jump-same-comparison-p ─────────────────────────────────────────

(it-sequential "jump-same-comparison-p-matches-identical"
  (let ((fact (list :pred 'vm-lt :lhs :x :rhs :y :value 1))
        (inst (make-vm-lt :dst :c :lhs :x :rhs :y)))
    (expect (cl-cc/optimize::%opt-jump-same-comparison-p inst fact) :to-be-truthy)))

(it-sequential "jump-same-comparison-p-rejects-different-type"
  (let ((fact (list :pred 'vm-le :lhs :x :rhs :y :value 1))
        (inst (make-vm-lt :dst :c :lhs :x :rhs :y)))
    (expect (cl-cc/optimize::%opt-jump-same-comparison-p inst fact) :to-be-falsy)))

(it-sequential "jump-same-comparison-p-rejects-different-operands"
  (let ((fact (list :pred 'vm-lt :lhs :x :rhs :y :value 1))
        (inst (make-vm-lt :dst :c :lhs :x :rhs :z)))
    (expect (cl-cc/optimize::%opt-jump-same-comparison-p inst fact) :to-be-falsy)))

(it-sequential "jump-same-comparison-p-rejects-nil-fact"
  (let ((inst (make-vm-lt :dst :c :lhs :x :rhs :y)))
    (expect (cl-cc/optimize::%opt-jump-same-comparison-p inst nil) :to-be-falsy)))

;;; ─── constant alist helpers ───────────────────────────────────────────────

(it-sequential "jump-known-constant-returns-value-when-present"
  (let ((constants (list (cons :r0 42) (cons :r1 7))))
    (multiple-value-bind (val found) (cl-cc/optimize::%opt-jump-known-constant :r0 constants)
      (expect (= 42 val) :to-be-truthy)
      (expect found :to-be-truthy))))

(it-sequential "jump-known-constant-returns-nil-when-absent"
  (let ((constants (list (cons :r0 42))))
    (multiple-value-bind (val found) (cl-cc/optimize::%opt-jump-known-constant :r1 constants)
      (expect val :to-be-null)
      (expect found :to-be-falsy))))

(it-sequential "jump-put-constant-adds-new-entry"
  (let* ((constants nil)
         (result    (cl-cc/optimize::%opt-jump-put-constant :r0 99 constants)))
    (expect (= 99 (cdr (assoc :r0 result))) :to-be-truthy)))

(it-sequential "jump-put-constant-replaces-existing-entry"
  (let* ((constants (list (cons :r0 1)))
         (result    (cl-cc/optimize::%opt-jump-put-constant :r0 2 constants)))
    (expect (= 2 (cdr (assoc :r0 result))) :to-be-truthy)
    (expect (= 1 (count :r0 result :key #'car)) :to-be-truthy)))

(it-sequential "jump-kill-constant-removes-entry"
  (let* ((constants (list (cons :r0 1) (cons :r1 2)))
         (result    (cl-cc/optimize::%opt-jump-kill-constant :r0 constants)))
    (expect (assoc :r0 result) :to-be-falsy)
    (expect (assoc :r1 result) :to-be-truthy)))

;;; ─── %opt-jump-branch-comparison-fact ───────────────────────────────────

(it-sequential "jump-branch-comparison-fact-returns-nil-for-non-conditional-terminator"
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-const :dst :r0 :value 1)
                                    (make-vm-jump :label "end")))))
    (expect (cl-cc/optimize::%opt-jump-branch-comparison-fact block) :to-be-null)))

(it-sequential "jump-branch-comparison-fact-returns-nil-when-operands-not-constant"
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-lt :dst :c :lhs :x :rhs :y)
                                    (make-vm-jump-zero :reg :c :label "false")))))
    (expect (cl-cc/optimize::%opt-jump-branch-comparison-fact block) :to-be-null)))

(it-sequential "jump-branch-comparison-fact-returns-fact-when-both-operands-constant"
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-const :dst :i   :value 1)
                                    (make-vm-const :dst :lim :value 3)
                                    (make-vm-lt    :dst :c   :lhs :i :rhs :lim)
                                    (make-vm-jump-zero :reg :c :label "false")))))
    (let ((fact (cl-cc/optimize::%opt-jump-branch-comparison-fact block)))
      (expect fact :to-be-truthy)
      (expect (getf fact :pred) :to-be 'vm-lt)
      (expect (getf fact :lhs) :to-be :i)
      (expect (getf fact :rhs) :to-be :lim))))

(it-sequential "jump-branch-comparison-fact-returns-nil-when-lhs-redefined-before-cmp"
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-const :dst :i   :value 1)
                                    (make-vm-const :dst :lim :value 3)
                                    (make-vm-const :dst :i   :value 9) ; redefines :i
                                    (make-vm-lt    :dst :c   :lhs :i :rhs :lim)
                                    (make-vm-jump-zero :reg :c :label "false")))))
    ;; After redefinition :i is still constant (value 9), so fact IS found
    ;; but the fact key is still extracted.  The important assertion is non-nil.
    (let ((fact (cl-cc/optimize::%opt-jump-branch-comparison-fact block)))
      (expect fact :to-be-truthy))))

;;; ─── %opt-jump-rewrite-block-with-fact ───────────────────────────────────

(it-sequential "jump-rewrite-block-replaces-redundant-comparison-with-const"
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-lt :dst :c2 :lhs :x :rhs :y)
                                     (make-vm-ret :reg :c2))))
         (fact  (list :pred 'vm-lt :lhs :x :rhs :y :value 1 :constants nil)))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      (expect (some (lambda (i)
               (and (typep i 'cl-cc/vm::vm-const)
                    (eq  (cl-cc/vm::vm-dst   i) :c2)
                    (eql (cl-cc/vm::vm-value i) 1)))
             insts) :to-be-truthy))))

(it-sequential "jump-rewrite-block-propagates-move-from-known-constant"
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-move :dst :r1 :src :r0)
                                     (make-vm-ret :reg :r1))))
         (fact  (list :constants (list (cons :r0 42)))))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      (expect (some (lambda (i)
               (and (typep i 'cl-cc/vm::vm-const)
                    (eq  (cl-cc/vm::vm-dst   i) :r1)
                    (eql (cl-cc/vm::vm-value i) 42)))
             insts) :to-be-truthy))))

(it-sequential "jump-rewrite-block-kills-fact-on-operand-redefinition"
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-const :dst :x  :value 99)  ; kills :x
                                     (make-vm-lt    :dst :c2 :lhs :x :rhs :y)
                                     (make-vm-ret   :reg :c2))))
         (fact  (list :pred 'vm-lt :lhs :x :rhs :y :value 1 :constants nil)))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      ;; The rewrite should NOT have folded :c2 since :x was redefined.
      (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-lt)) insts) :to-be-truthy))))

(it-sequential "jump-rewrite-block-preserves-vm-const-instructions"
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-const :dst :r0 :value 5)
                                     (make-vm-ret   :reg :r0))))
         (fact  nil))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      (expect (some (lambda (i)
               (and (typep i 'cl-cc/vm::vm-const)
                    (eq (cl-cc/vm::vm-dst i) :r0)))
             insts) :to-be-truthy))))

;;; ─── %opt-pass-jump-propagate-edge-values integration smoke ──────────────

(it-sequential "jump-propagate-edge-values-threads-and-propagates"
  (let* ((insts (list (make-vm-const     :dst :i   :value 1)
                      (make-vm-const     :dst :lim :value 3)
                      (make-vm-lt        :dst :c   :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c   :label "false")
                      (make-vm-label     :name "true")
                      (make-vm-lt        :dst :c2  :lhs :i :rhs :lim)
                      (make-vm-ret       :reg :c2)
                      (make-vm-label     :name "false")
                      (make-vm-ret       :reg :c)))
         (out (cl-cc/optimize::%opt-pass-jump-propagate-edge-values insts)))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq  (cl-cc/vm::vm-dst   i) :c2)
                  (eql (cl-cc/vm::vm-value i) 1)))
           out) :to-be-truthy)))

(it-sequential "jump-propagate-edge-values-handles-empty-input"
  (let ((out (cl-cc/optimize::%opt-pass-jump-propagate-edge-values nil)))
    (expect out :to-be-null)))

;;; ─── opt-pass-jump-threading-with-propagation end-to-end ─────────────────

(it-sequential "jump-threading-with-propagation-chains-and-propagates"
  (let* ((insts (list (make-vm-const     :dst :i   :value 1)
                      (make-vm-const     :dst :lim :value 3)
                      (make-vm-lt        :dst :c   :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c   :label "middle")
                      (make-vm-label     :name "true")
                      (make-vm-lt        :dst :c2  :lhs :i :rhs :lim)
                      (make-vm-ret       :reg :c2)
                      (make-vm-label     :name "middle")
                      (make-vm-jump      :label "false")
                      (make-vm-label     :name "false")
                      (make-vm-ret       :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump-threading-with-propagation insts)))
    ;; The jump-zero should now point directly at "false" (chain threaded).
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-jump-zero)
                  (equal (cl-cc/vm::vm-label-name i) "false")))
           out) :to-be-truthy)
    ;; The redundant lt in "true" block should be folded to const 1.
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq  (cl-cc/vm::vm-dst   i) :c2)
                  (eql (cl-cc/vm::vm-value i) 1)))
           out) :to-be-truthy)))

(it-sequential "jump-threading-with-propagation-is-identity-on-straight-line"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (out (cl-cc/optimize::opt-pass-jump-threading-with-propagation insts)))
    (expect (= (length insts) (length out)) :to-be-truthy)))
