;;;; tests/optimizer-tests.lisp — Optimizer Tests
;;;
;;; Tests verifying that the multi-pass optimizer:
;;;   - Correctly folds constants at compile time (semantics preserved)
;;;   - Eliminates dead code (pure instructions with unused results)
;;;   - Removes dead jumps and unreachable code
;;;   - Preserves all observable semantics (evaluated values)

(in-package :cl-cc/test)


;;; ── Helpers ──────────────────────────────────────────────────────────────

(defun opt-count-instr (expr type-sym)
  "Compile EXPR, run optimizer, count instructions of TYPE-SYM in the result."
  (let* ((result   (compile-string expr :target :vm))
         (instrs   (optimize-instructions (%get-instructions result)))
         (cl-type  (find-symbol (symbol-name type-sym) :cl-cc)))
    (count-if (lambda (i) (typep i cl-type)) instrs)))

(defun opt-has-p (expr type-sym)
  "T if compiling EXPR leaves at least one instruction of TYPE-SYM after optimization."
  (> (opt-count-instr expr type-sym) 0))

(defun make-bswap-tree-instructions ()
  "Construct the canonical masked-shift tree for a 32-bit byte swap."
  (list (make-vm-const :dst :r1 :value #xFF)
        (make-vm-logand :dst :r2 :lhs :r0 :rhs :r1)
        (make-vm-const :dst :r3 :value 24)
        (make-vm-ash   :dst :r4 :lhs :r2 :rhs :r3)
        (make-vm-const :dst :r5 :value #xFF00)
        (make-vm-logand :dst :r6 :lhs :r0 :rhs :r5)
        (make-vm-const :dst :r7 :value 8)
        (make-vm-ash   :dst :r8 :lhs :r6 :rhs :r7)
        (make-vm-const :dst :r9 :value #xFF0000)
        (make-vm-logand :dst :r10 :lhs :r0 :rhs :r9)
        (make-vm-const :dst :r11 :value -8)
        (make-vm-ash   :dst :r12 :lhs :r10 :rhs :r11)
        (make-vm-const :dst :r13 :value #xFF000000)
        (make-vm-logand :dst :r14 :lhs :r0 :rhs :r13)
        (make-vm-const :dst :r15 :value -24)
        (make-vm-ash   :dst :r16 :lhs :r14 :rhs :r15)
        (make-vm-logior :dst :r17 :lhs :r4 :rhs :r8)
        (make-vm-logior :dst :r18 :lhs :r12 :rhs :r16)
        (make-vm-logior :dst :r19 :lhs :r17 :rhs :r18)))

;;; ── Constant Folding ─────────────────────────────────────────────────────
;;; Each case verifies: (1) correct runtime value, (2) binary-op eliminated.

(it-sequential "optimizer-fold add"
  (destructuring-bind (expected expr instr) (list 7 "(+ 3 4)" 'vm-add)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold sub"
  (destructuring-bind (expected expr instr) (list 6 "(- 10 4)" 'vm-sub)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold mul"
  (destructuring-bind (expected expr instr) (list 12 "(* 3 4)" 'vm-mul)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold chained"
  (destructuring-bind (expected expr instr) (list 12 "(+ (* 2 3) (- 10 4))" 'vm-add)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-fold deeper-chain"
  (destructuring-bind (expected expr instr) (list 16 "(- (* 5 5) (* 3 3))" 'vm-sub)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

;;; ── Algebraic Identity Simplification ───────────────────────────────────
;;; Each case verifies: (1) correct runtime value, (2) instruction eliminated.

(it-sequential "optimizer-algebraic-identity add-zero"
  (destructuring-bind (expected expr instr) (list 5 "(let ((x 5))  (+ x 0))" 'vm-add)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-identity mul-one"
  (destructuring-bind (expected expr instr) (list 7 "(let ((x 7))  (* x 1))" 'vm-mul)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-identity mul-zero"
  (destructuring-bind (expected expr instr) (list 0 "(let ((x 99)) (* x 0))" 'vm-mul)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-identity sub-zero"
  (destructuring-bind (expected expr instr) (list 42 "(let ((x 42)) (- x 0))" 'vm-sub)
    (assert-run= expected expr) (expect (opt-has-p expr instr) :to-be-falsy)))

(it-sequential "optimizer-algebraic-type-producer-identity null-of-cons"
  (destructuring-bind (expected-value producer predicate) (list 0 (make-vm-cons :dst :r1 :car-src :r0 :cdr-src :r0) (make-vm-null-p :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize::opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity consp-of-cons"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-cons :dst :r1 :car-src :r0 :cdr-src :r0) (make-vm-cons-p :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize::opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity listp-of-cons"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-cons :dst :r1 :car-src :r0 :cdr-src :r0) (make-vm-listp :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize::opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity stringp-of-concatenate"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-concatenate :dst :r1 :str1 :r0 :str2 :r0) (make-vm-stringp :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize::opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-algebraic-type-producer-identity vectorp-of-coerce-to-vector"
  (destructuring-bind (expected-value producer predicate) (list 1 (make-vm-coerce-to-vector :dst :r1 :src :r0) (make-vm-vectorp :dst :r2 :src :r1))
    (let ((out (cl-cc/optimize::opt-pass-fold (list producer predicate))))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r2)
                              (eql (cl-cc/vm::vm-value i) expected-value)))
                       out) :to-be-truthy)
     (expect (some (lambda (i) (typep i (type-of predicate))) out) :to-be-falsy))))

(it-sequential "optimizer-sequence-fusion-recognizes-map-filter-chain"
  (let* ((instrs (list (make-vm-func-ref :dst :f-map :label 'mapcar)
                       (make-vm-func-ref :dst :f-filter :label 'remove-if)
                       (make-vm-call :dst :mapped :func :f-map :args '(:fn :xs))
                       (make-vm-call :dst :filtered :func :f-filter :args '(:pred :mapped))))
         (out (cl-cc/optimize::opt-pass-sequence-fusion instrs)))
    (expect (cl-cc/optimize::%opt-sequence-fusion-candidate-p instrs) :to-be-truthy)
    (expect out :to-be instrs)))

(it-sequential "optimizer-demand-analysis-records-absent-params"
  (let* ((instrs (list (make-vm-func-ref :dst :f :label 'demand-fixture :params '(:x :y))
                       (make-vm-label :name 'demand-fixture)
                       (make-vm-ret :reg :x)))
         (summaries (cl-cc/optimize::opt-analyze-program-demand instrs))
         (summary (gethash 'demand-fixture summaries)))
    (expect summary :to-be-truthy)
    (expect (member :x (cl-cc/optimize::opt-demand-summary-strict-params summary)) :to-be-truthy)
    (expect (member :y (cl-cc/optimize::opt-demand-summary-absent-params summary)) :to-be-truthy)))

(it-sequential "optimizer-dead-store-elim-collections-removes-overwritten-aset"
  (let* ((store1 (make-vm-aset :array-reg :arr :index-reg :idx :val-reg :v1))
         (store2 (make-vm-aset :array-reg :arr :index-reg :idx :val-reg :v2))
         (ret (make-vm-ret :reg :arr))
         (out (cl-cc/optimize::opt-pass-dead-store-elim (list store1 store2 ret))))
    (expect (member store1 out :test #'eq) :to-be-falsy)
    (expect (member store2 out :test #'eq) :to-be-truthy)))

;;; ── Dead Code Elimination: Semantics ─────────────────────────────────────

(it-sequential "optimizer-dce progn-dead-exprs"
  (destructuring-bind (expected expr) (list 99 "(progn (+ 1 2) (+ 3 4) 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-dce let-nested"
  (destructuring-bind (expected expr) (list 3 "(let ((x 1)) (let ((y 2)) (+ x y)))")
    (assert-run= expected expr)))

(it-sequential "optimizer-dce mixed-dead-subform"
  (destructuring-bind (expected expr) (list 42 "(let ((x 42)) (+ 0 x))")
    (assert-run= expected expr)))

;;; ── Constant Branch Elimination: Semantics ───────────────────────────────

(it-sequential "optimizer-branch-fold const-true"
  (destructuring-bind (expected expr) (list 42 "(if 1 42 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-branch-fold zero-true"
  (destructuring-bind (expected expr) (list 42 "(if 0 42 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-branch-fold t-true"
  (destructuring-bind (expected expr) (list 42 "(if t 42 99)")
    (assert-run= expected expr)))

(it-sequential "optimizer-branch-nil-false"
  (assert-run-false "(if nil 42 nil)"))

(it-sequential "optimizer-hot-cold-layout-keeps-signal-block-last"
  (let* ((hot   (make-vm-label :name "hot"))
         (jump  (make-vm-jump-zero :reg :r0 :label "cold"))
         (work  (make-vm-const :dst :r1 :value 1))
         (toend (make-vm-jump :label "end"))
         (cold  (make-vm-label :name "cold"))
         (sig   (cl-cc:make-vm-signal-error :error-reg :r1))
         (end   (make-vm-label :name "end"))
         (ret   (make-vm-ret :reg :r1))
         (cfg   (cl-cc/optimize:cfg-build (list hot jump work toend cold sig end ret)))
         (_idom (cl-cc/optimize:cfg-compute-dominators cfg))
         (_loop (cl-cc/optimize:cfg-compute-loop-depths cfg))
         (out   (cl-cc/optimize:cfg-flatten-hot-cold cfg))
         (labels (loop for inst in out
                        when (typep inst 'cl-cc/vm::vm-label)
                        collect (cl-cc/vm::vm-name inst))))
    (declare (ignore _idom _loop))
    (expect (member "hot" labels :test #'equal) :to-be-truthy)
    (expect (member "cold" labels :test #'equal) :to-be-truthy)
    (expect (member "end" labels :test #'equal) :to-be-truthy)
    (expect (< (position "hot" labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy)
    (expect (< (position "end" labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy)))

;;; ── Conditional Truthiness Preservation ──────────────────────────────────

(it-sequential "optimizer-branch-lowering preserves-truthiness const-true" (assert-run= 42 "(if 1 42 99)"))

(it-sequential "optimizer-branch-lowering preserves-truthiness zero-true" (assert-run= 42 "(if 0 42 99)"))

;;; ── Dominated Predicate Elimination ─────────────────────────────────────

(it-sequential "optimizer-dominated-check-elim null-p via dominated-type-check-elim"
  (destructuring-bind (pass-fn p1 p2 instr-type) (list #'cl-cc/optimize:opt-pass-dominated-type-check-elim (make-vm-null-p :dst :r1 :src :r0) (make-vm-null-p :dst :r2 :src :r0) 'cl-cc/vm::vm-null-p)
    (let* ((c    (make-vm-const :dst :r0 :value nil))
         (br   (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out  (funcall pass-fn (list c p1 br then p2 ret1 else ret0))))
    (expect (count-if (lambda (i) (typep i instr-type)) out) :to-equal 1)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) out) :to-be-truthy))))

(it-sequential "optimizer-dominated-check-elim not via dominated-type-check-elim"
  (destructuring-bind (pass-fn p1 p2 instr-type) (list #'cl-cc/optimize:opt-pass-dominated-type-check-elim (make-vm-not :dst :r1 :src :r0) (make-vm-not :dst :r2 :src :r0) 'cl-cc/vm::vm-not)
    (let* ((c    (make-vm-const :dst :r0 :value nil))
         (br   (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out  (funcall pass-fn (list c p1 br then p2 ret1 else ret0))))
    (expect (count-if (lambda (i) (typep i instr-type)) out) :to-equal 1)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) out) :to-be-truthy))))

(it-sequential "optimizer-dominated-check-elim not via nil-check-elim"
  (destructuring-bind (pass-fn p1 p2 instr-type) (list #'cl-cc/optimize:opt-pass-dominated-type-check-elim (make-vm-not :dst :r1 :src :r0) (make-vm-not :dst :r2 :src :r0) 'cl-cc/vm::vm-not)
    (let* ((c    (make-vm-const :dst :r0 :value nil))
         (br   (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out  (funcall pass-fn (list c p1 br then p2 ret1 else ret0))))
    (expect (count-if (lambda (i) (typep i instr-type)) out) :to-equal 1)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const branch-correlation-true-edge"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c    (make-vm-const :dst :r0 :value 1))
                    (p1   (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
                    (br   (make-vm-jump-zero :reg :r1 :label "else"))
                    (then (make-vm-label :name "then"))
                    (p2   (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
                    (ret1 (make-vm-ret :reg :r2))
                    (else (make-vm-label :name "else"))
                    (ret0 (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-branch-correlation
                (list c p1 br then p2 ret1 else ret0)))) :r2 1)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const branch-correlation-false-edge"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c    (make-vm-const :dst :r0 :value 1))
                    (p1   (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
                    (br   (make-vm-jump-zero :reg :r1 :label "else"))
                    (then (make-vm-label :name "then"))
                    (ret1 (make-vm-ret :reg :r1))
                    (else (make-vm-label :name "else"))
                    (p2   (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
                    (ret0 (make-vm-ret :reg :r2)))
                (cl-cc/optimize::opt-pass-branch-correlation
                 (list c p1 br then ret1 else p2 ret0)))) :r2 0)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const branch-correlation-join-agrees"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((pred-a (make-vm-label :name "pred-a"))
                    (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
                    (br-a   (make-vm-jump-zero :reg :r1 :label "join"))
                    (pred-b (make-vm-label :name "pred-b"))
                    (p2     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
                    (br-b   (make-vm-jump-zero :reg :r2 :label "join"))
                    (then   (make-vm-label :name "then"))
                    (ret2   (make-vm-ret :reg :r2))
                    (join   (make-vm-label :name "join"))
                    (p3     (cl-cc:make-vm-integer-p :dst :r3 :src :r0))
                    (ret3   (make-vm-ret :reg :r3)))
               (cl-cc/optimize::opt-pass-branch-correlation
                (list pred-a p1 br-a pred-b p2 br-b then ret2 join p3 ret3)))) :r3 0)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-rational-unary"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 3/4))
                    (u1  (cl-cc:make-vm-numerator :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-fold (list c1 u1 ret)))) :r1 3)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-car-constant-cons"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value '(11 . 12)))
                    (u1  (make-vm-car :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-fold (list c1 u1 ret)))) :r1 11)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-cdr-constant-cons"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value '(11 . 12)))
                    (u1  (make-vm-cdr :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-fold (list c1 u1 ret)))) :r1 12)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-rational-binary"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 8))
                    (c2  (make-vm-const :dst :r1 :value 12))
                    (g   (cl-cc:make-vm-gcd :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize::opt-pass-fold (list c1 c2 g ret)))) :r2 4)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-rational-cl-div"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 3))
                    (c2  (make-vm-const :dst :r1 :value 4))
                    (d   (cl-cc/vm::make-vm-cl-div :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize::opt-pass-fold (list c1 c2 d ret)))) :r2 3/4)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-pass-produces-const fold-floor-div"
  (destructuring-bind (make-out expected-dst expected-value) (list (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value -7))
                    (c2  (make-vm-const :dst :r1 :value 2))
                    (d   (cl-cc:make-vm-div :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize::opt-pass-fold (list c1 c2 d ret)))) :r2 -4)
    (let ((out (funcall make-out)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy))))

(it-sequential "optimizer-branch-correlation-join-disagrees"
  (let* ((pred-a (make-vm-label :name "pred-a"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br-a   (make-vm-jump-zero :reg :r1 :label "join"))
         (pred-b (make-vm-label :name "pred-b"))
         (p2     (cl-cc:make-vm-integer-p :dst :r2 :src :r4))
         (br-b   (make-vm-jump-zero :reg :r2 :label "other"))
         (join   (make-vm-label :name "join"))
         (p3     (cl-cc:make-vm-integer-p :dst :r3 :src :r0))
         (ret3   (make-vm-ret :reg :r3))
         (other  (make-vm-label :name "other"))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred-a p1 br-a pred-b p2 br-b join p3 ret3 other ret2))))
    (expect (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-const)
                               (eq :r3 (cl-cc/vm::vm-dst i))))
                        out) :to-be-falsy)))

;;; ── Data-Table Coverage Tests ──────────────────────────────────────────────

(it-sequential "optimizer-autovec-op-kind-table add"
  (destructuring-bind (inst expected-op) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2) :add)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table sub"
  (destructuring-bind (inst expected-op) (list (make-vm-sub  :dst :r0 :lhs :r1 :rhs :r2) :sub)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table mul"
  (destructuring-bind (inst expected-op) (list (make-vm-mul  :dst :r0 :lhs :r1 :rhs :r2) :mul)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table logand"
  (destructuring-bind (inst expected-op) (list (make-vm-logand :dst :r0 :lhs :r1 :rhs :r2) :logand)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table logior"
  (destructuring-bind (inst expected-op) (list (make-vm-logior :dst :r0 :lhs :r1 :rhs :r2) :logior)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table logxor"
  (destructuring-bind (inst expected-op) (list (make-vm-logxor :dst :r0 :lhs :r1 :rhs :r2) :logxor)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table min"
  (destructuring-bind (inst expected-op) (list (make-vm-min  :dst :r0 :lhs :r1 :rhs :r2) :min)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-autovec-op-kind-table max"
  (destructuring-bind (inst expected-op) (list (make-vm-max  :dst :r0 :lhs :r1 :rhs :r2) :max)
    (expect (cl-cc/optimize::%opt-autovec-op-kind inst) :to-be expected-op)))

(it-sequential "optimizer-control-or-label-deftype label"
  (destructuring-bind (inst expected) (list (make-vm-label :name "l") t)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype jump"
  (destructuring-bind (inst expected) (list (make-vm-jump :label "l") t)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype jump-zero"
  (destructuring-bind (inst expected) (list (make-vm-jump-zero :reg :r0 :label "l") t)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype ret"
  (destructuring-bind (inst expected) (list (make-vm-ret :reg :r0) t)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype call"
  (destructuring-bind (inst expected) (list (make-vm-call :dst :r0 :func :r1 :args nil) t)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype const"
  (destructuring-bind (inst expected) (list (make-vm-const :dst :r0 :value 42) nil)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype add"
  (destructuring-bind (inst expected) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2) nil)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-control-or-label-deftype move"
  (destructuring-bind (inst expected) (list (make-vm-move :dst :r0 :src :r1) nil)
    (expect (typep inst 'cl-cc/optimize::opt-control-or-label) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table string-length-string"
  (destructuring-bind (inst value expected) (list (make-vm-string-length  :dst :r0 :src :r1) "hello" t)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table string-length-number"
  (destructuring-bind (inst value expected) (list (make-vm-string-length  :dst :r0 :src :r1) 42 nil)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-not-anything"
  (destructuring-bind (inst value expected) (list (make-vm-not :dst :r0 :src :r1) 99 t)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-not-nil"
  (destructuring-bind (inst value expected) (list (make-vm-not :dst :r0 :src :r1) nil t)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table car-cons"
  (destructuring-bind (inst value expected) (list (make-vm-car :dst :r0 :src :r1) '(a . b) t)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table car-nil"
  (destructuring-bind (inst value expected) (list (make-vm-car :dst :r0 :src :r1) nil t)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table car-number"
  (destructuring-bind (inst value expected) (list (make-vm-car :dst :r0 :src :r1) 42 nil)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-neg-number"
  (destructuring-bind (inst value expected) (list (make-vm-neg :dst :r0 :src :r1) 7 t)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

(it-sequential "optimizer-fold-eligible-predicates-table vm-neg-string"
  (destructuring-bind (inst value expected) (list (make-vm-neg :dst :r0 :src :r1) "x" nil)
    (expect (cl-cc/optimize::%fold-unary-constant-eligible-p inst value) :to-equal expected)))

;;; End-to-end, bitwise, unary folding, inlining, and prolog peephole tests are in
;;; optimizer-e2e-tests.lisp. Low-level pass tests are in optimizer-tests-lowlevel2.lisp.
