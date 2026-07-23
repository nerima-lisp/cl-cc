;;;; optimizer-strength-inline-tests.lisp
;;;
;;; Integration tests for strength-reduction, reassociation,
;;; mul-by-const decomposition, adaptive inlining, and optimizer
;;; helper predicates.
;;;
;;; Extracted from optimizer-lowlevel-tests.lisp (> 600 lines).

(in-package :cl-cc/test)


;;; ── opt-pass-strength-reduce: Strength Reduction ─────────────────────────

(it-sequential "strength-reduce-cases mul-rhs-pow2"
  (destructuring-bind (const-val op verify) (list 8 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1) (lambda (out op)
             (expect (member op out) :to-be-falsy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out) :to-be-truthy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

(it-sequential "strength-reduce-cases mul-lhs-pow2"
  (destructuring-bind (const-val op verify) (list 4 (make-vm-mul :dst :r2 :lhs :r1 :rhs :r0) (lambda (out op)
             (expect (member op out) :to-be-falsy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out) :to-be-truthy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

(it-sequential "strength-reduce-cases mul-non-power-of-2"
  (destructuring-bind (const-val op verify) (list 7 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1) (lambda (out op)
             (expect (member op out) :to-be-truthy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out) :to-be-falsy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

(it-sequential "strength-reduce-cases div-rhs-pow2"
  (destructuring-bind (const-val op verify) (list 8 (cl-cc:make-vm-div :dst :r2 :lhs :r0 :rhs :r1) (lambda (out op)
             (expect (member op out) :to-be-falsy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out) :to-be-truthy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

(it-sequential "strength-reduce-cases div-non-power-of-2"
  (destructuring-bind (const-val op verify) (list 7 (cl-cc:make-vm-div :dst :r2 :lhs :r0 :rhs :r1) (lambda (out op)
             (expect (member op out) :to-be-truthy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out) :to-be-falsy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

(it-sequential "strength-reduce-cases mod-pow2"
  (destructuring-bind (const-val op verify) (list 8 (cl-cc:make-vm-mod :dst :r2 :lhs :r0 :rhs :r1) (lambda (out op)
             (expect (member op out) :to-be-falsy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) out) :to-be-truthy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

(it-sequential "strength-reduce-cases mod-non-power-of-2"
  (destructuring-bind (const-val op verify) (list 7 (cl-cc:make-vm-mod :dst :r2 :lhs :r0 :rhs :r1) (lambda (out op)
             (expect (member op out) :to-be-truthy)
             (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) out) :to-be-falsy)))
    (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op))))

;;; ── opt-pass-reassociate: Arithmetic Reassociation ──────────────────────

(it-sequential "reassociate-moves-constant-inward add"
  (destructuring-bind (const-val op1 op2 op-type) (list 1 (make-vm-add    :dst :r4 :lhs :r2 :rhs :r1) (make-vm-add    :dst :r5 :lhs :r4 :rhs :r3) 'cl-cc/vm::vm-add)
    (let* ((c    (make-vm-const :dst :r1 :value const-val))
         (a    (make-vm-move  :dst :r2 :src :r8))
         (b    (make-vm-move  :dst :r3 :src :r9))
         (ret  (make-vm-ret   :reg :r5))
         (out  (cl-cc/optimize::opt-pass-reassociate (list c a b op1 op2 ret)))
         (ops  (remove-if-not (lambda (i) (typep i op-type)) out)))
    (expect (or (null ops)
                     (= 2 (length ops))) :to-be-truthy))))

(it-sequential "reassociate-moves-constant-inward logand"
  (destructuring-bind (const-val op1 op2 op-type) (list 255 (make-vm-logand :dst :r4 :lhs :r2 :rhs :r1) (make-vm-logand :dst :r5 :lhs :r4 :rhs :r3) 'cl-cc/vm::vm-logand)
    (let* ((c    (make-vm-const :dst :r1 :value const-val))
         (a    (make-vm-move  :dst :r2 :src :r8))
         (b    (make-vm-move  :dst :r3 :src :r9))
         (ret  (make-vm-ret   :reg :r5))
         (out  (cl-cc/optimize::opt-pass-reassociate (list c a b op1 op2 ret)))
         (ops  (remove-if-not (lambda (i) (typep i op-type)) out)))
    (expect (or (null ops)
                     (= 2 (length ops))) :to-be-truthy))))

(it-sequential "strength-reduce-mul-by-const-decomposes"
  (let* ((c   (make-vm-const :dst :r1 :value 3))
         (mul (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c mul ret))))
    (expect (member mul out) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) out) :to-be-truthy)))

;;; ── %opt-mul-by-const-seq: Extracted Shift/Add Decomposition ────────────

(defun make-test-new-reg ()
  "Return a thunk that allocates fresh keyword registers :R1000, :R1001, ..."
  (let ((n 1000))
    (lambda () (prog1 (intern (format nil "R~A" n) :keyword) (incf n)))))

(it-sequential "mul-by-const-seq-cases zero"
  (destructuring-bind (n expected-type-1 expected-type-2) (list 0 'cl-cc/vm::vm-const nil)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-cases one"
  (destructuring-bind (n expected-type-1 expected-type-2) (list 1 'cl-cc/vm::vm-move nil)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-cases neg-one"
  (destructuring-bind (n expected-type-1 expected-type-2) (list -1 'cl-cc/vm::vm-move 'cl-cc/vm::vm-neg)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-cases two"
  (destructuring-bind (n expected-type-1 expected-type-2) (list 2 'cl-cc/vm::vm-ash nil)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-cases three"
  (destructuring-bind (n expected-type-1 expected-type-2) (list 3 'cl-cc/vm::vm-add nil)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-cases four"
  (destructuring-bind (n expected-type-1 expected-type-2) (list 4 'cl-cc/vm::vm-ash nil)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-cases six"
  (destructuring-bind (n expected-type-1 expected-type-2) (list 6 'cl-cc/vm::vm-add nil)
    (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (expect (some (lambda (i) (typep i expected-type-1)) seq) :to-be-truthy)
    (when expected-type-2
      (expect (some (lambda (i) (typep i expected-type-2)) seq) :to-be-truthy)))))

(it-sequential "mul-by-const-seq-correctness"
  (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 5 (make-test-new-reg)))
         (add (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) seq)))
    (expect add :to-be-truthy)
    (expect (cl-cc/vm::vm-dst add) :to-equal :r0)))

;;; ── opt-inline-eligible-p: Extracted Eligibility Predicate ──────────────

(it-sequential "opt-inline-eligible-p-cases short-no-captures"
  (destructuring-bind (captured inline-policy body expected) (list nil nil (list (make-vm-add :dst :r2 :lhs :r1 :rhs :r1) (make-vm-ret :reg :r2)) t)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-inline-eligible-p-cases captured-vars-ineligible"
  (destructuring-bind (captured inline-policy body expected) (list (list (cons :x :r5)) nil (list (make-vm-ret :reg :r1)) nil)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-inline-eligible-p-cases cheap-consts-eligible-over-threshold"
  (destructuring-bind (captured inline-policy body expected) (list nil nil (append (loop repeat 17 collect (make-vm-const :dst :r2 :value 0))
                   (list (make-vm-ret :reg :r1))) t)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-inline-eligible-p-cases arith-instrs-over-budget-ineligible"
  (destructuring-bind (captured inline-policy body expected) (list nil nil (append (loop repeat 16 collect (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
                    (list (make-vm-ret :reg :r2))) nil)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-inline-eligible-p-cases forced-inline-bypasses-cost-threshold"
  (destructuring-bind (captured inline-policy body expected) (list nil :inline (append (loop repeat 16 collect (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
                   (list (make-vm-ret :reg :r2))) t)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-inline-eligible-p-cases forced-inline-still-rejects-captures"
  (destructuring-bind (captured inline-policy body expected) (list (list (cons :x :r5)) :inline (list (make-vm-ret :reg :r1)) nil)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-inline-eligible-p-cases notinline-blocks-cheap-function"
  (destructuring-bind (captured inline-policy body expected) (list nil :notinline (list (make-vm-add :dst :r2 :lhs :r1 :rhs :r1) (make-vm-ret :reg :r2)) nil)
    (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured captured
                                 :optional-params nil :rest-param nil :key-params nil
                                 :inline-policy inline-policy))
         (def (list :closure ci :params '(:r1) :body body)))
    (expect (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))) :to-equal expected))))

(it-sequential "opt-adaptive-inline-threshold-cases"
  (let* ((cheap-ci (make-vm-closure :dst :r0 :label "cheap"
                                    :params '(:r1) :captured nil
                                    :optional-params nil :rest-param nil :key-params nil))
         (cheap-body (append (loop repeat 20 collect (make-vm-const :dst :r2 :value 0))
                             (list (make-vm-ret :reg :r1))))
         (cheap-def (list :closure cheap-ci :params '(:r1) :body cheap-body))
         (call-ci (make-vm-closure :dst :r0 :label "callish"
                                   :params '(:r1) :captured nil
                                   :optional-params nil :rest-param nil :key-params nil))
         (call-body (list (make-vm-call :dst :r2 :func :r3 :args '(:r1))
                          (make-vm-ret :reg :r2)))
         (call-def (list :closure call-ci :params '(:r1) :body call-body)))
    (expect (> (cl-cc/optimize::opt-adaptive-inline-threshold cheap-def) 15) :to-be-truthy)
    (expect (< (cl-cc/optimize::opt-adaptive-inline-threshold call-def) 15) :to-be-truthy)))

(it-sequential "opt-adaptive-inline-threshold-respects-pgo-scale"
  (let* ((ci (make-vm-closure :dst :r0 :label "cheap"
                              :params '(:r1) :captured nil
                              :optional-params nil :rest-param nil :key-params nil))
         (body (append (loop repeat 8 collect (make-vm-const :dst :r2 :value 0))
                       (list (make-vm-ret :reg :r1))))
         (def (list :closure ci :params '(:r1) :body body)))
    (let ((base (cl-cc/optimize::opt-adaptive-inline-threshold def)))
      (let ((cl-cc/optimize::*opt-inline-threshold-scale* 2))
        (expect (>= (cl-cc/optimize::opt-adaptive-inline-threshold def) base) :to-be-truthy)))))

(it-sequential "opt-adaptive-inline-threshold-ml-bonus-is-applied"
  (let* ((ci (make-vm-closure :dst :r0 :label "cheap"
                              :params '(:r1) :captured nil
                              :optional-params nil :rest-param nil :key-params nil))
         (body (append (loop repeat 6 collect (make-vm-const :dst :r2 :value 0))
                       (list (make-vm-ret :reg :r1))))
         (def (list :closure ci :params '(:r1) :body body)))
    (let ((cl-cc/optimize::*opt-enable-ml-inline-score* nil))
      (let ((without-ml (cl-cc/optimize::opt-adaptive-inline-threshold def)))
        (let ((cl-cc/optimize::*opt-enable-ml-inline-score* t)
              (cl-cc/optimize::*opt-inline-ml-model-version* "mlgo-v2"))
          (expect (>= (cl-cc/optimize::opt-adaptive-inline-threshold def)
                           without-ml) :to-be-truthy))))))

(it-sequential "opt-inline-inst-cost-respects-learned-target"
  (let ((inst (make-vm-add :dst :r0 :lhs :r1 :rhs :r2)))
    (let ((cl-cc/optimize::*opt-learned-cost-target* :x86-64))
      (let ((x86-cost (cl-cc/optimize::opt-inline-inst-cost inst)))
        (let ((cl-cc/optimize::*opt-learned-cost-target* :aarch64))
          (let ((a64-cost (cl-cc/optimize::opt-inline-inst-cost inst)))
            (expect (/= x86-cost a64-cost) :to-be-truthy)))))))

(it-sequential "opt-pass-inline-iterative-uses-adaptive-threshold"
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                :params '(:r1) :captured nil
                                :optional-params nil :rest-param nil :key-params nil))
         (fref (make-vm-func-ref :dst :r3 :label "f"))
         (body (append (loop repeat 18 collect (make-vm-const :dst :r2 :value 0))
                       (list (make-vm-ret :reg :r1))))
         (call (make-vm-call :dst :r4 :func :r3 :args '(:r5)))
         (ret  (make-vm-ret :reg :r4))
         (out  (cl-cc/optimize::opt-pass-inline-iterative
                (append (list ci) body (list fref call ret)))))
    (expect out :to-be-truthy)))

(it-sequential "opt-adaptive-max-iterations-cases small"
  (destructuring-bind (n-insts pred threshold) (list 10 #'< 20)
    (let ((iters (cl-cc/optimize::opt-adaptive-max-iterations
                (loop repeat n-insts collect (make-vm-const :dst :r0 :value 0)))))
    (expect (funcall pred iters threshold) :to-be-truthy))))

(it-sequential "opt-adaptive-max-iterations-cases large"
  (destructuring-bind (n-insts pred threshold) (list 900 #'> 20)
    (let ((iters (cl-cc/optimize::opt-adaptive-max-iterations
                (loop repeat n-insts collect (make-vm-const :dst :r0 :value 0)))))
    (expect (funcall pred iters threshold) :to-be-truthy))))

(it-sequential "optimize-instructions-accepts-adaptive-max-iterations"
  (let ((out (cl-cc/optimize:optimize-instructions
              (list (make-vm-const :dst :r0 :value 1)
                    (make-vm-ret :reg :r0))
              :max-iterations :adaptive
              :pass-pipeline '(:fold :dce))))
    (expect out :to-be-truthy)))

;;; ─── opt-falsep ──────────────────────────────────────────────────────────

(it-sequential "opt-falsep nil"
  (destructuring-bind (value expected) (list nil t)
    (expect (cl-cc/optimize:opt-falsep value) :to-equal expected)))

(it-sequential "opt-falsep zero"
  (destructuring-bind (value expected) (list 0 t)
    (expect (cl-cc/optimize:opt-falsep value) :to-equal expected)))

(it-sequential "opt-falsep t"
  (destructuring-bind (value expected) (list t nil)
    (expect (cl-cc/optimize:opt-falsep value) :to-equal expected)))

(it-sequential "opt-falsep positive"
  (destructuring-bind (value expected) (list 1 nil)
    (expect (cl-cc/optimize:opt-falsep value) :to-equal expected)))

;;; ─── opt-register-keyword-p ──────────────────────────────────────────────

(it-sequential "opt-register-keyword-p r0"
  (destructuring-bind (value expected) (list :r0 t)
    (expect (cl-cc/optimize::opt-register-keyword-p value) :to-equal expected)))

(it-sequential "opt-register-keyword-p r15"
  (destructuring-bind (value expected) (list :r15 t)
    (expect (cl-cc/optimize::opt-register-keyword-p value) :to-equal expected)))

(it-sequential "opt-register-keyword-p plain-symbol"
  (destructuring-bind (value expected) (list 'r0 nil)
    (expect (cl-cc/optimize::opt-register-keyword-p value) :to-equal expected)))

(it-sequential "opt-register-keyword-p plain-keyword"
  (destructuring-bind (value expected) (list :foo nil)
    (expect (cl-cc/optimize::opt-register-keyword-p value) :to-equal expected)))

;;; ─── opt-binary-lhs-rhs-p / opt-unary-src-p ─────────────────────────────

(it-sequential "opt-instruction-shape-predicates binary-add"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2) t)
    (expect (not (null (funcall pred-fn inst))) :to-equal expected)))

(it-sequential "opt-instruction-shape-predicates binary-lt"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-lt     :dst :r0 :lhs :r1 :rhs :r2) t)
    (expect (not (null (funcall pred-fn inst))) :to-equal expected)))

(it-sequential "opt-instruction-shape-predicates binary-neg"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-neg    :dst :r0 :src :r1) nil)
    (expect (not (null (funcall pred-fn inst))) :to-equal expected)))

(it-sequential "opt-instruction-shape-predicates unary-neg"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-unary-src-p (make-vm-neg    :dst :r0 :src :r1) t)
    (expect (not (null (funcall pred-fn inst))) :to-equal expected)))

(it-sequential "opt-instruction-shape-predicates unary-null-p"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-unary-src-p (make-vm-null-p :dst :r0 :src :r1) t)
    (expect (not (null (funcall pred-fn inst))) :to-equal expected)))

(it-sequential "opt-instruction-shape-predicates unary-add"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-unary-src-p (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2) nil)
    (expect (not (null (funcall pred-fn inst))) :to-equal expected)))

;;; ─── opt-foldable-unary-arith-p / opt-foldable-type-pred-p ──────────────

(it-sequential "opt-foldable-predicates arith-neg"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-foldable-unary-arith-p (make-vm-neg    :dst :r0 :src :r1) t)
    (expect (funcall pred-fn inst) :to-equal expected)))

(it-sequential "opt-foldable-predicates arith-null-p"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-foldable-unary-arith-p (make-vm-null-p :dst :r0 :src :r1) nil)
    (expect (funcall pred-fn inst) :to-equal expected)))

(it-sequential "opt-foldable-predicates pred-null-p"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-foldable-type-pred-p (make-vm-null-p :dst :r0 :src :r1) t)
    (expect (funcall pred-fn inst) :to-equal expected)))

(it-sequential "opt-foldable-predicates pred-neg"
  (destructuring-bind (pred-fn inst expected) (list #'cl-cc/optimize::opt-foldable-type-pred-p (make-vm-neg    :dst :r0 :src :r1) nil)
    (expect (funcall pred-fn inst) :to-equal expected)))
