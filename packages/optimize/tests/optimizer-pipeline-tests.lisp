;;;; tests/unit/optimize/optimizer-pipeline-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — pipeline mechanism
;;;;
;;;; Covers: opt-parse-pass-pipeline-string, opt-converged-p,
;;;;   opt-adaptive-max-iterations, opt-verify-instructions,
;;;;   opt-resolve-pass-pipeline, *opt-pass-registry* data, prolog-rewrite-stage.

(in-package :cl-cc/test)

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

(it-sequential "parse-pass-pipeline-string-cases single-pass"
  (destructuring-bind (input expected-len first-kw second-kw third-kw) (list "sccp" 1 :SCCP nil nil)
    (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string input)))
    (if (zerop expected-len)
        (expect result :to-be-null)
        (progn
          (expect (= expected-len (length result)) :to-be-truthy)
          (when first-kw  (expect (first  result) :to-be first-kw))
          (when second-kw (expect (second result) :to-be second-kw))
          (when third-kw  (expect (third  result) :to-be third-kw)))))))

(it-sequential "parse-pass-pipeline-string-cases multi-pass-comma-separated"
  (destructuring-bind (input expected-len first-kw second-kw third-kw) (list "sccp,cse,dce" 3 :SCCP :CSE :DCE)
    (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string input)))
    (if (zerop expected-len)
        (expect result :to-be-null)
        (progn
          (expect (= expected-len (length result)) :to-be-truthy)
          (when first-kw  (expect (first  result) :to-be first-kw))
          (when second-kw (expect (second result) :to-be second-kw))
          (when third-kw  (expect (third  result) :to-be third-kw)))))))

(it-sequential "parse-pass-pipeline-string-cases trims-whitespace"
  (destructuring-bind (input expected-len first-kw second-kw third-kw) (list " sccp , cse " 2 :SCCP :CSE nil)
    (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string input)))
    (if (zerop expected-len)
        (expect result :to-be-null)
        (progn
          (expect (= expected-len (length result)) :to-be-truthy)
          (when first-kw  (expect (first  result) :to-be first-kw))
          (when second-kw (expect (second result) :to-be second-kw))
          (when third-kw  (expect (third  result) :to-be third-kw)))))))

(it-sequential "parse-pass-pipeline-string-cases empty-returns-nil"
  (destructuring-bind (input expected-len first-kw second-kw third-kw) (list "" 0 nil nil nil)
    (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string input)))
    (if (zerop expected-len)
        (expect result :to-be-null)
        (progn
          (expect (= expected-len (length result)) :to-be-truthy)
          (when first-kw  (expect (first  result) :to-be first-kw))
          (when second-kw (expect (second result) :to-be second-kw))
          (when third-kw  (expect (third  result) :to-be third-kw)))))))

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

(it-sequential "opt-converged-p-cases both-nil"
  (destructuring-bind (scenario expected-result) (list :nil-case t)
    (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2))
         (a    (make-vm-const :dst :r0 :value 1))
         (b    (make-vm-const :dst :r0 :value 1)))
    (let ((result (ecase scenario
                    (:nil-case   (cl-cc/optimize::opt-converged-p nil nil))
                    (:same-case  (cl-cc/optimize::opt-converged-p prog prog))
                    (:equal-case (cl-cc/optimize::opt-converged-p (list a) (list b)))
                    (:diff-case  (cl-cc/optimize::opt-converged-p (list i1) (list i1 i2))))))
      (if expected-result
          (expect result :to-be-truthy)
          (expect result :to-be-falsy))))))

(it-sequential "opt-converged-p-cases same-object"
  (destructuring-bind (scenario expected-result) (list :same-case t)
    (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2))
         (a    (make-vm-const :dst :r0 :value 1))
         (b    (make-vm-const :dst :r0 :value 1)))
    (let ((result (ecase scenario
                    (:nil-case   (cl-cc/optimize::opt-converged-p nil nil))
                    (:same-case  (cl-cc/optimize::opt-converged-p prog prog))
                    (:equal-case (cl-cc/optimize::opt-converged-p (list a) (list b)))
                    (:diff-case  (cl-cc/optimize::opt-converged-p (list i1) (list i1 i2))))))
      (if expected-result
          (expect result :to-be-truthy)
          (expect result :to-be-falsy))))))

(it-sequential "opt-converged-p-cases structurally-equal"
  (destructuring-bind (scenario expected-result) (list :equal-case t)
    (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2))
         (a    (make-vm-const :dst :r0 :value 1))
         (b    (make-vm-const :dst :r0 :value 1)))
    (let ((result (ecase scenario
                    (:nil-case   (cl-cc/optimize::opt-converged-p nil nil))
                    (:same-case  (cl-cc/optimize::opt-converged-p prog prog))
                    (:equal-case (cl-cc/optimize::opt-converged-p (list a) (list b)))
                    (:diff-case  (cl-cc/optimize::opt-converged-p (list i1) (list i1 i2))))))
      (if expected-result
          (expect result :to-be-truthy)
          (expect result :to-be-falsy))))))

(it-sequential "opt-converged-p-cases different-length"
  (destructuring-bind (scenario expected-result) (list :diff-case nil)
    (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2))
         (a    (make-vm-const :dst :r0 :value 1))
         (b    (make-vm-const :dst :r0 :value 1)))
    (let ((result (ecase scenario
                    (:nil-case   (cl-cc/optimize::opt-converged-p nil nil))
                    (:same-case  (cl-cc/optimize::opt-converged-p prog prog))
                    (:equal-case (cl-cc/optimize::opt-converged-p (list a) (list b)))
                    (:diff-case  (cl-cc/optimize::opt-converged-p (list i1) (list i1 i2))))))
      (if expected-result
          (expect result :to-be-truthy)
          (expect result :to-be-falsy))))))

;;; ─── opt-adaptive-max-iterations ─────────────────────────────────────────────

(it-sequential "adaptive-max-iterations-regions tiny"
  (destructuring-bind (n-insts expected) (list 20 8)
    (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (expect (= expected (cl-cc/optimize::opt-adaptive-max-iterations insts)) :to-be-truthy))))

(it-sequential "adaptive-max-iterations-regions small"
  (destructuring-bind (n-insts expected) (list 100 14)
    (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (expect (= expected (cl-cc/optimize::opt-adaptive-max-iterations insts)) :to-be-truthy))))

(it-sequential "adaptive-max-iterations-regions medium"
  (destructuring-bind (n-insts expected) (list 200 20)
    (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (expect (= expected (cl-cc/optimize::opt-adaptive-max-iterations insts)) :to-be-truthy))))

(it-sequential "adaptive-max-iterations-regions large"
  (destructuring-bind (n-insts expected) (list 500 28)
    (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (expect (= expected (cl-cc/optimize::opt-adaptive-max-iterations insts)) :to-be-truthy))))

(it-sequential "adaptive-max-iterations-regions huge"
  (destructuring-bind (n-insts expected) (list 1000 35)
    (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (expect (= expected (cl-cc/optimize::opt-adaptive-max-iterations insts)) :to-be-truthy))))

(it-sequential "adaptive-max-iterations-respects-max-cap"
  (let ((insts (make-list 2000 :initial-element (make-vm-const :dst :r0 :value 1))))
    (expect (= 30 (cl-cc/optimize::opt-adaptive-max-iterations insts :max-iterations 30)) :to-be-truthy)))

(it-sequential "adaptive-max-iterations-respects-min-floor"
  (let ((insts nil))  ; empty → smallest budget
    (expect (>= (cl-cc/optimize::opt-adaptive-max-iterations insts :min-iterations 6) 6) :to-be-truthy)))

(it-sequential "adaptive-loop-unroll-factor-reacts-to-hotness"
  (multiple-value-bind (cold-factor cold-trip)
      (cl-cc/optimize::opt-adaptive-loop-unroll-factor nil :call-count 0)
    (multiple-value-bind (hot-factor hot-trip)
        (cl-cc/optimize::opt-adaptive-loop-unroll-factor nil :call-count 100)
      (expect (> hot-factor cold-factor) :to-be-truthy)
      (expect (> hot-trip cold-trip) :to-be-truthy))))

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

(it-sequential "verify-instructions-simple-sequence-passes"
  (expect (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))) :to-be-truthy))

(it-sequential "verify-instructions-jump-with-known-label-passes"
  (expect (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump  :label "target")
                      (make-vm-label :name "target")
                      (make-vm-ret   :reg :r0))) :to-be-truthy))

(it-sequential "verify-instructions-invalid-cases duplicate-label"
  (destructuring-bind (insts) (list (list (make-vm-const :dst :r0 :value 1)
                                     (make-vm-label :name "dup")
                                     (make-vm-label :name "dup")
                                     (make-vm-ret   :reg :r0)))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/optimize:opt-verify-instructions insts)) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))))

(it-sequential "verify-instructions-invalid-cases unknown-target"
  (destructuring-bind (insts) (list (list (make-vm-jump :label "ghost")
                                     (make-vm-ret  :reg :r0)))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/optimize:opt-verify-instructions insts)) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))))

;;; ─── opt-delimited-continuations-form ───────────────────────────────────────

(it-sequential "delimited-continuations-reset-unwrapped"
  (let* ((input '(cl-cc/optimize::reset (+ 1 2)))
         (once  (cl-cc/optimize::opt-delimited-continuations-form input))
         (twice (cl-cc/optimize::opt-delimited-continuations-form once)))
    (expect once :to-equal '(+ 1 2))
    (expect twice :to-equal once)))

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

(it-sequential "resolve-pass-pipeline-nil-returns-convergence-passes"
  (expect (cl-cc/optimize::opt-resolve-pass-pipeline nil) :to-be cl-cc/optimize::*opt-convergence-passes*))

(it-sequential "resolve-pass-pipeline-functions-pass-through-unchanged"
  (let* ((fn (lambda (x) x))
         (pipeline (list fn)))
    (expect (first (cl-cc/optimize::opt-resolve-pass-pipeline pipeline)) :to-be fn)))

(it-sequential "resolve-pass-pipeline-keywords-resolve-to-functions"
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline (list :fold :dce))))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (every #'functionp result) :to-be-truthy)))

(it-sequential "resolve-pass-pipeline-string-parses-and-resolves"
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline "fold,dce")))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (every #'functionp result) :to-be-truthy)))

(it-sequential "resolve-pass-pipeline-unknown-pass-signals-error"
  (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy)))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

(defparameter *opt-pass-registry-presence-keys*
  '(:prolog-rewrite
    :egraph
    :fold
    :cons-slot-forward
    :pure-call-optimization
    :dce
    :cse))

(defparameter *opt-pass-registry-function-bindings*
  `((:if-conversion . ,#'cl-cc/optimize::opt-pass-if-conversion)
    (:fma-recognition . ,#'cl-cc/optimize::opt-pass-fma-recognition)))

(defparameter *opt-default-convergence-prefix*
  '(:prolog-rewrite
    :call-site-splitting
    :devirtualize
    :if-conversion
    :closure-capture-dedup
    :closure-thunk-sharing))

(defparameter *opt-default-convergence-positional-keys*
  '((7 . :inline)
    (8 . :overflow-check-elim)
    (9 . :sccp)
    (10 . :cons-slot-forward)))

(defparameter *opt-default-convergence-ordering-edges*
  '((:devirtualize :if-conversion)
    (:if-conversion :inline)
    (:fma-recognition :schedule-local)
    (:copy-prop :pure-call-optimization)
    (:pure-call-optimization :gvn)
    (:pure-call-optimization :cse)
    (:pure-call-optimization :dce)))

(defun %opt-default-convergence-key-position (key)
  (or (position key cl-cc/optimize::*opt-default-convergence-pass-keys*)
      (error "Missing optimizer convergence key: ~S" key)))

(defun %assert-opt-default-convergence-edge (before after)
  (assert-true (< (%opt-default-convergence-key-position before)
                  (%opt-default-convergence-key-position after))))

(it-sequential "opt-pass-registry-key-presence"
  (dolist (key *opt-pass-registry-presence-keys*)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy))
  (dolist (binding *opt-pass-registry-function-bindings*)
    (expect (gethash (car binding) cl-cc/optimize::*opt-pass-registry*) :to-be (cdr binding))))

(it-sequential "opt-default-convergence-pass-keys-ordering"
  (expect (subseq cl-cc/optimize::*opt-default-convergence-pass-keys* 0 6) :to-equal *opt-default-convergence-prefix*)
  (dolist (case *opt-default-convergence-positional-keys*)
    (destructuring-bind (position . expected-key) case
      (expect (nth (1- position) cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be expected-key)))
  (expect (member :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be-truthy)
  (expect (member :fma-recognition cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be-truthy)
  (dolist (edge *opt-default-convergence-ordering-edges*)
    (destructuring-bind (before after) edge
      (%assert-opt-default-convergence-edge before after))))

(it-sequential "opt-convergence-passes-type-invariants"
  (expect (listp cl-cc/optimize::*opt-convergence-passes*) :to-be-truthy)
  (expect (> (length cl-cc/optimize::*opt-convergence-passes*) 10) :to-be-truthy)
  (expect (every #'functionp cl-cc/optimize::*opt-convergence-passes*) :to-be-truthy)
  (expect (first cl-cc/optimize::*opt-convergence-passes*) :to-be #'cl-cc/optimize::%maybe-apply-prolog-rewrite)
  (expect (member #'cl-cc/optimize:optimize-with-egraph cl-cc/optimize::*opt-convergence-passes*) :to-be-falsy)
  (expect (member #'cl-cc/optimize::opt-pass-fold cl-cc/optimize::*opt-convergence-passes*) :to-be-falsy)
  (expect (member #'cl-cc/optimize::opt-pass-strength-reduce cl-cc/optimize::*opt-convergence-passes*) :to-be-falsy))

;;; ─── FR-099 FMA recognition ───────────────────────────────────────────────

(defun %test-count-type (type insts)
  (count-if (lambda (inst) (typep inst type)) insts))

(it-sequential "fr-099-fma-recognition-cases mul-plus-accumulator"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)) 1 0 0)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    ;; vm-float-mul inherits vm-mul (and vm-float-add inherits vm-add), so a
    ;; single typep against the parent already counts both float and integer
    ;; multiplies/adds. Counting the parent type is the correct total — summing
    ;; parent + child would double-count every float instruction.
    (when expected-float-mul-or-mul
      (expect (= expected-float-mul-or-mul (%test-count-type 'cl-cc/vm::vm-mul out)) :to-be-truthy))
    (when expected-float-add-or-add
      (expect (= expected-float-add-or-add (%test-count-type 'cl-cc/vm::vm-add out)) :to-be-truthy)))))

(it-sequential "fr-099-fma-recognition-cases commuted-add"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r2 :rhs :r3)) 1 0 0)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    ;; vm-float-mul inherits vm-mul (and vm-float-add inherits vm-add), so a
    ;; single typep against the parent already counts both float and integer
    ;; multiplies/adds. Counting the parent type is the correct total — summing
    ;; parent + child would double-count every float instruction.
    (when expected-float-mul-or-mul
      (expect (= expected-float-mul-or-mul (%test-count-type 'cl-cc/vm::vm-mul out)) :to-be-truthy))
    (when expected-float-add-or-add
      (expect (= expected-float-add-or-add (%test-count-type 'cl-cc/vm::vm-add out)) :to-be-truthy)))))

(it-sequential "fr-099-fma-recognition-cases multiple-consumers-no-fuse"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)
                 (cl-cc/vm::make-vm-float-add :dst :r5 :lhs :r3 :rhs :r6)) 0 1 2)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    ;; vm-float-mul inherits vm-mul (and vm-float-add inherits vm-add), so a
    ;; single typep against the parent already counts both float and integer
    ;; multiplies/adds. Counting the parent type is the correct total — summing
    ;; parent + child would double-count every float instruction.
    (when expected-float-mul-or-mul
      (expect (= expected-float-mul-or-mul (%test-count-type 'cl-cc/vm::vm-mul out)) :to-be-truthy))
    (when expected-float-add-or-add
      (expect (= expected-float-add-or-add (%test-count-type 'cl-cc/vm::vm-add out)) :to-be-truthy)))))

(it-sequential "fr-099-fma-recognition-cases integer-arithmetic-no-fuse"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (make-vm-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (make-vm-add :dst :r4 :lhs :r3 :rhs :r2)) 0 nil nil)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    ;; vm-float-mul inherits vm-mul (and vm-float-add inherits vm-add), so a
    ;; single typep against the parent already counts both float and integer
    ;; multiplies/adds. Counting the parent type is the correct total — summing
    ;; parent + child would double-count every float instruction.
    (when expected-float-mul-or-mul
      (expect (= expected-float-mul-or-mul (%test-count-type 'cl-cc/vm::vm-mul out)) :to-be-truthy))
    (when expected-float-add-or-add
      (expect (= expected-float-add-or-add (%test-count-type 'cl-cc/vm::vm-add out)) :to-be-truthy)))))

(it-sequential "fr-099-fma-recognition-cases cross-block-boundary-no-fuse"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (make-vm-label :name "next")
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)) 0 1 1)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    ;; vm-float-mul inherits vm-mul (and vm-float-add inherits vm-add), so a
    ;; single typep against the parent already counts both float and integer
    ;; multiplies/adds. Counting the parent type is the correct total — summing
    ;; parent + child would double-count every float instruction.
    (when expected-float-mul-or-mul
      (expect (= expected-float-mul-or-mul (%test-count-type 'cl-cc/vm::vm-mul out)) :to-be-truthy))
    (when expected-float-add-or-add
      (expect (= expected-float-add-or-add (%test-count-type 'cl-cc/vm::vm-add out)) :to-be-truthy)))))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

(it-sequential "verify-optimizer-flag-runs-verifier-on-valid-input"
  (let ((cl-cc/optimize:*verify-optimizer-instructions* t)
        (insts (list (make-vm-const :dst :r0 :value 42) (make-vm-ret :reg :r0))))
    (expect (listp (cl-cc/optimize:optimize-instructions insts)) :to-be-truthy)))

(it-sequential "verify-optimizer-flag-nil-skips-verifier"
  (let ((cl-cc/optimize:*verify-optimizer-instructions* nil)
        (insts (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))))
    (expect (listp (cl-cc/optimize:optimize-instructions insts)) :to-be-truthy)))

(it-sequential "pure-call-policy-gate-disables-pass"
  (let* ((callee-label "pure-square")
         (insts (list (make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-label   :name callee-label)
                      (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
                      (make-vm-ret     :reg :r1)
                      (make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-const   :dst :r0 :value 7)
                      (make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                      (make-vm-ret     :reg :r4)))
         (cl-cc/optimize::*opt-enable-pure-call-optimization* nil)
         (optimized (cl-cc/optimize:optimize-instructions
                     insts
                     :max-iterations 1
                     :pass-pipeline '(:pure-call-optimization))))
    (expect (= 2 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) optimized)) :to-be-truthy)
    (expect (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-move)
                  (eq (cl-cc/vm::vm-dst inst) :r4)
                  (eq (cl-cc/vm::vm-src inst) :r3)))
           optimized) :to-be-falsy)))

(it-sequential "optimize-policy-config-speed-threshold"
  (let ((cl-cc/optimize::*opt-enable-pure-call-optimization* t))
    (expect (cl-cc/optimize:opt-configure-optimization-policy :speed 2) :to-be-falsy)
    (expect (cl-cc/optimize:opt-configure-optimization-policy :speed 3) :to-be-truthy)))

(it-sequential "optimize-instructions-speed-keyword-controls-pure-call-pass"
  (let* ((callee-label "pure-square")
         (insts (list (make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-label   :name callee-label)
                      (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
                      (make-vm-ret     :reg :r1)
                      (make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-const   :dst :r0 :value 7)
                      (make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                      (make-vm-ret     :reg :r4)))
         (slow (cl-cc/optimize:optimize-instructions
                insts :max-iterations 1 :speed 2 :pass-pipeline '(:pure-call-optimization)))
         (fast (cl-cc/optimize:optimize-instructions
                insts :max-iterations 1 :speed 3 :pass-pipeline '(:pure-call-optimization))))
    (expect (= 2 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) slow)) :to-be-truthy)
    (expect (= 1 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) fast)) :to-be-truthy)))

;;; ─── Prolog rewrite stage ──────────────────────────────────────────────────

(it-sequential "prolog-rewrite-stage-disabled-is-identity"
  (let ((cl-cc/optimize::*enable-prolog-peephole* nil)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (expect (cl-cc/optimize::%maybe-apply-prolog-rewrite insts) :to-be insts)))

(it-sequential "prolog-rewrite-stage-invokes-prolog-backends"
  (let ((cl-cc/optimize::*enable-prolog-peephole* t)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (let ((result (cl-cc/optimize::%maybe-apply-prolog-rewrite insts)))
      (expect (listp result) :to-be-truthy)
      (expect (= 2 (length result)) :to-be-truthy)
      (expect (mapcar #'cl-cc/optimize::instruction->sexp result) :to-equal (mapcar #'cl-cc/optimize::instruction->sexp insts)))))

;;; ─── Copy-on-Write helper layer (FR-253 partial) ───────────────────────────

(it-sequential "optimize-cow-copy-is-constant-time-share"
  (let* ((cow (cl-cc/optimize:make-opt-cow-object :payload '((a . 1)) :refcount 1))
         (shared (cl-cc/optimize:opt-cow-copy cow)))
    (expect shared :to-be cow)
    (expect (= 2 (cl-cc/optimize:opt-cow-object-refcount cow)) :to-be-truthy)))

(it-sequential "optimize-cow-write-detaches-when-shared"
  (let* ((original (cl-cc/optimize:make-opt-cow-object :payload '((a . 1)) :refcount 1))
         (shared (cl-cc/optimize:opt-cow-copy original))
         (written (cl-cc/optimize:opt-cow-write
                   shared
                   (lambda (payload)
                     (setf (cdar payload) 99)))))
    (expect (eq written original) :to-be-falsy)
    (expect (= 1 (cl-cc/optimize:opt-cow-object-refcount original)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize:opt-cow-object-refcount written)) :to-be-truthy)
    (expect (cl-cc/optimize:opt-cow-object-payload original) :to-equal '((a . 1)))
    (expect (cl-cc/optimize:opt-cow-object-payload written) :to-equal '((a . 99)))))

;;; ─── Region/bump allocation helpers (FR-254 partial) ───────────────────────

(it-sequential "optimize-bump-region-mark-reset-restores-cursor"
  (let ((region (cl-cc/optimize:make-opt-bump-region :cursor 0 :limit 64 :marks nil)))
    (expect (= 0 (cl-cc/optimize:opt-bump-allocate region 8)) :to-be-truthy)
    (expect (= 8 (cl-cc/optimize:opt-bump-mark region)) :to-be-truthy)
    (expect (= 8 (cl-cc/optimize:opt-bump-allocate region 4)) :to-be-truthy)
    (expect (= 12 (cl-cc/optimize:opt-bump-region-cursor region)) :to-be-truthy)
    (cl-cc/optimize:opt-bump-reset region)
    (expect (= 8 (cl-cc/optimize:opt-bump-region-cursor region)) :to-be-truthy)))

(it-sequential "optimize-slab-pool-reuses-freed-object"
  (let* ((pool (cl-cc/optimize:make-opt-slab-pool :object-size 2 :free-list nil :next-id 0 :allocated-count 0))
         (obj1 (cl-cc/optimize:opt-slab-allocate pool)))
    (cl-cc/optimize:opt-slab-free pool obj1)
    (let ((obj2 (cl-cc/optimize:opt-slab-allocate pool)))
      (expect obj2 :to-equal obj1))))

;;; ─── Inline cache helper layer (FR-009 / FR-019 partial) ──────────────────

(it-sequential "optimize-ic-resolve-target-prefers-site-local-entry"
  (let ((site (cl-cc/optimize::make-opt-ic-site :state :monomorphic :entries nil)))
    (cl-cc/optimize::opt-ic-transition site :shape-a :target-a)
    (multiple-value-bind (target source)
        (cl-cc/optimize::opt-ic-resolve-target site :shape-a)
      (expect target :to-be :target-a)
      (expect source :to-be :site-local))))

(it-sequential "optimize-ic-resolve-target-uses-shared-megamorphic-cache"
  (let* ((site (cl-cc/optimize::make-opt-ic-site
                :state :megamorphic :entries nil :max-polymorphic-entries 2))
         (cache (cl-cc/optimize::make-opt-megamorphic-cache :max-size 2)))
    (cl-cc/optimize::opt-mega-cache-put cache :shape-z :target-z)
    (multiple-value-bind (target source)
        (cl-cc/optimize::opt-ic-resolve-target site :shape-z cache)
      (expect target :to-be :target-z)
      (expect source :to-be :megamorphic-shared))))

;;; ─── ThinLTO / Tiered JIT / Deopt helpers (FR-301/310/312 partial) ───────

(it-sequential "optimize-pgo-build-hot-chain-prefers-hottest-successors"
  (let ((edges (make-hash-table :test #'equal)))
    (setf (gethash (cons :entry :cold) edges) 1
          (gethash (cons :entry :hot) edges) 10
          (gethash (cons :hot :exit) edges) 5
          (gethash (cons :cold :exit) edges) 1)
    (expect (cl-cc/optimize:opt-pgo-build-hot-chain
                   :entry
                   '((:entry . (:cold :hot))
                     (:hot . (:exit))
                     (:cold . (:exit)))
                   edges) :to-equal '(:entry :hot :exit))))

(it-sequential "optimize-pgo-rotate-loop-places-preferred-exit-at-bottom"
  (expect (cl-cc/optimize:opt-pgo-rotate-loop
                 '(:header :exit :latch)
                 :exit) :to-equal '(:latch :header :exit)))

(it-sequential "optimize-merge-module-summaries-aggregates-exports-and-counts"
  (let* ((a (cl-cc/optimize::make-opt-module-summary
             :module :a :exports '(fa fb) :function-count 2 :type-summaries nil))
         (b (cl-cc/optimize::make-opt-module-summary
             :module :b :exports '(fb fc) :function-count 3 :type-summaries nil))
         (merged (cl-cc/optimize::opt-merge-module-summaries (list a b))))
    (expect (getf merged :modules) :to-equal '(:a :b))
    (expect (member 'fa (getf merged :exports)) :to-be-truthy)
    (expect (member 'fb (getf merged :exports)) :to-be-truthy)
    (expect (member 'fc (getf merged :exports)) :to-be-truthy)
    (expect (= 5 (getf merged :function-count)) :to-be-truthy)))

(it-sequential "optimize-thinlto-import-decision-respects-budget-linkage-and-cycles"
  (let ((candidate (cl-cc/optimize:make-opt-function-summary
                    :name 'callee :inst-count 12 :exported-p nil :importable-p t))
        (exported (cl-cc/optimize:make-opt-function-summary
                   :name 'public :inst-count 12 :exported-p t :importable-p t))
        (too-large (cl-cc/optimize:make-opt-function-summary
                    :name 'big :inst-count 200 :exported-p nil :importable-p t)))
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                  candidate '(other) :budget 20) :to-be-truthy)
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                   exported '(other) :budget 20) :to-be-falsy)
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                   too-large '(other) :budget 20) :to-be-falsy)
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                   candidate '(callee) :budget 20) :to-be-falsy)))

(it-sequential "adaptive-compilation-threshold-cases warmup"
  (destructuring-bind (base warmup-p pressure failures expected) (list 900 t nil nil 300)
    (expect (= expected (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base base
             :warmup-p warmup-p
             :cache-pressure pressure
             :failures failures)) :to-be-truthy)))

(it-sequential "adaptive-compilation-threshold-cases pressure"
  (destructuring-bind (base warmup-p pressure failures expected) (list 900 nil 0.8 nil 1800)
    (expect (= expected (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base base
             :warmup-p warmup-p
             :cache-pressure pressure
             :failures failures)) :to-be-truthy)))

(it-sequential "adaptive-compilation-threshold-cases failures"
  (destructuring-bind (base warmup-p pressure failures expected) (list 900 nil nil 2 2700)
    (expect (= expected (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base base
             :warmup-p warmup-p
             :cache-pressure pressure
             :failures failures)) :to-be-truthy)))

(it-sequential "adaptive-compilation-threshold-cases combined"
  (destructuring-bind (base warmup-p pressure failures expected) (list 900 t 0.8 2 1800)
    (expect (= expected (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base base
             :warmup-p warmup-p
             :cache-pressure pressure
             :failures failures)) :to-be-truthy)))

(it-sequential "tier-transition-cases interp-below"
  (destructuring-bind (tier count bt ot expected) (list :interpreter 99 100 nil :interpreter)
    (expect (cl-cc/optimize:opt-tier-transition
              tier count
              :baseline-threshold bt
              :optimized-threshold ot) :to-be expected)))

(it-sequential "tier-transition-cases interp-at"
  (destructuring-bind (tier count bt ot expected) (list :interpreter 100 100 nil :baseline)
    (expect (cl-cc/optimize:opt-tier-transition
              tier count
              :baseline-threshold bt
              :optimized-threshold ot) :to-be expected)))

(it-sequential "tier-transition-cases baseline-below"
  (destructuring-bind (tier count bt ot expected) (list :baseline 999 nil 1000 :baseline)
    (expect (cl-cc/optimize:opt-tier-transition
              tier count
              :baseline-threshold bt
              :optimized-threshold ot) :to-be expected)))

(it-sequential "tier-transition-cases baseline-at"
  (destructuring-bind (tier count bt ot expected) (list :baseline 1000 nil 1000 :optimized)
    (expect (cl-cc/optimize:opt-tier-transition
              tier count
              :baseline-threshold bt
              :optimized-threshold ot) :to-be expected)))

(it-sequential "optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers"
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 42
                 :register-map '((:rax . :r0) (:rbx . :r1))
                 :inlined-frames nil))
         (machine '((:rax . 11) (:rbx . 22) (:rcx . 33)))
         (state (cl-cc/optimize::opt-materialize-deopt-state frame machine)))
    (expect state :to-equal '((:r0 . 11) (:r1 . 22)))))

(it-sequential "optimize-osr-trigger-p-uses-hotness-threshold"
  (let ((osr (cl-cc/optimize::make-opt-osr-point
              :loop-id :L1
              :vm-pc 77
              :live-registers nil
              :hotness 1200)))
    (expect (cl-cc/optimize::opt-osr-trigger-p osr :threshold 1000) :to-be-truthy)
    (expect (cl-cc/optimize::opt-osr-trigger-p osr :threshold 2000) :to-be-falsy)))

(it-sequential "optimize-osr-materialize-entry-maps-machine-to-vm-registers"
  (let* ((osr (cl-cc/optimize::make-opt-osr-point
               :loop-id :L2
               :vm-pc 88
               :live-registers '((:rax . :r0) (:r10 . :r7))
               :hotness 1500))
         (machine '((:rax . 3) (:r10 . 9) (:rbx . 99)))
         (state (cl-cc/optimize::opt-osr-materialize-entry osr machine)))
    (expect state :to-equal '((:r0 . 3) (:r7 . 9)))))

(it-sequential "optimize-shape-descriptor-slots-map-to-stable-offsets"
  (let* ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots
                 7 '(name age active))))
    (expect (= 0 (cl-cc/optimize::opt-shape-slot-offset shape 'name)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-shape-slot-offset shape 'age)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize::opt-shape-slot-offset shape 'active)) :to-be-truthy)))

(it-sequential "optimize-shape-transition-cache-stores-forward-transitions"
  (let ((cache (cl-cc/optimize::make-opt-shape-transition-cache :max-size 2)))
    (cl-cc/optimize::opt-shape-transition-put cache 1 'slot-a 2)
    (cl-cc/optimize::opt-shape-transition-put cache 2 'slot-b 3)
    (multiple-value-bind (child found)
        (cl-cc/optimize::opt-shape-transition-get cache 1 'slot-a)
      (expect found :to-be-truthy)
      (expect (= 2 child) :to-be-truthy))))

(defparameter *opt-ic-patch-plan-cases*
  '((:site1 :uninitialized :monomorphic :t1 :install-monomorphic)
    (:site2 :monomorphic :polymorphic :t2 :promote-polymorphic)
    (:site3 :polymorphic :megamorphic :t3 :promote-megamorphic)))

(defun %assert-opt-ic-patch-plan-case (site current-state next-state target expected-kind)
  (assert-eq expected-kind
             (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan
               site current-state next-state target))))

(it-sequential "optimize-ic-make-patch-plan-classifies-state-transitions"
  (dolist (case *opt-ic-patch-plan-cases*)
    (apply #'%assert-opt-ic-patch-plan-case case)))

(it-sequential "optimize-build-inline-polymorphic-dispatch-builds-guard-chain"
  (let* ((entries '((:shape-a . :method-a) (:shape-b . :method-b)))
         (chain (cl-cc/optimize::opt-build-inline-polymorphic-dispatch entries :obj)))
    (expect (= 2 (length chain)) :to-be-truthy)
    (expect (getf (first chain) :shape) :to-be :shape-a)
    (expect (getf (first chain) :receiver) :to-be :obj)
    (expect (getf (second chain) :target) :to-be :method-b)))

(it-sequential "wasm-tailcall-opcode-cases direct-enabled"
  (destructuring-bind (tail-p indirect-p enabled-p expected) (list t nil t :return-call)
    (expect (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-p
              :indirect-p indirect-p
              :enabled-p enabled-p) :to-be expected)))

(it-sequential "wasm-tailcall-opcode-cases indirect-enabled"
  (destructuring-bind (tail-p indirect-p enabled-p expected) (list t t t :return-call-indirect)
    (expect (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-p
              :indirect-p indirect-p
              :enabled-p enabled-p) :to-be expected)))

(it-sequential "wasm-tailcall-opcode-cases non-tail-disabled"
  (destructuring-bind (tail-p indirect-p enabled-p expected) (list nil nil nil :call)
    (expect (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-p
              :indirect-p indirect-p
              :enabled-p enabled-p) :to-be expected)))

(it-sequential "wasm-tailcall-opcode-disabled-tail-signals"
  (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/optimize::opt-wasm-select-tailcall-opcode
     :tail-position-p t
     :indirect-p nil
     :enabled-p nil)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy)))

(defun %opt-wasm-gc-struct-layout (&key (fields '((slot-a . i32)))
                                        (nullable-p t))
  (cl-cc/optimize:opt-build-wasm-gc-layout
   :kind :struct
   :fields fields
   :nullable-p nullable-p))

(defun %opt-wasm-gc-array-layout (&key (fields '(eqref))
                                       (nullable-p nil))
  (cl-cc/optimize:opt-build-wasm-gc-layout
   :kind :array
   :fields fields
   :nullable-p nullable-p))

(defun %opt-wasm-gc-bad-array-layout ()
  (%opt-wasm-gc-array-layout :fields '(eqref i32)))

(it-sequential "optimize-build-wasm-gc-layout-preserves-kind-and-fields"
  (let ((layout (%opt-wasm-gc-struct-layout
                 :fields '((slot-a . i32) (slot-b . externref)))))
    (expect (cl-cc/optimize::opt-wasm-gc-kind layout) :to-be :struct)
    (expect (cl-cc/optimize::opt-wasm-gc-fields layout) :to-equal '((slot-a . i32) (slot-b . externref)))
    (expect (cl-cc/optimize::opt-wasm-gc-nullable-p layout) :to-be-truthy)))

(it-sequential "optimize-wasm-gc-layout-validates-struct-and-array-shapes"
  (let ((struct-layout (%opt-wasm-gc-struct-layout
                        :fields '((slot-a . i32) (slot-b . eqref))))
        (array-layout (%opt-wasm-gc-array-layout))
        (bad-array-layout (%opt-wasm-gc-bad-array-layout)))
    (expect (cl-cc/optimize:opt-wasm-gc-layout-valid-p struct-layout) :to-be-truthy)
    (expect (cl-cc/optimize:opt-wasm-gc-layout-valid-p array-layout) :to-be-truthy)
    (expect (cl-cc/optimize:opt-wasm-gc-layout-valid-p bad-array-layout) :to-be-falsy)))

(it-sequential "optimize-wasm-gc-runtime-host-compatibility-requires-feature-and-valid-layout"
  (let ((layout (%opt-wasm-gc-struct-layout))
        (bad-layout (%opt-wasm-gc-bad-array-layout)))
    (expect (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      layout
      :host-supports-wasm-gc-p t) :to-be-truthy)
    (expect (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      layout
      :host-supports-wasm-gc-p nil) :to-be-falsy)
    (expect (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      bad-layout
      :host-supports-wasm-gc-p t) :to-be-falsy)))

(it-sequential "optimize-wasm-gc-optimization-plan-reflects-layout-kind"
  (let* ((struct-layout (%opt-wasm-gc-struct-layout))
         (array-layout (%opt-wasm-gc-array-layout))
         (struct-plan (cl-cc/optimize:opt-build-wasm-gc-optimization-plan struct-layout))
         (array-plan (cl-cc/optimize:opt-build-wasm-gc-optimization-plan array-layout)))
    (expect (getf struct-plan :layout-valid-p) :to-be-truthy)
    (expect (getf struct-plan :inline-field-access-p) :to-be-truthy)
    (expect (getf struct-plan :bounds-check-elision-p) :to-be-falsy)
    (expect (getf array-plan :layout-valid-p) :to-be-truthy)
    (expect (getf array-plan :inline-field-access-p) :to-be-falsy)
    (expect (getf array-plan :bounds-check-elision-p) :to-be-truthy)))

(it-sequential "optimize-build-dwarf-line-row-preserves-location-fields"
  (let* ((loc (cl-cc/optimize::make-opt-debug-loc
               :file "src/foo.lisp" :line 42 :column 7 :symbol 'foo))
         (row (cl-cc/optimize::opt-build-dwarf-line-row #x1000 loc)))
    (expect (getf row :address) :to-be #x1000)
    (expect (getf row :file) :to-equal "src/foo.lisp")
    (expect (= 42 (getf row :line)) :to-be-truthy)
    (expect (= 7 (getf row :column)) :to-be-truthy)))

(it-sequential "optimize-build-wasm-source-map-entry-preserves-offset-and-source"
  (let* ((loc (cl-cc/optimize::make-opt-debug-loc
               :file "src/bar.lisp" :line 10 :column 3 :symbol 'bar))
         (entry (cl-cc/optimize::opt-build-wasm-source-map-entry 128 loc)))
    (expect (= 128 (getf entry :offset)) :to-be-truthy)
    (expect (getf entry :source) :to-equal "src/bar.lisp")
    (expect (= 10 (getf entry :line)) :to-be-truthy)
    (expect (= 3 (getf entry :column)) :to-be-truthy)))

(it-sequential "optimize-format-diagnostic-reason-renders-rpass-like-message"
  (expect (cl-cc/optimize::opt-format-diagnostic-reason
                 "inline" "skipped" "callee too large") :to-equal "inline: skipped (callee too large)"))

(it-sequential "optimize-build-tls-plan-selects-architecture-specific-base-register"
  (let ((x86 (cl-cc/optimize::opt-build-tls-plan :target :x86-64 :hot-access-p t))
        (arm (cl-cc/optimize::opt-build-tls-plan :target :aarch64 :hot-access-p t)))
    (expect (cl-cc/optimize::opt-tls-plan-uses-inline-tls-p x86) :to-be-truthy)
    (expect (cl-cc/optimize::opt-tls-plan-base-register x86) :to-be :fs)
    (expect (cl-cc/optimize::opt-tls-plan-base-register arm) :to-be :tpidr_el0)))

(defparameter *opt-atomic-opcode-cases*
  '((:x86-64 :incf :acq-rel :lock-xadd)
    (:x86-64 :cas :seq-cst :lock-cmpxchg)
    (:aarch64 :incf :acq-rel :ldadd)
    (:aarch64 :cas :seq-cst :ldxr-stxr)))

(defun %assert-opt-atomic-opcode-case (target operation memory-order expected-opcode)
  (assert-eq expected-opcode
             (cl-cc/optimize::opt-select-atomic-opcode
              :target target
              :operation operation
              :memory-order memory-order)))

(it-sequential "optimize-select-atomic-opcode-reflects-target-and-operation"
  (dolist (case *opt-atomic-opcode-cases*)
    (apply #'%assert-opt-atomic-opcode-case case)))

(it-sequential "optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention"
  (let ((enabled (cl-cc/optimize::opt-build-htm-plan
                  :target :x86-64
                  :supports-htm-p t
                  :low-contention-p t))
        (disabled (cl-cc/optimize::opt-build-htm-plan
                   :target :x86-64
                   :supports-htm-p t
                   :low-contention-p nil)))
    (expect (cl-cc/optimize::opt-htm-plan-uses-htm-p enabled) :to-be-truthy)
    (expect (cl-cc/optimize::opt-htm-plan-begin-opcode enabled) :to-be :xbegin)
    (expect (cl-cc/optimize::opt-htm-plan-end-opcode enabled) :to-be :xend)
    (expect (cl-cc/optimize::opt-htm-plan-abort-opcode enabled) :to-be :xabort)
    (expect (cl-cc/optimize::opt-htm-plan-fallback-lock-p enabled) :to-be-truthy)
    (expect (cl-cc/optimize::opt-htm-plan-uses-htm-p disabled) :to-be-falsy)))

(it-sequential "optimize-build-concurrent-gc-plan-selects-satb-and-short-stw-for-latency-sensitive-mode"
  (let ((concurrent (cl-cc/optimize::opt-build-concurrent-gc-plan
                     :latency-sensitive-p t
                     :heap-size (* 128 1024 1024)))
        (stw (cl-cc/optimize::opt-build-concurrent-gc-plan
              :latency-sensitive-p nil
              :heap-size (* 128 1024 1024))))
    (expect (cl-cc/optimize::opt-conc-gc-plan-concurrent-mark-p concurrent) :to-be-truthy)
    (expect (cl-cc/optimize::opt-conc-gc-plan-write-barrier concurrent) :to-be :satb)
    (expect (cl-cc/optimize::opt-conc-gc-plan-mutator-assist-p concurrent) :to-be-truthy)
    (expect (cl-cc/optimize::opt-conc-gc-plan-stw-phases concurrent) :to-equal '(:initial-mark :final-remark))
    (expect (cl-cc/optimize::opt-conc-gc-plan-concurrent-mark-p stw) :to-be-falsy)
    (expect (cl-cc/optimize::opt-conc-gc-plan-write-barrier stw) :to-be :incremental-update)
    (expect (cl-cc/optimize::opt-conc-gc-plan-stw-phases stw) :to-equal '(:full-mark-sweep))))

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(it-sequential "optimize-specialize-constant-args-builds-residual-body"
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((if (= x 0) 0 (* x y)))
           '((x . 0)))))
    (expect (cl-cc/optimize:opt-partial-spec-original-name specialization) :to-equal 'f)
    (expect (cl-cc/optimize:opt-partial-spec-signature specialization) :to-equal '((x . 0)))
    (expect (cl-cc/optimize:opt-partial-spec-static-args specialization) :to-equal '((x . 0)))
    (expect (cl-cc/optimize:opt-partial-spec-dynamic-args specialization) :to-equal '(y))
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal '((if (= 0 0) 0 (* 0 y))))))

(it-sequential "optimize-specialize-constant-args-respects-lexical-binders-and-quoted-data"
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((quote x)
             (let ((x y) (z x)) (+ x z y))
             (let* ((z x) (x y)) (+ x z y))
             (lambda (x) (+ x y))
             (setq x y)
             (x y))
           '((x . 0) (y . 7)))))
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal '((quote x)
                    (let ((x 7) (z 0)) (+ x z 7))
                    (let* ((z 0) (x 7)) (+ x z 7))
                    (lambda (x) (+ x 7))
                    (setq x 7)
                    (x 7)))))

(it-sequential "optimize-specialize-constant-args-kills-signature-after-setq"
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((setq x y)
             (+ x y)
             (setq x (+ x 1))
             (+ x y)
             (setq x y y x)
             (+ x y))
           '((x . 0) (y . 7)))))
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal '((setq x 7)
                    (+ x 7)
                    (setq x (+ x 1))
                    (+ x 7)
                    (setq x 7 y x)
                    (+ x y)))))

(it-sequential "optimize-sccp-analyze-binding-times-classifies-lattice-values"
  (let* ((analysis
           (cl-cc/optimize:opt-sccp-analyze-binding-times
            '(x y z)
            `((x . ,(cl-cc/optimize:opt-lattice-constant 42))
              (y . ,(cl-cc/optimize:opt-lattice-overdefined))))))
    (expect (cl-cc/optimize:opt-binding-time-kind (first analysis)) :to-be :static)
    (expect (cl-cc/optimize:opt-binding-time-value (first analysis)) :to-equal 42)
    (expect (cl-cc/optimize:opt-binding-time-kind (second analysis)) :to-be :dynamic)
    (expect (cl-cc/optimize:opt-binding-time-kind (third analysis)) :to-be :dynamic)
    (expect (cl-cc/optimize:opt-binding-time-lattice (third analysis)) :to-be-null)))

(it-sequential "optimize-build-specialization-plan-reuses-cache-for-constant-signature"
  (let* ((cache (make-hash-table :test #'equal))
         (first-plan
           (cl-cc/optimize:opt-build-specialization-plan
            'f '(x y) '((x . 3)) :cache cache))
         (second-plan
           (cl-cc/optimize:opt-build-specialization-plan
            'f '(x y) '((x . 3)) :cache cache)))
    (expect first-plan :to-be-truthy)
    (expect second-plan :to-be-truthy)
    (expect (cl-cc/optimize:opt-specialization-plan-clone-needed-p first-plan) :to-be-truthy)
    (expect (cl-cc/optimize:opt-specialization-plan-cache-hit-p first-plan) :to-be-falsy)
    (expect (cl-cc/optimize:opt-specialization-plan-clone-needed-p second-plan) :to-be-falsy)
    (expect (cl-cc/optimize:opt-specialization-plan-cache-hit-p second-plan) :to-be-truthy)
    (expect (cl-cc/optimize:opt-specialization-plan-specialized-name second-plan) :to-equal (cl-cc/optimize:opt-specialization-plan-specialized-name first-plan))
    (expect (cl-cc/optimize:opt-specialization-plan-signature first-plan) :to-equal '((x . 3)))
    (expect (cl-cc/optimize:opt-specialization-plan-dynamic-args first-plan) :to-equal '(y))))

(it-sequential "optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids"
  (let* ((plan (cl-cc/optimize:opt-pgo-build-counter-plan
                :entry
                '((:entry :left :right)
                  (:left :exit)
                  (:right :exit)
                  (:exit))))
         (bb (getf plan :bb-counters))
         (edge (getf plan :edge-counters)))
    (expect bb :to-equal '((:entry . 0) (:left . 1) (:exit . 2) (:right . 3)))
    (expect edge :to-equal '(((:entry . :left) . 0)
                    ((:entry . :right) . 1)
                    ((:left . :exit) . 2)
                    ((:right . :exit) . 3)))
    (expect (= 4 (getf plan :total-bb)) :to-be-truthy)
    (expect (= 4 (getf plan :total-edge)) :to-be-truthy)))

(it-sequential "optimize-pgo-make-profile-template-zero-initializes-counts"
  (let* ((plan (cl-cc/optimize:opt-pgo-build-counter-plan
                :entry
                '((:entry :left :right)
                  (:left :exit)
                  (:right :exit)
                  (:exit))))
         (profile (cl-cc/optimize:opt-pgo-make-profile-template plan)))
    (expect (getf profile :magic) :to-be :cl-cc-pgo-v1)
    (expect (getf profile :bb-counts) :to-equal '((:entry . 0) (:left . 0) (:exit . 0) (:right . 0)))
    (expect (getf profile :branch-counts) :to-equal '(((:entry . :left) . 0)
                    ((:entry . :right) . 0)
                    ((:left . :exit) . 0)
                    ((:right . :exit) . 0)))
    (expect (= (getf plan :total-bb) (getf profile :total-bb)) :to-be-truthy)
    (expect (= (getf plan :total-edge) (getf profile :total-edge)) :to-be-truthy)))

(it-sequential "optimize-partial-evaluate-program-propagates-constants-through-call-graph"
  (let* ((defs
           '((callee :params (:x :y)
              :body ((+ x y)))
             (caller :params (:a)
              :body ((callee a 7)))))
         (result (cl-cc/optimize:opt-partial-evaluate-program
                  defs
                  :constant-bindings-by-function '((caller . ((:a . 3))))))
         (reports (cl-cc/optimize::opt-partial-program-function-results result))
         (callee-report (cdr (assoc 'callee reports :test #'equal))))
    (expect callee-report :to-be-truthy)
    (expect (assoc :y (cl-cc/optimize:opt-partial-eval-signature callee-report) :test #'equal) :to-be-truthy)
    (expect (cdr (assoc :y (cl-cc/optimize:opt-partial-eval-signature callee-report) :test #'equal)) :to-equal 7)))

(it-sequential "optimize-partial-evaluate-program-uses-offline-bta-to-prune-static-forms"
  (let* ((report (cl-cc/optimize:opt-partial-evaluate-function
                  'f
                  '(x)
                  '((+ x 1) x)
                  :constant-bindings '((x . 9))))
         (kinds (cl-cc/optimize:opt-partial-eval-form-kinds report))
         (dynamic-body (cl-cc/optimize:opt-partial-eval-dynamic-body report)))
    (expect (listp kinds) :to-be-truthy)
    (expect (member :static kinds) :to-be-truthy)
    (expect (listp dynamic-body) :to-be-truthy)))

(it-sequential "opt-pass-specialize-known-args-emits-specialized-clone-and-redirects-call"
  (let* ((func-label "add2")
         (closure (make-vm-closure :dst :r1 :label func-label :params '(:x :y) :captured nil))
         (body (list (make-vm-label :name func-label)
                     (make-vm-add :dst :r9 :lhs :x :rhs :y)
                     (make-vm-ret :reg :r9)))
         (caller (list (make-vm-const :dst :r2 :value 5)
                       (make-vm-call :dst :r3 :func :r1 :args '(:r2 :r4))))
         (optimized (cl-cc/optimize:opt-pass-specialize-known-args
                     (append (list closure) body caller)))
         (has-specialized-label
           (some (lambda (inst)
                   (and (typep inst 'cl-cc/vm::vm-label)
                        (search "__spec__" (cl-cc/vm::vm-name inst))))
                 optimized))
         (rewritten-call
           (find-if (lambda (inst)
                      (and (typep inst 'cl-cc/vm::vm-call)
                           (equal (cl-cc/vm::vm-args inst) '(:r4))))
                    optimized)))
    (expect has-specialized-label :to-be-truthy)
    (expect rewritten-call :to-be-truthy)))

(defun %opt-canonical-loop-program (body-instructions)
  (append (list (make-vm-const :dst :ri :value 0)
                (make-vm-const :dst :rlim :value 8)
                (make-vm-const :dst :rstep :value 1)
                (make-vm-label :name :l0)
                (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                (make-vm-jump-zero :reg :rc :label :l1))
          body-instructions
          (list (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                (make-vm-jump :label :l0)
                (make-vm-label :name :l1))))

(defun %opt-instruction-sexps (instructions)
  (mapcar #'instruction->sexp instructions))

(defun %opt-assert-program-changed (optimized program)
  (assert-false (equal (%opt-instruction-sexps optimized)
                       (%opt-instruction-sexps program))))

(defun %opt-assert-program-unchanged (optimized program)
  (assert-equal (%opt-instruction-sexps optimized)
                (%opt-instruction-sexps program)))

(defun %opt-sortable-loop-body ()
  (list (make-vm-mul :dst :r7 :lhs :r3 :rhs :r4)
        (make-vm-move :dst :r8 :src :r5)))

(defun %opt-loop-label-count (label instructions)
  (count-if (lambda (inst)
              (and (typep inst 'vm-label)
                   (eq (cl-cc/vm::vm-name inst) label)))
            instructions))

(defun %opt-assert-sortable-loop-pass-reorders (pass)
  (let* ((program (%opt-canonical-loop-program (%opt-sortable-loop-body)))
         (optimized (funcall pass program)))
    (%opt-assert-program-changed optimized program)
    (assert-true (typep (nth 6 optimized) 'vm-move))
    (assert-true (typep (nth 7 optimized) 'vm-mul))))

(defun %opt-loop-split-marker-p (instruction)
  (and (typep instruction 'vm-label)
       (search "__SPLIT" (string (cl-cc/vm::vm-name instruction)))))

(defun %opt-adjacent-loop-program (&key (first-init 0)
                                        (second-init 0)
                                        (second-condition :rc))
  (list (make-vm-const :dst :ri :value first-init)
        (make-vm-const :dst :rj :value second-init)
        (make-vm-const :dst :rlim :value 4)
        (make-vm-const :dst :rstep :value 1)
        (make-vm-label :name :la)
        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
        (make-vm-jump-zero :reg :rc :label :lax)
        (make-vm-move :dst :r10 :src :r11)
        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
        (make-vm-jump :label :la)
        (make-vm-label :name :lax)
        (make-vm-label :name :lb)
        (make-vm-lt :dst second-condition :lhs :rj :rhs :rlim)
        (make-vm-jump-zero :reg second-condition :label :lbx)
        (make-vm-move :dst :r12 :src :r13)
        (make-vm-add :dst :rj :lhs :rj :rhs :rstep)
        (make-vm-jump :label :lb)
        (make-vm-label :name :lbx)))

(it-sequential "optimize-affine-loop-summary-builds-descriptor"
  (let ((summary (cl-cc/optimize::opt-build-affine-loop-summary
                  :induction-vars '(:i)
                  :bounds '((:i 0 100))
                  :accesses '((:a :i)))))
    (expect (getf summary :kind) :to-be :affine-loop-summary)
    (expect (getf summary :induction-vars) :to-equal '(:i))
    (expect (getf summary :bounds) :to-equal '((:i 0 100)))))

(it-sequential "optimize-loop-interchange-plan-requires-safety safe"
  (destructuring-bind (dependence-safe-p expected-applied-p) (list t t)
    (let ((plan (cl-cc/optimize::opt-loop-interchange-plan
               :loops '(:i :j)
               :cache-locality-score 3
               :dependence-safe-p dependence-safe-p)))
    (expect (not (null (getf plan :applied-p))) :to-equal expected-applied-p))))

(it-sequential "optimize-loop-interchange-plan-requires-safety unsafe"
  (destructuring-bind (dependence-safe-p expected-applied-p) (list nil nil)
    (let ((plan (cl-cc/optimize::opt-loop-interchange-plan
               :loops '(:i :j)
               :cache-locality-score 3
               :dependence-safe-p dependence-safe-p)))
    (expect (not (null (getf plan :applied-p))) :to-equal expected-applied-p))))

(it-sequential "optimize-pass-loop-interchange-handles-nested-canonical-loop"
  (%opt-assert-sortable-loop-pass-reorders
   #'cl-cc/optimize::opt-pass-loop-interchange))

(it-sequential "optimize-pass-loop-interchange-skips-side-effecting-loop"
  (let* ((program (%opt-canonical-loop-program
                   (list (make-vm-set-global :src :r2 :name 'g))))
         (optimized (cl-cc/optimize::opt-pass-loop-interchange program)))
    (%opt-assert-program-unchanged optimized program)))

(it-sequential "optimize-polyhedral-schedule-plan-preserves-objective"
  (let ((plan (cl-cc/optimize::opt-polyhedral-schedule-plan
               :statements '(:s0 :s1)
               :constraints '((:s0-before :s1))
               :objective :throughput-max)))
    (expect (getf plan :kind) :to-be :polyhedral-schedule)
    (expect (getf plan :objective) :to-be :throughput-max)
    (expect (getf plan :statements) :to-equal '(:s0 :s1))))

(it-sequential "optimize-loop-fusion-fission-plan-selects-strategy fusion"
  (destructuring-bind (register-pressure expected-strategy) (list 16 :fusion)
    (let ((plan (cl-cc/optimize::opt-loop-fusion-fission-plan
               :loops '(:l0 :l1)
               :register-pressure register-pressure
               :instruction-budget 20)))
    (expect (getf plan :strategy) :to-be expected-strategy))))

(it-sequential "optimize-loop-fusion-fission-plan-selects-strategy fission"
  (destructuring-bind (register-pressure expected-strategy) (list 64 :fission)
    (let ((plan (cl-cc/optimize::opt-loop-fusion-fission-plan
               :loops '(:l0 :l1)
               :register-pressure register-pressure
               :instruction-budget 20)))
    (expect (getf plan :strategy) :to-be expected-strategy))))

(it-sequential "optimize-ml-inline-score-plan-is-deterministic"
  (let ((a (cl-cc/optimize::opt-ml-inline-score-plan
            :features '(:hot-loop :small-body)
            :model-version "mlgo-v2"))
        (b (cl-cc/optimize::opt-ml-inline-score-plan
            :features '(:hot-loop :small-body)
            :model-version "mlgo-v2")))
    (expect (getf a :kind) :to-be :ml-inline-score)
    (expect (= (getf a :score) (getf b :score)) :to-be-truthy)
    (expect (= 2 (getf a :feature-count)) :to-be-truthy)))

(it-sequential "optimize-learned-codegen-cost-plan-is-target-aware"
  (let ((x86 (cl-cc/optimize::opt-learned-codegen-cost-plan
              :opcode-features '(:mul :add)
              :target :x86-64))
        (arm (cl-cc/optimize::opt-learned-codegen-cost-plan
              :opcode-features '(:mul :add)
              :target :aarch64)))
    (expect (getf x86 :kind) :to-be :learned-codegen-cost)
    (expect (getf x86 :target) :to-be :x86-64)
    (expect (getf arm :target) :to-be :aarch64)
    (expect (/= (getf x86 :predicted-cost)
                     (getf arm :predicted-cost)) :to-be-truthy)))

(it-sequential "optimize-pass-affine-loop-analysis-captures-real-loop-summary"
  (let* ((program (%opt-canonical-loop-program
                   (list (make-vm-get-global :dst :r2 :name 'g))))
         (_ (cl-cc/optimize::opt-pass-affine-loop-analysis program))
         (summaries cl-cc/optimize::*opt-last-affine-loop-summaries*)
         (summary (first summaries)))
    (expect (listp summaries) :to-be-truthy)
    (expect summary :to-be-truthy)
    (expect (getf summary :kind) :to-be :affine-loop-summary)
    (expect (getf summary :induction-vars) :to-equal '(:ri))
    (expect (some (lambda (access)
                         (eq (getf access :kind) :read-global))
                       (getf summary :accesses)) :to-be-truthy)))

(it-sequential "optimize-pass-polyhedral-schedule-reorders-loop-body"
  (%opt-assert-sortable-loop-pass-reorders
   #'cl-cc/optimize::opt-pass-polyhedral-schedule))

(it-sequential "optimize-pass-loop-fusion-fission-fuses-adjacent-loops"
  (let* ((program (%opt-adjacent-loop-program))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (%opt-assert-program-changed optimized program)
    (expect (= 1 (%opt-loop-label-count :la optimized)) :to-be-truthy)
    (expect (= 0 (%opt-loop-label-count :lb optimized)) :to-be-truthy)))

(it-sequential "optimize-pass-loop-fusion-fission-skips-unsafe-fusion"
  (let* ((program (%opt-adjacent-loop-program :second-init 1
                                              :second-condition :rc2))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (%opt-assert-program-unchanged optimized program)))

(it-sequential "optimize-pass-loop-fusion-fission-splits-oversized-loop"
  (let* ((core (loop for idx from 0 below 36
                     collect (make-vm-move :dst :r8 :src :r5)))
         (program (%opt-canonical-loop-program core))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (%opt-assert-program-changed optimized program)
    (expect (some #'%opt-loop-split-marker-p optimized) :to-be-truthy)))
