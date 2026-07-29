;;;; tests/unit/optimize/optimizer-pipeline-core-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — pipeline mechanism (core)
;;;;
;;;; Covers: opt-parse-pass-pipeline-string, opt-converged-p,
;;;;   opt-adaptive-max-iterations, opt-verify-instructions,
;;;;   opt-resolve-pass-pipeline, *opt-pass-registry* data, prolog-rewrite-stage,
;;;;   COW helpers, bump/slab allocators, inline cache layer.

(in-package :cl-cc/test)

;;; ─── %opt-trim-whitespace ────────────────────────────────────────────────────

(it-sequential "opt-trim-whitespace-cases spaces"
  (destructuring-bind (expected input) (list "hello" "  hello  ")
    (expect (cl-cc/optimize::%opt-trim-whitespace input) :to-equal expected)))

(it-sequential "opt-trim-whitespace-cases tabs"
  (destructuring-bind (expected input) (list "world" (format nil "~Cworld~C" #\Tab #\Tab))
    (expect (cl-cc/optimize::%opt-trim-whitespace input) :to-equal expected)))

(it-sequential "opt-trim-whitespace-cases newlines"
  (destructuring-bind (expected input) (list "foo" (format nil "~Cfoo~C" #\Newline #\Newline))
    (expect (cl-cc/optimize::%opt-trim-whitespace input) :to-equal expected)))

(it-sequential "opt-trim-whitespace-cases mixed"
  (destructuring-bind (expected input) (list "bar" (format nil " ~C~C bar ~C~C " #\Tab #\Newline #\Newline #\Tab))
    (expect (cl-cc/optimize::%opt-trim-whitespace input) :to-equal expected)))

(it-sequential "opt-trim-whitespace-cases no-trim"
  (destructuring-bind (expected input) (list "bare" "bare")
    (expect (cl-cc/optimize::%opt-trim-whitespace input) :to-equal expected)))

(it-sequential "opt-trim-whitespace-cases empty"
  (destructuring-bind (expected input) (list "" "")
    (expect (cl-cc/optimize::%opt-trim-whitespace input) :to-equal expected)))

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

(it-sequential "verify-instructions-valid-cases simple-sequence"
  (destructuring-bind (insts) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret   :reg :r0)))
    (expect (cl-cc/optimize:opt-verify-instructions insts) :to-be-truthy)))

(it-sequential "verify-instructions-valid-cases jump-with-known-label"
  (destructuring-bind (insts) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-jump  :label "target")
                 (make-vm-label :name "target")
                 (make-vm-ret   :reg :r0)))
    (expect (cl-cc/optimize:opt-verify-instructions insts) :to-be-truthy)))

(it-sequential "verify-instructions-invalid-cases duplicate-label"
  (destructuring-bind (insts) (list (list (make-vm-const :dst :r0 :value 1)
                                     (make-vm-label :name "dup")
                                     (make-vm-label :name "dup")
                                     (make-vm-ret   :reg :r0)))
    (signals error (cl-cc/optimize:opt-verify-instructions insts))))

(it-sequential "verify-instructions-invalid-cases unknown-target"
  (destructuring-bind (insts) (list (list (make-vm-jump :label "ghost")
                                     (make-vm-ret  :reg :r0)))
    (signals error (cl-cc/optimize:opt-verify-instructions insts))))

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

(it-sequential "resolve-pass-pipeline-nil-returns-convergence-passes"
  (expect (cl-cc/optimize::opt-resolve-pass-pipeline nil) :to-be cl-cc/optimize::*opt-convergence-passes*))

(it-sequential "resolve-pass-pipeline-functions-pass-through-unchanged"
  (let* ((fn (lambda (x) x))
         (pipeline (list fn)))
    (expect (first (cl-cc/optimize::opt-resolve-pass-pipeline pipeline)) :to-be fn)))

(it-sequential "resolve-pass-pipeline-multi-pass-input-forms keywords"
  (destructuring-bind (input) (list (list :fold :dce))
    (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline input)))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (every #'functionp result) :to-be-truthy))))

(it-sequential "resolve-pass-pipeline-multi-pass-input-forms string"
  (destructuring-bind (input) (list "fold,dce")
    (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline input)))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (every #'functionp result) :to-be-truthy))))

(it-sequential "resolve-pass-pipeline-unknown-pass-signals-error"
  (signals error (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

;;; Three focused tests replace the former monolithic opt-pass-data-integrity:
;;;   1. structural list invariants
;;;   2. registry key presence (deftest-each — one case per pass)
;;;   3. pipeline ordering invariants (deftest-each — one case per constraint)

(it-sequential "opt-convergence-passes-list-structure"
  (expect (listp cl-cc/optimize::*opt-convergence-passes*) :to-be-truthy)
  (expect (> (length cl-cc/optimize::*opt-convergence-passes*) 10) :to-be-truthy)
  (expect (every #'functionp cl-cc/optimize::*opt-convergence-passes*) :to-be-truthy)
  (expect (first cl-cc/optimize::*opt-convergence-passes*) :to-be #'cl-cc/optimize::%maybe-apply-prolog-rewrite))

(it-sequential "opt-convergence-passes-raw-fns-absent optimize-with-egraph"
  (destructuring-bind (fn) (list #'cl-cc/optimize:optimize-with-egraph)
    (expect (member fn cl-cc/optimize::*opt-convergence-passes*) :to-be-falsy)))

(it-sequential "opt-convergence-passes-raw-fns-absent opt-pass-fold"
  (destructuring-bind (fn) (list #'cl-cc/optimize::opt-pass-fold)
    (expect (member fn cl-cc/optimize::*opt-convergence-passes*) :to-be-falsy)))

(it-sequential "opt-convergence-passes-raw-fns-absent opt-pass-strength-reduce"
  (destructuring-bind (fn) (list #'cl-cc/optimize::opt-pass-strength-reduce)
    (expect (member fn cl-cc/optimize::*opt-convergence-passes*) :to-be-falsy)))

(it-sequential "opt-convergence-keys-first-six-fixed"
  (expect (subseq cl-cc/optimize::*opt-default-convergence-pass-keys* 0 6) :to-equal '(:prolog-rewrite :call-site-splitting :devirtualize :if-conversion
                  :closure-capture-dedup :closure-thunk-sharing)))

(it-sequential "opt-convergence-keys-positional-slots 7th-inline"
  (destructuring-bind (n expected) (list 7 :inline)
    (expect (nth (1- n) cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be expected)))

(it-sequential "opt-convergence-keys-positional-slots 8th-overflow-check"
  (destructuring-bind (n expected) (list 8 :overflow-check-elim)
    (expect (nth (1- n) cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be expected)))

(it-sequential "opt-convergence-keys-positional-slots 9th-sccp"
  (destructuring-bind (n expected) (list 9 :sccp)
    (expect (nth (1- n) cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be expected)))

(it-sequential "opt-convergence-keys-positional-slots 10th-cons-slot-forward"
  (destructuring-bind (n expected) (list 10 :cons-slot-forward)
    (expect (nth (1- n) cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be expected)))

(it-sequential "opt-pass-registry-required-keys-present prolog-rewrite"
  (destructuring-bind (key) (list :prolog-rewrite)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present egraph"
  (destructuring-bind (key) (list :egraph)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present fold"
  (destructuring-bind (key) (list :fold)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present cons-slot-forward"
  (destructuring-bind (key) (list :cons-slot-forward)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present pure-call-optimization"
  (destructuring-bind (key) (list :pure-call-optimization)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present dce"
  (destructuring-bind (key) (list :dce)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present cse"
  (destructuring-bind (key) (list :cse)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present if-conversion"
  (destructuring-bind (key) (list :if-conversion)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-registry-required-keys-present fma-recognition"
  (destructuring-bind (key) (list :fma-recognition)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "opt-pass-pipeline-ordering-invariants devirt-before-if-conv"
  (destructuring-bind (earlier later) (list :devirtualize :if-conversion)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

(it-sequential "opt-pass-pipeline-ordering-invariants if-conv-before-inline"
  (destructuring-bind (earlier later) (list :if-conversion :inline)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

(it-sequential "opt-pass-pipeline-ordering-invariants copy-prop-before-pure"
  (destructuring-bind (earlier later) (list :copy-prop :pure-call-optimization)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

(it-sequential "opt-pass-pipeline-ordering-invariants pure-before-gvn"
  (destructuring-bind (earlier later) (list :pure-call-optimization :gvn)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

(it-sequential "opt-pass-pipeline-ordering-invariants pure-before-cse"
  (destructuring-bind (earlier later) (list :pure-call-optimization :cse)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

(it-sequential "opt-pass-pipeline-ordering-invariants pure-before-dce"
  (destructuring-bind (earlier later) (list :pure-call-optimization :dce)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

(it-sequential "opt-pass-pipeline-ordering-invariants fma-before-schedule-local"
  (destructuring-bind (earlier later) (list :fma-recognition :schedule-local)
    (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (expect (< (position earlier keys) (position later keys)) :to-be-truthy))))

;;; ─── FR-099 FMA recognition ───────────────────────────────────────────────

(defun %test-count-type (type insts)
  (count-if (lambda (inst) (typep inst type)) insts))

(it-sequential "fr-099-fma-recognition-cases mul-plus-accumulator"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)) 1 0 0)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    (when expected-float-mul-or-mul
      (let ((float-mul-count (%test-count-type 'cl-cc/vm::vm-float-mul out))
            (int-mul-count   (%test-count-type 'cl-cc/vm::vm-mul out)))
        (expect (= expected-float-mul-or-mul (+ float-mul-count int-mul-count)) :to-be-truthy)))
    (when expected-float-add-or-add
      (let ((float-add-count (%test-count-type 'cl-cc/vm::vm-float-add out))
            (int-add-count   (%test-count-type 'cl-cc/vm::vm-add out)))
        (expect (= expected-float-add-or-add (+ float-add-count int-add-count)) :to-be-truthy))))))

(it-sequential "fr-099-fma-recognition-cases commuted-add"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r2 :rhs :r3)) 1 0 0)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    (when expected-float-mul-or-mul
      (let ((float-mul-count (%test-count-type 'cl-cc/vm::vm-float-mul out))
            (int-mul-count   (%test-count-type 'cl-cc/vm::vm-mul out)))
        (expect (= expected-float-mul-or-mul (+ float-mul-count int-mul-count)) :to-be-truthy)))
    (when expected-float-add-or-add
      (let ((float-add-count (%test-count-type 'cl-cc/vm::vm-float-add out))
            (int-add-count   (%test-count-type 'cl-cc/vm::vm-add out)))
        (expect (= expected-float-add-or-add (+ float-add-count int-add-count)) :to-be-truthy))))))

(it-sequential "fr-099-fma-recognition-cases multiple-consumers-no-fuse"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)
                 (cl-cc/vm::make-vm-float-add :dst :r5 :lhs :r3 :rhs :r6)) 0 1 2)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    (when expected-float-mul-or-mul
      (let ((float-mul-count (%test-count-type 'cl-cc/vm::vm-float-mul out))
            (int-mul-count   (%test-count-type 'cl-cc/vm::vm-mul out)))
        (expect (= expected-float-mul-or-mul (+ float-mul-count int-mul-count)) :to-be-truthy)))
    (when expected-float-add-or-add
      (let ((float-add-count (%test-count-type 'cl-cc/vm::vm-float-add out))
            (int-add-count   (%test-count-type 'cl-cc/vm::vm-add out)))
        (expect (= expected-float-add-or-add (+ float-add-count int-add-count)) :to-be-truthy))))))

(it-sequential "fr-099-fma-recognition-cases integer-arithmetic-no-fuse"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (make-vm-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (make-vm-add :dst :r4 :lhs :r3 :rhs :r2)) 0 nil nil)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    (when expected-float-mul-or-mul
      (let ((float-mul-count (%test-count-type 'cl-cc/vm::vm-float-mul out))
            (int-mul-count   (%test-count-type 'cl-cc/vm::vm-mul out)))
        (expect (= expected-float-mul-or-mul (+ float-mul-count int-mul-count)) :to-be-truthy)))
    (when expected-float-add-or-add
      (let ((float-add-count (%test-count-type 'cl-cc/vm::vm-float-add out))
            (int-add-count   (%test-count-type 'cl-cc/vm::vm-add out)))
        (expect (= expected-float-add-or-add (+ float-add-count int-add-count)) :to-be-truthy))))))

(it-sequential "fr-099-fma-recognition-cases cross-block-boundary-no-fuse"
  (destructuring-bind (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add) (list (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (make-vm-label :name "next")
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)) 0 1 1)
    (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (expect (= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out)) :to-be-truthy)
    (when expected-float-mul-or-mul
      (let ((float-mul-count (%test-count-type 'cl-cc/vm::vm-float-mul out))
            (int-mul-count   (%test-count-type 'cl-cc/vm::vm-mul out)))
        (expect (= expected-float-mul-or-mul (+ float-mul-count int-mul-count)) :to-be-truthy)))
    (when expected-float-add-or-add
      (let ((float-add-count (%test-count-type 'cl-cc/vm::vm-float-add out))
            (int-add-count   (%test-count-type 'cl-cc/vm::vm-add out)))
        (expect (= expected-float-add-or-add (+ float-add-count int-add-count)) :to-be-truthy))))))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

(it-sequential "verify-optimizer-flag-runs-verifier-on-valid-input"
  (let ((cl-cc/optimize:*verify-optimizer-instructions* t)
        (insts (list (make-vm-const :dst :r0 :value 42) (make-vm-ret :reg :r0))))
    (expect (listp (cl-cc/optimize:optimize-instructions insts)) :to-be-truthy)))

(it-sequential "verify-optimizer-flag-nil-skips-verifier"
  (let ((cl-cc/optimize:*verify-optimizer-instructions* nil)
        (insts (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))))
    (expect (listp (cl-cc/optimize:optimize-instructions insts)) :to-be-truthy)))

(defun %make-pure-square-insts ()
  "Build a small program where a closure squares its argument — used to test pure-call optimization."
  (let ((label "pure-square"))
    (list (make-vm-closure :dst :r9 :label label :params '(:r0) :captured nil)
          (make-vm-label   :name label)
          (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
          (make-vm-ret     :reg :r1)
          (make-vm-closure :dst :r2 :label label :params '(:r0) :captured nil)
          (make-vm-const   :dst :r0 :value 7)
          (make-vm-call    :dst :r3 :func :r2 :args '(:r0))
          (make-vm-call    :dst :r4 :func :r2 :args '(:r0))
          (make-vm-ret     :reg :r4))))

(it-sequential "pure-call-policy-gate-disables-pass"
  (let* ((insts (%make-pure-square-insts))
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
  (let* ((insts (%make-pure-square-insts))
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
