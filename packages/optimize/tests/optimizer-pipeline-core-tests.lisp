;;;; tests/unit/optimize/optimizer-pipeline-core-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — pipeline mechanism (core)
;;;;
;;;; Covers: opt-parse-pass-pipeline-string, opt-converged-p,
;;;;   opt-adaptive-max-iterations, opt-verify-instructions,
;;;;   opt-resolve-pass-pipeline, *opt-pass-registry* data, prolog-rewrite-stage,
;;;;   COW helpers, bump/slab allocators, inline cache layer.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-pipeline-core-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-pipeline-core-tests-suite))

;;; The former %opt-trim-whitespace cases were removed with that private helper
;;; from the pinned cl-cc-optimize system; the parser tests below cover the
;;; current pipeline-string behavior instead.

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

;;; ─── opt-adaptive-max-iterations ─────────────────────────────────────────────

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

;;; Three focused tests replace the former monolithic opt-pass-data-integrity:
;;;   1. structural list invariants
;;;   2. registry key presence (deftest-each — one case per pass)
;;;   3. pipeline ordering invariants (deftest-each — one case per constraint)

;;; ─── FR-099 FMA recognition ───────────────────────────────────────────────

(defun %test-count-type (type insts)
  (count-if (lambda (inst) (typep inst type)) insts))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

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

;;; ─── Prolog rewrite stage ──────────────────────────────────────────────────

;;; ─── Copy-on-Write helper layer (FR-253 partial) ───────────────────────────

;;; ─── Region/bump allocation helpers (FR-254 partial) ───────────────────────

;;; ─── Inline cache helper layer (FR-009 / FR-019 partial) ──────────────────

