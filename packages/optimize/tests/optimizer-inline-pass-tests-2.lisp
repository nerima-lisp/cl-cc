;;;; tests/unit/optimize/optimizer-inline-pass-tests-2.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp (continued)
;;;;
;;;; Covers:
;;;;   Inlining policy — opt-inline-inst-cost, opt-inline-body-cost,
;;;;                     opt-adaptive-inline-threshold, opt-inline-eligible-p
;;;;   Full passes     — opt-pass-global-dce, opt-pass-inline

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-inline-pass-tests-2-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-inline-pass-tests-2-suite))

;;; ─── opt-inline-inst-cost ───────────────────────────────────────────────────

;;; ─── opt-inline-body-cost ───────────────────────────────────────────────────

;;; ─── opt-adaptive-inline-threshold ──────────────────────────────────────────

;;; ─── opt-inline-eligible-p ──────────────────────────────────────────────────

(defun %make-eligible-def (label params body-insts)
  "Build a plist-style func def suitable for opt-inline-eligible-p."
  (let ((ci (make-vm-closure :dst :r9 :label label :params params :captured nil)))
    (list :closure ci
          :params params
          :body (append body-insts (list (make-vm-ret :reg (first params)))))))

;;; ─── opt-pass-global-dce ────────────────────────────────────────────────────

;;; ─── opt-pass-inline ────────────────────────────────────────────────────────

