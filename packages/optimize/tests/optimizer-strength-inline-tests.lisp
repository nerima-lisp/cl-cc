;;;; optimizer-strength-inline-tests.lisp
;;;
;;; Integration tests for strength-reduction, reassociation,
;;; mul-by-const decomposition, adaptive inlining, and optimizer
;;; helper predicates.
;;;
;;; Extracted from optimizer-lowlevel-tests.lisp (> 600 lines).

(in-package :cl-cc/test)


;;; ── opt-pass-strength-reduce: Strength Reduction ─────────────────────────

;;; ── opt-pass-reassociate: Arithmetic Reassociation ──────────────────────

;;; ── %opt-mul-by-const-seq: Extracted Shift/Add Decomposition ────────────

(defun make-test-new-reg ()
  "Return a thunk that allocates fresh keyword registers :R1000, :R1001, ..."
  (let ((n 1000))
    (lambda () (prog1 (intern (format nil "R~A" n) :keyword) (incf n)))))

;;; ── opt-inline-eligible-p: Extracted Eligibility Predicate ──────────────

;;; ─── opt-falsep ──────────────────────────────────────────────────────────

;;; ─── opt-register-keyword-p ──────────────────────────────────────────────

;;; ─── opt-binary-lhs-rhs-p / opt-unary-src-p ─────────────────────────────

;;; ─── opt-foldable-unary-arith-p / opt-foldable-type-pred-p ──────────────

