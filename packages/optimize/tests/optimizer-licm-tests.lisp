;;;; tests/unit/optimize/optimizer-licm-tests.lisp
;;;; Unit tests for src/optimize/optimizer-licm.lisp
;;;;
;;;; Covers: opt-inst-loop-invariant-p, %opt-pre-expression-key,
;;;;   %opt-pre-splice-before-terminator, opt-pass-licm (trivial paths),
;;;;   optimize-with-egraph.

(in-package :cl-cc/test)

(defmacro assert-licm-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── opt-inst-loop-invariant-p ───────────────────────────────────────────

;;; ─── %opt-pre-expression-key ─────────────────────────────────────────────

;;; ─── %opt-pre-splice-before-terminator ───────────────────────────────────

;;; ─── %opt-pre-env-evict-dst ──────────────────────────────────────────────

;;; ─── opt-pass-licm (trivial paths) ───────────────────────────────────────

;;; ─── %opt-pre-reconstruct-inst ──────────────────────────────────────────

;;; ─── %opt-pre-available-in-any-p ────────────────────────────────────────

(defun %pre-envs-with-key (key in-env1-p in-env2-p)
  "Build two predecessor (label . env) pairs; seed KEY in each env according to the boolean flags."
  (let ((e1 (make-hash-table :test #'equal))
        (e2 (make-hash-table :test #'equal)))
    (when in-env1-p (setf (gethash key e1) :r0))
    (when in-env2-p (setf (gethash key e2) :r0))
    (list (cons :p1 e1) (cons :p2 e2))))

;;; ─── %licm-collect-def-sites ────────────────────────────────────────────

;;; ─── %licm-loop-def-regs ────────────────────────────────────────────────

;;; ─── %licm-find-loop-headers (smoke test via opt-pass-licm) ────────────

;;; ─── %opt-rewrite-block-terminator (shared helper via licm) ─────────────

;;; ─── %licm-redirect-successor ────────────────────────────────────────────

;;; ─── %licm-collect-members ──────────────────────────────────────────────

;;; ─── %licm-collect-invariants ────────────────────────────────────────────

;;; ─── %opt-pre-block-out-env ──────────────────────────────────────────────

;;; ─── %opt-pre-join-elim ──────────────────────────────────────────────────

;;; ─── %opt-pre-emit-compensating ────────────────────────────────────────

(defun %make-pre-compensate-state (&optional (prior-src nil))
  "Build (pair inst pred-inserts) for %opt-pre-emit-compensating tests.
   When PRIOR-SRC is non-nil, pre-seed key :k with that register in the env."
  (let* ((pred  (make-instance 'cl-cc/optimize:basic-block))
         (env   (make-hash-table :test #'eq))
         (table (make-hash-table :test #'eq))
         (inst  (make-vm-const :dst :r0 :value 7))
         (pair  (cons pred env)))
    (when prior-src (setf (gethash :k env) prior-src))
    (values pair inst table pred)))

;;; ─── opt-pass-pre ────────────────────────────────────────────────────────

;;; ─── optimize-with-egraph ────────────────────────────────────────────────

