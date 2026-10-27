;;;; tests/unit/vm/vm-environment-tests.lisp
;;;; Unit tests for src/vm/vm-environment.lisp
;;;;
;;;; Covers: vm-boundp, vm-fboundp, vm-makunbound,
;;;;   vm-fdefinition, vm-random, vm-make-random-state,
;;;;   vm-get-universal-time, vm-get-internal-real-time,
;;;;   vm-get-internal-run-time, vm-decode-universal-time,
;;;;   vm-encode-universal-time.

(in-package :cl-cc/test)

;;; ─── helpers ─────────────────────────────────────────────────────────────

(defun %env-unary (ctor-fn sym)
  "Run a unary environment instruction using SYM as the :src register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 sym)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

;;; ─── vm-boundp ───────────────────────────────────────────────────────────

;;; ─── vm-fboundp ──────────────────────────────────────────────────────────

;;; ─── vm-makunbound ───────────────────────────────────────────────────────

;;; ─── vm-fdefinition ──────────────────────────────────────────────────────

;;; ─── vm-random ───────────────────────────────────────────────────────────

;;; ─── vm-make-random-state ────────────────────────────────────────────────

;;; ─── vm-get-universal-time ───────────────────────────────────────────────

;;; ─── vm-get-internal-real-time ───────────────────────────────────────────

;;; ─── vm-get-internal-run-time ────────────────────────────────────────────

;;; ─── vm-decode-universal-time ────────────────────────────────────────────

;;; ─── vm-encode-universal-time ────────────────────────────────────────────

