;;;; tests/unit/vm/vm-execute-tests.lisp — Core VM Execute-Instruction Tests
;;;;
;;;; Tests for src/vm/vm-execute.lisp:
;;;; vm-falsep, vm-classify-arg, vm-save/restore-registers,
;;;; vm-list-to-lisp-list, and execute-instruction for const/move/halt/jump-zero.

(in-package :cl-cc/test)


;;; ─── vm-falsep ──────────────────────────────────────────────────────────────

;;; ─── vm-classify-arg ────────────────────────────────────────────────────────

;;; ─── vm-save-registers / vm-restore-registers ───────────────────────────────

;;; ─── vm-list-to-lisp-list ───────────────────────────────────────────────────

;;; ─── execute-instruction: vm-const ──────────────────────────────────────────

;;; ─── execute-instruction: vm-halt ───────────────────────────────────────────

;;; ─── execute-instruction: vm-jump-zero ──────────────────────────────────────
