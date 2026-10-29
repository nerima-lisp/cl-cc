;;;; tests/unit/optimize/optimizer-inline-pass-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp
;;;;
;;;; Covers:
;;;;   Memo utilities  — opt-make-pure-function-memo-table,
;;;;                     opt-pure-function-memo-get, opt-pure-function-memo-put
;;;;   Body analysis   — opt-function-body-instruction-tables (single-pass, returns values)
;;;;   Reachability    — opt-reachable-function-labels

(in-package :cl-cc/test)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

;;; Note: opt-collect-function-defs requires (vm-closure-params inst) to be
;;; non-nil — functions with zero parameters are excluded from collection.
;;; Always give test closures at least one param so they are collected.

(defun make-inline-pass-func-insts (label params body-insts)
  "Return instruction list: vm-closure + vm-label + BODY-INSTS + vm-ret."
  (list* (make-vm-closure :dst :r9 :label label :params params :captured nil)
         (make-vm-label  :name label)
         (append body-insts
                 (list (make-vm-ret :reg (first params))))))

(defun collect-inline-pass-func-defs (label params body-insts)
  "Build func-defs table directly for functions with the given structure."
  (cl-cc/optimize::opt-collect-function-defs
   (make-inline-pass-func-insts label params body-insts)))

;;; ─── opt-make-pure-function-memo-table ──────────────────────────────────────

;;; ─── opt-pure-function-memo-get / opt-pure-function-memo-put ────────────────

;;; ─── opt-function-body-instruction-tables ───────────────────────────────────

;;; ─── opt-reachable-function-labels ──────────────────────────────────────────

