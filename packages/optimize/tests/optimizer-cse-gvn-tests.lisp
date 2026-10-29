;;;; tests/unit/optimize/optimizer-cse-gvn-tests.lisp
;;;; Unit tests for src/optimize/optimizer-cse-gvn.lisp
;;;;
;;;; Covers: opt-pass-cse (redundant elimination, label flush, vm-const kept),
;;;;   opt-pass-gvn (straight-line, empty), opt-pass-dead-labels (removes
;;;;   unreferenced labels, keeps referenced ones), opt-pass-leaf-detect
;;;;   (leaf flag, non-leaf flag).

(in-package :cl-cc/test)

;;; ─── opt-pass-cse ────────────────────────────────────────────────────────

;;; ─── opt-pass-gvn ────────────────────────────────────────────────────────

;;; ─── opt-pass-dead-labels ────────────────────────────────────────────────

;;; ─── opt-pass-leaf-detect ────────────────────────────────────────────────

;;; ─── cse-state struct helpers ────────────────────────────────────────────

;;; ─── GVN helpers: %gvn-kill, %gvn-get-val, %gvn-record, %gvn-key ────────

;;; ─── %gvn-maybe-replace ──────────────────────────────────────────────────

;;; ─── %cse-emit-or-cse ────────────────────────────────────────────────────

