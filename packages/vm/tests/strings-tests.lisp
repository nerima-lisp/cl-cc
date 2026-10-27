;;;; tests/unit/vm/strings-tests.lisp — VM String & Character Operations Unit Tests
;;;;
;;;; Tests for string comparison, manipulation, character access, and symbol
;;;; instructions executed via the VM.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for string tests."
  (make-instance 'cl-cc/vm:vm-io-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm:execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── String Comparisons (case-sensitive) ──────────────────────────────────

;;; ─── String Comparisons (case-insensitive) ────────────────────────────────

;;; ─── String Length ────────────────────────────────────────────────────────

;;; ─── Character Access ─────────────────────────────────────────────────────

;;; ─── Character Comparisons ────────────────────────────────────────────────

;;; ─── String Manipulation ──────────────────────────────────────────────────

;;; ─── String Trim ──────────────────────────────────────────────────────────

;;; ─── String Search ────────────────────────────────────────────────────────

;;; ─── Make String ──────────────────────────────────────────────────────────

;;; ─── Character Predicates ─────────────────────────────────────────────────

;;; ─── Stringp / Characterp Predicates ──────────────────────────────────────

;;; ─── Parse Integer ────────────────────────────────────────────────────────

