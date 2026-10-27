;;;; packages/vm/tests/vm-sequence-tests.lisp
;;;;
;;;; Dedicated unit tests for vm-sequence.lisp:
;;;;   - define-builtin-sequence-methods macro expansion sanity
;;;;   - copy-instance: hash-table path, missing __class__ error, standard-object path
;;;;   - copy-structure: all dispatch branches
;;;;   - deep-copy: cycle detection via seen table, function/character immediates
;;;;   - sequence-protocol-p: true/false cases

(in-package :cl-cc/test)



;;; ── 1. sequence-protocol-p ────────────────────────────────────────────────

;;; ── 2. length / elt / (setf elt) / subseq for each built-in type ─────────

;;; ── 3. make-sequence-like ─────────────────────────────────────────────────

;;; ── 4. copy-instance paths ────────────────────────────────────────────────

;;; ── 5. copy-structure dispatch branches ──────────────────────────────────

;;; ── 6. deep-copy: immediate values and cycle detection ───────────────────

