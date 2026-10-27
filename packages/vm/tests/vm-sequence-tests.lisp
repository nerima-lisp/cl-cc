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

(it-sequential "deep-copy-cycle-in-cons-is-preserved"
  (let* ((pair (cons :head nil))
         (seen (make-hash-table :test #'eq)))
    ;; register pair in seen before calling deep-copy to simulate a cycle break
    (setf (gethash pair seen) pair)
    ;; deep-copy should find it in seen and return the registered value
    (expect (cl-cc/vm:deep-copy pair seen) :to-be pair)))

(it-sequential "deep-copy-hash-table-cycle-via-seen"
  (let* ((original (make-hash-table))
         (sentinel :already-copied)
         (seen (make-hash-table :test #'eq)))
    (setf (gethash original seen) sentinel)
    (expect (cl-cc/vm:deep-copy original seen) :to-be sentinel)))

