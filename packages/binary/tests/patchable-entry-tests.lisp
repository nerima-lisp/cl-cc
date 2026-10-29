;;;; packages/binary/tests/patchable-entry-tests.lisp — Patchable entry tests
;;;;
;;;; Tests for: emit-nop-sequence, emit-patchable-function-entry,
;;;; and with-patchable-entries (patchable-entry.lisp / FR-584).

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; Helper
;;; ------------------------------------------------------------

(defun %nop-bytes (count)
  "Return emitted NOP bytes for COUNT as a list."
  (coerce (cl-cc/binary::with-output-to-vector (stream)
            (cl-cc/binary::emit-nop-sequence stream count))
          'list))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — correct byte count
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; emit-nop-sequence — correct encoding for each single NOP
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; emit-nop-sequence — multi-chunk decomposition
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; emit-patchable-function-entry — non-zero before/after
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; with-patchable-entries — dynamic binding
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; patch-function-entry — size guard
;;; ------------------------------------------------------------

