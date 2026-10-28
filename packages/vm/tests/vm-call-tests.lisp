;;;; tests/unit/vm/vm-call-tests.lisp — vm-call / vm-tail-call / vm-ret Tests
;;;;
;;;; Tests for call/tail-call/ret instruction behavior in src/vm/vm-execute.lisp.

(in-package :cl-cc/test)


;;; ─── vm-call / vm-tail-call / vm-ret ───────────────────────────────────────

(defun %make-test-closure (entry-label params)
  "Build a bare vm-closure-object with no captures or optional params."
  (make-instance 'cl-cc:vm-closure-object
                 :entry-label entry-label
                 :params params))

(defun %labels (&rest kv-pairs)
  "Build a labels hash table from alternating label/pc pairs.
Uses vm-label-table-store so the internal sxhash-bucketed layout matches
what vm-label-table-lookup expects — raw (setf gethash) produces entries
that lookup can't find."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label pc) on kv-pairs by #'cddr
          do (cl-cc/vm:vm-label-table-store ht label pc))
    ht))

(progn)
