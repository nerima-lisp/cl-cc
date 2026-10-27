;;;; tests/unit/vm/vm-dispatch-gf-multi-tests.lisp
;;;; Coverage for src/vm/vm-dispatch-gf-multi.lisp:
;;;;   %vm-gf-uses-composite-keys-p, %vm-resolve-single-dispatch,
;;;;   %vm-resolve-composite-dispatch, vm-resolve-gf-method,
;;;;   vm-resolve-multi-dispatch, vm-try-dispatch-combinations,
;;;;   vm-try-dispatch-sub.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-single-dispatch-gf-ht (methods-plist)
  "Build a minimal generic-function hash-table for single dispatch.
METHODS-PLIST is a list of (class-name . method-fn) pairs."
  (let ((gf-ht (make-hash-table :test #'equal))
        (methods-ht (make-hash-table :test #'equal)))
    (dolist (pair methods-plist)
      (setf (gethash (car pair) methods-ht) (cdr pair)))
    (setf (gethash :__methods__ gf-ht) methods-ht)
    (setf (gethash :__name__ gf-ht) 'test-gf)
    (setf (gethash :__eql-methods__ gf-ht) nil)
    gf-ht))

(defun make-indexed-single-dispatch-gf-ht (methods-plist eql-plist)
  "Build a single-dispatch GF with an authoritative EQL dispatch index.
EQL-PLIST is a list of (eql-value . method-fn) pairs."
  (let ((gf-ht (make-single-dispatch-gf-ht methods-plist))
        (eql-index (make-hash-table :test #'equal)))
    (dolist (pair eql-plist)
      (setf (gethash (car pair) eql-index) (list (cdr pair))))
    (setf (gethash :__eql-index__ gf-ht) eql-index)
    gf-ht))

(defun make-composite-dispatch-gf-ht (methods-plist)
  "Build a generic-function hash-table with list keys for composite dispatch."
  (let ((gf-ht (make-hash-table :test #'equal))
        (methods-ht (make-hash-table :test #'equal)))
    (dolist (pair methods-plist)
      (setf (gethash (car pair) methods-ht) (cdr pair)))
    (setf (gethash :__methods__ gf-ht) methods-ht)
    (setf (gethash :__name__ gf-ht) 'test-multi-gf)
    (setf (gethash :__eql-methods__ gf-ht) nil)
    gf-ht))

;;; ─── %vm-gf-uses-composite-keys-p ────────────────────────────────────────

;;; ─── %vm-resolve-single-dispatch ──────────────────────────────────────────

;;; ─── vm-try-dispatch-combinations ────────────────────────────────────────

;;; ─── vm-try-dispatch-sub ─────────────────────────────────────────────────

;;; ─── vm-resolve-gf-method (integration) ──────────────────────────────────

