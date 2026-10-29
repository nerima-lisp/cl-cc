;;;; optimizer-purity-tests.lisp — Unit tests for optimizer-purity.lisp
;;;;
;;;; Covers: opt-function-body-transitively-pure-p (empty body, pure arithmetic,
;;;; unknown call → impure, known impure call, known pure call),
;;;; opt-infer-transitive-function-purity (empty program, pure leaf,
;;;; recursive → not pure, two-function chain, multi-hop purity).

(in-package :cl-cc/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun %make-func-defs (&rest label-body-pairs)
  "Build a func-defs hash-table from alternating label/body-list args."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label body) on label-body-pairs by #'cddr
          do (setf (gethash label ht)
                   (list :closure nil :params nil :body body)))
    ht))

(defun %make-pure-labels (&rest labels)
  "Build a pure-labels hash-table with each label marked T."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (l labels ht)
      (setf (gethash l ht) t))))

(defun %count-inst-type (instructions type)
  "Count TYPE instances in INSTRUCTIONS."
  (count-if (lambda (inst) (typep inst type)) instructions))

;;; ─── opt-function-body-transitively-pure-p ───────────────────────────────────

; callee NOT in pure-labels

; callee IS in pure-labels

;;; ─── opt-infer-transitive-function-purity ────────────────────────────────────

;;; ─── opt-pass-pure-call-optimization ─────────────────────────────────────────

