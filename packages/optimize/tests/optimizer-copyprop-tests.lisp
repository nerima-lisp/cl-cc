;;;; tests/unit/optimize/optimizer-copyprop-tests.lisp — Copy Propagation Tests
;;;;
;;;; Tests for src/optimize/optimizer-copyprop.lisp:
;;;;   opt-map-tree, %opt-copy-prop-env-copy/equal-p/canonical/merge,
;;;;   %opt-copy-prop-add/kill, %opt-value<, copyprop-pass-state helpers,
;;;;   opt-pass-copy-prop.

(in-package :cl-cc/test)


;;; ── Helpers ─────────────────────────────────────────────────────────────────

(defun %make-copy-env (&rest pairs)
  "Build a copy-propagation environment hash-table from PAIRS (k v k v ...)."
  (let ((env (make-hash-table :test #'eq)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k env) v))
    env))

(defun %copy-env-get (env key)
  "Return the value for KEY in ENV, or NIL if absent."
  (gethash key env))

;;; ── opt-map-tree ────────────────────────────────────────────────────────────

;;; ── %opt-copy-prop-env-copy / %opt-copy-prop-env-equal-p ───────────────────

;;; ── %opt-copy-prop-canonical ────────────────────────────────────────────────

;;; ── %opt-copy-prop-add / %opt-copy-prop-kill ────────────────────────────────

;;; ── %opt-copy-prop-merge ────────────────────────────────────────────────────

;;; ── %opt-value-rank / *opt-type-rank-table* ─────────────────────────────────

;;; ── %opt-value< ─────────────────────────────────────────────────────────────

;;; ── opt-pass-copy-prop integration ──────────────────────────────────────────

(defun %copyprop-find (instrs type-name)
  "Return the first instruction of TYPE-NAME in INSTRS, or NIL."
  (let ((type (find-symbol (symbol-name type-name) :cl-cc)))
    (find-if (lambda (i) (typep i type)) instrs)))

;;; ── copyprop-pass-state helpers ─────────────────────────────────────────────

;;; ── %opt-copy-prop-build-reverse ─────────────────────────────────────────

;;; ── %opt-copy-prop-transfer-block ────────────────────────────────────────

;;; ── %opt-copy-prop-rewrite-inst ─────────────────────────────────────────

;;; ── %opt-copy-prop-rewrite-block ────────────────────────────────────────

