;;;; tests/unit/optimize/effects-tests.lisp — Effect-Kind System Tests
;;;
;;; Tests for vm-inst-effect-kind, opt-inst-pure-p, opt-inst-dce-eligible-p,
;;; and opt-inst-cse-eligible-p (Phase 0 of optimizer modernization).

(in-package :cl-cc/test)

;;; ─── vm-inst-effect-kind ─────────────────────────────────────────────────

;;; ─── opt-inst-pure-p ─────────────────────────────────────────────────────

;;; ─── opt-inst-dce-eligible-p ─────────────────────────────────────────────

;;; ─── opt-inst-cse-eligible-p ─────────────────────────────────────────────

;;; ─── Effect Kind: IO / Alloc / Read-Only / Write-Global ─────────────────

;;; ─── CSE / DCE Properties of New Kinds ──────────────────────────────────

;;; ─── DCE Extended Coverage ───────────────────────────────────────────────

;;; ─── effect-row->effect-kind bridge ─────────────────────────────────────

(defun make-effect-row (&rest effect-names)
  "Helper: build a type-effect-row with the given effect name symbols."
  (cl-cc/type:make-type-effect-row
   :effects (mapcar (lambda (n) (cl-cc/type:make-type-effect-op :name n :args nil)) effect-names)
   :row-var nil))

