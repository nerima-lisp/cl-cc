;;;; tests/unit/vm/conditions-tests.lisp — VM Condition System Unit Tests
;;;;
;;;; Tests for handler stack management, restart bindings,
;;;; condition construction, and signal dispatch.

(in-package :cl-cc/test)



;;; ─── Condition Construction ───────────────────────────────────────────────

;;; ─── Condition Hierarchy ──────────────────────────────────────────────────

;;; ─── Condition Report ─────────────────────────────────────────────────────

(it-sequential "vm-conditions-carry-structured-diagnostics"
  (let* ((state (make-instance 'cl-cc/vm:vm-io-state))
         (fix-it (cl-cc/parse:make-fix-it :text "bind x" :span '(0 . 1)))
         (cond (cl-cc/vm:make-vm-type-error state 'fixnum "hello"
                                             :error-code "E1001"
                                             :fix-it fix-it)))
    (expect (cl-cc:vm-condition-error-code cond) :to-equal "E1001")
    (expect (cl-cc:vm-condition-fix-it cond) :to-be fix-it))
  (let* ((state (make-instance 'cl-cc/vm:vm-io-state))
         (cond (cl-cc/vm:make-vm-unbound-variable state 'x)))
    (expect (cl-cc:vm-condition-error-code cond) :to-be-null)
    (expect (cl-cc:vm-condition-fix-it cond) :to-be-null)))

;;; ─── Handler Stack ────────────────────────────────────────────────────────

;;; ─── Restart Bindings ─────────────────────────────────────────────────────


;;; ─── Signal Dispatch ──────────────────────────────────────────────────────

;;; ─── Clear Context ────────────────────────────────────────────────────────

