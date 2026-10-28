;;;; tests/unit/vm/list-alist-tests.lisp — VM Alist, Copy, Predicate, and PC Tests
;;;
;;; Tests for vm-assoc, vm-acons, vm-equal, vm-nconc, vm-copy-list/tree,
;;; vm-subst, vm-listp, vm-atom, and PC advancement for list instructions.

(in-package :cl-cc/test)

(defun %prepare-vm-list-predicate-input (state value)
  (if (eq value :cow-list)
      (progn
        (cl-cc:vm-reg-set state 2 '(a b))
        (exec1 (cl-cc:make-vm-copy-list :dst 1 :src 2) state))
      (cl-cc:vm-reg-set state 1 value)))

;;; ─── Association lists ──────────────────────────────────────────────────────

;;; ─── equal / nconc / copy-list / copy-tree / subst ──────────────────────────

;;; ─── Type predicates ────────────────────────────────────────────────────────

;;; ─── PC advancement ─────────────────────────────────────────────────────────
