;;;; Unit tests for FR-602 loop unswitching.

(in-package :cl-cc/test)

(defun %fr602-unswitch-loop (&key (condition-side-effect-p nil))
  "Build a counted loop containing an invariant if/else in its body."
  (append (list (make-vm-const :dst :i :value 0)
                (make-vm-const :dst :limit :value 8)
                (make-vm-const :dst :one :value 1))
          (unless condition-side-effect-p
            (list (make-vm-const :dst :flag :value 1)))
          (list (make-vm-label :name "loop")
                (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                (make-vm-jump-zero :reg :cond :label "exit"))
          (when condition-side-effect-p
            (list (make-vm-call :dst :flag :func :predicate :args (list :i))))
          (list (make-vm-jump-zero :reg :flag :label "else")
                (make-vm-print :reg :i)
                (make-vm-jump :label "join")
                (make-vm-label :name "else")
                (make-vm-print :reg :limit)
                (make-vm-label :name "join")
                (make-vm-add :dst :i :lhs :i :rhs :one)
                (make-vm-jump :label "loop")
                (make-vm-label :name "exit")
                (make-vm-ret :reg :i))))

