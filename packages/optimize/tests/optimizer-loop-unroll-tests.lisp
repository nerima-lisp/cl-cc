;;;; Unit tests for FR-601 loop unrolling.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-integration-suite))

(defun %fr601-counted-loop (&key (limit-constant-p t) (limit-value 3))
  "Build a small counted loop in the optimizer's canonical loop shape."
  (append (list (make-vm-const :dst :i :value 0))
          (when limit-constant-p
            (list (make-vm-const :dst :limit :value limit-value)))
          (list (make-vm-const :dst :one :value 1)
                (make-vm-label :name "loop")
                (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                (make-vm-jump-zero :reg :cond :label "exit")
                (make-vm-print :reg :i)
                (make-vm-add :dst :i :lhs :i :rhs :one)
                (make-vm-jump :label "loop")
                (make-vm-label :name "exit")
                (make-vm-ret :reg :i))))

