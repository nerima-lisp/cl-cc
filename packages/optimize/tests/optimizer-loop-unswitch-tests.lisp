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

(it-sequential "loop-unswitch-fr602-hoists-invariant-condition"
  (let* ((out (cl-cc/optimize::opt-pass-loop-unswitch (%fr602-unswitch-loop))))
    (expect (%test-label-position out "loop_unsw_true") :to-be-truthy)
    (expect (%test-label-position out "loop_unsw_false") :to-be-truthy)
    (expect (%test-label-position out "exit") :to-be-truthy)
    (expect (%test-label-position out "loop") :to-be-falsy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-jump-zero)
                              (eq (cl-cc/vm::vm-reg inst) :flag)
                              (equal (cl-cc/vm::vm-label-name inst) "loop_unsw_false")))
                       out) :to-be-truthy)
    (expect (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-jump-zero)
                               (equal (cl-cc/vm::vm-label-name inst) "else")))
                        out) :to-be-falsy)))

(it-sequential "loop-unswitch-fr602-skips-side-effecting-condition"
  (let* ((insts (%fr602-unswitch-loop :condition-side-effect-p t))
         (out (cl-cc/optimize::opt-pass-loop-unswitch insts)))
    (expect (mapcar #'cl-cc/vm::instruction->sexp out) :to-equal (mapcar #'cl-cc/vm::instruction->sexp insts))))
