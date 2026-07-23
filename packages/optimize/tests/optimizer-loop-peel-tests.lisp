;;;; Unit tests for FR-682 loop peeling.

(in-package :cl-cc/test)

(it-sequential "fr-682-loop-peel-peels-array-boundary-first-iteration"
  (let* ((aref (cl-cc:make-vm-aref :dst :elt :array-reg :arr :index-reg :i))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      aref
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :elt)))
         (out (cl-cc/optimize::opt-pass-loop-peel insts))
         (loop-pos (%test-label-position out "loop")))
    (expect loop-pos :to-be-truthy)
    (expect (> loop-pos 3) :to-be-truthy)
    (expect (typep (nth (- loop-pos 3) out) 'cl-cc/vm::vm-lt) :to-be-truthy)
    (expect (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-aref)
                              (cl-cc/optimize::opt-bounds-check-eliminable-marked-p inst)))
                       out) :to-be-truthy)))

(it-sequential "fr-682-loop-peel-skips-call-bearing-loop"
  (let* ((call (make-vm-call :dst :tmp :func :fn :args (list :i)))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      call
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :tmp)))
         (out (cl-cc/optimize::opt-pass-loop-peel insts)))
    (expect (mapcar #'cl-cc/vm::instruction->sexp out) :to-equal (mapcar #'cl-cc/vm::instruction->sexp insts))))
