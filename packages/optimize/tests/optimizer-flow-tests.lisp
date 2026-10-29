;;;; tests/unit/optimize/optimizer-flow-tests.lisp
;;;; Unit tests for optimizer-flow-core.lisp — DCE, jump threading, loop transforms
;;;;
;;;; Covers: opt-pass-dce, opt-build-label-index, opt-thread-label,
;;;;   opt-pass-jump, %opt-rewrite-block-terminator, opt-pass-unreachable,
;;;;   loop-rotation, loop-peeling, loop-unrolling, code-sinking,
;;;;   opt-pass-dominated-type-check-elim, opt-passes-preserve-straight-line.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-flow-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-flow-tests-suite))

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %make-test-basic-block (&key (successors nil) (predecessors nil)
                                     (loop-depth 0) (rpo-index 0)
                                     (instructions nil))
  "Build a minimal basic-block for testing."
  (cl-cc/optimize::make-basic-block
   :successors successors
   :predecessors predecessors
   :loop-depth loop-depth
    :rpo-index rpo-index
    :instructions instructions))

(defun %test-label-position (instructions label-name)
  "Return LABEL-NAME's position in INSTRUCTIONS, or NIL."
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm:vm-label)
                      (equal (cl-cc/vm:vm-name inst) label-name)))
               instructions))

(defun %test-jump-zero-position (instructions target-label)
  "Return the position of a vm-jump-zero targeting TARGET-LABEL, or NIL."
  (position-if (lambda (inst)
                  (and (typep inst 'cl-cc/vm:vm-jump-zero)
                       (equal (cl-cc/vm:vm-label-name inst) target-label)))
                instructions))

(defun %make-slp-array-map (&key (op :add))
  "Build a four-lane straight-line scalar array map for SLP tests."
  (append
   (loop for i below 4
         collect (make-vm-const :dst (intern (format nil "I~D" i) :keyword) :value i))
   (loop for i below 4
         for idx = (intern (format nil "I~D" i) :keyword)
         for lhs = (intern (format nil "A~D" i) :keyword)
         for rhs = (intern (format nil "B~D" i) :keyword)
         for val = (intern (format nil "V~D" i) :keyword)
         append (list (cl-cc/vm:make-vm-aref :dst lhs :array-reg :array-a :index-reg idx)
                      (cl-cc/vm:make-vm-aref :dst rhs :array-reg :array-b :index-reg idx)
                      (ecase op
                        (:add (make-vm-add :dst val :lhs lhs :rhs rhs))
                        (:logxor (cl-cc/vm:make-vm-logxor :dst val :lhs lhs :rhs rhs)))
                      (cl-cc/vm:make-vm-aset :array-reg :array-c :index-reg idx :val-reg val)))))

(defun %make-counted-loop-program (&key limit (include-limit-p t))
  "Build the canonical counted loop shape used by loop-unrolling tests."
  (append
   (list (make-vm-const :dst :i :value 0))
   (when include-limit-p
     (list (make-vm-const :dst :lim :value limit)))
   (list (make-vm-const :dst :one :value 1)
         (make-vm-label :name "Lh")
         (make-vm-lt :dst :c :lhs :i :rhs :lim)
         (make-vm-jump-zero :reg :c :label "Lexit")
         (make-vm-add :dst :sum :lhs :sum :rhs :i)
         (make-vm-add :dst :i :lhs :i :rhs :one)
         (make-vm-jump :label "Lh")
         (make-vm-label :name "Lexit")
         (make-vm-ret :reg :sum))))

(defun %count-vm-jumps-to-label (instructions label-name)
  (count-if (lambda (inst)
              (and (typep inst 'cl-cc/vm:vm-jump)
                   (equal (cl-cc/vm:vm-label-name inst) label-name)))
            instructions))

(defun %count-vm-instructions-of-type (instructions type)
  (count-if (lambda (inst) (typep inst type)) instructions))

(defun %count-index-steps (instructions)
  (count-if (lambda (inst)
              (and (typep inst 'cl-cc/vm:vm-add)
                   (eq (cl-cc/vm:vm-dst inst) :i)))
            instructions))

;;; ─── opt-pass-dce / opt-build-label-index / opt-thread-label ────────────

;;; ─── opt-pass-jump / %opt-rewrite-block-terminator / opt-pass-unreachable ─

;;; ─── opt-pass-hot-cold-layout ─────────────────────────────────────────────

