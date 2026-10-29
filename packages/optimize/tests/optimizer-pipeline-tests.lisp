;;;; tests/unit/optimize/optimizer-pipeline-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — pipeline mechanism
;;;;
;;;; Covers: opt-parse-pass-pipeline-string, opt-converged-p,
;;;;   opt-adaptive-max-iterations, opt-verify-instructions,
;;;;   opt-resolve-pass-pipeline, *opt-pass-registry* data, prolog-rewrite-stage.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-pipeline-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-pipeline-tests-suite))

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

;;; ─── opt-adaptive-max-iterations ─────────────────────────────────────────────

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

;;; ─── opt-delimited-continuations-form ───────────────────────────────────────

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

(defparameter *opt-pass-registry-presence-keys*
  '(:prolog-rewrite
    :egraph
    :fold
    :cons-slot-forward
    :pure-call-optimization
    :dce
    :cse))

(defparameter *opt-pass-registry-function-bindings*
  `((:if-conversion . ,#'cl-cc/optimize::opt-pass-if-conversion)
    (:fma-recognition . ,#'cl-cc/optimize::opt-pass-fma-recognition)))

(defparameter *opt-default-convergence-prefix*
  '(:prolog-rewrite
    :call-site-splitting
    :devirtualize
    :if-conversion
    :closure-capture-dedup
    :closure-thunk-sharing))

(defparameter *opt-default-convergence-positional-keys*
  '((7 . :inline)
    (8 . :overflow-check-elim)
    (9 . :sccp)
    (10 . :cons-slot-forward)))

(defparameter *opt-default-convergence-ordering-edges*
  '((:devirtualize :if-conversion)
    (:if-conversion :inline)
    (:fma-recognition :schedule-local)
    (:copy-prop :pure-call-optimization)
    (:pure-call-optimization :gvn)
    (:pure-call-optimization :cse)
    (:pure-call-optimization :dce)))

(defun %opt-default-convergence-key-position (key)
  (or (position key cl-cc/optimize::*opt-default-convergence-pass-keys*)
      (error "Missing optimizer convergence key: ~S" key)))

(defun %assert-opt-default-convergence-edge (before after)
  (expect (< (%opt-default-convergence-key-position before) (%opt-default-convergence-key-position after)) :to-be-truthy))

;;; ─── FR-099 FMA recognition ───────────────────────────────────────────────

(defun %test-count-type (type insts)
  (count-if (lambda (inst) (typep inst type)) insts))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

;;; ─── Prolog rewrite stage ──────────────────────────────────────────────────

;;; ─── Copy-on-Write helper layer (FR-253 partial) ───────────────────────────

;;; ─── Region/bump allocation helpers (FR-254 partial) ───────────────────────

;;; ─── Inline cache helper layer (FR-009 / FR-019 partial) ──────────────────

;;; ─── ThinLTO / Tiered JIT / Deopt helpers (FR-301/310/312 partial) ───────

(defparameter *opt-ic-patch-plan-cases*
  '((:site1 :uninitialized :monomorphic :t1 :install-monomorphic)
    (:site2 :monomorphic :polymorphic :t2 :promote-polymorphic)
    (:site3 :polymorphic :megamorphic :t3 :promote-megamorphic)))

(defun %assert-opt-ic-patch-plan-case (site current-state next-state target expected-kind)
  (expect (cl-cc/optimize::opt-ic-patch-patch-kind (cl-cc/optimize::opt-ic-make-patch-plan site current-state next-state target)) :to-be expected-kind))

(defun %opt-wasm-gc-struct-layout (&key (fields '((slot-a . i32)))
                                        (nullable-p t))
  (cl-cc/optimize:opt-build-wasm-gc-layout
   :kind :struct
   :fields fields
   :nullable-p nullable-p))

(defun %opt-wasm-gc-array-layout (&key (fields '(eqref))
                                       (nullable-p nil))
  (cl-cc/optimize:opt-build-wasm-gc-layout
   :kind :array
   :fields fields
   :nullable-p nullable-p))

(defun %opt-wasm-gc-bad-array-layout ()
  (%opt-wasm-gc-array-layout :fields '(eqref i32)))

(defparameter *opt-atomic-opcode-cases*
  '((:x86-64 :incf :acq-rel :lock-xadd)
    (:x86-64 :cas :seq-cst :lock-cmpxchg)
    (:aarch64 :incf :acq-rel :ldadd)
    (:aarch64 :cas :seq-cst :ldxr-stxr)))

(defun %assert-opt-atomic-opcode-case (target operation memory-order expected-opcode)
  (expect (cl-cc/optimize::opt-select-atomic-opcode :target target :operation operation :memory-order memory-order) :to-be expected-opcode))

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(defun %opt-canonical-loop-program (body-instructions)
  (append (list (make-vm-const :dst :ri :value 0)
                (make-vm-const :dst :rlim :value 8)
                (make-vm-const :dst :rstep :value 1)
                (make-vm-label :name :l0)
                (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                (make-vm-jump-zero :reg :rc :label :l1))
          body-instructions
          (list (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                (make-vm-jump :label :l0)
                (make-vm-label :name :l1))))

(defun %opt-instruction-sexps (instructions)
  (mapcar #'instruction->sexp instructions))

(defun %opt-assert-program-changed (optimized program)
  (expect (equal (%opt-instruction-sexps optimized) (%opt-instruction-sexps program)) :to-be-falsy))

(defun %opt-assert-program-unchanged (optimized program)
  (expect (%opt-instruction-sexps program) :to-equal (%opt-instruction-sexps optimized)))

(defun %opt-sortable-loop-body ()
  (list (make-vm-mul :dst :r7 :lhs :r3 :rhs :r4)
        (make-vm-move :dst :r8 :src :r5)))

(defun %opt-loop-label-count (label instructions)
  (count-if (lambda (inst)
              (and (typep inst 'vm-label)
                   (eq (cl-cc/vm:vm-name inst) label)))
            instructions))

(defun %opt-assert-sortable-loop-pass-reorders (pass)
  (let* ((program (%opt-canonical-loop-program (%opt-sortable-loop-body)))
         (optimized (funcall pass program)))
    (%opt-assert-program-changed optimized program)
    (expect (typep (nth 6 optimized) 'vm-move) :to-be-truthy)
    (expect (typep (nth 7 optimized) 'vm-mul) :to-be-truthy)))

(defun %opt-loop-split-marker-p (instruction)
  (and (typep instruction 'vm-label)
       (search "__SPLIT" (string (cl-cc/vm:vm-name instruction)))))

(defun %opt-adjacent-loop-program (&key (first-init 0)
                                        (second-init 0)
                                        (second-condition :rc))
  (list (make-vm-const :dst :ri :value first-init)
        (make-vm-const :dst :rj :value second-init)
        (make-vm-const :dst :rlim :value 4)
        (make-vm-const :dst :rstep :value 1)
        (make-vm-label :name :la)
        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
        (make-vm-jump-zero :reg :rc :label :lax)
        (make-vm-move :dst :r10 :src :r11)
        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
        (make-vm-jump :label :la)
        (make-vm-label :name :lax)
        (make-vm-label :name :lb)
        (make-vm-lt :dst second-condition :lhs :rj :rhs :rlim)
        (make-vm-jump-zero :reg second-condition :label :lbx)
        (make-vm-move :dst :r12 :src :r13)
        (make-vm-add :dst :rj :lhs :rj :rhs :rstep)
        (make-vm-jump :label :lb)
        (make-vm-label :name :lbx)))

(in-package :cl-cc/test)

