;;;; tests/pbt/vm-pbt-tests.lisp - Property-Based Tests for VM
;;;
;;; This module provides property-based tests for VM operations.

(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

;; ----------------------------------------------------------------------------
;; VM Instruction Roundtrip Properties
;; ----------------------------------------------------------------------------

(defproperty vm-const-roundtrip
    (val (gen-integer :min -1000 :max 1000))
  (let* ((original (make-vm-const :dst :r0 :value val))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (and (typep restored 'vm-const)
         (eq (vm-dst restored) :r0)
         (= (vm-value restored) val))))

(defproperty vm-move-roundtrip
    (dummy (gen-integer :min 0 :max 0))
  (let* ((ignored-dummy dummy)
         (original (make-vm-move :dst :r0 :src :r1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (declare (ignore ignored-dummy))
    (and (typep restored 'vm-move)
         (eq (vm-dst restored) :r0)
         (eq (vm-src restored) :r1))))

(defproperty vm-add-roundtrip
    (dummy (gen-integer :min 0 :max 0))
  (let* ((ignored-dummy dummy)
         (original (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (declare (ignore ignored-dummy))
    (and (typep restored 'vm-add)
         (eq (vm-dst restored) :r0)
         (eq (vm-lhs restored) :r1)
         (eq (vm-rhs restored) :r2))))

;; ----------------------------------------------------------------------------
;; VM Execution Properties
;; ----------------------------------------------------------------------------

(defproperty vm-const-sets-register
    (val (gen-integer :min -1000 :max 1000))
  (let* ((state (make-instance 'vm-io-state))
         (inst (make-vm-const :dst :r0 :value val)))
    (execute-instruction inst state 0 (make-hash-table))
    (= (vm-reg-get state :r0) val)))

(defproperty vm-move-copies-value
    (val (gen-integer :min -1000 :max 1000))
  (let* ((state (make-instance 'vm-io-state))
         (const-inst (make-vm-const :dst :r0 :value val))
         (move-inst (make-vm-move :dst :r1 :src :r0)))
    (execute-instruction const-inst state 0 (make-hash-table))
    (execute-instruction move-inst state 1 (make-hash-table))
    (= (vm-reg-get state :r1) val)))

(defproperty vm-add-computes-sum
    (a (gen-integer :min -500 :max 500)
     b (gen-integer :min -500 :max 500))
  (let* ((state (make-instance 'vm-io-state))
         (inst-a (make-vm-const :dst :r0 :value a))
         (inst-b (make-vm-const :dst :r1 :value b))
         (inst-add (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (execute-instruction inst-a state 0 (make-hash-table))
    (execute-instruction inst-b state 1 (make-hash-table))
    (execute-instruction inst-add state 2 (make-hash-table))
    (= (vm-reg-get state :r2) (+ a b))))

(defproperty vm-sub-computes-difference
    (a (gen-integer :min -500 :max 500)
     b (gen-integer :min -500 :max 500))
  (let* ((state (make-instance 'vm-io-state))
         (inst-a (make-vm-const :dst :r0 :value a))
         (inst-b (make-vm-const :dst :r1 :value b))
         (inst-sub (make-vm-sub :dst :r2 :lhs :r0 :rhs :r1)))
    (execute-instruction inst-a state 0 (make-hash-table))
    (execute-instruction inst-b state 1 (make-hash-table))
    (execute-instruction inst-sub state 2 (make-hash-table))
    (= (vm-reg-get state :r2) (- a b))))

(defproperty vm-mul-computes-product
    (a (gen-integer :min -100 :max 100)
     b (gen-integer :min -100 :max 100))
  (let* ((state (make-instance 'vm-io-state))
         (inst-a (make-vm-const :dst :r0 :value a))
         (inst-b (make-vm-const :dst :r1 :value b))
         (inst-mul (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1)))
    (execute-instruction inst-a state 0 (make-hash-table))
    (execute-instruction inst-b state 1 (make-hash-table))
    (execute-instruction inst-mul state 2 (make-hash-table))
    (= (vm-reg-get state :r2) (* a b))))

;; ----------------------------------------------------------------------------
;; End-to-End Compile+Run Semantic Equivalence
;; ----------------------------------------------------------------------------
;; The properties above check that individual VM instructions compute the right
;; value in isolation. Nothing generates a whole source expression and drives it
;; through the real reader -> AST -> lowering -> codegen -> VM pipeline via
;; RUN-STRING to confirm the produced value matches a reference evaluation --
;; the "compiling and running an expression is equivalent to interpreting it"
;; property. These close that gap for integer arithmetic.
;;
;; Operand range [-9,9] and depth 3 keep every generated value a fixnum, so
;; CL-CC's result must be EQUAL to host CL's own EVAL of the same form: a true
;; invariant, not a statistical one (no bignum/overflow divergence to make it
;; flaky). The `compiler-robustness' fuzz test only checks that random programs
;; don't crash and terminate; it never checks the value is correct, which is
;; exactly what these add.

(defun %gen-arith-sexp (depth)
  "Build a random +/-/* expression over integer literals in [-9,9].
DEPTH bounds nesting; a fresh leaf is emitted at depth 0 or randomly earlier."
  (if (or (<= depth 0) (zerop (random 3 (%pbt-rng))))
      (- (random 19 (%pbt-rng)) 9)
      (list (nth (random 3 (%pbt-rng)) '(+ - *))
            (%gen-arith-sexp (1- depth))
            (%gen-arith-sexp (1- depth)))))

(defun gen-arith-sexp (&key (max-depth 3))
  "Generator of bounded integer +/-/* s-expressions, fixnum-safe by construction."
  (make-generator (lambda () (%gen-arith-sexp max-depth))))

(defproperty compile-run-matches-reference-arith
    (expr (gen-arith-sexp :max-depth 3))
  ;; Full pipeline result must equal the host reference evaluation.
  (equal (run-string (write-to-string expr))
         (eval expr)))

(defproperty compile-run-add-identity-preserved
    (expr (gen-arith-sexp :max-depth 2))
  ;; Metamorphic: the (+ e 0) algebraic identity must survive the whole
  ;; compile+optimize+run pipeline, exercising algebraic simplification on
  ;; random subexpressions rather than fixed examples.
  (equal (run-string (write-to-string (list '+ expr 0)))
         (run-string (write-to-string expr))))
