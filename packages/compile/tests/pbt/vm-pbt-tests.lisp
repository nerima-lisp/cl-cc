;;;; tests/pbt/vm-pbt-tests.lisp - Property-Based Tests for VM
;;;
;;; Property-based tests for VM instruction encoding and execution, expressed
;;; with cl-weave's NATIVE property API (cl-weave:describe + it-property +
;;; gen-integer/gen-member/gen-recursive) rather than the home-grown cl-cc/pbt
;;; defproperty DSL.
;;;
;;; IT-PROPERTY decides pass/fail from a *signaled* condition, not from the
;;; body's return value (see cl-weave's RUN-PROPERTY / PROPERTY-FAILURE-CONDITION),
;;; so every body below asserts through CL-WEAVE:EXPECT. A bare boolean body
;;; would report PASS even when the property is false.

(in-package :cl-cc/pbt)


(defun %vm-binop-result (make-instruction a b)
  "Load A and B into :R0/:R1, run MAKE-INSTRUCTION's opcode over them, return :R2."
  (let ((state (make-instance 'vm-io-state)))
    (execute-instruction (make-vm-const :dst :r0 :value a) state 0 (make-hash-table))
    (execute-instruction (make-vm-const :dst :r1 :value b) state 1 (make-hash-table))
    (execute-instruction (funcall make-instruction :dst :r2 :lhs :r0 :rhs :r1)
                         state 2 (make-hash-table))
    (vm-reg-get state :r2)))

(defun gen-arith-sexp (&key (max-depth 3))
  "Generator of bounded integer +/-/* s-expressions, fixnum-safe by construction.
Reimplemented on cl-weave's GEN-RECURSIVE: the base case is a literal in
[-9,9] and the recursive step is a binary (op lhs rhs) form, which GEN-TUPLE
already produces as a three-element list. MAX-DEPTH bounds nesting exactly as
the hand-rolled version's DEPTH argument did, and GEN-RECURSIVE takes the base
case with the same 1-in-3 probability the original's (zerop (random 3)) did."
  (cl-weave:gen-recursive
   (cl-weave:gen-integer :min -9 :max 9)
   (lambda (self)
     (cl-weave:gen-tuple (cl-weave:gen-member '(+ - *)) self self))
   :max-depth max-depth))

;; ----------------------------------------------------------------------------
;; VM Instruction Roundtrip Properties
;; ----------------------------------------------------------------------------

(cl-weave:describe "VM instruction sexp roundtrip properties"

  (cl-weave:it-property "vm-const-roundtrip"
      ((val (cl-weave:gen-integer :min -1000 :max 1000)))
    (let* ((original (make-vm-const :dst :r0 :value val))
           (restored (sexp->instruction (instruction->sexp original))))
      (cl-weave:expect restored :to-be-type-of 'vm-const)
      (cl-weave:expect (vm-dst restored) :to-be :r0)
      (cl-weave:expect (vm-value restored) :to-be val)))

  ;; vm-move / vm-add carry no generated operands: the originals bound a
  ;; throwaway (gen-integer :min 0 :max 0) purely to satisfy defproperty's
  ;; required binding list. They are example tests, so they use IT.
  (cl-weave:it "vm-move-roundtrip"
    (let* ((original (make-vm-move :dst :r0 :src :r1))
           (restored (sexp->instruction (instruction->sexp original))))
      (cl-weave:expect restored :to-be-type-of 'vm-move)
      (cl-weave:expect (vm-dst restored) :to-be :r0)
      (cl-weave:expect (vm-src restored) :to-be :r1)))

  (cl-weave:it "vm-add-roundtrip"
    (let* ((original (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
           (restored (sexp->instruction (instruction->sexp original))))
      (cl-weave:expect restored :to-be-type-of 'vm-add)
      (cl-weave:expect (vm-dst restored) :to-be :r0)
      (cl-weave:expect (vm-lhs restored) :to-be :r1)
      (cl-weave:expect (vm-rhs restored) :to-be :r2))))

;; ----------------------------------------------------------------------------
;; VM Execution Properties
;; ----------------------------------------------------------------------------

(cl-weave:describe "VM instruction execution properties"

  (cl-weave:it-property "vm-const-sets-register"
      ((val (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((state (make-instance 'vm-io-state)))
      (execute-instruction (make-vm-const :dst :r0 :value val)
                           state 0 (make-hash-table))
      (cl-weave:expect (vm-reg-get state :r0) :to-be val)))

  (cl-weave:it-property "vm-move-copies-value"
      ((val (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((state (make-instance 'vm-io-state)))
      (execute-instruction (make-vm-const :dst :r0 :value val)
                           state 0 (make-hash-table))
      (execute-instruction (make-vm-move :dst :r1 :src :r0)
                           state 1 (make-hash-table))
      (cl-weave:expect (vm-reg-get state :r1) :to-be val)))

  (cl-weave:it-property "vm-add-computes-sum"
      ((a (cl-weave:gen-integer :min -500 :max 500))
       (b (cl-weave:gen-integer :min -500 :max 500)))
    (cl-weave:expect (%vm-binop-result #'make-vm-add a b) :to-be (+ a b)))

  (cl-weave:it-property "vm-sub-computes-difference"
      ((a (cl-weave:gen-integer :min -500 :max 500))
       (b (cl-weave:gen-integer :min -500 :max 500)))
    (cl-weave:expect (%vm-binop-result #'make-vm-sub a b) :to-be (- a b)))

  (cl-weave:it-property "vm-mul-computes-product"
      ((a (cl-weave:gen-integer :min -100 :max 100))
       (b (cl-weave:gen-integer :min -100 :max 100)))
    (cl-weave:expect (%vm-binop-result #'make-vm-mul a b) :to-be (* a b))))

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
;; flaky).

(cl-weave:describe "compile+run semantic equivalence properties"

  (cl-weave:it-property "compile-run-matches-reference-arith"
      ((expr (gen-arith-sexp :max-depth 3)))
    ;; Full pipeline result must equal the host reference evaluation.
    (cl-weave:expect (run-string (write-to-string expr))
                     :to-equal (eval expr)))

  (cl-weave:it-property "tier-0-and-tier-1-match-reference-arith"
      ((expr (gen-arith-sexp :max-depth 3)))
    (labels ((condition-category (condition)
               (cond
                 ((typep condition (quote undefined-function)) :undefined-function)
                 ((or (typep condition (quote arithmetic-error))
                      (typep condition (quote type-error)))
                  :arithmetic-type-error)
                 ((let ((name (string-upcase (princ-to-string (type-of condition)))))
                    (or (search "VM-" name) (search "TRAP" name)))
                  :vm-trap)
                 (t :other-error)))
             (capture (thunk)
               (handler-case
                   (list :value (funcall thunk))
                 (error (condition)
                   (list :condition (condition-category condition)))))
             (run-tier (tier)
               (let* ((result (cl-cc:compile-string
                               (write-to-string expr)
                               :target :vm
                               :compilation-tier tier))
                      (program (cl-cc/compile:compilation-result-program result)))
                 (cl-cc:run-compiled program))))
      (let ((tier-0 (capture (lambda () (run-tier 0))))
            (tier-1 (capture (lambda () (run-tier 1))))
            (reference (capture (lambda () (eval expr)))))
        (cl-weave:expect tier-0 :to-equal reference)
        (cl-weave:expect tier-1 :to-equal reference))))

  (cl-weave:it-property "compile-run-add-identity-preserved"
      ((expr (gen-arith-sexp :max-depth 2)))
    ;; Metamorphic: the (+ e 0) algebraic identity must survive the whole
    ;; compile+optimize+run pipeline, exercising algebraic simplification on
    ;; random subexpressions rather than fixed examples.
    (cl-weave:expect (run-string (write-to-string (list (quote +) expr 0)))
                     :to-equal (run-string (write-to-string expr)))))
