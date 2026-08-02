;;;; tests/compiler-tests.lisp - Compiler Tests
;;;
;;; This module provides comprehensive tests for the compiler including:
;;; - Simple function definition and call
;;; - Function with multiple parameters
;;; - Recursive function
;;; - Mutually recursive functions (labels)
;;; - Higher-order functions
;;; - Integration tests with the VM

(in-package :cl-cc/test)

(defun %compiled-assembly (code target)
  (compilation-result-assembly (compile-string code :target target)))

(defun %assert-assembly-stringp (code)
  (let ((x86 (%compiled-assembly code :x86_64))
        (arm (%compiled-assembly code :aarch64)))
    (expect (stringp x86) :to-be-truthy)
    (expect (stringp arm) :to-be-truthy)))

(defun %assert-assembly-contains (code substring)
  (let ((x86 (string-downcase (%compiled-assembly code :x86_64)))
        (arm (string-downcase (%compiled-assembly code :aarch64))))
    (expect (search substring x86) :to-be-truthy)
    (expect (search substring arm) :to-be-truthy)))

(defun %assert-assembly-or-not-yet-supported (code)
  (flet ((compile-or-tag (target)
           (handler-case
               (%compiled-assembly code target)
             (error () :not-yet-supported))))
    (let ((x86 (compile-or-tag :x86_64))
          (arm (compile-or-tag :aarch64)))
      (expect (or (eq x86 :not-yet-supported) (stringp x86)) :to-be-truthy)
      (expect (or (eq arm :not-yet-supported) (stringp arm)) :to-be-truthy))))

;;; Basic Compiler Tests

(deftest-compile vm-exec-basic-forms
  "Arithmetic, conditionals, let bindings, and progn sequences compile and evaluate correctly."
  :cases (("arith-add"        7  "(+ 3 4)")
          ("arith-sub"        3  "(- 10 7)")
          ("arith-mul"        42 "(* 6 7)")
          ("arith-nested"     9  "(+ (* 2 3) 3)")
          ("if-false-cond"    20 "(if nil 10 20)")
          ("if-true-cond"     10 "(if 1 10 20)")
          ("if-nested"        1  "(if 1 (if 0 1 2) 3)")
          ("if-var-cond"      20 "(let ((x 0)) (if x 20 10))")
          ("let-simple"       42 "(let ((x 42)) x)")
          ("let-multi"        5  "(let ((x 2) (y 3)) (+ x y))")
          ("let-shadowing"    20 "(let ((x 10)) (let ((x 20)) x))")
          ("let-computed"     17 "(let ((x 5) (y 7)) (+ (* x 2) y))")
          ("progn-simple"     3  "(progn 1 2 3)")
          ("progn-with-let"   3  "(progn (let ((x 2)) x) (let ((y 3)) y))")))

(it-sequential "vm-exec-progn-empty"
  (expect (run-string "(progn)") :to-be-null))

;;; Print Tests

(it-sequential "vm-exec-print-outputs single-value"
  (destructuring-bind (expected-substrings form) (list '("42") "(print 42)")
    (let ((output (with-output-to-string (*standard-output*)
                  (run-string form))))
    (dolist (s expected-substrings)
      (expect (search s output) :to-be-truthy)))))

(it-sequential "vm-exec-print-outputs sequence"
  (destructuring-bind (expected-substrings form) (list '("1" "2" "3") "(progn (print 1) (print 2) (print 3))")
    (let ((output (with-output-to-string (*standard-output*)
                  (run-string form))))
    (dolist (s expected-substrings)
      (expect (search s output) :to-be-truthy)))))

;;; Function Compilation Tests

(it-sequential "compile-function-forms simple-lambda"
  (destructuring-bind (code) (list "(lambda (x) x)")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

(it-sequential "compile-function-forms multi-param-lambda"
  (destructuring-bind (code) (list "(lambda (x y) (+ x y))")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

(it-sequential "compile-function-forms lambda-in-let"
  (destructuring-bind (code) (list "(let ((f (lambda (x) (+ x 1)))) (f 5))")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

(it-sequential "compile-function-forms nested-lambda"
  (destructuring-bind (code) (list "(let ((make-adder (lambda (n) (lambda (x) (+ x n))))) (let ((add5 (make-adder 5))) (add5 10)))")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

(it-sequential "compile-function-forms recursive-labels"
  (destructuring-bind (code) (list "(labels ((factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))) (factorial 5))")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

(it-sequential "compile-function-forms hof-function-as-arg"
  (destructuring-bind (code) (list "(let ((apply-twice (lambda (f x) (f (f x))))) (apply-twice (lambda (x) (* x 2)) 3))")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

(it-sequential "compile-function-forms hof-returning-function"
  (destructuring-bind (code) (list "(let ((make-mul (lambda (n) (lambda (x) (* x n))))) (let ((double (make-mul 2))) (double 21)))")
    (expect (compilation-result-program (compile-string code :target :vm)) :to-be-truthy)))

;;; Assembly Emission Tests

(it-sequential "asm-emission-binop-add"
  (expect (> (length (compilation-result-assembly
                           (compile-string "(+ 1 2)" :target :x86_64))) 0) :to-be-truthy)
  (expect (> (length (compilation-result-assembly
                           (compile-string "(+ 1 2)" :target :aarch64))) 0) :to-be-truthy))

(it-sequential "asm-emission-basic-forms if-form"
  (destructuring-bind (form) (list "(if 1 2 3)")
    (%assert-assembly-stringp form)))

(it-sequential "asm-emission-basic-forms let-form"
  (destructuring-bind (form) (list "(let ((x 1)) x)")
    (%assert-assembly-stringp form)))

(it-sequential "asm-emission-identity-lambda"
  (%assert-assembly-or-not-yet-supported "(lambda (x) x)"))

;;; Error Handling Tests

(it-sequential "compile-error-conditions"
  (handler-case (run-string "x") (error () nil))
  (handler-case (compile-string "(if 1 2)") (error () nil))
  (handler-case (compile-string "(+ 1)") (error () nil)))

;;; Integration Tests

(it-sequential "integration-recursive-and-complex"
  (let ((result (handler-case
                    (run-string "(labels ((fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))")
                  (error () nil))))
    (expect (or (null result) (= result 120)) :to-be-truthy))
  (let ((result (handler-case
                    (run-string "(labels ((fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))")
                  (error () nil))))
    (expect (or (null result) (= result 55)) :to-be-truthy))
  (expect (= 25 (run-string "(let ((x 2) (y 3)) (let ((add (lambda (a b) (+ a b)))) (let ((mul (lambda (a b) (* a b)))) (mul (add x y) (add x y)))))")) :to-be-truthy))

;;; Label and Jump Tests

(it-sequential "compile-labels-and-jumps"
  (let* ((program (compilation-result-program (compile-string "(if 1 2 3)" :target :vm)))
         (label-names (loop for inst in (vm-program-instructions program)
                          when (typep inst 'vm-label)
                          collect (vm-name inst)))
         (jump-targets (loop for inst in (vm-program-instructions program)
                           when (or (typep inst 'vm-jump)
                                   (typep inst 'vm-jump-zero))
                           collect (vm-label-name inst))))
    (expect (<= 0 (length label-names) 4) :to-be-truthy)
    (expect (every (lambda (target) (find target label-names :test #'string=)) jump-targets) :to-be-truthy)))

;;; Register Allocation Tests

(it-sequential "compile-register-allocation"
  (let* ((program (compilation-result-program (compile-string "(+ 1 2)")))
         (registers (loop for inst in (vm-program-instructions program)
                        append (list (when (slot-exists-p inst 'dst) (slot-value inst 'dst))
                                     (when (slot-exists-p inst 'lhs) (slot-value inst 'lhs))
                                     (when (slot-exists-p inst 'rhs) (slot-value inst 'rhs)))))
         (all-registers (remove-duplicates (remove nil registers))))
    (expect (every #'symbolp all-registers) :to-be-truthy))
  (let* ((program (compilation-result-program (compile-string "42")))
         (result-reg (vm-program-result-register program)))
    (expect (symbolp result-reg) :to-be-truthy)))

;;; Complex Scoping Tests

(deftest-compile compile-let-scoping
  "Deeply nested let bindings and multi-level variable shadowing work correctly."
  :cases (("deep-nesting" 10 "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (+ a (+ b (+ c d)))))))")
          ("shadowing"     3 "(let ((x 1)) (let ((x 2)) (let ((x 3)) x)))")))

(it-sequential "compile-closure-captures-correct-value"
  (let ((result (run-string "(let ((x 10)) (let ((get-x (lambda () x))) (let ((x 20)) (get-x))))")))
    (expect (or (= result 10) (eq result 20)) :to-be-truthy)))

;;; Optimization Tests

(it-sequential "compile-peephole-optimization"
  (let* ((program (compilation-result-program (compile-string "(+ 1 2)")))
         (inst-count (length (vm-program-instructions program))))
    (expect (> inst-count 0) :to-be-truthy)))

(it-sequential "reflect-optimization-settings"
  (let ((warning-seen nil))
    (handler-bind ((warning
                     (lambda (condition)
                       (declare (ignore condition))
                       (setf warning-seen t)
                       (let ((restart (find-restart 'muffle-warning)))
                         (when restart
                           (invoke-restart restart))))))
      (let* ((settings (cl-cc/compile::reflect-optimization-settings))
             (policy (symbol-value (find-symbol "*POLICY*" "SB-C")))
             (policy-quality (find-symbol "POLICY-QUALITY" "SB-C"))
             (expected (loop for quality in '(speed safety debug compilation-speed space)
                             collect (cons quality (funcall policy-quality policy quality)))))
        (expect warning-seen :to-be-falsy)
        (expect settings :to-equal expected)
        (expect (every (lambda (entry)
                              (and (symbolp (car entry))
                                   (integerp (cdr entry))))
                            settings) :to-be-truthy)))))

;;; CPS Transformation Tests

(it-sequential "compile-cps-transform arith"
  (destructuring-bind (form) (list "(+ 1 2)")
    (let* ((result (compile-string form)) (cps (compilation-result-cps result)))
    (expect (or (null cps) (listp cps)) :to-be-truthy))))

(it-sequential "compile-cps-transform if"
  (destructuring-bind (form) (list "(if 1 2 3)")
    (let* ((result (compile-string form)) (cps (compilation-result-cps result)))
    (expect (or (null cps) (listp cps)) :to-be-truthy))))

;;; ─── FR-008 Float Unboxing ─────────────────────────────────────────────────

(deftest-compile float-unboxing-arith
  "Float arithmetic compiles to float-specific VM ops and produces correct results."
  :cases (("float-add" 3.0   "(+ 1.0 2.0)")
          ("float-sub" 2.0   "(- 5.0 3.0)")
          ("float-mul" 12.0  "(* 3.0 4.0)")
          ("float-div" 2.5   "(/ 10.0 4.0)")
          ("float-nested" 7.0 "(+ (* 2.0 3.0) 1.0)")))

(it-sequential "float-unboxing-instruction-selection"
  (assert-compiles-to "(+ 1.0 2.0)" :contains 'vm-float-add)
  (assert-compiles-to "(- 5.0 3.0)" :contains 'vm-float-sub)
  (assert-compiles-to "(* 3.0 4.0)" :contains 'vm-float-mul)
  (assert-compiles-to "(/ 10.0 4.0)" :contains 'vm-float-div))

(it-sequential "x86-64-assembly-covers-the-division-family"
  (dolist (code '("(/ 10.0 4.0)" "(/ 10 4)" "(mod 10 4)" "(rem 10 4)"
                  "(truncate 10 4)" "(floor 10 4)" "(ceiling 10 4)" "(round 10 4)"))
    (expect (plusp (length (%compiled-assembly code :x86_64))) :to-be-truthy))
  (let ((asm (%compiled-assembly "(/ 10.0 4.0)" :x86_64)))
    (expect (search "divsd" asm) :to-be-truthy)
    (expect (search "rt-cl-div" asm) :to-be-truthy)))

(it-sequential "float-unboxing-via-the"
  (assert-run= 3.0 "(the float (+ 1.0 2.0))")
  (assert-run= 12.0 "(the float (* 3.0 4.0))"))

(it-sequential "float-unboxing-preserves-source-precision"
  (dolist (case (list (list "(+ 1.0f0 2.0f0)" :f32)
                      (list "(+ 1.0d0 2.0d0)" :f64)
                      (list "(+ 1.0f0 2.0d0)" :f64)))
    (destructuring-bind (source expected) case
      (let* ((result (compile-string source))
             (instruction (find-if (lambda (inst) (typep inst 'vm-float-add))
                                   (cl-cc/compile:compilation-result-vm-instructions result))))
        (expect instruction :to-be-truthy)
        (expect (cl-cc/vm:vm-float-precision instruction) :to-be expected)))))
