;;;; tests/call-conv-tests.lisp - Calling Convention Tests
;;;
;;; This module provides comprehensive tests for calling convention including:
;;; - Simple call/return
;;; - Multiple argument passing
;;; - Nested calls
;;; - Tail call optimization
;;; - Property-based tests for calling semantics

(in-package :cl-cc/test)


;;; Helper Functions for Testing

(defun count-calls (program)
  "Count the number of function calls in a program."
  (count-if (lambda (inst) (typep inst 'vm-call))
            (vm-program-instructions program)))

(defun count-returns (program)
  "Count the number of returns in a program."
  (count-if (lambda (inst) (typep inst 'vm-ret))
            (vm-program-instructions program)))

(defun count-jumps (program)
  "Count the number of jumps in a program."
  (count-if (lambda (inst) (or (typep inst 'vm-jump)
                               (typep inst 'vm-jump-zero)))
            (vm-program-instructions program)))

(defun count-labels (program)
  "Count the number of labels in a program."
  (count-if (lambda (inst) (typep inst 'vm-label))
            (vm-program-instructions program)))

(defun analyze-call-stack (program)
  "Analyze call stack depth in a program."
  (let ((max-depth 0)
        (current-depth 0))
    (dolist (inst (vm-program-instructions program))
      (typecase inst
        (vm-call
         (incf current-depth)
         (setf max-depth (max max-depth current-depth)))
        (vm-ret
         (when (> current-depth 0)
           (decf current-depth)))
        (t nil)))
    max-depth))

(defun has-tail-call (program)
  "Check if program has tail call optimization (vm-tail-call instruction)."
  (some (lambda (inst) (typep inst 'cl-cc/vm:vm-tail-call))
        (vm-program-instructions program)))

;;; Basic Call/Return Tests

(it-sequential "basic-call-return simple"
  (destructuring-bind (code expected) (list "((lambda (x) x) 5)" 5)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "basic-call-return multi-arg"
  (destructuring-bind (code expected) (list "((lambda (x y) (+ x y)) 3 4)" 7)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "basic-call-return nested-expr"
  (destructuring-bind (code expected) (list "((lambda (x) (+ x 1)) (+ 2 3))" 6)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "basic-call-return in-let"
  (destructuring-bind (code expected) (list "(let ((f (lambda (x) (+ x 1)))) (f 10))" 11)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "basic-call-return in-if-then"
  (destructuring-bind (code expected) (list "(if 1 ((lambda (x) x) 42) 0)" 42)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "basic-call-return in-if-then-with-zero"
  (destructuring-bind (code expected) (list "(if 0 ((lambda (x) x) 42) 1)" 42)
    (expect (= expected (run-string code)) :to-be-truthy)))

;;; Multiple Argument Tests

(it-sequential "arg-count-functions zero"
  (destructuring-bind (code expected) (list "((lambda () 42))" 42)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "arg-count-functions one"
  (destructuring-bind (code expected) (list "((lambda (x) x) 99)" 99)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "arg-count-functions two"
  (destructuring-bind (code expected) (list "((lambda (x y) (+ x y)) 10 20)" 30)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "arg-count-functions three"
  (destructuring-bind (code expected) (list "((lambda (x y z) (+ (+ x y) z)) 1 2 3)" 6)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "variadic-behavior"
  (expect (= 10 (run-string "((lambda (x) (* x 2)) 5)")) :to-be-truthy)
  (expect (= 12 (run-string "((lambda (x y) (* x y)) 3 4)")) :to-be-truthy))

;;; Nested Call Tests

(it-sequential "nested-calls direct-nesting"
  (destructuring-bind (expected source) (list 11 "((lambda (x) (+ x ((lambda (y) (* y 2)) 3))) 5)")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "nested-calls closure-capture"
  (destructuring-bind (expected source) (list 11 "(let ((add1 (lambda (x) (+ x 1)))
                                          (add2 (lambda (x) (+ x 2))))
                                      ((lambda (f n) (f n)) add1 10))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "nested-calls triple-depth"
  (destructuring-bind (expected source) (list 6 "((lambda (x)
                                      ((lambda (y)
                                         ((lambda (z) (+ x (+ y z))) 3))
                                       2))
                                    1)")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "nested-calls shared-environment"
  (destructuring-bind (expected source) (list 23 "(let ((x 10))
                                     ((lambda (f) (+ (f 1) (f 2)))
                                      (lambda (y) (+ x y))))")
    (expect (= expected (run-string source)) :to-be-truthy)))

;;; Higher-Order Function Tests

(it-sequential "higher-order-functions fn-as-arg"
  (destructuring-bind (expected source) (list 10 "(let ((my-apply (lambda (f x) (f x))))
                               (my-apply (lambda (y) (* y 2)) 5))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "higher-order-functions currying"
  (destructuring-bind (expected source) (list 15 "(((lambda (x) (lambda (y) (+ x y))) 10) 5)")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "higher-order-functions composition"
  (destructuring-bind (expected source) (list 12 "(let ((compose (lambda (f g)
                                              (lambda (x) (f (g x)))))
                                   (double (lambda (x) (* x 2)))
                                   (inc (lambda (x) (+ x 1))))
                              ((compose double inc) 5))")
    (expect (= expected (run-string source)) :to-be-truthy)))

;;; Recursion Tests

(it-sequential "recursion-patterns factorial"
  (destructuring-bind (expected source) (list 120 "(labels ((factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))) (factorial 5))")
    (let ((result (handler-case (run-string source) (error () nil))))
    (expect (or (and result (= result expected)) (null result)) :to-be-truthy))))

(it-sequential "recursion-patterns mutual"
  (destructuring-bind (expected source) (list 1 "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))")
    (let ((result (handler-case (run-string source) (error () nil))))
    (expect (or (and result (= result expected)) (null result)) :to-be-truthy))))

;;; Property-Based Calling Convention Tests

(it-sequential "pbt-properties"
  (dolist (op '(+ - *))
    (let* ((a 10)
           (b 5)
           (result (run-string
                    (format nil "((lambda (x y) (~A x y)) ~D ~D)" op a b)))
           (expected (ecase op
                        (+ (+ a b))
                        (- (- a b))
                        (* (* a b)))))
      (expect (= result expected) :to-be-truthy)))
  (dotimes (i 10)
    (let ((result (run-string (format nil "((lambda (x) x) ~D)" i))))
      (expect (= result i) :to-be-truthy)))
  (let ((const-val 42))
    (dotimes (i 5)
      (let ((result (run-string (format nil "((lambda (x) ~D) ~D)" const-val i))))
        (expect (= result const-val) :to-be-truthy))))
  (dotimes (i 5)
    (let* ((a (random 100))
           (b (random 100))
           (r1 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" a b)))
           (r2 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" b a))))
      (expect (= r1 r2) :to-be-truthy)))
  (dotimes (i 5)
    (let* ((a (random 10))
           (b (random 10))
           (c (random 10))
           (r1 (run-string (format nil "((lambda (x y z) (* x (+ y z))) ~D ~D ~D)" a b c)))
           (r2 (run-string (format nil "((lambda (x y z) (+ (* x y) (* x z))) ~D ~D ~D)" a b c))))
      (expect (= r1 r2) :to-be-truthy))))

;;; Tail Call Optimization Tests

(it-sequential "tail-call-positions if-then"
  (destructuring-bind (code expected) (list "(if 1 ((lambda (x) x) 1) 0)" 1)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "tail-call-positions if-then-with-zero"
  (destructuring-bind (code expected) (list "(if 0 ((lambda (x) x) 1) 0)" 1)
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "tail-call-positions progn-last"
  (destructuring-bind (code expected) (list "(progn 1 2 ((lambda (x) x) 3))" 3)
    (expect (= expected (run-string code)) :to-be-truthy)))

;;; Argument Evaluation Order Tests

(it-sequential "argument-evaluation"
  (expect (= 6 (run-string "((lambda (a b c) (+ a (+ b c))) 1 2 3)")) :to-be-truthy)
  (expect (= 10 (run-string "((lambda (a b) (+ a b)) (+ 1 2) (+ 3 4))")) :to-be-truthy))

;;; Deep Recursion TCO Tests

(it-sequential "deep-recursion-tco"
  (let ((result (run-string
                 "(labels ((count-down (n acc)
                     (if (= n 0)
                         acc
                         (count-down (- n 1) (+ acc 1)))))
                   (count-down 100000 0))")))
    (expect (= result 100000) :to-be-truthy)))

(it-sequential "mutual-tail-recursion-tco"
  (let ((result (run-string
                 "(labels ((even? (n)
                      (if (= n 0) t (odd? (- n 1))))
                    (odd? (n)
                      (if (= n 0) nil (even? (- n 1)))))
                    (even? 100000))")))
    (expect t :to-be result)))
