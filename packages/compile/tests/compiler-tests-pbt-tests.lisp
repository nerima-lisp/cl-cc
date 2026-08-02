;;;; tests/integration/compiler-tests-pbt-tests.lisp — Compiler Property-Based Tests
;;;;
;;;; Continuation of compiler-tests.lisp.
;;;; Property-based tests for integer compilation, binary op commutativity,
;;;; if-branch semantics, and let-binding value preservation.

(in-package :cl-cc/test)

(it-sequential "pbt-integer-compilation"
  (let ((passes 0)
        (trials 100))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((val (random 1000))
             (result (handler-case (run-string (format nil "~D" val))
                       (error () nil))))
        (when (and result (= result val))
          (incf passes))))
    (expect (>= (/ passes trials) 0.90) :to-be-truthy)))

(it-sequential "pbt-binary-op-commutative addition"
  (destructuring-bind (op range) (list "+" 100)
    (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((a  (random range))
             (b  (random range))
             (r1 (handler-case (run-string (format nil "(~A ~D ~D)" op a b))
                   (error () nil)))
             (r2 (handler-case (run-string (format nil "(~A ~D ~D)" op b a))
                   (error () nil))))
        (when (and r1 r2 (= r1 r2))
          (incf passes))))
    (expect (>= (/ passes trials) 0.90) :to-be-truthy))))

(it-sequential "pbt-binary-op-commutative multiplication"
  (destructuring-bind (op range) (list "*" 50)
    (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((a  (random range))
             (b  (random range))
             (r1 (handler-case (run-string (format nil "(~A ~D ~D)" op a b))
                   (error () nil)))
             (r2 (handler-case (run-string (format nil "(~A ~D ~D)" op b a))
                   (error () nil))))
        (when (and r1 r2 (= r1 r2))
          (incf passes))))
    (expect (>= (/ passes trials) 0.90) :to-be-truthy))))

(it-sequential "pbt-if-uses-common-lisp-truthiness"
  (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((nil-condition-p (zerop (random 2)))
             (condition-source (if nil-condition-p "nil" "0"))
             (then-val (random 100))
             (else-val (random 100))
             (expected (if nil-condition-p else-val then-val))
             (result (handler-case
                         (run-string (format nil "(if ~A ~D ~D)" condition-source then-val else-val))
                       (error () nil))))
        (when (and result (= result expected))
          (incf passes))))
    (expect (>= (/ passes trials) 0.90) :to-be-truthy)))

(it-sequential "pbt-let-bindings-preserve-value"
  (let ((passes 0)
        (trials 50))
    (dotimes (_ trials)
      (declare (ignore _))
      (let* ((val (random 1000))
             (result (handler-case (run-string (format nil "(let ((x ~D)) x)" val))
                       (error () nil))))
        (when (and result (= result val))
          (incf passes))))
    (expect (>= (/ passes trials) 0.90) :to-be-truthy)))
