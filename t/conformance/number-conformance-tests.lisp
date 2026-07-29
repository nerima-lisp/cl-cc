;;;; tests/conformance/number-conformance-tests.lisp
;;;; ANSI CL Number Tower Conformance Tests
;;;;
;;;; Tests numeric operations that should work per ANSI CL. These run as
;;;; regular conformance tests; native x86-64 bignum lowering remains tracked
;;;; separately from the VM/runtime arithmetic helpers.

(in-package :cl-cc/test)



;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun num-run (form-string)
  "Run a numeric FORM-STRING through cl-cc and return the result."
  (cl-cc:run-string form-string))

;;; ──────────────────────────────────────────────────────────────────────
;;; Bignum Operations (fixnum overflow → bignum promotion)
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-bignum-add-overflow"
  (let ((result (num-run "(+ most-positive-fixnum 1)")))
    (expect (> result most-positive-fixnum) :to-be-truthy)))

(it-sequential "num-bignum-mul-overflow"
  (let ((result (num-run "(* most-positive-fixnum 2)")))
    (expect (> result most-positive-fixnum) :to-be-truthy)))

(it-sequential "num-bignum-sub-overflow"
  (let ((result (num-run "(- most-negative-fixnum 1)")))
    (expect (< result most-negative-fixnum) :to-be-truthy)))

(it-sequential "num-bignum-expt-large"
  (let ((result (num-run "(expt 2 100)")))
    (expect (> result 0) :to-be-truthy)
    (expect (= 1267650600228229401496703205376 result) :to-be-truthy)))

(it-sequential "num-bignum-gcd-large"
  (let ((result (num-run "(gcd 1234567890123456789 9876543210987654321)")))
    (expect (= 90000000009 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Ratio Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-ratio-division"
  (let ((result (num-run "(/ 1 3)")))
    (expect result :to-equal 1/3)))

(it-sequential "num-ratio-add"
  (let ((result (num-run "(+ 1/2 1/3)")))
    (expect result :to-equal 5/6)))

(it-sequential "num-ratio-mul"
  (let ((result (num-run "(* 2/3 3/4)")))
    (expect result :to-equal 1/2)))

(it-sequential "num-ratio-typep"
  (let ((result (num-run "(typep (/ 1 3) 'ratio)")))
    (expect result :to-be-truthy)))

(it-sequential "num-rational-function"
  (let ((result (num-run "(rational 0.5)")))
    (expect result :to-equal 1/2)))

(it-sequential "num-rationalize-function"
  (let ((result (num-run "(rationalize 1/3)")))
    (expect result :to-equal 1/3)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Complex Number Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-complex-constructor"
  (let ((result (num-run "(complex 3 4)")))
    (expect (num-run "(realpart (complex 3 4))") :to-equal 3)
    (expect (num-run "(imagpart (complex 3 4))") :to-equal 4)))

(it-sequential "num-complex-add"
  (let ((result (num-run "(+ #c(1 2) #c(3 4))")))
    (expect (num-run "(realpart (+ #c(1 2) #c(3 4)))") :to-equal 4)
    (expect (num-run "(imagpart (+ #c(1 2) #c(3 4)))") :to-equal 6)))

(it-sequential "num-complex-mul"
  (let ((result (num-run "(* #c(1 2) #c(3 4))")))
    (expect (num-run "(realpart (* #c(1 2) #c(3 4)))") :to-equal -5)
    (expect (num-run "(imagpart (* #c(1 2) #c(3 4)))") :to-equal 10)))

(it-sequential "num-complex-conjugate"
  (let ((result (num-run "(conjugate #c(3 4))")))
    (expect (num-run "(realpart (conjugate #c(3 4)))") :to-equal 3)
    (expect (num-run "(imagpart (conjugate #c(3 4)))") :to-equal -4)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Numeric Contagion Rules
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-contagion-integer-ratio"
  (let ((result (num-run "(+ 1 1/2)")))
    (expect result :to-equal 3/2)))

(it-sequential "num-contagion-ratio-float"
  (let ((result (num-run "(+ 1/2 0.5)")))
    (expect (= 1.0 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Numeric Predicates on Extended Types
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-bignump-predicate"
  (let ((result (num-run "(bignump (expt 2 100))")))
    (expect result :to-be-truthy)))

(it-sequential "num-float-type-hierarchy"
  (let ((result (num-run "(and (floatp 3.14) (not (integerp 3.14)))")))
    (expect result :to-be-truthy)))

(it-sequential "num-real-type-hierarchy"
  (let ((result (num-run "(and (realp 42) (realp 3.14) (realp 1/2))")))
    (expect result :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Native x86-64 Fixnum Boundary
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-native-fixnum-add-overflow"
  (let ((result (num-run
                 "(let ((x most-positive-fixnum))
                    (list x (+ x 1) (type-of (+ x 1))))")))
    (expect result :to-be-truthy)))

(it-sequential "num-native-bignum-identity"
  (let ((result (num-run "1267650600228229401496703205376")))
    (expect (= 1267650600228229401496703205376 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Arithmetic Functions on Extended Types
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "num-isqrt-large"
  (let ((result (num-run "(isqrt (expt 2 100))")))
    (expect (= (isqrt (expt 2 100)) result) :to-be-truthy)))

(it-sequential "num-floor-large"
  (let ((result (num-run "(floor (expt 2 100) 3)")))
    (expect (= (floor (expt 2 100) 3) result) :to-be-truthy)))
