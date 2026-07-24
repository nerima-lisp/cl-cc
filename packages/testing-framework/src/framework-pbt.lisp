;;;; framework-pbt.lisp — Property-Based Test DSL
;;;;
;;;; %pbt-run, assert-pbt, deftest-pbt: statistical property testing
;;;; integrated with the compiler test helpers in framework-compiler.lisp.
;;;;
;;;; Load order: after framework-compiler.lisp.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Section N-1: Property-Based Test DSL
;;; ------------------------------------------------------------
;;;
;;; Many compiler correctness properties follow the same shape:
;;;   generate N random inputs, compile+run each, count passes, assert ratio.
;;; deftest-pbt captures that shape so tests read as specifications.

(defun %pbt-run (trials threshold check-fn)
  "Run CHECK-FN TRIALS times; assert the success ratio meets THRESHOLD.
Returns the pass ratio for inspection."
  (let ((passes 0))
    (dotimes (i trials)
      (when (funcall check-fn) (incf passes)))
    (let ((ratio (/ passes trials)))
      (assert-true (>= ratio threshold))
      ratio)))


