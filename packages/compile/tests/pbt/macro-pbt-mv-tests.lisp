;;;; tests/pbt/macro-pbt-mv-tests.lisp — SETF, PSETQ, MVB, MVSQ, MVL, DEFUN, Nested
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.

(in-package :cl-cc/pbt)

;;; Property: SETF Macro Expansion

;;; Property: MULTIPLE-VALUE-BIND / -SETQ / -LIST Macro Expansion

;;; Property: DEFUN Macro Expansion

;;; Property: Nested Macro Expansion

