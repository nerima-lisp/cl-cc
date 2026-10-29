;;;; tests/pbt/macro-pbt-binding-tests.lisp — PBT for LET*/PROG1/PROG2/SETF/PSETQ/MVB/MVS/MVL/DEFUN
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.
;;;
;;; Note: these properties duplicate macro-pbt-props-tests.lisp's LET*/PROG1/
;;; PROG2 block and macro-pbt-mv-tests.lisp's SETF/PSETQ/MV/DEFUN block, with
;;; a -binding-pbt name suffix. They are migrated as-is rather than deduplicated.

(in-package :cl-cc/pbt)

;;; Property: PROG1 and PROG2 Macro Expansion

;;; Property: SETF and PSETQ Macro Expansion

;;; Property: MULTIPLE-VALUE-BIND / -SETQ / -LIST Macro Expansion

;;; Property: DEFUN Macro Expansion

