;;;; tests/pbt/macro-pbt-props-tests.lisp — Macro Expansion Properties
;;;
;;; Expressed with cl-weave's NATIVE property API. Each body asserts through
;;; CL-WEAVE:EXPECT rather than returning a boolean: IT-PROPERTY decides
;;; pass/fail from a *signaled* condition (RUN-PROPERTY wraps the body in
;;; PROPERTY-FAILURE-CONDITION, which only catches ERROR) and discards the
;;; return value, so a bare boolean body would report PASS even when false.

(in-package :cl-cc/pbt)


;;; Property: WHEN Macro Expansion

;;; Property: UNLESS Macro Expansion

;;; Property: COND Macro Expansion

;;; Property: AND Macro Expansion

;;; Property: OR Macro Expansion

;;; Property: LET* Macro Expansion

;;; Property: PROG1 and PROG2 Macro Expansion

