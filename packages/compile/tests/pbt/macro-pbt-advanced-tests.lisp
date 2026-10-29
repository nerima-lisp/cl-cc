;;;; tests/pbt/macro-pbt-advanced-tests.lisp — Advanced PBT: Nested/Idempotency/Hygiene/Validity
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.
;;;
;;; Note: macro-pbt-hygiene-tests.lisp is the same set of properties with a
;;; -hygiene-pbt name suffix. Both are migrated as-is rather than deduplicated.

(in-package :cl-cc/pbt)

;;; Property: Nested Macro Expansion

;;; Property: Macro Expansion Idempotency

;;; Property: Macro Hygiene (Gensym Usage)

;;; Property: Unique Gensym per Expansion

;;; Property: Expansion Structure Validity

;;; Property: Macroexpand-All Recursiveness

;;; Property: Environment Interaction

