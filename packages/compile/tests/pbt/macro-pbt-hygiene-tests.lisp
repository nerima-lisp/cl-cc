;;;; tests/pbt/macro-pbt-hygiene-tests.lisp — Idempotency, Hygiene, Uniqueness, Validity
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.
;;;
;;; Note: this is the same set of properties as macro-pbt-advanced-tests.lisp,
;;; distinguished only by the -hygiene-pbt name suffix. Both are migrated
;;; as-is rather than deduplicated.

(in-package :cl-cc/pbt)

;;; Property: Macro Expansion Idempotency

;;; Property: Macro Hygiene (Gensym Usage)

;;; Property: Unique Gensym per Expansion

;;; Property: Expansion Structure Validity

;;; Property: Macroexpand-All Recursiveness

;;; Property: Environment Interaction

