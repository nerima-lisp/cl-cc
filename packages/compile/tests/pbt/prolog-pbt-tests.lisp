;;;; tests/pbt/prolog-pbt-tests.lisp - Property-Based Tests for Prolog Engine
;;;
;;; This module provides property-based tests for the external cl-prolog
;;; unification engine's UNIFY / LOGIC-SUBSTITUTE / LOGIC-VAR-P, imported in
;;; pbt/package.lisp. UNIFY returns (VALUES ENV OK) rather than a single
;;; :UNIFY-FAIL sentinel value.

(in-package :cl-cc/pbt)


;; ----------------------------------------------------------------------------
;; Unification Properties
;; ----------------------------------------------------------------------------

(defproperty unify-variable-with-integer
    (n (gen-integer :min -1000 :max 1000))
  (multiple-value-bind (env ok) (unify '?x n)
    (and ok (equal (logic-substitute '?x env) n))))

(defproperty unify-variable-with-symbol
    (s (gen-symbol :prefix "SYM"))
  (multiple-value-bind (env ok) (unify '?x s)
    (and ok (eq (logic-substitute '?x env) s))))

(defproperty unify-two-variables-binds
    (n (gen-integer :min -1000 :max 1000))
  (multiple-value-bind (env1 ok1) (unify '?x '?y)
    (multiple-value-bind (env2 ok2) (unify '?x n env1)
      (and ok1 ok2
           (equal (logic-substitute '?y env2) n)))))

(defproperty unify-fails-on-different-integers
    (a (gen-integer :min 0 :max 500)
     b (gen-integer :min 501 :max 1000))
  (multiple-value-bind (env ok) (unify a b)
    (declare (ignore env))
    (not ok)))

(defproperty substitute-preserves-non-variables
    (n (gen-integer :min -1000 :max 1000))
  (= (logic-substitute n nil) n))

(defproperty unify-variable-with-list
    (a (gen-integer :min -100 :max 100)
     b (gen-integer :min -100 :max 100))
  (multiple-value-bind (env ok) (unify '?x (list a b))
    (and ok (equal (logic-substitute '?x env) (list a b)))))

(defproperty logic-var-p-works
    (n (gen-integer :min -1000 :max 1000))
  (and (logic-var-p '?x)
       (logic-var-p '?foo)
       (not (logic-var-p 'x))
       (not (logic-var-p n))))
