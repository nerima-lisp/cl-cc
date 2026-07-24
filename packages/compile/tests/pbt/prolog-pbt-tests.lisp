;;;; tests/pbt/prolog-pbt-tests.lisp - Property-Based Tests for Prolog Engine
;;;
;;; Property-based tests for the external cl-prolog unification engine's
;;; UNIFY / LOGIC-SUBSTITUTE / LOGIC-VAR-P (imported in pbt/package.lisp).
;;; UNIFY returns (VALUES ENV OK) rather than a single :UNIFY-FAIL sentinel.
;;;
;;; These use cl-weave's NATIVE property API (cl-weave:describe / it-property /
;;; gen-integer / gen-symbol) directly, rather than the home-grown cl-cc/pbt
;;; defproperty DSL — an it-property body is simply a boolean-valued property.

(in-package :cl-cc/pbt)

(cl-weave:describe "cl-prolog unification properties"
  (cl-weave:it-property "unify binds a logic variable to an integer"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (multiple-value-bind (env ok) (unify '?x n)
      (and ok (equal (logic-substitute '?x env) n))))

  (cl-weave:it-property "unify binds a logic variable to a symbol"
      ((s (cl-weave:gen-symbol)))
    (multiple-value-bind (env ok) (unify '?x s)
      (and ok (eq (logic-substitute '?x env) s))))

  (cl-weave:it-property "unify chains two variables, then binds through"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (multiple-value-bind (env1 ok1) (unify '?x '?y)
      (multiple-value-bind (env2 ok2) (unify '?x n env1)
        (and ok1 ok2 (equal (logic-substitute '?y env2) n)))))

  (cl-weave:it-property "unify fails on two distinct integers"
      ((a (cl-weave:gen-integer :min 0 :max 500))
       (b (cl-weave:gen-integer :min 501 :max 1000)))
    (multiple-value-bind (env ok) (unify a b)
      (declare (ignore env))
      (not ok)))

  (cl-weave:it-property "logic-substitute preserves non-variables"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (= (logic-substitute n nil) n))

  (cl-weave:it-property "unify binds a logic variable to a list"
      ((a (cl-weave:gen-integer :min -100 :max 100))
       (b (cl-weave:gen-integer :min -100 :max 100)))
    (multiple-value-bind (env ok) (unify '?x (list a b))
      (and ok (equal (logic-substitute '?x env) (list a b)))))

  (cl-weave:it-property "logic-var-p recognizes ?-prefixed symbols only"
      ((n (cl-weave:gen-integer :min -1000 :max 1000)))
    (and (logic-var-p '?x)
         (logic-var-p '?foo)
         (not (logic-var-p 'x))
         (not (logic-var-p n)))))
