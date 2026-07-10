(in-package :cl-cc/prolog)

;;; Declarative Prolog rule specifications

(defparameter *prolog-list-rule-specs*
  '(((member ?x (cons ?x ?rest)))
    ((member ?x (cons ?y ?rest))
     ((member ?x ?rest)))
    ((append nil ?l ?l))
    ((append (cons ?x ?l1) ?l2 (cons ?x ?l3))
     ((append ?l1 ?l2 ?l3)))
    ((reverse nil nil))
    ((reverse (cons ?x ?xs) ?result)
     ((reverse ?xs ?rev-xs)
      (append ?rev-xs (cons ?x nil) ?result)))
    ((length nil 0))
    ((length (cons ?x ?rest) (+ 1 ?n))
     ((length ?rest ?n))))
  "List-oriented declarative rules encoded as data.")

(defparameter *prolog-environment-rule-specs*
  '(((type-of (const ?val) ?env (integer-type))
     ((:when (integerp ?val))))
    ((type-of (var ?name) ?env ?type)
     ((env-lookup ?env ?name ?type)))
    ((type-of (if ?cond ?then ?else) ?env ?type)
     ((type-of ?cond ?env (boolean-type))
      (type-of ?then ?env ?type)
      (type-of ?else ?env ?type)))
    ((env-lookup (cons (cons ?name ?type) ?rest) ?name ?type))
    ((env-lookup (cons ?binding ?rest) ?name ?type)
     ((env-lookup ?rest ?name ?type))))
  "Environment-oriented declarative rules encoded as data.")

(defparameter *prolog-declarative-rule-specs*
  (append *prolog-list-rule-specs*
          *prolog-environment-rule-specs*)
  "Declarative Prolog rules encoded as data.")

(defparameter *prolog-integer-binop-type-operators*
  '(+ - * / mod)
  "Arithmetic operators whose binop forms always infer INTEGER-TYPE in Prolog rules.")

(defparameter *prolog-comparison-type-operators*
  '(< > <= >= = /=)
  "Comparison operators whose cmp forms always infer BOOLEAN-TYPE in Prolog rules.")

(defparameter *prolog-type-rule-specs*
  `((integer-type binop ,*prolog-integer-binop-type-operators*)
    (boolean-type cmp   ,*prolog-comparison-type-operators*))
  "Data table for generating Prolog type inference rules.
Each entry: (RESULT-TYPE EXPR-KIND OPERATOR-LIST).")
