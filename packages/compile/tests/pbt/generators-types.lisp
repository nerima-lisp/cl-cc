;;;; tests/pbt/generators-types.lisp - Type Expression Generators
;;;
;;; Generators for type expressions used in type system property-based testing,
;;; built on cl-weave 1.0.0's combinators. Replaces the home-grown
;;; generators.lisp.
;;;
;;; Two notes on the translation:
;;;
;;;   - GEN-ONE-OF over a list of *values* became GEN-MEMBER; cl-weave's
;;;     GEN-ONE-OF chooses among *generators*. GEN-FMAP became GEN-MAP and
;;;     GEN-LIST-OF became GEN-LIST.
;;;
;;;   - GEN-TYPE-EXPR is now built with GEN-RECURSIVE. The original decided
;;;     terminal-vs-compound with a (random 100) call inside the constructor,
;;;     which only varied per case because the home-grown DEFPROPERTY
;;;     re-evaluated the generator expression on every iteration. CL-WEAVE's
;;;     IT-PROPERTY builds its generator list once, so that shape would have
;;;     frozen for a whole property. GEN-RECURSIVE puts the choice inside
;;;     generation, where it belongs, and bounds depth explicitly.

(in-package :cl-cc/pbt)

;;; Configuration

(defparameter *max-type-depth* 3
  "Maximum depth for recursive type expressions.")

;;; Type Expression Generators

(defun gen-primitive-type ()
  "Generate a primitive type specifier."
  (cl-weave:gen-member '(fixnum single-float double-float string boolean symbol
                         integer number character list cons null
                         t)))

(defun gen-type-variable ()
  "Generate a type variable for polymorphism testing (?a, ?b, etc.)."
  (cl-weave:gen-map (lambda (c) (intern (format nil "?~A" c) :keyword))
                    (cl-weave:gen-member '(a b c d e f x y z))))

(defun gen-simple-compound-type ()
  "Generate simple compound types like (or T1 T2), (and T1 T2)."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (op types) parts
       (cons op types)))
   (cl-weave:gen-tuple (cl-weave:gen-member '(or and))
                       (cl-weave:gen-list (gen-primitive-type)
                                          :min-length 2 :max-length 4))))

(defun gen-values-type ()
  "Generate (values T1 T2 ...) type for multiple values."
  (cl-weave:gen-map
   (lambda (types) (cons 'values types))
   (cl-weave:gen-list (gen-primitive-type) :min-length 0 :max-length 5)))

(defun gen-terminal-type ()
  "Generate a terminal type: either a primitive specifier or a type variable."
  (cl-weave:gen-one-of (gen-primitive-type) (gen-type-variable)))

(defun gen-fn-type (&optional (element (gen-terminal-type)))
  "Generate function type (function arg-types return-type).
ELEMENT generates the argument and return types; it defaults to a terminal type
so this is usable standalone as well as inside GEN-TYPE-EXPR's recursion."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (args ret) parts
       (list 'function args ret)))
   (cl-weave:gen-tuple (cl-weave:gen-list element :min-length 0 :max-length 4)
                       element)))

(defun gen-array-type ()
  "Generate array type specifiers."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (base dims) parts
       (if dims
           (list base dims)
           base)))
   (cl-weave:gen-tuple
    (cl-weave:gen-member '(simple-array array vector simple-vector
                           bit-vector simple-bit-vector string simple-string))
    (cl-weave:gen-member '(nil (1) (*) (* *) ((*) (*)))))))

(defun gen-cons-type (&optional (element (gen-terminal-type)))
  "Generate (cons car-type cdr-type) type specifiers."
  (cl-weave:gen-map
   (lambda (types) (cons 'cons types))
   (cl-weave:gen-tuple element element)))

(defun gen-type-expr (&key (max-depth *max-type-depth*))
  "Generate random type expressions for testing.

   Generates:
   - Primitives: fixnum, single-float, string, boolean, symbol
   - Compound: (function (T1 T2) R), (or T1 T2), (values T1 T2), array and
     cons specifiers
   - Variables: ?a, ?b (for polymorphism testing)"
  (cl-weave:gen-recursive
   (gen-terminal-type)
   (lambda (self)
     (cl-weave:gen-one-of (gen-simple-compound-type)
                          (gen-values-type)
                          (gen-fn-type self)
                          (gen-array-type)
                          (gen-cons-type self)))
   :max-depth max-depth))
