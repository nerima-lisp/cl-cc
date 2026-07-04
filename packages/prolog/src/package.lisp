(cl:in-package :cl-user)

;;;; packages/prolog/src/package.lisp - CL-CC Prolog Package
;;;;
;;;; This package owns the Prolog engine, DCG transformer, and peephole rule
;;;; tables. The facade :cl-cc package uses :cl-cc/prolog for the public
;;;; Prolog API.
;;;;
;;;; Bootstrap symbols (binop, const, var, cmp, integer-type, boolean-type,
;;;; env-lookup, make-cst-token, lexer-token-p/type/value, our-eval) are
;;;; provided by :cl-cc/bootstrap (cl-cc-bootstrap.asd), which loads before
;;;; this package.

(defpackage :cl-cc/prolog
  (:use :cl :cl-cc/bootstrap)
  (:export
   ;; Logic variables & unification
   #:logic-var-p #:unify #:unify-failed-p
   #:logic-substitute
   ;; Fact/rule macro
   #:def-rule
   ;; Data tables & builtins
   #:*peephole-rules*
   ;; Solver
   #:query-all
   ;; DCG
   #:def-dcg-rule
   #:phrase #:phrase-all))
