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

(in-package :cl-cc/prolog)

;;; Prolog data tables
;;;
;;; Keep these definitions in a tracked file so the Nix source snapshot always
;;; includes the data required by the ASDF components that load later.

(defparameter *builtin-predicate-specs*
  '((! prolog-cut-handler)
    (and solve-conjunction)
    (or prolog-or-handler)
    (= prolog-unify-handler)
    (/= prolog-not-unify-handler)
    (:when prolog-when-handler))
  "Data table of built-in Prolog predicates and their CPS handlers.")

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

(defparameter *dcg-sync-tokens* '(:T-RPAREN :T-SEMI :T-EOF)
  "Token types used as synchronization points for error recovery.")

(defparameter *dcg-builtin-specs*
  '((dcg-alt %dcg-alt)
    (dcg-opt %dcg-opt)
    (dcg-star %dcg-star)
    (dcg-plus %dcg-plus)
    (dcg-error-recovery %dcg-error-recovery)
    (dcg-token-match %dcg-token-match)
    (dcg-token-match-value %dcg-token-match-value))
  "DCG builtin predicate registrations.")

(defparameter *peephole-rules*
  '(;; (:const :R1 42)(:move :R2 :R1) -> (:const :R2 42)
    ;; Fires when copy-prop is blocked by a label reset but DCE kept the const alive.
    ((:const ?src ?val) (:move ?dst ?src) ((:const ?dst ?val)))

    ;; (:jump "L0")(:label "L0") -> (:label "L0")
    ;; Eliminates a jump to the immediately following label (dead branch after threading).
    ((:jump ?lbl) (:label ?lbl) ((:label ?lbl)))

    ;; (:const ?r ?v1)(:const ?r ?v2) -> (:const ?r ?v2)
    ;; Second const-load to the same register makes the first dead.
    ;; Safe in a 2-window because no instruction can read ?r between adjacent instructions.
    ((:const ?r ?_v1) (:const ?r ?v2) ((:const ?r ?v2)))

    ;; (:move ?mid ?src)(:move ?dst ?mid) -> (:move ?mid ?src)(:move ?dst ?src)
    ;; Copy-propagation through a move chain: ?mid still gets ?src (in case it
    ;; is read elsewhere), but ?dst now reads directly from ?src, enabling DCE
    ;; to later eliminate ?mid if it has no remaining readers.
    ((:move ?mid ?src) (:move ?dst ?mid) ((:move ?mid ?src) (:move ?dst ?src)))

    ;; Arithmetic and comparison identities that simplify the current
    ;; instruction while preserving the following instruction unchanged.
    ((:add ?dst ?src 0)   ?next ((:move ?dst ?src) ?next))
    ((:add ?dst 0 ?src)   ?next ((:move ?dst ?src) ?next))
    ((:sub ?dst ?src 0)   ?next ((:move ?dst ?src) ?next))
    ((:sub ?dst 0 ?src)   ?next ((:neg ?dst ?src) ?next))
    ((:sub ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:mul ?dst ?src 1)   ?next ((:move ?dst ?src) ?next))
    ((:mul ?dst 1 ?src)   ?next ((:move ?dst ?src) ?next))
    ((:mul ?dst ?src 0)   ?next ((:const ?dst 0) ?next))
    ((:mul ?dst 0 ?src)   ?next ((:const ?dst 0) ?next))
    ((:div ?dst ?src 1)   ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst ?src -1) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst -1 ?src) ?next ((:move ?dst ?src) ?next))
    ((:logand ?dst ?src 0) ?next ((:const ?dst 0) ?next))
    ((:logior ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst 0 ?src) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst ?src -1) ?next ((:const ?dst -1) ?next))
    ((:logxor ?dst ?src 0) ?next ((:move ?dst ?src) ?next))
    ((:eq ?dst ?src ?src)   ?next ((:const ?dst 1) ?next))
    ((:gt ?dst ?src ?src)   ?next ((:const ?dst 0) ?next))
    ((:le ?dst ?src ?src)   ?next ((:const ?dst 1) ?next))
    ((:logand ?dst ?src ?src) ?next ((:move ?dst ?src) ?next))
    ((:logior ?dst ?src ?src) ?next ((:move ?dst ?src) ?next))
    ((:logxor ?dst ?src ?src) ?next ((:const ?dst 0) ?next))
    ((:num-eq ?dst ?src ?src) ?next ((:const ?dst 1) ?next))
    ((:lt ?dst ?src ?src)   ?next ((:const ?dst 0) ?next))
    ((:ge ?dst ?src ?src)   ?next ((:const ?dst 1) ?next))

    ;; Negated comparisons can be collapsed into the opposite comparison.
    ((:lt ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:ge ?dst ?lhs ?rhs)))
    ((:gt ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:le ?dst ?lhs ?rhs)))
    ((:le ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:gt ?dst ?lhs ?rhs)))
    ((:ge ?tmp ?lhs ?rhs) (:not ?dst ?tmp) ((:lt ?dst ?lhs ?rhs)))

    ;; Unconditional transfers make the immediately-following instruction dead.
    ((:jump ?lbl1) (:jump ?lbl2) ((:jump ?lbl1)))
    ((:jump ?lbl) (:ret ?reg) ((:jump ?lbl)))
    ((:jump ?lbl) (:halt ?reg) ((:jump ?lbl)))
    ((:ret ?reg) (:jump ?lbl) ((:ret ?reg)))
    ((:halt ?reg) (:jump ?lbl) ((:halt ?reg)))
    ((:ret ?reg1) (:ret ?reg2) ((:ret ?reg1)))
    ((:halt ?reg1) (:halt ?reg2) ((:halt ?reg1)))
    ((:ret ?reg1) (:halt ?reg2) ((:ret ?reg1)))
    ((:halt ?reg1) (:ret ?reg2) ((:halt ?reg1))))
  "Peephole rules assembled as data.")
