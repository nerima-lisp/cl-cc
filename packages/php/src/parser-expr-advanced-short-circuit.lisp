;;;; frontend/php/parser-expr-advanced-short-circuit.lisp -- PHP Short-Circuit Expression Lowering
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Null-coalesce / Elvis Lowering ─────────────────────────────────────────

(defun %php-not-null-cond (expr)
  "Build (not (%php-null-p EXPR)) as an AST expression."
  (%php-call 'not (%php-call 'cl-cc/php::%php-null-p expr)))

(defun %php-lower-null-coalesce (lhs rhs)
  "Lower LHS ?? RHS without evaluating LHS twice."
  (let ((tmp (gensym "PHP-COALESCE-")))
    (make-ast-let
     :bindings (list (cons tmp lhs))
     :body (list (make-ast-if
                  :cond (%php-not-null-cond (make-ast-var :name tmp))
                  :then (make-ast-var :name tmp)
                  :else rhs)))))

(defun %php-lower-elvis (condition else-expr)
  "Lower CONDITION ?: ELSE-EXPR without evaluating CONDITION twice."
  (let ((tmp (gensym "PHP-ELVIS-")))
    (make-ast-let
     :bindings (list (cons tmp condition))
     :body (list (make-ast-if
                  :cond (make-ast-var :name tmp)
                  :then (make-ast-var :name tmp)
                  :else else-expr)))))
