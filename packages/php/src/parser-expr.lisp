;;;; frontend/php/parser-expr.lisp -- PHP expression parser anchor
;;;;
;;;; Domain files loaded immediately after this anchor:
;;;;   parser-expr-primary  -- literals, constants, casts, class-relative names
;;;;   parser-expr-new      -- new expressions and runtime-backed constructors
;;;;   parser-expr-postfix  -- member access, calls, indexing, postfix inc/dec
;;;;   parser-expr-operator -- unary/binary/ternary/assignment/arglist parsing
;;;;
;;;; Depends on parser.lisp for: php-tok-type, php-tok-value, php-peek,
;;;; php-peek-type, php-peek-value, php-consume, php-expect, php-var-sym,
;;;; php-ident-sym (all loaded before this file).
(in-package :cl-cc/php)

;;; Bound dynamically by the class statement parser around the class body so
;;; self::, static::, and parent:: in method bodies resolve to the enclosing class
;;; object and its first superclass.
(defvar *php-current-class* nil
  "php-ident-sym of the class whose body is currently being parsed, or NIL.")

(defvar *php-current-supers* nil
  "List of php-ident-sym superclass names for the class being parsed.")
