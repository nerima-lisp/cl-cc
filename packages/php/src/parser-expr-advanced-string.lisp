;;;; frontend/php/parser-expr-advanced-string.lisp -- PHP String and Exception Expression Helpers
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── String / Exception Helpers ─────────────────────────────────────────────

(defun %php-exception-class-from-expression (expr)
  "Return the PHP class symbol visible in a throw expression, when known."
  (when (and (ast-make-instance-p expr)
             (ast-var-p (ast-make-instance-class expr)))
    (ast-var-name (ast-make-instance-class expr))))

(defun %php-exception-payload-call (expr)
  "Wrap thrown PHP EXPR with class metadata for VM catch/throw lowering."
  (%php-call 'cl-cc/php::%php-make-exception
             (make-ast-quote :value (%php-exception-class-from-expression expr))
             expr))

(defun %php-parse-interp-expr (text)
  "Parse a complex-interpolation expression TEXT (e.g. $a[\"k\"], $o->x, $o->m())
into an AST node. The text is tokenized as a standalone PHP expression and parsed
with php-parse-expr; %php-concat stringifies the resulting value."
  (let ((stream (tokenize-php-source (concatenate 'string "<?php " text))))
    (values (php-parse-expr stream nil))))

(defun %php-string-token-ast (value)
  "Lower a lexer string VALUE to AST, preserving simple interpolation."
  (if (and (consp value) (eq (first value) :php-interpolated-string))
      (let ((args (mapcar (lambda (segment)
                            (ecase (first segment)
                              (:string (make-ast-quote :value (second segment)))
                              (:var (make-ast-var :name (php-var-sym (second segment))))
                              ;; {$expr} complex interpolation — array/prop access etc.
                              (:expr (%php-parse-interp-expr (second segment)))))
                          (second value))))
        (if args
            (make-ast-call :func (make-ast-var :name 'cl-cc/php::%php-concat)
                           :args args)
            (make-ast-quote :value "")))
      (make-ast-quote :value value)))
