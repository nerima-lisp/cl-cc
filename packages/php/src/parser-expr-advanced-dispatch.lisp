;;;; frontend/php/parser-expr-advanced-dispatch.lisp -- PHP Keyword Expression Dispatch
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Keyword Expression Dispatcher ──────────────────────────────────────────
;;;
;;; This function is called from parser-expr.lisp (php-parse-primary) for
;;; keyword-led expressions.  It lives here because all its targets (%php-parse-
;;; arrow-function, %php-parse-match-expression, etc.) are defined in this file.

(defun %php-clone-expression-ast (expr &optional overrides)
  "Lower a PHP clone expression, optionally applying PHP 8.5 clone-with overrides."
  (let ((clone-sym (gensym "PHP-CLONE-")))
    (make-ast-let
     :bindings (list (cons clone-sym
                           (make-ast-call
                            :func (make-ast-var :name 'cl-cc/php::%php-clone)
                            :args (list expr))))
     :body (append
            (list
             (make-ast-if
              :cond (make-ast-call
                     :func (make-ast-var :name 'cl-cc/php::%php-has-method)
                     :args (list (make-ast-var :name clone-sym)
                                 (make-ast-quote :value (php-ident-sym "__clone"))))
              :then (%php-method-call-with-args
                     (make-ast-slot-value
                      :object (make-ast-var :name clone-sym)
                      :slot (php-ident-sym "__clone"))
                     (php-ident-sym "__clone")
                     nil
                     (make-ast-var :name clone-sym))
              :else (make-ast-quote :value nil)))
            (when overrides
              (list
               (make-ast-call
                :func (make-ast-var :name 'cl-cc/php::%php-clone-with)
                :args (list (make-ast-var :name clone-sym) overrides))))
            (list (make-ast-var :name clone-sym))))))

(defun %php-clone-call-ast (args)
  "Lower PHP 8.5 function-style clone call arguments."
  (case (length args)
    (1 (%php-clone-expression-ast (first args)))
    (2 (%php-clone-expression-ast (first args) (second args)))
    (otherwise
     (error "PHP parse error: clone() expects object or object and override array"))))

(defun %php-parse-keyword-expr (stream kw known-vars)
  "Dispatch keyword-led expression to the appropriate handler."
  (multiple-value-bind (tok rest) (php-consume stream)
    (declare (ignore tok))
    (case kw
      (:clone
       (if (and (eq (php-peek-type rest) :T-LPAREN)
                (eq (php-peek-type (cdr rest)) :T-ELLIPSIS))
           (values (make-ast-quote :value nil)
                   (cdddr rest) known-vars)
           (if (eq (php-peek-type rest) :T-LPAREN)
               (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
                 (values (%php-clone-call-ast args) rest2 kv2))
               (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
                 (values (%php-clone-expression-ast expr) rest2 kv2)))))
      (:fn
       (%php-parse-arrow-function rest known-vars))
      (:match
       (%php-parse-match-expression rest known-vars))
      (:yield
        (%php-parse-yield-expression rest known-vars))
      (:throw
       (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
          (values (make-ast-throw :tag (make-ast-quote :value 'php-exception)
                                   :value (%php-exception-payload-call expr))
                  rest2 kv2)))
      (:array
        (if (eq (php-peek-type rest) :T-LPAREN)
            (%php-parse-array-expr rest known-vars :open :T-LPAREN :close :T-RPAREN)
            (values (make-ast-quote :value nil) rest known-vars)))
      (:list
       ;; list($a, $b) = expr is destructuring assignment. Parse it to the SAME
       ;; array-literal node that the short form [$a, $b] produces, so it flows
       ;; through the existing assignment-target path (%php-array-literal-call-p ->
       ;; %php-lower-list-assign). The old %php-list-bind call node was not a
       ;; recognized assignment target -> "unsupported assignment target".
       (if (eq (php-peek-type rest) :T-LPAREN)
           (%php-parse-array-expr rest known-vars :open :T-LPAREN :close :T-RPAREN)
           (values (make-ast-quote :value nil) rest known-vars)))
      (:function
       (%php-parse-anonymous-function rest known-vars)))))
