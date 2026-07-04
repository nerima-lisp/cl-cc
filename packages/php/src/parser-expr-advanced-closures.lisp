;;;; frontend/php/parser-expr-advanced-closures.lisp -- PHP Closure Expression Lowering
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Capture / Closure Helpers ───────────────────────────────────────────────

(defun %php-capture-wrapper (captures value)
  "Wrap VALUE in explicit let-bindings that snapshot CAPTURES by value."
  (make-ast-let
   :bindings (mapcar (lambda (name) (cons name (make-ast-var :name name))) captures)
   :body (list value)))

(defun %php-find-free-vars (body)
  "Return a list of free variable symbols referenced in BODY AST."
  (let ((free nil))
    (labels ((walk (node)
               (when (typep node 'cl-cc/ast:ast-var)
                 (pushnew (cl-cc/ast:ast-var-name node) free :test #'eq))
               (when (typep node 'cl-cc/ast:ast-node)
                 (dolist (child (cl-cc/ast:ast-children node))
                   (when child (walk child))))))
      (walk body)
      free)))

(defun %php-arrow-captures (body params known-vars)
  "Return PHP variables captured by an arrow function body."
  (set-difference (intersection (%php-find-free-vars body) known-vars :test #'eq)
                  params :test #'eq))

(defun %php-parse-arrow-function (stream known-vars)
  "Parse fn(params) => expr and lower it to captured ast-lambda."
  (multiple-value-bind (params rest param-types _param-attrs by-ref-indices
                        param-defaults variadic-param)
      (php-parse-param-list stream)
    (declare (ignore param-types _param-attrs))
    (multiple-value-bind (return-type rest2) (php-parse-return-type rest)
      (declare (ignore return-type))
      (unless (and (eq (php-peek-type rest2) :T-OP)
                   (equal "=>" (php-peek-value rest2)))
        (error "PHP parse error: expected => after arrow function parameters"))
      (multiple-value-bind (arrow-token rest3) (php-consume rest2)
        (declare (ignore arrow-token))
        (multiple-value-bind (body rest4 kv4)
            (php-parse-expr rest3 (append params
                                          (when variadic-param (list variadic-param))
                                          known-vars))
          (multiple-value-bind (required optionals)
              (%php-split-params-by-defaults params param-defaults)
            (let* ((by-ref-params (mapcar (lambda (idx) (nth idx params)) by-ref-indices))
                   (callable-body (if by-ref-params
                                      (%php-rewrite-ref-vars (list body) by-ref-params)
                                      (list body))))
              (multiple-value-bind (rest-param wrapped-body)
                  (%php-variadic-rest-binding variadic-param callable-body)
                (let* ((captures (%php-arrow-captures body params known-vars))
                       (lambda (make-ast-lambda
                                :params required
                                :optional-params optionals
                                :rest-param rest-param
                                :declarations (when by-ref-indices
                                                (list (list :php-by-ref-indices
                                                            by-ref-indices)))
                                :body wrapped-body)))
                  (values (%php-capture-wrapper captures lambda) rest4 kv4))))))))))

(defun %php-parse-closure-use-list (stream)
  "Parse optional PHP closure use($x, &$y) and return (captures by-ref-set rest).
Captures is a list of variable symbols; by-ref-set is a hash-table of the by-ref ones."
  (if (and (eq (php-peek-type stream) :T-KEYWORD)
           (eq (php-peek-value stream) :use))
      (let ((current (%php-consume-expected :T-LPAREN (cdr stream)))
            (captures nil)
            (by-ref (make-hash-table)))
        (unless (eq (php-peek-type current) :T-RPAREN)
          (loop
            ;; Detect &$var — by-reference capture
            (let* ((is-ref (%php-reference-token-p current))
                   (after-amp (if is-ref (cdr current) current)))
              (multiple-value-bind (var-token rest) (php-expect :T-VAR after-amp)
                (let ((var-sym (php-var-sym (php-tok-value var-token))))
                  (push var-sym captures)
                  (when is-ref
                    (setf (gethash var-sym by-ref) t))
                  (setf current rest))))
            (if (eq (php-peek-type current) :T-COMMA)
                (setf current (cdr current))
                (return))))
        (values (nreverse captures) by-ref (%php-consume-expected :T-RPAREN current)))
      (values nil (make-hash-table) stream)))

(defun %php-parse-anonymous-function (stream known-vars)
  "Parse function(params) use($x, &$y) { body } as an ast-lambda with captures.
By-reference captures (&$var) are wrapped in ref boxes so mutations propagate."
  (let* ((returns-by-ref (%php-reference-token-p stream))
         (stream (if returns-by-ref (cdr stream) stream)))
  (multiple-value-bind (params rest param-types _param-attrs by-ref-indices
                        param-defaults variadic-param)
      (php-parse-param-list stream)
    (declare (ignore param-types _param-attrs))
    (multiple-value-bind (captures by-ref rest2) (%php-parse-closure-use-list rest)
      (multiple-value-bind (return-type rest3) (php-parse-return-type rest2)
        (declare (ignore return-type))
        (multiple-value-bind (body-stmts rest4 kv4)
            (php-parse-block rest3 (append params captures
                                           (when variadic-param (list variadic-param))
                                           known-vars))
          (multiple-value-bind (required optionals)
              (%php-split-params-by-defaults params param-defaults)
            (let* ((ref-captures (remove-if-not (lambda (sym) (gethash sym by-ref))
                                                captures))
                   (by-ref-params (mapcar (lambda (idx) (nth idx params)) by-ref-indices))
                   (ref-vars (remove-duplicates (append ref-captures by-ref-params)
                                                :test #'eq))
                   (callable-body (%php-callable-body body-stmts))
                   (callable-body (if ref-vars
                                      (%php-rewrite-ref-vars callable-body ref-vars)
                                      callable-body)))
              (multiple-value-bind (rest-param wrapped-body)
                  (%php-variadic-rest-binding variadic-param callable-body)
                (let* ((ref-bindings
                        (when (> (hash-table-count by-ref) 0)
                          (remove nil
                                  (mapcar (lambda (sym)
                                            (when (gethash sym by-ref)
                                              (cons sym
                                                    (%php-call 'cl-cc/php::%php-make-ref
                                                               (make-ast-var :name sym)))))
                                          captures))))
                       (lambda-ast
                        (make-ast-lambda
                         :params required
                         :optional-params optionals
                         :rest-param rest-param
                         :declarations (append (when by-ref-indices
                                                 (list (list :php-by-ref-indices
                                                             by-ref-indices)))
                                               (when returns-by-ref
                                                 (list (list :php-returns-by-ref t))))
                         :body wrapped-body))
                       (wrapped (if ref-bindings
                                    (make-ast-let :bindings ref-bindings
                                                  :body (list lambda-ast))
                                    lambda-ast)))
              (values (%php-capture-wrapper captures wrapped) rest4 kv4)))))))))))
