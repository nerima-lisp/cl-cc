;;;; packages/php/src/parser-expr-primary.lisp -- Primary-expression parsing: literals, variables, constants, casts, and class-relative names.
;;;;
;;;; Split from parser-expr.lisp so expression domains stay independently reviewable.
(in-package :cl-cc/php)

;;; ─── Member-name helper ──────────────────────────────────────────────────────
;;; PHP permits reserved keywords (from, list, class, print, default, ...) and
;;; type words as method / property / constant names after -> ?-> and ::, e.g.
;;; Status::from(1) or $obj->print().  The plain T-IDENT expectation rejected
;;; them, leaving the enum from/tryFrom/cases lowering unreachable.  This helper
;;; accepts identifiers, keywords, and type words and returns the member's name
;;; as a string.

(defun %php-member-name (stream)
  "Consume a member name (after -> / ?-> / ::) accepting T-IDENT, T-KEYWORD, or
T-TYPE.  Returns (values name-string rest)."
  (let ((type (php-peek-type stream)))
    (cond
      ((member type '(:T-IDENT :T-KEYWORD :T-TYPE) :test #'eq)
       (multiple-value-bind (tok rest) (php-consume stream)
         (let ((v (php-tok-value tok)))
           (values (if (stringp v) v (string-downcase (symbol-name v))) rest))))
      ;; Static property access C::$n — the member is lexed as a T-VAR ("$n").
      ;; Strip the leading $ so the name rendezvous with the slot declaration,
      ;; which stores the bare (php-ident-sym) name.
      ((eq type :T-VAR)
       (multiple-value-bind (tok rest) (php-consume stream)
         (let* ((v (php-tok-value tok))
                (bare (if (and (stringp v) (plusp (length v)) (char= (char v 0) #\$))
                          (subseq v 1) v)))
           (values bare rest))))
      (t (error "PHP parse error: expected member name but got ~S" (php-peek stream))))))

(defun %php-void-cast-start-p (stream)
  "Return true when STREAM starts with PHP 8.5's `(void)` cast prefix."
  (and (eq (php-peek-type stream) :T-LPAREN)
       (eq (php-peek-type (cdr stream)) :T-TYPE)
       (eq (php-peek-value (cdr stream)) :VOID)
       (eq (php-peek-type (cddr stream)) :T-RPAREN)))

(defun %php-void-cast-ast (expr)
  "Evaluate EXPR for side effects and return PHP null, matching `(void) EXPR`."
  (make-ast-progn
   :forms (list expr (make-ast-quote :value +php-null+))))

(defun %php-cast-token-name (tok)
  "Return TOK's value as a lowercase PHP cast/type spelling."
  (let ((value (php-tok-value tok)))
    (string-downcase
     (typecase value
       (string value)
       (symbol (symbol-name value))
       (t (princ-to-string value))))))

(defun %php-cast-token-kind (tok)
  "Return the canonical cast kind represented by TOK, or NIL."
  (when (and tok
             (member (php-tok-type tok) '(:T-TYPE :T-KEYWORD :T-IDENT) :test #'eq))
    (let ((name (%php-cast-token-name tok)))
      (cond
        ((member name '("int" "integer") :test #'string=) :int)
        ((member name '("float" "double" "real") :test #'string=) :float)
        ((member name '("string" "binary") :test #'string=) :string)
        ((member name '("bool" "boolean") :test #'string=) :bool)
        ((string= name "array") :array)
        ((string= name "object") :object)))))

(defun %php-cast-start-p (stream)
  "Return true when STREAM starts with a PHP scalar/array/object cast prefix."
  (and (eq (php-peek-type stream) :T-LPAREN)
       (eq (php-peek-type (cddr stream)) :T-RPAREN)
       (%php-cast-token-kind (second stream))))

(defun %php-cast-helper-symbol (kind)
  "Map a canonical cast KIND to the runtime helper used to perform it."
  (ecase kind
    (:int 'cl-cc/php::%php-intval)
    (:float 'cl-cc/php::%php-floatval)
    (:string 'cl-cc/php::%php-strval)
    (:bool 'cl-cc/php::%php-boolval)
    (:array 'cl-cc/php::%php-settype-array-value)
    (:object 'cl-cc/php::%php-settype-object-value)))

(defun %php-deprecated-cast-name (tok)
  "Return TOK's PHP 8.5-deprecated cast spelling, or NIL."
  (let ((name (%php-cast-token-name tok)))
    (when (member name '("integer" "boolean" "double" "binary") :test #'string=)
      name)))

(defun %php-deprecated-cast-replacement (name)
  "Return the canonical PHP cast spelling replacing deprecated NAME."
  (cond
    ((string= name "integer") "int")
    ((string= name "boolean") "bool")
    ((string= name "double") "float")
    ((string= name "binary") "string")))

(defun %php-deprecated-cast-message (name)
  "Return the PHP 8.5 deprecation message for non-canonical cast NAME."
  (format nil "Non-canonical cast (~A) is deprecated, use the (~A) cast instead"
          name
          (%php-deprecated-cast-replacement name)))

(defun %php-cast-ast (kind expr &optional deprecated-name)
  "Lower a PHP cast expression to the corresponding runtime conversion helper."
  (let ((cast-call (%php-call (%php-cast-helper-symbol kind) expr)))
    (if deprecated-name
        (make-ast-progn
         :forms (list (%php-call 'cl-cc/php::%php-trigger-error
                                  (make-ast-quote
                                   :value (%php-deprecated-cast-message deprecated-name))
                                  (make-ast-int :value 8192))
                      cast-call))
        cast-call)))

;;; ─── Expression Parser ──────────────────────────────────────────────────────

(defun php-parse-primary (stream known-vars)
  "Parse a primary expression (literal, variable, identifier, parenthesized)."
  (let ((type (php-peek-type stream)))
    (cond
      ((eq type :T-INT)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-int :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-FLOAT)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-quote :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-STRING)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (%php-string-token-ast (php-tok-value tok)) rest known-vars)))
      ((eq type :T-VAR)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-var :name (php-var-sym (php-tok-value tok))) rest known-vars)))
      ((eq type :T-KEYWORD)
       (let ((kw (php-peek-value stream)))
         (cond
            ((eq kw :null)
             (multiple-value-bind (tok rest) (php-consume stream)
               (declare (ignore tok))
              (values (make-ast-quote :value +php-null+) rest known-vars)))
           ((eq kw :true)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value t) rest known-vars)))
           ((eq kw :false)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value nil) rest known-vars)))
           ((eq kw :new)
            (php-parse-new stream known-vars))
           ((member kw '(:clone :fn :match :yield :throw :array :list :function))
            (%php-parse-keyword-expr stream kw known-vars))
           (t (error "PHP parse error: unexpected keyword ~S in expression" kw)))))
      ((eq type :T-LBRACKET)
       (%php-parse-array-expr stream known-vars))
      ((eq type :T-LPAREN)
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
           (multiple-value-bind (tok2 rest3) (php-expect :T-RPAREN rest2)
             (declare (ignore tok2))
             (values expr rest3 kv2)))))
      ((and (eq type :T-BACKSLASH)
            (let ((next-type (php-peek-type (cdr stream)))
                  (next-value (php-peek-value (cdr stream))))
              (and (member next-type '(:T-IDENT :T-KEYWORD :T-TYPE) :test #'eq)
                   (string= "clone"
                            (string-downcase
                             (if (stringp next-value)
                                 next-value
                                 (symbol-name next-value))))
                   (eq (php-peek-type (cddr stream)) :T-LPAREN))))
       (multiple-value-bind (args rest2 kv2) (php-parse-arglist (cddr stream) known-vars)
         (values (%php-clone-call-ast args) rest2 kv2)))
       ((member type '(:T-IDENT :T-BACKSLASH) :test #'eq)
        ;; Could be a function call, a predefined constant, or a user constant.
        (multiple-value-bind (qualified-name rest) (php-parse-qualified-name stream)
          (let ((const-name (php-resolve-qualified-name qualified-name :const)))
            (if (eq (php-peek-type rest) :T-LPAREN)
                (multiple-value-bind (call rest2 kv2)
                    (%php-parse-function-call qualified-name
                                              (php-ident-sym
                                               (php-resolve-qualified-name qualified-name :function))
                                              rest known-vars)
                  (values call rest2 kv2))
                ;; Bare identifier: dynamic predefined constants lower to their
                ;; runtime helper; literal predefined constants lower to values.
                ;; Anything else stays an ast-var so user define()/const values
                ;; still resolve at runtime.
                (multiple-value-bind (helper dynamic-found)
                    (%php-lookup-dynamic-constant const-name)
                  (if dynamic-found
                      (values (make-ast-call :func (make-ast-var :name helper) :args nil)
                              rest known-vars)
                      (multiple-value-bind (value found) (%php-lookup-constant const-name)
                        (if found
                            (values (make-ast-quote :value value) rest known-vars)
                            (values (make-ast-var :name (php-ident-sym const-name))
                                    rest known-vars)))))))))
      ;; self:: / static:: / parent:: — class-relative static access.  These lex
      ;; as T-TYPE keywords (:SELF/:STATIC/:PARENT).  Only meaningful immediately
      ;; before :: ; resolve to the enclosing class object (a global var) so the
      ;; postfix :: handler does the member lookup.  self/static → current class,
      ;; parent → its first superclass.
      ((and (eq type :T-TYPE)
            (member (php-peek-value stream) '(:SELF :STATIC :PARENT) :test #'eq))
       (multiple-value-bind (tok rest) (php-consume stream)
         (if (eq (php-peek-type rest) :T-DOUBLE-COLON)
             (let* ((kw (php-tok-value tok))
                    (cls (if (eq kw :PARENT)
                             (first *php-current-supers*)
                             *php-current-class*)))
               (if cls
                   (values (make-ast-var :name cls) rest known-vars)
                   (error "PHP parse error: ~A:: used outside of a~:[ class~; subclass~] context"
                          kw (eq kw :PARENT))))
             (error "PHP parse error: unexpected token ~S in expression" (php-peek stream)))))
      (t (error "PHP parse error: unexpected token ~S in expression" (php-peek stream))))))
