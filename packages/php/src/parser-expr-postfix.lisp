;;;; packages/php/src/parser-expr-postfix.lisp -- Postfix parsing: member access, static access, calls, indexing, and postfix inc/dec.
;;;;
;;;; Split from parser-expr.lisp so expression domains stay independently reviewable.
(in-package :cl-cc/php)

;;; ─── Postfix ++/-- lowering ──────────────────────────────────────────────────
;;;
;;; Data-driven: OP is "++" or "--"; the arithmetic op is derived from the table.
;;; Yields the ORIGINAL value (captures it in a gensym) then mutates $var.

(defparameter *php-postfix-incdec-ops*
  '(("++" . cl-cc/php::%php-add) ("--" . cl-cc/php::%php-sub))
  "Maps postfix operator string to the PHP arithmetic helper symbol.")

(defun %php-lower-postfix-incdec (op obj &optional (var-known t))
  "Lower PHP postfix OP on OBJ: capture the old value, adjust the place by ±1, and
yield the OLD value. Handles a $variable, an object property ($o->p), and an array
element ($a[k]); other targets are returned unchanged. VAR-KNOWN applies to
simple variables: when NIL, introduce the variable from PHP's null-as-zero
increment/decrement base instead of reading an unbound Lisp variable."
  (let ((arith-helper (cdr (assoc op *php-postfix-incdec-ops* :test #'equal))))
    (flet ((plus1 (val-ast)
             (%php-call arith-helper val-ast (make-ast-int :value 1))))
      (cond
        ((ast-var-p obj)
         (let ((var-sym (ast-var-name obj)))
           (if var-known
               (let ((tmp (gensym "PHP-POSTFIX-")))
                 (make-ast-let
                  :bindings (list (cons tmp (make-ast-var :name var-sym)))
                  :body (list (make-ast-setq :var var-sym
                                             :value (plus1 (make-ast-var :name tmp)))
                              (make-ast-var :name tmp))))
               (%php-variable-init-expression-marker
                var-sym
                (plus1 (make-ast-int :value 0))
                (%php-null-quote)))))
        ;; $o->p++ : bind the receiver once, read the old slot value, write +1, then
        ;; yield the old value. Nested lets give sequential (let*) scoping.
        ((ast-slot-value-p obj)
         (let ((recv (gensym "PHP-RECV-")) (old (gensym "PHP-OLD-"))
               (prop (ast-slot-value-slot obj)))
           (make-ast-let
            :bindings (list (cons recv (ast-slot-value-object obj)))
            :body (list (make-ast-let
                         :bindings (list (cons old (make-ast-slot-value
                                                    :object (make-ast-var :name recv) :slot prop)))
                         :body (list (make-ast-set-slot-value
                                      :object (make-ast-var :name recv) :slot prop
                                      :value (plus1 (make-ast-var :name old)))
                                     (make-ast-var :name old)))))))
        ;; $a[k]++ : bind the array and key once, read the old element, write +1.
        ((%php-array-ref-call-p obj)
         (destructuring-bind (arr-expr key-expr) (ast-call-args obj)
           (let ((arr (gensym "PHP-ARR-")) (key (gensym "PHP-KEY-")) (old (gensym "PHP-OLD-")))
             (make-ast-let
              :bindings (list (cons arr arr-expr))
              :body (list (make-ast-let
                           :bindings (list (cons key key-expr))
                           :body (list (make-ast-let
                                        :bindings (list (cons old (%php-array-ref-call
                                                                   (make-ast-var :name arr)
                                                                   (make-ast-var :name key))))
                                        :body (list (%php-array-set-call
                                                     (make-ast-var :name arr)
                                                     (make-ast-var :name key)
                                                     (plus1 (make-ast-var :name old)))
                                                    (make-ast-var :name old))))))))))
        (t obj)))))

(defun %php-method-call-with-args (func method user-args &optional receiver-arg)
  "Lower PHP method call arguments, including named and spread arguments.
RECEIVER-ARG is the implicit $this argument for instance calls; named argument
ordering only applies to user-visible parameters."
  (let ((lowering-result
          (if (%php-args-have-named-p user-args)
              (%php-reorder-named-args-for-call method user-args)
              (%php-static-call-lowering-result user-args))))
    (%php-emit-lowered-call func lowering-result :receiver-arg receiver-arg)))

(defun php-parse-postfix (stream known-vars)
  "Parse postfix expressions: method calls, property access, array access."
  (multiple-value-bind (obj rest kv) (php-parse-primary stream known-vars)
    (loop
      (let ((type (php-peek-type rest)))
        (cond
          ;; -> method call or property
          ((eq type :T-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-str rest3) (%php-member-name rest2)
               (let ((prop (php-ident-sym name-str)))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       ;; Bind the receiver to a temp (evaluated once) and pass it
                       ;; as the method's implicit first argument ($this); the
                       ;; method declares $this as its first parameter.
                       (let ((recv (gensym "PHP-RECV-")))
                         (setf obj (make-ast-let
                                    :bindings (list (cons recv obj))
                                    :body (list (%php-method-call-with-args
                                                 (make-ast-slot-value
                                                  :object (make-ast-var :name recv)
                                                  :slot prop)
                                                 prop
                                                 args
                                                 (make-ast-var :name recv))))
                               rest rest4
                               kv kv4)))
                     (setf obj (make-ast-slot-value :object obj :slot prop)
                           rest rest3))))))
          ;; ?-> nullsafe
          ((eq type :T-NULLSAFE-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-str rest3) (%php-member-name rest2)
               ;; $o?->member: bind the receiver to a temp (evaluated ONCE — the
               ;; receiver may have side effects, e.g. getObj()?->x), and short-
               ;; circuit to PHP null when it is null. The null test is
               ;; %php-null-p, NOT (ast-binop := …): `=' is CL NUMERIC equality,
               ;; so (= obj nil) on an object raised "not of type NUMBER".
               (let* ((prop (php-ident-sym name-str))
                      (recv (gensym "PHP-NULLSAFE-"))
                      (recv-var (make-ast-var :name recv))
                      (null-check (%php-call 'cl-cc/php::%php-null-p recv-var))
                      (access (if (eq (php-peek-type rest3) :T-LPAREN)
                                  nil   ; computed below with the arglist
                                  (make-ast-slot-value :object recv-var :slot prop))))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       ;; method call passes the receiver as $this (first arg)
                       (setf obj (make-ast-let
                                  :bindings (list (cons recv obj))
                                  :body (list (make-ast-if
                                               :cond null-check
                                               :then (make-ast-quote :value +php-null+)
                                               :else (%php-method-call-with-args
                                                      (make-ast-slot-value
                                                       :object recv-var :slot prop)
                                                      prop
                                                      args
                                                      recv-var))))
                             rest rest4
                             kv kv4))
                     (setf obj (make-ast-let
                                :bindings (list (cons recv obj))
                                :body (list (make-ast-if
                                             :cond null-check
                                             :then (make-ast-quote :value +php-null+)
                                             :else access)))
                           rest rest3))))))
          ;; :: static member/method access. Enum built-ins lower to PHP helpers.
          ((eq type :T-DOUBLE-COLON)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-str rest3) (%php-member-name rest2)
               (let ((member (php-ident-sym name-str)))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       (setf obj (cond
                                   ((string= (symbol-name member) "CASES")
                                    (%php-call 'cl-cc/php::%php-enum-cases obj))
                                   ((string= (symbol-name member) "FROM")
                                    (%php-call 'cl-cc/php::%php-enum-from obj (first args)))
                                   ((string= (symbol-name member) "TRYFROM")
                                    (%php-call 'cl-cc/php::%php-enum-try-from obj (first args)))
                                   ((and (%php-closure-class-ast-p obj)
                                         (string= (symbol-name member) "GETCURRENT"))
                                    (when args
                                      (error "PHP parse error: Closure::getCurrent() expects no arguments"))
                                    (%php-call 'cl-cc/php::%php-current-closure))
                                   ((and (%php-locale-class-ast-p obj)
                                         (string= (symbol-name member) "ISRIGHTTOLEFT"))
                                    (unless (= (length args) 1)
                                      (error "PHP parse error: Locale::isRightToLeft() expects exactly 1 argument"))
                                    (%php-call 'cl-cc/php::%php-locale-is-right-to-left
                                               (first args)))
                                   ((and (%php-locale-class-ast-p obj)
                                         (string= (symbol-name member) "ADDLIKELYSUBTAGS"))
                                    (unless (= (length args) 1)
                                      (error "PHP parse error: Locale::addLikelySubtags() expects exactly 1 argument"))
                                    (%php-call 'cl-cc/php::%php-locale-add-likely-subtags
                                               (first args)))
                                   ((and (%php-locale-class-ast-p obj)
                                         (string= (symbol-name member) "MINIMIZESUBTAGS"))
                                    (unless (= (length args) 1)
                                      (error "PHP parse error: Locale::minimizeSubtags() expects exactly 1 argument"))
                                    (%php-call 'cl-cc/php::%php-locale-minimize-subtags
                                               (first args)))
                                   ((and (%php-uri-class-ast-kind obj)
                                         (string= (symbol-name member) "PARSE"))
                                    (when (null args)
                                      (error "PHP parse error: Uri::parse() expects at least 1 argument"))
                                    (%php-uri-parse-ast (%php-uri-class-ast-kind obj) args))
                                   ((and (%php-fiber-class-ast-p obj)
                                         (string= (symbol-name member) "SUSPEND"))
                                    (apply #'%php-call 'cl-cc/php::%php-fiber-suspend args))
                                   (t
                                    (%php-method-call-with-args
                                     (make-ast-slot-value :object obj :slot member)
                                     member
                                     args)))
                             rest rest4
                             kv kv4))
                     (setf obj (if (%php-predefined-class-constant-class-ast-p obj)
                                   (%php-call 'cl-cc/php::%php-predefined-class-constant
                                              (make-ast-quote :value (symbol-name (ast-var-name obj)))
                                              (make-ast-quote :value (symbol-name member)))
                                   (make-ast-slot-value :object obj :slot member))
                           rest rest3))))))
          ;; Postfix ++/-- — yield the ORIGINAL value, then adjust by ±1.
          ((and (eq type :T-OP) (member (php-peek-value rest) '("++" "--") :test #'equal))
           (multiple-value-bind (tok rest2) (php-consume rest)
             (let* ((var-sym (and (ast-var-p obj) (ast-var-name obj)))
                    (var-known (or (null var-sym) (member var-sym kv))))
               (setf obj (%php-lower-postfix-incdec (php-tok-value tok) obj var-known)
                     rest rest2)
               (when (and var-sym (not var-known))
                 (push var-sym kv)))))
          ;; Array access: $a[0] or $a[$i]
           ((eq type :T-LBRACKET)
            (multiple-value-bind (tok rest2) (php-consume rest)
              (declare (ignore tok))
              (if (eq (php-peek-type rest2) :T-RBRACKET)
                  ;; Empty subscript $a[] — array append target (assignment LHS).
                  (multiple-value-bind (tok2 rest3) (php-consume rest2)
                    (declare (ignore tok2))
                    (setf obj (%php-array-append-call obj)
                          rest rest3))
                  (multiple-value-bind (idx rest3 kv3) (php-parse-expr rest2 kv)
                    (multiple-value-bind (tok2 rest4) (php-expect :T-RBRACKET rest3)
                      (declare (ignore tok2))
                      (setf obj (%php-array-ref-call obj idx)
                            rest rest4
                            kv kv3))))))
           ;; Dynamic / variable call: $f(args), or a chained call such as
           ;; getCallback()(args). A named call foo(...) is already consumed in
           ;; php-parse-primary, so an LPAREN here always means "call the value of
           ;; OBJ". Lower to (call OBJ args); codegen calls the closure value (same
           ;; path JS uses for fn(args)).
           ((eq type :T-LPAREN)
            (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest kv)
              (setf obj (%php-call-with-spread obj args)
                    rest rest2
                    kv kv2)))
          (t (return)))))
    (values obj rest kv)))
