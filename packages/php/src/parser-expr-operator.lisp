;;;; packages/php/src/parser-expr-operator.lisp -- Unary, binary, ternary, assignment, and argument-list expression parsing.
;;;;
;;;; Split from parser-expr.lisp so expression domains stay independently reviewable.
(in-package :cl-cc/php)

;;; ─── Binary Op AST Builder (data-driven) ────────────────────────────────────
;;;
;;; Operators that lower to PHP runtime helpers are listed here; everything else
;;; falls through to a plain ast-binop.  Adding a new operator = one data line.

(defparameter *php-binary-op-helper-table*
  '(("."   . cl-cc/php::%php-concat)
    ;; Arithmetic + - * coerce their operands (null->0, true->1, numeric string
    ;; ->number) — lowering them to raw CL +,-,* errored on any non-number
    ;; operand (null + 3, "5" + 3). %, /, ** already route to helpers / handle
    ;; their own coercion.
    ("+"   . cl-cc/php::%php-add)
    ("-"   . cl-cc/php::%php-sub)
    ("*"   . cl-cc/php::%php-mul)
    ("/"   . cl-cc/php::%php-div)
    ("%"   . cl-cc/php::%php-modulo)
    ("<<"  . cl-cc/php::%php-shift-left)
    (">>"  . cl-cc/php::%php-shift-right)
    ("<=>" . cl-cc/php::%php-spaceship)
    ;; Relational ops must yield a PHP boolean, not the VM's integer 1/0 (which
    ;; broke gettype(5>3), (5>3)===true and match(true){$cond=>…}). %php-lt/gt/le/ge
    ;; derive from %php-spaceship for correct PHP comparison semantics.
    ("<"   . cl-cc/php::%php-lt)
    (">"   . cl-cc/php::%php-gt)
    ("<="  . cl-cc/php::%php-le)
    (">="  . cl-cc/php::%php-ge)
    ("&"   . cl-cc/php::%php-bitwise-and)
    ("^"   . cl-cc/php::%php-bitwise-xor)
    ("|"   . cl-cc/php::%php-bitwise-or)
    ;; Equality / identity. PHP == and === carry type-juggling semantics that no
    ;; single VM compare instruction expresses, so they lower to runtime helpers
    ;; rather than a plain ast-binop. Without these, (intern "==") produced an
    ;; unknown cl-cc/php::== op symbol that codegen could not emit, so EVERY
    ;; function/closure whose body used == / != / === / !== silently failed to
    ;; compile and was dropped (then "Undefined function" at the call site).
    ;; Relational ops (< > <= >=) work because (intern "<") returns the inherited
    ;; CL:< that codegen already handles.
    ("=="  . cl-cc/php::%php-eq-loose)
    ("===" . cl-cc/php::%php-eq-strict)
    ("!="  . cl-cc/php::%php-neq-loose)
    ("!==" . cl-cc/php::%php-neq-strict)
    ("|>"  . cl-cc/php::%php-pipe))
  "Alist mapping PHP binary operator strings to runtime helper symbols.")

(defun %php-binary-op-ast (op lhs rhs)
  "Lower PHP binary OP with LHS and RHS to the appropriate AST node."
  (cond
    ;; Short-circuit logical operators. PHP && / || evaluate the RHS only when the
    ;; LHS does not already decide the result, and they yield a real boolean — so
    ;; they lower to an ast-if over truthiness, NOT a binop or helper call (a helper
    ;; would eagerly evaluate both operands, breaking `$x && expensive()`). Each
    ;; branch is %php-truthy so the result is PHP true (t) / false (nil).
    ;; Without this, (intern "&&") produced an unknown cl-cc/php::&& op symbol that
    ;; codegen could not emit, so every expression using && / || failed to compile.
    ((equal op "&&")
     (make-ast-if :cond (%php-truthy-call lhs)
                  :then (%php-truthy-call rhs)
                  :else (make-ast-quote :value nil)))
    ((equal op "||")
     (make-ast-if :cond (%php-truthy-call lhs)
                  :then (make-ast-quote :value t)
                  :else (%php-truthy-call rhs)))
    (t
     (let ((helper (cdr (assoc op *php-binary-op-helper-table* :test #'equal))))
       (if helper
           (%php-call helper lhs rhs)
           (make-ast-binop :op (intern op) :lhs lhs :rhs rhs))))))

(defun php-parse-power (stream known-vars)
  "Parse PHP exponentiation. ** is right-associative and binds above unary."
  (multiple-value-bind (lhs rest kv) (php-parse-postfix stream known-vars)
    (if (and (eq (php-peek-type rest) :T-OP)
             (equal "**" (php-peek-value rest)))
        (multiple-value-bind (op-tok rest2) (php-consume rest)
          (declare (ignore op-tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-unary rest2 kv)
            (values (%php-call 'expt lhs rhs) rest3 kv3)))
        (values lhs rest kv))))

(defun php-lower-prefix-incdec (op operand &optional (var-known t))
  "Lower PHP prefix ++/-- on OPERAND, yielding the NEW value (unlike postfix,
which yields the original). OP is \"++\" or \"--\". A simple $var is set in
place; array elements ($arr[i]) and object properties ($obj->p) lower through
the compound-assign place machinery (++ ≡ += 1, -- ≡ -= 1). VAR-KNOWN applies
to simple variables and avoids reading an unbound variable on first use."
  (let ((cop (if (equal op "++") "+=" "-=")))
    (cond
      ((ast-var-p operand)
       (let ((value (%php-call (if (equal op "++")
                                   'cl-cc/php::%php-add
                                   'cl-cc/php::%php-sub)
                               (if var-known operand (make-ast-int :value 0))
                               (make-ast-int :value 1))))
         (if var-known
             (make-ast-setq :var (ast-var-name operand) :value value)
             (%php-variable-init-expression-marker
              (ast-var-name operand)
              value
              operand))))
      ((%php-array-ref-call-p operand)
       (%php-lower-compound-assign cop operand (make-ast-int :value 1) :array))
      ((ast-slot-value-p operand)
       (%php-lower-compound-assign cop operand (make-ast-int :value 1) :property))
      (t
       (%php-unsupported
        (format nil "PHP prefix ~A is only supported on a $variable, array element, or property" op)
        operand)))))

(defun %php-lower-error-suppression (expr)
  "Lower PHP @EXPR by temporarily setting error_reporting to 0."
  (let ((old-level (gensym "PHP-ERROR-REPORTING-")))
    (make-ast-let
     :bindings (list (cons old-level
                           (%php-call 'cl-cc/php::%php-error-reporting
                                      (make-ast-int :value 0))))
     :body (list
            (make-ast-unwind-protect
             :protected expr
             :cleanup (list (%php-call 'cl-cc/php::%php-error-reporting
                                       (make-ast-var :name old-level))))))))

(defun php-parse-unary (stream known-vars)
  "Parse unary expressions: casts, prefix ++/--, !, -, +, ~."
  (cond
    ((%php-cast-start-p stream)
     (let* ((cast-token (second stream))
            (kind (%php-cast-token-kind cast-token))
            (deprecated-name (%php-deprecated-cast-name cast-token)))
       (multiple-value-bind (expr rest2 kv2) (php-parse-unary (cdddr stream) known-vars)
         (values (%php-cast-ast kind expr deprecated-name) rest2 kv2))))
    ((%php-reference-token-p stream)
      ;; PHP reference operator (&expr): keep an internal marker so assignment
      ;; lowering can model $b = &$a with the same ref-box machinery used by
      ;; by-reference params and foreach.
      (multiple-value-bind (tok rest) (php-consume stream)
        (declare (ignore tok))
        (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
          (values (%php-reference-marker expr) rest2 kv2))))
    ;; Prefix ++ / -- : increment/decrement the place, then yield the new value
    ((and (eq (php-peek-type stream) :T-OP)
          (member (php-peek-value stream) '("++" "--") :test #'equal))
      (multiple-value-bind (op-tok rest) (php-consume stream)
        (multiple-value-bind (operand rest2 kv2) (php-parse-postfix rest known-vars)
          (let* ((var-sym (and (ast-var-p operand) (ast-var-name operand)))
                 (var-known (or (null var-sym) (member var-sym kv2)))
                 (new-kv (if (and var-sym (not var-known)) (cons var-sym kv2) kv2)))
            (values (php-lower-prefix-incdec (php-tok-value op-tok) operand var-known)
                    rest2 new-kv)))))
    ((and (eq (php-peek-type stream) :T-OP)
           (member (php-peek-value stream) '("!" "-" "+" "~" "@") :test #'equal))
      (multiple-value-bind (tok rest) (php-consume stream)
        (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
          (values (cond
                    ((equal "~" (php-tok-value tok))
                     (%php-call 'cl-cc/php::%php-bitwise-not expr))
                    ;; Logical NOT: yield PHP false (nil) when EXPR is truthy, else
                    ;; PHP true (t). Was lowered to a call on an undefined cl-cc/php::!
                    ;; function, so any expression with ! failed.
                    ((equal "!" (php-tok-value tok))
                     (make-ast-if :cond (%php-truthy-call expr)
                                  :then (make-ast-quote :value nil)
                                  :else (make-ast-quote :value t)))
                    ((equal "+" (php-tok-value tok))
                     (%php-call 'cl-cc/php::%php-unary-plus expr))
                    ((equal "-" (php-tok-value tok))
                     (%php-call 'cl-cc/php::%php-unary-minus expr))
                    ((equal "@" (php-tok-value tok))
                     (%php-lower-error-suppression expr)))
                  rest2 kv2))))
     (t
      (php-parse-power stream known-vars))))

(defun php-parse-binop (stream known-vars ops next-parser)
  "Left-associative binary operator parsing."
  (multiple-value-bind (lhs rest kv) (funcall next-parser stream known-vars)
    (loop
      (if (and (eq (php-peek-type rest) :T-OP)
               (member (php-peek-value rest) ops :test #'equal))
          (multiple-value-bind (op-tok rest2) (php-consume rest)
            (multiple-value-bind (rhs rest3 kv3) (funcall next-parser rest2 kv)
              (setf lhs (%php-binary-op-ast (php-tok-value op-tok) lhs rhs)
                     rest rest3
                     kv kv3)))
          (return)))
    (values lhs rest kv)))

;;; ─── Binary Operator Precedence Chain (data-driven) ─────────────────────────
;;;
;;; Each entry: (function-name (op-strings...) next-level-function)
;;; Ordered from highest to lowest precedence.  Adding a new level = one data line.

(defmacro define-php-binop-levels (&body levels)
  "Generate left-associative binary-operator parser functions from a data table.
   Each entry: (name (ops...) next-parser)."
  `(progn
     ,@(mapcar (lambda (entry)
                 (destructuring-bind (fname ops next) entry
                   `(defun ,fname (stream known-vars)
                      (php-parse-binop stream known-vars ',ops #',next))))
               levels)))

(define-php-binop-levels
  (php-parse-mul        ("*" "/" "%")               php-parse-unary)
  (php-parse-add        ("+" "-")                   php-parse-mul)
  (php-parse-pipe       ("|>")                       php-parse-add)
  (php-parse-shift      ("<<" ">>")                 php-parse-pipe)
  (php-parse-concat     (".")                        php-parse-shift)
  (php-parse-relational ("<" ">" "<=" ">=" "<=>")   php-parse-concat)
  (php-parse-cmp        ("==" "===" "!=" "!==")     php-parse-relational)
  (php-parse-bit-and    ("&")                        php-parse-cmp)
  (php-parse-bit-xor    ("^")                        php-parse-bit-and)
  (php-parse-bit-or     ("|")                        php-parse-bit-xor)
  (php-parse-and        ("&&")                       php-parse-bit-or)
  (php-parse-or         ("||")                       php-parse-and))

(defun php-parse-coalesce (stream known-vars)
  "Parse right-associative null coalescing ?? without evaluating the left side twice."
  (multiple-value-bind (lhs rest kv) (php-parse-or stream known-vars)
    (if (and (eq (php-peek-type rest) :T-OP)
             (equal "??" (php-peek-value rest)))
        (multiple-value-bind (op-tok rest2) (php-consume rest)
          (declare (ignore op-tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-coalesce rest2 kv)
            (values (%php-lower-null-coalesce lhs rhs) rest3 kv3)))
        (values lhs rest kv))))

(defun php-parse-ternary (stream known-vars)
  "Parse PHP ternary and Elvis operators."
  (multiple-value-bind (cond-expr rest kv) (php-parse-coalesce stream known-vars)
    (if (eq (php-peek-type rest) :T-NULLABLE)
        (multiple-value-bind (question-token rest2) (php-consume rest)
          (declare (ignore question-token))
          (if (eq (php-peek-type rest2) :T-COLON)
              (multiple-value-bind (colon-token rest3) (php-consume rest2)
                (declare (ignore colon-token))
                (multiple-value-bind (else-expr rest4 kv4) (php-parse-expr rest3 kv)
                  (values (%php-lower-elvis cond-expr else-expr) rest4 kv4)))
              (multiple-value-bind (then-expr rest3 kv3) (php-parse-expr rest2 kv)
                (multiple-value-bind (colon-token rest4) (php-expect :T-COLON rest3)
                  (declare (ignore colon-token))
                  (multiple-value-bind (else-expr rest5 kv5) (php-parse-expr rest4 kv3)
                    (values (make-ast-if :cond cond-expr :then then-expr :else else-expr)
                            rest5 kv5))))))
        (values cond-expr rest kv))))

(defun php-parse-expr (stream known-vars)
  "Parse an expression. Handles variable and PHP array-element assignment."
  (multiple-value-bind (lhs rest kv) (php-parse-ternary stream known-vars)
    (cond
      ((%php-assignment-op rest)
       (let ((op (%php-assignment-op rest))
             (rest2 (cdr rest)))
           (multiple-value-bind (val rest3 kv3) (php-parse-ternary rest2 kv)
            (cond
              ((ast-var-p lhs)
               (let* ((var-sym (ast-var-name lhs))
                      (already-known (member var-sym known-vars))
                      (new-kv (if already-known kv3 (cons var-sym kv3))))
                 (values
                  (if (equal op "=")
                      (if (%php-reference-marker-p val)
                          (let ((source (%php-reference-marker-expr val)))
                            (unless (ast-var-p source)
                              (error "PHP parse error: reference assignment only supports variable sources"))
                            (%php-reference-assignment-marker var-sym (ast-var-name source)))
                          (if already-known
                              (make-ast-setq :var var-sym :value val)
                              (make-ast-let :bindings (list (cons var-sym val)) :body nil)))
                      (%php-lower-compound-assign op lhs val :var already-known))
                  rest3
                  new-kv)))
              ;; $a[] = v  — append. The array is a mutable hash-table held by
              ;; reference, so pushing onto it is the whole effect (no reassign).
              ((%php-array-append-call-p lhs)
               (let ((arr (first (ast-call-args lhs))))
                 (unless (equal op "=")
                   (error "PHP parse error: ~A not allowed on [] append target" op))
                 (values (%php-call 'cl-cc/php::%php-array-push arr val) rest3 kv3)))
              ((%php-array-ref-call-p lhs)
               (destructuring-bind (arr key) (ast-call-args lhs)
                 (values (if (equal op "=")
                             (%php-array-set-call arr key val)
                             (%php-lower-compound-assign op lhs val :array))
                         rest3 kv3)))
              ((ast-slot-value-p lhs)
               (values (if (equal op "=")
                           (make-ast-set-slot-value
                            :object (ast-slot-value-object lhs)
                            :slot (ast-slot-value-slot lhs)
                            :value val)
                           (%php-lower-compound-assign op lhs val :property))
                       rest3 kv3))
              ;; List/array destructuring assignment: [$a, $b] = expr (and the
              ;; list($a, $b) = expr, which lowers to the same array LHS).
              ((and (equal op "=") (%php-array-literal-call-p lhs))
               (values (%php-lower-list-assign lhs val) rest3 kv3))
              (t
               (error "PHP parse error: unsupported assignment target ~S" lhs))))))
      ((%php-reference-token-p rest)
       ;; A `&` following a complete expression at this level is infix bitwise-AND
       ;; (the precedence chain normally consumes it; this is a defensive fallback).
       (multiple-value-bind (tok rest2) (php-consume rest)
         (declare (ignore tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-ternary rest2 kv)
           (values (%php-binary-op-ast "&" lhs rhs) rest3 kv3))))
      (t
       (values lhs rest kv)))))

(defun php-parse-arglist (stream known-vars)
  "Parse (arg1, arg2, ...). Assumes stream starts with T-LPAREN."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values nil rest2 known-vars))
        (let ((args nil)
              (current rest)
              (kv known-vars))
          (loop
            (cond
              ;; First-class callable syntax: f(...)  ->  marker arg the caller
              ;; can turn into a callable reference. Detected as '...' then ')'.
              ((and (eq (php-peek-type current) :T-ELLIPSIS)
                    (eq (php-peek-type (cdr current)) :T-RPAREN))
               (push (make-ast-call :func (make-ast-var :name '%php-first-class-callable)
                                    :args nil)
                     args)
               (setf current (cdr current)))
              ;; Spread argument: ...expr  ->  (%php-spread expr)
              ((eq (php-peek-type current) :T-ELLIPSIS)
               (multiple-value-bind (_tok rest-after) (php-consume current)
                 (declare (ignore _tok))
                 (multiple-value-bind (arg rest2 kv2) (php-parse-expr rest-after kv)
                   (push (make-ast-call :func (make-ast-var :name '%php-spread)
                                        :args (list arg))
                         args)
                   (setf current rest2 kv kv2))))
              ;; Named argument: ident: expr  ->  (%php-named-arg "ident" expr)
              ((%php-named-arg-p current)
               (let* ((name-tok (php-peek current))
                      (name-str (let ((v (php-tok-value name-tok)))
                                  (if (stringp v) v (string-downcase (symbol-name v)))))
                      (after-colon (cddr current)))   ; skip ident and ':'
                 (multiple-value-bind (arg rest2 kv2) (php-parse-expr after-colon kv)
                   (push (make-ast-call :func (make-ast-var :name '%php-named-arg)
                                        :args (list (make-ast-quote :value name-str) arg))
                         args)
                   (setf current rest2 kv kv2))))
              ;; Ordinary positional argument
              (t
               (multiple-value-bind (arg rest2 kv2) (php-parse-expr current kv)
                 (push arg args)
                 (setf current rest2 kv kv2))))
            (if (and current (eq (php-peek-type current) :T-COMMA))
                (setf current (cdr current))
                (return)))
          (multiple-value-bind (tok2 rest2) (php-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse args) rest2 kv))))))
