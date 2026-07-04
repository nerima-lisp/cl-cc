;;;; frontend/php/parser-expr-advanced-compound.lisp -- PHP Compound Assignment Lowering
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Compound Assignment Lowering ────────────────────────────────────────────
;;;
;;; Data table maps PHP compound-assignment operator strings to runtime helper
;;; symbols.  Lowering logic stays in %PHP-COMPOUND-VALUE.

(defparameter *php-compound-op-table*
  '(("+="  . cl-cc/php::%php-add)
    ("-="  . cl-cc/php::%php-sub)
    ("*="  . cl-cc/php::%php-mul)
    ("/="  . cl-cc/php::%php-div)
    (".="  . cl-cc/php::%php-concat)
    ("%="  . cl-cc/php::%php-modulo)
    ("**=" . expt)
    ("&="  . cl-cc/php::%php-bitwise-and)
    ("|="  . cl-cc/php::%php-bitwise-or)
    ("^="  . cl-cc/php::%php-bitwise-xor)
    ("<<=" . cl-cc/php::%php-shift-left)
    (">>=" . cl-cc/php::%php-shift-right))
  "Alist mapping PHP compound-assignment operator strings to runtime helper symbols.")

(defun %php-compound-value (op lhs rhs)
  "Return the read-modify-write value for PHP compound assignment OP."
  (let ((entry (assoc op *php-compound-op-table* :test #'equal)))
    (if entry
        (%php-call (cdr entry) lhs rhs)
        (error "PHP parse error: unsupported compound assignment operator ~S" op))))

(defun %php-nullish-cond (value)
  "Return a condition that is true when VALUE is PHP null — the inverse of
%php-not-null-cond.  Was (ast-binop :op 'or (null value) (%php-null-p value)):
codegen has no :or-binop emitter, so EVERY ??= (the only caller) failed to
compile and the whole program was dropped; and `(null value)' wrongly treated
PHP false as nullish.  A single `(eq value null)' fixes both."
  (%php-call 'eq value (%php-null-quote)))

(defun %php-compound-undefined-current (op)
  "AST for the value an UNDEFINED variable takes as the left operand of `$x OP= r'.
PHP treats the missing var as null, which coerces to 0 for arithmetic/bitwise
operators and to '' for the `.' (concat) operator."
  (cond
    ((equal op ".=") (make-ast-quote :value ""))
    (t (make-ast-int :value 0))))

(defun %php-lower-compound-assign (op lhs-expr rhs-expr target-kind &optional (var-known t))
  "Build the lowered AST for PHP compound assignment OP on LHS-EXPR.  VAR-KNOWN
applies to the :VAR kind: when NIL the variable does not yet exist, so it is
INTRODUCED (PHP treats the undefined left operand as null) rather than read +
setq'd — without this `$a += 3' (no prior $a) read an unbound $a and dropped the
whole program."
  (ecase target-kind
    (:var
     (let ((var-sym (ast-var-name lhs-expr)))
       (if var-known
           (let* ((tmp (gensym "PHP-COMPOUND-LHS-"))
                  (tmp-var (make-ast-var :name tmp)))
             (make-ast-let
              :bindings (list (cons tmp lhs-expr))
              :body (list (if (equal op "??=")
                              (make-ast-if
                               :cond (%php-nullish-cond tmp-var)
                               :then (make-ast-setq :var var-sym :value rhs-expr)
                               :else tmp-var)
                              (make-ast-setq
                               :var var-sym
                               :value (%php-compound-value op tmp-var rhs-expr))))))
           ;; Undefined var: introduce it. ??= on undefined (null) yields RHS; any
           ;; other op applies to the coerced-null left operand (0 or '').
           (make-ast-let
            :bindings (list (cons var-sym
                                  (if (equal op "??=")
                                      rhs-expr
                                      (%php-compound-value
                                       op (%php-compound-undefined-current op) rhs-expr))))
            :body nil))))
    (:array
     (destructuring-bind (array key) (ast-call-args lhs-expr)
       (let* ((array-sym (gensym "PHP-COMPOUND-ARRAY-"))
              (key-sym (gensym "PHP-COMPOUND-KEY-"))
              (array-var (make-ast-var :name array-sym))
              (key-var (make-ast-var :name key-sym))
              (current (%php-array-ref-call array-var key-var)))
         (make-ast-let
          :bindings (list (cons array-sym array)
                          (cons key-sym key))
          :body (list (if (equal op "??=")
                          (let* ((current-sym (gensym "PHP-COMPOUND-CURRENT-"))
                                 (current-var (make-ast-var :name current-sym)))
                            (make-ast-let
                             :bindings (list (cons current-sym current))
                             :body (list (make-ast-if
                                          :cond (%php-nullish-cond current-var)
                                          :then (%php-array-set-call array-var key-var rhs-expr)
                                          :else current-var))))
                          (%php-array-set-call
                           array-var key-var
                           (%php-compound-value op current rhs-expr))))))))
    (:property
     (let* ((object-sym (gensym "PHP-COMPOUND-OBJECT-"))
            (object-var (make-ast-var :name object-sym))
            (slot (ast-slot-value-slot lhs-expr))
            (current (make-ast-slot-value :object object-var :slot slot)))
       (make-ast-let
        :bindings (list (cons object-sym (ast-slot-value-object lhs-expr)))
        :body (list (if (equal op "??=")
                        (let* ((current-sym (gensym "PHP-COMPOUND-CURRENT-"))
                               (current-var (make-ast-var :name current-sym)))
                          (make-ast-let
                           :bindings (list (cons current-sym current))
                           :body (list (make-ast-if
                                        :cond (%php-nullish-cond current-var)
                                        :then (make-ast-set-slot-value
                                               :object object-var :slot slot :value rhs-expr)
                                        :else current-var))))
                        (make-ast-set-slot-value
                         :object object-var
                         :slot slot
                         :value (%php-compound-value op current rhs-expr)))))))))
