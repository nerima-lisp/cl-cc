(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest-each php-parser-operator-helper-lowering
  "Binary/unary operators lower to named PHP helper functions via %php-first-binding-value."
  :cases (("modulo"         "<?php $r = 7 % 4;"         "%PHP-MODULO")
          ("bitwise-not"    "<?php $r = ~1;"             "%PHP-BITWISE-NOT")
          ("unary-plus"     "<?php $r = +'7';"           "%PHP-UNARY-PLUS")
          ("unary-minus"    "<?php $r = -'7';"           "%PHP-UNARY-MINUS")
          ("spaceship"      "<?php $r = $a <=> $b;"      "%PHP-SPACESHIP")
          ("str-interp"     "<?php $s = \"Hello $name\";" "%PHP-CONCAT")
          ("braced-interp"  "<?php $s = \"Hello {$name}\";" "%PHP-CONCAT")
          ("array-ref"      "<?php $x = $a[0];"          "%PHP-ARRAY-REF")
          ("bitwise-and"    "<?php $x = $a & $b;"        "%PHP-BITWISE-AND"))
  (src expected-fn)
  (let ((val (%php-first-binding-value src)))
    (assert-true (cl-cc:ast-call-p val))
    (assert-string= expected-fn (%php-call-name val))))

(deftest php-parser-exponentiation-is-right-associative
  "** parses above unary and associates to the right."
  (let ((value (%php-first-binding-value "<?php $result = 2 ** 3 ** 2;")))
    (assert-true (cl-cc:ast-call-p value))
    (assert-string= "EXPT" (%php-call-name value))
    (assert-true (cl-cc:ast-call-p (second (cl-cc:ast-call-args value))))
    (assert-string= "EXPT" (%php-call-name (second (cl-cc:ast-call-args value))))))

(deftest php-parser-shift-operators-lower-to-helpers
  "<< and >> lower to PHP shift helpers."
  (let ((left (%php-first-binding-value "<?php $result = 1 << 3;"))
        (right (%php-first-binding-value "<?php $result = 8 >> 1;")))
    (assert-string= "%PHP-SHIFT-LEFT" (%php-call-name left))
    (assert-string= "%PHP-SHIFT-RIGHT" (%php-call-name right))))

(deftest php-parser-shift-precedence-is-below-addition
  "Addition binds tighter than shifts in PHP 8.x.  (+ now lowers to a %php-add
helper call — operand-coercing — so the shift's left operand is that call.)"
  (let ((value (%php-first-binding-value "<?php $result = 1 + 2 << 3;")))
    (assert-string= "%PHP-SHIFT-LEFT" (%php-call-name value))
    (assert-string= "%PHP-ADD" (%php-call-name (first (cl-cc:ast-call-args value))))))

(deftest php-parser-concat-precedence-is-below-addition
  "String concatenation binds looser than + and - in PHP 8.x."
  (let ((value (%php-first-binding-value "<?php $result = 1 + 2 . 3;")))
    (assert-string= "%PHP-CONCAT" (%php-call-name value))
    (assert-string= "%PHP-ADD" (%php-call-name (first (cl-cc:ast-call-args value))))))

(deftest php-parser-bitwise-operators-lower-to-helpers
  "&, ^, and | lower to PHP bitwise helpers."
  (let ((and-value (%php-first-binding-value "<?php $result = 6 & 3;"))
        (xor-value (%php-first-binding-value "<?php $result = 6 ^ 3;"))
        (or-value (%php-first-binding-value "<?php $result = 4 | 1;")))
    (assert-string= "%PHP-BITWISE-AND" (%php-call-name and-value))
    (assert-string= "%PHP-BITWISE-XOR" (%php-call-name xor-value))
    (assert-string= "%PHP-BITWISE-OR" (%php-call-name or-value))))

(deftest php-parser-bitwise-precedence-follows-php-order
  "Comparison binds above &, which binds above ^, which binds above |."
  (let ((value (%php-first-binding-value "<?php $result = 1 == 1 & 6 ^ 3 | 8;")))
    (assert-string= "%PHP-BITWISE-OR" (%php-call-name value))
    (let ((xor-node (first (cl-cc:ast-call-args value))))
      (assert-string= "%PHP-BITWISE-XOR" (%php-call-name xor-node))
      (let ((and-node (first (cl-cc:ast-call-args xor-node))))
        (assert-string= "%PHP-BITWISE-AND" (%php-call-name and-node))
        ;; == lowers to a %php-eq-loose call (PHP loose-equality type juggling),
        ;; and binds tighter than &, so it is the AND node's first operand.
        (let ((eq-node (first (cl-cc:ast-call-args and-node))))
          (assert-true (cl-cc:ast-call-p eq-node))
          (assert-string= "%PHP-EQ-LOOSE" (%php-call-name eq-node)))))))

(deftest php-parser-arrow-function-expression
  "Characterization: fn($x) => $x + 1 should parse to a capture-wrapped ast-lambda."
  (let ((value (%php-first-binding-value "<?php $inc = fn($x) => $x + 1;")))
    ;; fn arrow functions wrap the lambda in a capture let-binding
    (assert-true (cl-cc:ast-let-p value))
    (let ((lambda (first (cl-cc:ast-let-body value))))
      (assert-true (cl-cc:ast-lambda-p lambda))
      (assert-equal '("x") (mapcar #'symbol-name (cl-cc:ast-lambda-params lambda))))))

(defun %php-generator-body-block (ast)
  "For a yield-containing function AST, return the inner (block nil ...) that
%php-callable-body wraps. The generator lowering is:
  (let ((gen (%php-generator-enter)))
    (%php-generator-exit gen <block>)
    gen)
so the block is the second arg of the %php-generator-exit call."
  (let* ((let-form  (first (cl-cc:ast-defun-body ast)))
         (exit-call (first (cl-cc:ast-let-body let-form))))
    (second (cl-cc:ast-call-args exit-call))))

(deftest php-parser-yield-expression-lowering
  "A function with yield becomes a generator: its body is threaded through
%php-generator-enter / -exit, and yield lowers to the %php-yield helper."
  (let* ((ast (%php-first "<?php function g() { yield 1; }"))
         (let-form (first (cl-cc:ast-defun-body ast)))
         (enter-call (cdr (first (cl-cc:ast-let-bindings let-form))))
         (block (%php-generator-body-block ast))
         (yield-call (first (cl-cc:ast-block-body block))))
    (cl-cc/php:php-check-supported-forms (list ast))
    (assert-true (cl-cc:ast-let-p let-form))
    (assert-string= "%PHP-GENERATOR-ENTER" (%php-call-name enter-call))
    (assert-true (cl-cc:ast-block-p block))
    (assert-true (cl-cc:ast-call-p yield-call))
    (assert-string= "%PHP-YIELD" (%php-call-name yield-call))))

(deftest php-parser-yield-from-expression-lowering
  "yield from lowers to the %php-yield-from helper inside the generator body."
  (let* ((ast (%php-first "<?php function g() { yield from $items; }"))
         (block (%php-generator-body-block ast))
         (yield-call (first (cl-cc:ast-block-body block))))
    (cl-cc/php:php-check-supported-forms (list ast))
    (assert-true (cl-cc:ast-block-p block))
    (assert-true (cl-cc:ast-call-p yield-call))
    (assert-string= "%PHP-YIELD-FROM" (%php-call-name yield-call))))

(deftest php-parser-pipe-operator-lowers-to-helper-call
  "PHP 8.5 pipe syntax lowers through the parser to the runtime pipe helper."
  (let ((ast (%php-first "<?php \"  HI  \" |> trim(...);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-eq 'cl-cc/php::%php-pipe
               (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))
    (assert-= 2 (length (cl-cc:ast-call-args ast)))
    (assert-true (cl-cc:ast-lambda-p (second (cl-cc:ast-call-args ast))))))

(deftest php-parser-void-cast-lowers-to-progn
  "PHP 8.5 (void) statements parse as a progn that discards the value and returns null."
  (let ((value (%php-first "<?php (void) foo();")))
    (assert-true (cl-cc:ast-progn-p value))
    (assert-= 2 (length (cl-cc:ast-progn-forms value)))
    (assert-true (cl-cc:ast-call-p (first (cl-cc:ast-progn-forms value))))
    (assert-true (cl-cc:ast-quote-p (second (cl-cc:ast-progn-forms value))))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc:ast-quote-value (second (cl-cc:ast-progn-forms value))))))

(deftest php-parser-void-cast-is-statement-only
  "PHP 8.5 (void) is statement syntax, not a general expression."
  (assert-signals error
    (cl-cc/php:parse-php-source "<?php $x = (void) foo();")))

(deftest php-parser-scalar-casts-lower-to-runtime-helpers
  "PHP cast expressions lower to the runtime conversion helpers."
  (flet ((cast-call-name (src)
           (%php-call-name (%php-first-binding-value src))))
    (assert-string= "%PHP-INTVAL" (cast-call-name "<?php $x = (int) \"42\";"))
    (assert-string= "%PHP-STRVAL" (cast-call-name "<?php $x = (string) 7;"))
    (assert-string= "%PHP-FLOATVAL" (cast-call-name "<?php $x = (float) \"1.5\";"))
    (assert-string= "%PHP-BOOLVAL" (cast-call-name "<?php $x = (bool) \"x\";"))))

(deftest php-parser-cast-aliases-lower-to-canonical-runtime-helpers
  "PHP 8.5-deprecated cast aliases warn, then lower to canonical conversion helpers."
  (labels ((cast-progn (src)
             (let ((value (%php-first-binding-value src)))
               (assert-true (cl-cc:ast-progn-p value))
               (assert-= 2 (length (cl-cc:ast-progn-forms value)))
               value))
           (cast-call-name (src)
             (let* ((forms (cl-cc:ast-progn-forms (cast-progn src)))
                    (warn (first forms))
                    (cast (second forms))
                    (args (cl-cc:ast-call-args warn)))
               (assert-string= "%PHP-TRIGGER-ERROR" (%php-call-name warn))
               (assert-= 2 (length args))
               (assert-true (cl-cc:ast-int-p (second args)))
               (assert-= 8192 (cl-cc:ast-int-value (second args)))
               (%php-call-name cast))))
    (assert-string= "%PHP-INTVAL" (cast-call-name "<?php $x = (integer) \"42\";"))
    (assert-string= "%PHP-BOOLVAL" (cast-call-name "<?php $x = (boolean) \"x\";"))
    (assert-string= "%PHP-FLOATVAL" (cast-call-name "<?php $x = (double) \"1.5\";"))
    (assert-string= "%PHP-STRVAL" (cast-call-name "<?php $x = (binary) 7;"))))

(deftest php-parser-array-and-object-casts-lower-to-runtime-helpers
  "PHP array and object cast expressions lower to the runtime conversion helpers."
  (flet ((cast-call-name (src)
           (%php-call-name (%php-first-binding-value src))))
    (assert-string= "%PHP-SETTYPE-ARRAY-VALUE" (cast-call-name "<?php $x = (array) 7;"))
    (assert-string= "%PHP-SETTYPE-OBJECT-VALUE" (cast-call-name "<?php $x = (object) [\"x\" => 1];"))))

(deftest php-parser-clone-function-accepts-single-argument
  "PHP 8.5 clone($object) function-style syntax lowers to the clone helper."
  (let* ((value (%php-first-binding-value "<?php $b = clone($a);"))
         (clone-call (cdr (first (cl-cc:ast-let-bindings value)))))
    (assert-true (cl-cc:ast-let-p value))
    (assert-string= "%PHP-CLONE" (%php-call-name clone-call))))

(deftest php-parser-clone-with-lowers-to-helper-call
  "PHP 8.5 clone-with syntax lowers through the parser to the clone-with helper."
  (let* ((value (%php-first-binding-value "<?php $b = clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-call-p with-call))
    (assert-eq 'cl-cc/php::%php-clone-with
               (cl-cc:ast-var-name (cl-cc:ast-call-func with-call)))))

(deftest php-parser-qualified-clone-function-accepts-single-argument
  "PHP 8.5 \\clone($object) fully qualified syntax lowers to the clone helper."
  (let* ((value (%php-first-binding-value "<?php $b = \\clone($a);"))
         (clone-call (cdr (first (cl-cc:ast-let-bindings value)))))
    (assert-true (cl-cc:ast-let-p value))
    (assert-string= "%PHP-CLONE" (%php-call-name clone-call))))

(deftest php-parser-qualified-clone-with-lowers-to-helper-call
  "PHP 8.5 \\clone($object, overrides) lowers to the clone-with helper."
  (let* ((value (%php-first-binding-value "<?php $b = \\clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-call-p with-call))
    (assert-eq 'cl-cc/php::%php-clone-with
               (cl-cc:ast-var-name (cl-cc:ast-call-func with-call)))))

(deftest-each php-parser-call-syntax-variants
  "Modern PHP call syntax variants parse without error."
  ;; foo(...$args) lowers to an apply over a runtime-spread argument list, so it
  ;; is an ast-apply (not an ast-call). The named-arg variants stay ast-call.
  :cases (("spread-arg"    "<?php foo(...$args);"                #'cl-cc:ast-apply-p)
          ("named-args"    "<?php foo(name: 'x', age: 5);"      #'cl-cc:ast-call-p)
          ("named-mixed"   "<?php foo('pos', name: 'x');"       #'cl-cc:ast-call-p))
  (src pred)
  (assert-true (funcall pred (%php-first src))))

(deftest php-parser-named-args-after-dynamic-spread
  "Named arguments after dynamic spread lower to apply instead of an unsupported
parser error."
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php function f($a,$b,$c) { return $c; } f(...$args, c: 3);"))
         (call (second asts)))
    (assert-true (cl-cc:ast-apply-p call))))

(deftest php-parser-named-argument-metadata-is-source-local
  "Named-argument parameter metadata from one parse must not affect later sources."
  (cl-cc/php:parse-php-source
   "<?php function foo($value) { return $value; } echo foo(value: 'x');")
  (let ((ast (%php-first "<?php foo(name: 'x');")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-NAMED-ARG"
                    (%php-call-name (first (cl-cc:ast-call-args ast))))))

(deftest php-parser-first-class-callable
  "strlen(...) parses as a first-class callable reference."
  (let ((ast (%php-first "<?php $f = strlen(...);")))
    ;; assignment lowers to ast-let/ast-setq with a call value; just ensure it parsed.
    (assert-true ast)))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
