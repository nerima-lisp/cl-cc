(in-package :cl-cc/test)

(it-sequential "php-parser-operator-helper-lowering modulo"
  (destructuring-bind (src expected-fn) (list "<?php $r = 7 % 4;" "%PHP-MODULO")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering bitwise-not"
  (destructuring-bind (src expected-fn) (list "<?php $r = ~1;" "%PHP-BITWISE-NOT")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering unary-plus"
  (destructuring-bind (src expected-fn) (list "<?php $r = +'7';" "%PHP-UNARY-PLUS")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering unary-minus"
  (destructuring-bind (src expected-fn) (list "<?php $r = -'7';" "%PHP-UNARY-MINUS")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering spaceship"
  (destructuring-bind (src expected-fn) (list "<?php $r = $a <=> $b;" "%PHP-SPACESHIP")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering str-interp"
  (destructuring-bind (src expected-fn) (list "<?php $s = \"Hello $name\";" "%PHP-CONCAT")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering braced-interp"
  (destructuring-bind (src expected-fn) (list "<?php $s = \"Hello {$name}\";" "%PHP-CONCAT")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering array-ref"
  (destructuring-bind (src expected-fn) (list "<?php $x = $a[0];" "%PHP-ARRAY-REF")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-operator-helper-lowering bitwise-and"
  (destructuring-bind (src expected-fn) (list "<?php $x = $a & $b;" "%PHP-BITWISE-AND")
    (let ((val (%php-first-binding-value src)))
    (expect (cl-cc:ast-call-p val) :to-be-truthy)
    (expect (%php-call-name val) :to-equal expected-fn))))

(it-sequential "php-parser-exponentiation-is-right-associative"
  (let ((value (%php-first-binding-value "<?php $result = 2 ** 3 ** 2;")))
    (expect (cl-cc:ast-call-p value) :to-be-truthy)
    (expect (%php-call-name value) :to-equal "EXPT")
    (expect (cl-cc:ast-call-p (second (cl-cc:ast-call-args value))) :to-be-truthy)
    (expect (%php-call-name (second (cl-cc:ast-call-args value))) :to-equal "EXPT")))

(it-sequential "php-parser-shift-operators-lower-to-helpers"
  (let ((left (%php-first-binding-value "<?php $result = 1 << 3;"))
        (right (%php-first-binding-value "<?php $result = 8 >> 1;")))
    (expect (%php-call-name left) :to-equal "%PHP-SHIFT-LEFT")
    (expect (%php-call-name right) :to-equal "%PHP-SHIFT-RIGHT")))

(it-sequential "php-parser-shift-precedence-is-below-addition"
  (let ((value (%php-first-binding-value "<?php $result = 1 + 2 << 3;")))
    (expect (%php-call-name value) :to-equal "%PHP-SHIFT-LEFT")
    (expect (%php-call-name (first (cl-cc:ast-call-args value))) :to-equal "%PHP-ADD")))

(it-sequential "php-parser-concat-precedence-is-below-addition"
  (let ((value (%php-first-binding-value "<?php $result = 1 + 2 . 3;")))
    (expect (%php-call-name value) :to-equal "%PHP-CONCAT")
    (expect (%php-call-name (first (cl-cc:ast-call-args value))) :to-equal "%PHP-ADD")))

(it-sequential "php-parser-bitwise-operators-lower-to-helpers"
  (let ((and-value (%php-first-binding-value "<?php $result = 6 & 3;"))
        (xor-value (%php-first-binding-value "<?php $result = 6 ^ 3;"))
        (or-value (%php-first-binding-value "<?php $result = 4 | 1;")))
    (expect (%php-call-name and-value) :to-equal "%PHP-BITWISE-AND")
    (expect (%php-call-name xor-value) :to-equal "%PHP-BITWISE-XOR")
    (expect (%php-call-name or-value) :to-equal "%PHP-BITWISE-OR")))

(it-sequential "php-parser-bitwise-precedence-follows-php-order"
  (let ((value (%php-first-binding-value "<?php $result = 1 == 1 & 6 ^ 3 | 8;")))
    (expect (%php-call-name value) :to-equal "%PHP-BITWISE-OR")
    (let ((xor-node (first (cl-cc:ast-call-args value))))
      (expect (%php-call-name xor-node) :to-equal "%PHP-BITWISE-XOR")
      (let ((and-node (first (cl-cc:ast-call-args xor-node))))
        (expect (%php-call-name and-node) :to-equal "%PHP-BITWISE-AND")
        ;; == lowers to a %php-eq-loose call (PHP loose-equality type juggling),
        ;; and binds tighter than &, so it is the AND node's first operand.
        (let ((eq-node (first (cl-cc:ast-call-args and-node))))
          (expect (cl-cc:ast-call-p eq-node) :to-be-truthy)
          (expect (%php-call-name eq-node) :to-equal "%PHP-EQ-LOOSE"))))))

(it-sequential "php-parser-arrow-function-expression"
  (let ((value (%php-first-binding-value "<?php $inc = fn($x) => $x + 1;")))
    ;; fn arrow functions wrap the lambda in a capture let-binding
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (let ((lambda (first (cl-cc:ast-let-body value))))
      (expect (cl-cc:ast-lambda-p lambda) :to-be-truthy)
      (expect (mapcar #'symbol-name (cl-cc:ast-lambda-params lambda)) :to-equal '("x")))))

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

(it-sequential "php-parser-yield-expression-lowering"
  (let* ((ast (%php-first "<?php function g() { yield 1; }"))
         (let-form (first (cl-cc:ast-defun-body ast)))
         (enter-call (cdr (first (cl-cc:ast-let-bindings let-form))))
         (block (%php-generator-body-block ast))
         (yield-call (first (cl-cc:ast-block-body block))))
    (cl-cc/php:php-check-supported-forms (list ast))
    (expect (cl-cc:ast-let-p let-form) :to-be-truthy)
    (expect (%php-call-name enter-call) :to-equal "%PHP-GENERATOR-ENTER")
    (expect (cl-cc:ast-block-p block) :to-be-truthy)
    (expect (cl-cc:ast-call-p yield-call) :to-be-truthy)
    (expect (%php-call-name yield-call) :to-equal "%PHP-YIELD")))

(it-sequential "php-parser-yield-from-expression-lowering"
  (let* ((ast (%php-first "<?php function g() { yield from $items; }"))
         (block (%php-generator-body-block ast))
         (yield-call (first (cl-cc:ast-block-body block))))
    (cl-cc/php:php-check-supported-forms (list ast))
    (expect (cl-cc:ast-block-p block) :to-be-truthy)
    (expect (cl-cc:ast-call-p yield-call) :to-be-truthy)
    (expect (%php-call-name yield-call) :to-equal "%PHP-YIELD-FROM")))

(it-sequential "php-parser-pipe-operator-lowers-to-helper-call"
  (let ((ast (%php-first "<?php \"  HI  \" |> trim(...);")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (cl-cc:ast-var-name (cl-cc:ast-call-func ast)) :to-be 'cl-cc/php::%php-pipe)
    (expect (= 2 (length (cl-cc:ast-call-args ast))) :to-be-truthy)
    (expect (cl-cc:ast-lambda-p (second (cl-cc:ast-call-args ast))) :to-be-truthy)))

(it-sequential "php-parser-void-cast-lowers-to-progn"
  (let ((value (%php-first "<?php (void) foo();")))
    (expect (cl-cc:ast-progn-p value) :to-be-truthy)
    (expect (= 2 (length (cl-cc:ast-progn-forms value))) :to-be-truthy)
    (expect (cl-cc:ast-call-p (first (cl-cc:ast-progn-forms value))) :to-be-truthy)
    (expect (cl-cc:ast-quote-p (second (cl-cc:ast-progn-forms value))) :to-be-truthy)
    (expect (cl-cc:ast-quote-value (second (cl-cc:ast-progn-forms value))) :to-be cl-cc/php:+php-null+)))

(it-sequential "php-parser-void-cast-is-statement-only"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/php:parse-php-source "<?php $x = (void) foo();")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "php-parser-scalar-casts-lower-to-runtime-helpers"
  (flet ((cast-call-name (src)
           (%php-call-name (%php-first-binding-value src))))
    (expect (cast-call-name "<?php $x = (int) \"42\";") :to-equal "%PHP-INTVAL")
    (expect (cast-call-name "<?php $x = (string) 7;") :to-equal "%PHP-STRVAL")
    (expect (cast-call-name "<?php $x = (float) \"1.5\";") :to-equal "%PHP-FLOATVAL")
    (expect (cast-call-name "<?php $x = (bool) \"x\";") :to-equal "%PHP-BOOLVAL")))

(it-sequential "php-parser-cast-aliases-lower-to-canonical-runtime-helpers"
  (labels ((cast-progn (src)
             (let ((value (%php-first-binding-value src)))
               (expect (cl-cc:ast-progn-p value) :to-be-truthy)
               (expect (= 2 (length (cl-cc:ast-progn-forms value))) :to-be-truthy)
               value))
           (cast-call-name (src)
             (let* ((forms (cl-cc:ast-progn-forms (cast-progn src)))
                    (warn (first forms))
                    (cast (second forms))
                    (args (cl-cc:ast-call-args warn)))
               (expect (%php-call-name warn) :to-equal "%PHP-TRIGGER-ERROR")
               (expect (= 2 (length args)) :to-be-truthy)
               (expect (cl-cc:ast-int-p (second args)) :to-be-truthy)
               (expect (= 8192 (cl-cc:ast-int-value (second args))) :to-be-truthy)
               (%php-call-name cast))))
    (expect (cast-call-name "<?php $x = (integer) \"42\";") :to-equal "%PHP-INTVAL")
    (expect (cast-call-name "<?php $x = (boolean) \"x\";") :to-equal "%PHP-BOOLVAL")
    (expect (cast-call-name "<?php $x = (double) \"1.5\";") :to-equal "%PHP-FLOATVAL")
    (expect (cast-call-name "<?php $x = (binary) 7;") :to-equal "%PHP-STRVAL")))

(it-sequential "php-parser-array-and-object-casts-lower-to-runtime-helpers"
  (flet ((cast-call-name (src)
           (%php-call-name (%php-first-binding-value src))))
    (expect (cast-call-name "<?php $x = (array) 7;") :to-equal "%PHP-SETTYPE-ARRAY-VALUE")
    (expect (cast-call-name "<?php $x = (object) [\"x\" => 1];") :to-equal "%PHP-SETTYPE-OBJECT-VALUE")))

(it-sequential "php-parser-clone-function-accepts-single-argument"
  (let* ((value (%php-first-binding-value "<?php $b = clone($a);"))
         (clone-call (cdr (first (cl-cc:ast-let-bindings value)))))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (%php-call-name clone-call) :to-equal "%PHP-CLONE")))

(it-sequential "php-parser-clone-with-lowers-to-helper-call"
  (let* ((value (%php-first-binding-value "<?php $b = clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (cl-cc:ast-call-p with-call) :to-be-truthy)
    (expect (cl-cc:ast-var-name (cl-cc:ast-call-func with-call)) :to-be 'cl-cc/php::%php-clone-with)))

(it-sequential "php-parser-qualified-clone-function-accepts-single-argument"
  (let* ((value (%php-first-binding-value "<?php $b = \\clone($a);"))
         (clone-call (cdr (first (cl-cc:ast-let-bindings value)))))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (%php-call-name clone-call) :to-equal "%PHP-CLONE")))

(it-sequential "php-parser-qualified-clone-with-lowers-to-helper-call"
  (let* ((value (%php-first-binding-value "<?php $b = \\clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (cl-cc:ast-call-p with-call) :to-be-truthy)
    (expect (cl-cc:ast-var-name (cl-cc:ast-call-func with-call)) :to-be 'cl-cc/php::%php-clone-with)))

(it-sequential "php-parser-call-syntax-variants spread-arg"
  (destructuring-bind (src pred) (list "<?php foo(...$args);" #'cl-cc:ast-apply-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-call-syntax-variants named-args"
  (destructuring-bind (src pred) (list "<?php foo(name: 'x', age: 5);" #'cl-cc:ast-call-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-call-syntax-variants named-mixed"
  (destructuring-bind (src pred) (list "<?php foo('pos', name: 'x');" #'cl-cc:ast-call-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-named-args-after-dynamic-spread"
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php function f($a,$b,$c) { return $c; } f(...$args, c: 3);"))
         (call (second asts)))
    (expect (cl-cc:ast-apply-p call) :to-be-truthy)))

(it-sequential "php-parser-named-argument-metadata-is-source-local"
  (cl-cc/php:parse-php-source
   "<?php function foo($value) { return $value; } echo foo(value: 'x');")
  (let ((ast (%php-first "<?php foo(name: 'x');")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (%php-call-name (first (cl-cc:ast-call-args ast))) :to-equal "%PHP-NAMED-ARG")))

(it-sequential "php-parser-first-class-callable"
  (let ((ast (%php-first "<?php $f = strlen(...);")))
    ;; assignment lowers to ast-let/ast-setq with a call value; just ensure it parsed.
    (expect ast :to-be-truthy)))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
