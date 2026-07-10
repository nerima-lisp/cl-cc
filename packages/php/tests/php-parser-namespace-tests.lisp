(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest php-parser-namespace-use-metadata-preservation
  "namespace/use declarations annotate subsequent top-level AST nodes."
  (let ((asts (cl-cc/php:parse-php-source "<?php namespace App\\Lib; use Vendor\\Thing as Thing; function f() { return 1; }")))
    (assert-= 1 (length asts))
    (assert-string= "App\\Lib" (cl-cc:ast-namespace (first asts)))
    (assert-equal '((:type :class :name "Vendor\\Thing" :alias "Thing"))
                  (cl-cc:ast-imports (first asts)))))

(deftest php-parser-qualified-name-rejects-single-token-namespace-separators
  "Qualified names require explicit T-BACKSLASH separator tokens."
  (assert-signals error
    (cl-cc/php::php-parse-qualified-name
     (list (cl-cc/php::make-php-token :T-IDENT "Vendor\\Thing")))))

(deftest php-parser-braced-namespace-and-group-use-metadata
  "Braced namespaces and grouped function/const imports annotate enclosed forms."
  (let ((asts (cl-cc/php:parse-php-source
               "<?php namespace App\\Lib { use function Vendor\\Fns\\{foo, bar as baz}; function f() { return 1; } class C {} }")))
    (assert-= 2 (length asts))
    (assert-true (every (lambda (ast) (string= "App\\Lib" (cl-cc:ast-namespace ast))) asts))
    (assert-equal '((:type :function :name "Vendor\\Fns\\foo" :alias nil)
                    (:type :function :name "Vendor\\Fns\\bar" :alias "baz"))
                  (cl-cc:ast-imports (first asts)))
    (assert-equal (cl-cc:ast-imports (first asts))
                  (cl-cc:ast-imports (second asts)))))

(defun %php-new-make-instance (value)
  "Extract the ast-make-instance from a `new C(...)' lowering. The lowering wraps
it in (let ((inst (make-instance C))) (if (has __construct) ...) inst), so the
make-instance is the first binding's value; falls back to VALUE itself."
  (if (cl-cc:ast-make-instance-p value)
      value
      (cdr (first (cl-cc:ast-let-bindings value)))))

(deftest php-parser-clone-lowers-to-runtime-helper
  "clone $obj lowers to a shallow-copy helper followed by an optional __clone call."
  (let* ((value (%php-first-binding-value "<?php $b = clone $a;"))
         (copy-call (cdr (first (cl-cc:ast-let-bindings value))))
         (body (cl-cc:ast-let-body value)))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-call-p copy-call))
    (assert-eq 'cl-cc/php::%php-clone
               (cl-cc:ast-var-name (cl-cc:ast-call-func copy-call)))
    (assert-true (cl-cc:ast-if-p (first body)))
    (assert-true (cl-cc:ast-var-p (second body)))))

(deftest php-parser-use-alias-resolves-new-class-name
  "A class import alias resolves `new Alias()` to the imported fully-qualified class name."
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; use Vendor\\Thing as Thing; $x = new Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (assert-true (cl-cc:ast-make-instance-p mi))
    (assert-true (cl-cc:ast-var-p class-ref))
    (assert-string= "VENDOR\\THING" (symbol-name (cl-cc:ast-var-name class-ref)))))

(deftest php-parser-default-use-alias-resolves-new-class-name
  "A class import without `as` uses the final namespace segment as its alias."
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; use Vendor\\Thing; $x = new Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (assert-string= "VENDOR\\THING" (symbol-name (cl-cc:ast-var-name class-ref)))))

(deftest php-parser-fully-qualified-new-class-name-stays-global
  "A leading namespace separator on `new` names resolves from the PHP global namespace."
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; $x = new \\Vendor\\Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (assert-string= "VENDOR\\THING" (symbol-name (cl-cc:ast-var-name class-ref)))))

(deftest php-parser-namespace-resolves-relative-class-declaration-and-ancestry
  "Namespaced class declarations and relative ancestry names are resolved consistently."
  (let* ((ast (%php-first "<?php namespace App\\Lib; class Box extends Base implements Iface {}"))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-string= "APP\\LIB\\BOX" (symbol-name (cl-cc:ast-defclass-name ast)))
    (assert-equal '("APP\\LIB\\BASE" "APP\\LIB\\IFACE") supers)))

(deftest php-parser-class-ancestry-resolves-imports-and-absolute-names
  "Class ancestry resolves imported aliases and absolute names without namespace prefixing."
  (let* ((ast (%php-first "<?php namespace App\\Lib; use Vendor\\Base; class Box extends Base implements \\Contracts\\Iface {}"))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-equal '("VENDOR\\BASE" "CONTRACTS\\IFACE") supers)))

(deftest php-parser-function-import-alias-resolves-call-name
  "Function imports resolve unqualified and aliased function call names.
After php-finish-let-bindings, $x let wraps $y let in its body."
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php namespace App\\Lib; use function Vendor\\Fns\\{foo, bar as baz}; $x = foo(); $y = baz();"))
         (let-x       (first asts))
         (let-y       (first (cl-cc:ast-let-body let-x)))
         (first-call  (cdr (first (cl-cc:ast-let-bindings let-x))))
         (second-call (cdr (first (cl-cc:ast-let-bindings let-y)))))
    (assert-string= "VENDOR\\FNS\\FOO"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func first-call))))
    (assert-string= "VENDOR\\FNS\\BAR"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func second-call))))))

(deftest php-parser-function-import-overrides-builtin-name
  "A function import named like a PHP builtin must not lower to the builtin helper."
  (let* ((call (%php-first-binding-value
                "<?php namespace App\\Lib; use function Vendor\\Fns\\count; $x = count($items);")))
    (assert-string= "VENDOR\\FNS\\COUNT"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-function-import-alias-overrides-builtin-name
  "A function import alias named like a PHP builtin must keep the imported target."
  (let* ((call (%php-first-binding-value
                "<?php namespace App\\Lib; use function Vendor\\Fns\\strlen as count; $x = count($items);")))
    (assert-string= "VENDOR\\FNS\\STRLEN"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-unqualified-function-call-keeps-global-fallback-name
  "Unimported unqualified function calls in a namespace keep their fallback-safe bare name."
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = helper();")))
    (assert-string= "HELPER"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-qualified-function-call-resolves-relative-to-namespace
  "Qualified relative function calls are namespace-relative in PHP."
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = Tools\\helper();")))
    (assert-string= "APP\\LIB\\TOOLS\\HELPER"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-fully-qualified-function-call-stays-global
  "A leading namespace separator on function calls resolves from the PHP global namespace."
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = \\Vendor\\Fns\\foo();")))
    (assert-string= "VENDOR\\FNS\\FOO"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))))))

(deftest php-parser-unqualified-constant-keeps-global-fallback-name
  "Unimported unqualified constants in a namespace keep their fallback-safe bare name."
  (let ((value (%php-first-binding-value "<?php namespace App\\Lib; $x = SOME_CONST;")))
    (assert-true (cl-cc:ast-var-p value))
    (assert-string= "SOME_CONST" (symbol-name (cl-cc:ast-var-name value)))))

(deftest php-parser-qualified-constant-resolves-relative-to-namespace
  "Qualified relative constants are namespace-relative in PHP."
  (let ((value (%php-first-binding-value "<?php namespace App\\Lib; $x = Config\\VALUE;")))
    (assert-true (cl-cc:ast-var-p value))
    (assert-string= "APP\\LIB\\CONFIG\\VALUE" (symbol-name (cl-cc:ast-var-name value)))))

(deftest php-parser-qualified-catch-types-resolve-imports-and-absolute-names
  "Catch union types resolve imported aliases and fully-qualified names."
  (let* ((ast (%php-first "<?php namespace App\\Lib; use Vendor\\Ex; try { throw new Ex(); } catch (Ex | \\Other\\Alt $e) { echo $e; }"))
         (inner (cl-cc:ast-unwind-protected ast))
         (top-dispatch (first (cl-cc:ast-let-body inner)))
         (catch-dispatch (cl-cc:ast-if-then top-dispatch))
         (match-cond (cl-cc:ast-if-cond catch-dispatch))
         (class-arg (second (cl-cc:ast-call-args match-cond))))
    (assert-equal '("VENDOR\\EX" "OTHER\\ALT")
                  (mapcar #'symbol-name (cl-cc:ast-quote-value class-arg)))))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
