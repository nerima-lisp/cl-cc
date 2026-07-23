(in-package :cl-cc/test)

(it-sequential "php-parser-namespace-use-metadata-preservation"
  (let ((asts (cl-cc/php:parse-php-source "<?php namespace App\\Lib; use Vendor\\Thing as Thing; function f() { return 1; }")))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-namespace (first asts)) :to-equal "App\\Lib")
    (expect (cl-cc:ast-imports (first asts)) :to-equal '((:type :class :name "Vendor\\Thing" :alias "Thing")))))

(it-sequential "php-parser-qualified-name-rejects-single-token-namespace-separators"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/php::php-parse-qualified-name
     (list (cl-cc/php::make-php-token :T-IDENT "Vendor\\Thing")))) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "php-parser-braced-namespace-and-group-use-metadata"
  (let ((asts (cl-cc/php:parse-php-source
               "<?php namespace App\\Lib { use function Vendor\\Fns\\{foo, bar as baz}; function f() { return 1; } class C {} }")))
    (expect (= 2 (length asts)) :to-be-truthy)
    (expect (every (lambda (ast) (string= "App\\Lib" (cl-cc:ast-namespace ast))) asts) :to-be-truthy)
    (expect (cl-cc:ast-imports (first asts)) :to-equal '((:type :function :name "Vendor\\Fns\\foo" :alias nil)
                    (:type :function :name "Vendor\\Fns\\bar" :alias "baz")))
    (expect (cl-cc:ast-imports (second asts)) :to-equal (cl-cc:ast-imports (first asts)))))

(defun %php-new-make-instance (value)
  "Extract the ast-make-instance from a `new C(...)' lowering. The lowering wraps
it in (let ((inst (make-instance C))) (if (has __construct) ...) inst), so the
make-instance is the first binding's value; falls back to VALUE itself."
  (if (cl-cc:ast-make-instance-p value)
      value
      (cdr (first (cl-cc:ast-let-bindings value)))))

(it-sequential "php-parser-clone-lowers-to-runtime-helper"
  (let* ((value (%php-first-binding-value "<?php $b = clone $a;"))
         (copy-call (cdr (first (cl-cc:ast-let-bindings value))))
         (body (cl-cc:ast-let-body value)))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (cl-cc:ast-call-p copy-call) :to-be-truthy)
    (expect (cl-cc:ast-var-name (cl-cc:ast-call-func copy-call)) :to-be 'cl-cc/php::%php-clone)
    (expect (cl-cc:ast-if-p (first body)) :to-be-truthy)
    (expect (cl-cc:ast-var-p (second body)) :to-be-truthy)))

(it-sequential "php-parser-use-alias-resolves-new-class-name"
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; use Vendor\\Thing as Thing; $x = new Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (expect (cl-cc:ast-make-instance-p mi) :to-be-truthy)
    (expect (cl-cc:ast-var-p class-ref) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-var-name class-ref)) :to-equal "VENDOR\\THING")))

(it-sequential "php-parser-default-use-alias-resolves-new-class-name"
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; use Vendor\\Thing; $x = new Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (expect (symbol-name (cl-cc:ast-var-name class-ref)) :to-equal "VENDOR\\THING")))

(it-sequential "php-parser-fully-qualified-new-class-name-stays-global"
  (let* ((mi (%php-new-make-instance
              (%php-first-binding-value
               "<?php namespace App\\Lib; $x = new \\Vendor\\Thing();")))
         (class-ref (cl-cc:ast-make-instance-class mi)))
    (expect (symbol-name (cl-cc:ast-var-name class-ref)) :to-equal "VENDOR\\THING")))

(it-sequential "php-parser-namespace-resolves-relative-class-declaration-and-ancestry"
  (let* ((ast (%php-first "<?php namespace App\\Lib; class Box extends Base implements Iface {}"))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (expect (symbol-name (cl-cc:ast-defclass-name ast)) :to-equal "APP\\LIB\\BOX")
    (expect supers :to-equal '("APP\\LIB\\BASE" "APP\\LIB\\IFACE"))))

(it-sequential "php-parser-class-ancestry-resolves-imports-and-absolute-names"
  (let* ((ast (%php-first "<?php namespace App\\Lib; use Vendor\\Base; class Box extends Base implements \\Contracts\\Iface {}"))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (expect supers :to-equal '("VENDOR\\BASE" "CONTRACTS\\IFACE"))))

(it-sequential "php-parser-function-import-alias-resolves-call-name"
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php namespace App\\Lib; use function Vendor\\Fns\\{foo, bar as baz}; $x = foo(); $y = baz();"))
         (let-x       (first asts))
         (let-y       (first (cl-cc:ast-let-body let-x)))
         (first-call  (cdr (first (cl-cc:ast-let-bindings let-x))))
         (second-call (cdr (first (cl-cc:ast-let-bindings let-y)))))
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func first-call))) :to-equal "VENDOR\\FNS\\FOO")
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func second-call))) :to-equal "VENDOR\\FNS\\BAR")))

(it-sequential "php-parser-function-import-overrides-builtin-name"
  (let* ((call (%php-first-binding-value
                "<?php namespace App\\Lib; use function Vendor\\Fns\\count; $x = count($items);")))
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))) :to-equal "VENDOR\\FNS\\COUNT")))

(it-sequential "php-parser-function-import-alias-overrides-builtin-name"
  (let* ((call (%php-first-binding-value
                "<?php namespace App\\Lib; use function Vendor\\Fns\\strlen as count; $x = count($items);")))
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))) :to-equal "VENDOR\\FNS\\STRLEN")))

(it-sequential "php-parser-unqualified-function-call-keeps-global-fallback-name"
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = helper();")))
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))) :to-equal "HELPER")))

(it-sequential "php-parser-qualified-function-call-resolves-relative-to-namespace"
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = Tools\\helper();")))
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))) :to-equal "APP\\LIB\\TOOLS\\HELPER")))

(it-sequential "php-parser-fully-qualified-function-call-stays-global"
  (let* ((call (%php-first-binding-value "<?php namespace App\\Lib; $x = \\Vendor\\Fns\\foo();")))
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func call))) :to-equal "VENDOR\\FNS\\FOO")))

(it-sequential "php-parser-unqualified-constant-keeps-global-fallback-name"
  (let ((value (%php-first-binding-value "<?php namespace App\\Lib; $x = SOME_CONST;")))
    (expect (cl-cc:ast-var-p value) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-var-name value)) :to-equal "SOME_CONST")))

(it-sequential "php-parser-qualified-constant-resolves-relative-to-namespace"
  (let ((value (%php-first-binding-value "<?php namespace App\\Lib; $x = Config\\VALUE;")))
    (expect (cl-cc:ast-var-p value) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-var-name value)) :to-equal "APP\\LIB\\CONFIG\\VALUE")))

(it-sequential "php-parser-qualified-catch-types-resolve-imports-and-absolute-names"
  (let* ((ast (%php-first "<?php namespace App\\Lib; use Vendor\\Ex; try { throw new Ex(); } catch (Ex | \\Other\\Alt $e) { echo $e; }"))
         (inner (cl-cc:ast-unwind-protected ast))
         (top-dispatch (first (cl-cc:ast-let-body inner)))
         (catch-dispatch (cl-cc:ast-if-then top-dispatch))
         (match-cond (cl-cc:ast-if-cond catch-dispatch))
         (class-arg (second (cl-cc:ast-call-args match-cond))))
    (expect (mapcar #'symbol-name (cl-cc:ast-quote-value class-arg)) :to-equal '("VENDOR\\EX" "OTHER\\ALT"))))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
