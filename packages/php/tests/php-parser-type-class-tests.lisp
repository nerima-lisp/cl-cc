(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest php-parser-function-type-annotation-preservation
  "Characterization: function parameter and return type annotations should be preserved as declarations."
  (let ((ast (%php-first "<?php function add(int $a, ?string $b = null, int|string $c): bool { return true; }")))
    (assert-true (cl-cc:ast-defun-p ast))
    (let ((decls (cl-cc:ast-defun-declarations ast)))
      (assert-equal "bool" (getf decls :php-return-type))
      (assert-equal '(("a" . "int") ("b" . "?string") ("c" . "int|string"))
                    (mapcar (lambda (entry)
                              (cons (symbol-name (car entry)) (cdr entry)))
                            (getf decls :php-param-types))))))

(deftest php-parser-function-special-return-types
  "PHP return type annotations preserve void, never, mixed, static, nullable, union, and intersection spelling."
  (dolist (case '(("void" . "void")
                  ("never" . "never")
                  ("mixed" . "mixed")
                  ("static" . "static")
                  ("?int" . "?int")
                  ("int|string|null" . "int|string|null")
                  ("Countable&Iterator" . "countable&iterator")
                  ("(Countable&Iterator)|Traversable" . "(countable&iterator)|traversable")
                  ("Countable|(Iterator&Traversable)" . "countable|(iterator&traversable)")))
    (let* ((source (format nil "<?php function f(): ~A { return 1; }" (car case)))
           (ast (%php-first source)))
      (assert-equal (cdr case)
                    (getf (cl-cc:ast-defun-declarations ast) :php-return-type)))))

(deftest php-parser-function-return-by-reference-metadata
  "PHP function &name(...) parses and records the return-by-reference marker."
  (let* ((ast (%php-first "<?php function &current_item(): mixed { return $item; }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-equal "mixed" (getf decls :php-return-type))
    (assert-true (getf decls :php-returns-by-ref))))

(deftest php-parser-anonymous-function-return-by-reference-metadata
  "PHP function &() closure syntax parses and records the return-by-reference marker."
  (let* ((value (%php-first-binding-value "<?php $f = function &() { return $item; };"))
         (lambda (if (cl-cc:ast-let-p value)
                     (first (cl-cc:ast-let-body value))
                     value))
         (decls (cl-cc:ast-lambda-declarations lambda)))
    (assert-true (cl-cc:ast-lambda-p lambda))
    (assert-true (member '(:php-returns-by-ref t) decls :test #'equal))))

(deftest php-parser-dnf-type-annotation-preservation
  "PHP 8.2 DNF type annotations are preserved on callables and class members."
  (let* ((fn (%php-first "<?php function f((A&B)|C $x): D|(E&F) { return $x; }"))
         (class (%php-first "<?php class Box { public (A&B)|C $value; const D|(E&F) KIND = 1; }"))
         (decls (cl-cc:ast-defun-declarations fn))
         (slots (cl-cc:ast-defclass-slots class)))
    (assert-equal '(("x" . "(a&b)|c"))
                  (mapcar (lambda (entry)
                            (cons (symbol-name (car entry)) (cdr entry)))
                          (getf decls :php-param-types)))
    (assert-equal "d|(e&f)" (getf decls :php-return-type))
    (assert-equal '("(a&b)|c" "d|(e&f)") (mapcar #'cl-cc:ast-slot-type slots))))

(deftest php-parser-reference-parameter-and-foreach-are-supported
  "By-reference function parameters and foreach values parse as supported forms."
  (dolist (src '("<?php function f(&$x) { return $x; }"
                 "<?php foreach ($items as &$item) { echo $item; }"))
    (let ((asts (cl-cc/php:parse-php-source src)))
      (assert-true (cl-cc/php:php-check-supported-forms asts)))))

(deftest php-parser-trait-is-supported-by-check
  "Traits parse as trait class-like declarations and pass support checks."
  (let ((ast (%php-first "<?php trait T { public $x; }")))
    (assert-eq :trait (cl-cc:ast-defclass-php-kind ast))
    (assert-true (cl-cc/php:php-check-supported-forms (list ast)))))

(deftest php-parser-interface-is-supported-by-check
  "Interfaces parse as interface class-like declarations and pass support checks."
  (let ((ast (%php-first "<?php interface I {}")))
    (assert-eq :interface (cl-cc:ast-defclass-php-kind ast))
    (assert-true (cl-cc/php:php-check-supported-forms (list ast)))))

(deftest php-parser-unit-enum-cases
  "Unit enums parse as PHP enum defclasses with singleton case class slots."
  (let* ((form (%php-first "<?php enum Suit { case Hearts; case Diamonds; }"))
         ;; An enum now lowers to (progn defclass (link-cases)); unwrap the defclass.
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-eq :enum (cl-cc:ast-defclass-php-kind ast))
    (assert-null (cl-cc:ast-defclass-php-enum-type ast))
    (assert-equal '("HEARTS" "DIAMONDS")
                  (mapcar (lambda (slot) (symbol-name (cl-cc:ast-slot-name slot))) slots))
    (assert-true (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (getf (cl-cc:ast-imports slot) :php-enum-case)))
                        slots))
    (assert-equal '("HEARTS" "DIAMONDS")
                  (mapcar (lambda (case) (symbol-name (getf case :name)))
                          (cl-cc:ast-defclass-php-enum-cases ast)))
    (cl-cc/php:php-check-supported-forms (list ast))))

(deftest php-parser-backed-enum-cases
  "Backed enums preserve int/string backing metadata and per-case values."
  (let* ((form (%php-first "<?php enum Status: int { case Draft = 0; case Published = 1; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-eq :enum (cl-cc:ast-defclass-php-kind ast))
    (assert-eq :int (cl-cc:ast-defclass-php-enum-type ast))
    (assert-= 2 (length slots))
    (assert-true (every (lambda (slot)
                          (cl-cc:ast-call-p (cl-cc:ast-slot-initform slot)))
                        slots))))

(deftest php-parser-enum-implements-methods-traits-and-constants
  "Enum bodies accept implements, methods, trait uses, and constants."
  (let* ((form (%php-first "<?php enum Status implements JsonSerializable { use HasLabels; const FOO = 'x'; public function label() { return 'ok'; } case Draft = 0; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slot-names (mapcar (lambda (slot) (symbol-name (cl-cc:ast-slot-name slot)))
                             (cl-cc:ast-defclass-slots ast))))
    (assert-true (member "JSONSERIALIZABLE"
                         (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))
                         :test #'string=))
    (assert-true (member "FOO" slot-names :test #'string=))
    (assert-true (member "LABEL" slot-names :test #'string=))
    (assert-true (member "DRAFT" slot-names :test #'string=))))

(deftest php-parser-enum-static-builtins
  "Enum static built-ins lower to runtime helper calls.
Enum defclass is first; $x/$y/$z assignments nest (php-finish-let-bindings)."
  (let* ((asts (cl-cc/php:parse-php-source "<?php enum Status: int { case Draft = 0; case Published = 1; } $x = Status::from(1); $y = Status::tryFrom(99); $z = Status::cases();"))
         ;; enum defclass is first; let-x is second (wraps y and z in its body chain)
         (let-x  (second asts))
         (let-y  (first (cl-cc:ast-let-body let-x)))
         (let-z  (first (cl-cc:ast-let-body let-y)))
         (from-call     (cdr (first (cl-cc:ast-let-bindings let-x))))
         (try-from-call (cdr (first (cl-cc:ast-let-bindings let-y))))
         (cases-call    (cdr (first (cl-cc:ast-let-bindings let-z)))))
    (assert-string= "%PHP-ENUM-FROM" (%php-call-name from-call))
    (assert-string= "%PHP-ENUM-TRY-FROM" (%php-call-name try-from-call))
    (assert-string= "%PHP-ENUM-CASES" (%php-call-name cases-call))))

(deftest-each php-parser-reference-syntax-is-supported
  "By-reference parameter, foreach value, and closure capture syntax parse and pass support checks."
  :cases (("closure-use-ref"    "<?php $fn = function() use (&$x) { return $x; };")
          ("function-ref-param" "<?php function f(&$x) { return $x; }")
          ("foreach-ref-value"  "<?php foreach ($items as &$item) { echo $item; }"))
  (src)
  (let ((asts (cl-cc/php:parse-php-source src)))
    (assert-true (cl-cc/php:php-check-supported-forms asts))))

(deftest php-parser-class-typed-properties
  "Characterization: class typed properties should preserve their declared PHP types on slot definitions."
  (let* ((ast (%php-first "<?php class User { public int $id; private ?string $name; readonly public int|float $score; }"))
          (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 3 (length slots))
    (assert-equal '("int" "?string" "int|float") (mapcar #'cl-cc:ast-slot-type slots))
    (assert-true (member :readonly (getf (cl-cc:ast-imports (third slots)) :php-modifiers)))))

(deftest php-parser-readonly-class-marks-instance-properties
  "PHP 8.2 readonly class declarations mark instance properties, not static members or methods."
  (let* ((ast (%php-first "<?php readonly class User { public int $id; public static int $count; public function name() { return 1; } }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (labels ((slot (name)
               (find name slots
                     :key (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                     :test #'string=)))
      (let ((id (slot "ID"))
            (count (slot "COUNT"))
            (name (slot "NAME")))
        (assert-true (getf (cl-cc:ast-imports id) :readonly-p))
        (assert-true (member :readonly (getf (cl-cc:ast-imports id) :php-modifiers)))
        (assert-false (getf (cl-cc:ast-imports count) :readonly-p))
        (assert-false (getf (cl-cc:ast-imports name) :readonly-p))))))

(deftest php-parser-class-typed-constants
  "PHP class constants preserve optional type annotations as class-scoped metadata slots."
  (let* ((ast (%php-first "<?php class C { const int FOO = 1; const BAR = 'x'; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-equal '("FOO" "BAR") (mapcar (lambda (slot)
                                             (symbol-name (cl-cc:ast-slot-name slot)))
                                           slots))
    (assert-equal '("int" nil) (mapcar #'cl-cc:ast-slot-type slots))
     (assert-true (every (lambda (slot)
                           (and (eq :class (cl-cc:ast-slot-allocation slot))
                                (getf (cl-cc:ast-imports slot) :php-class-constant)))
                         slots))))

(deftest php-parser-multiple-class-constants-in-one-declaration
  "PHP class const declarations may contain several constants sharing one optional type."
  (let* ((ast (%php-first "<?php class C { public const int FOO = 1, BAR = 2; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-equal '("FOO" "BAR")
                  (mapcar (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                          slots))
    (assert-equal '("int" "int") (mapcar #'cl-cc:ast-slot-type slots))
    (assert-true (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (member :public (getf (cl-cc:ast-imports slot) :php-modifiers))
                               (getf (cl-cc:ast-imports slot) :php-class-constant)))
                        slots))))

(deftest php-parser-attribute-grouped-class-constants-signal-error
  "PHP 8.5 attributes on grouped class const declarations are rejected."
  (assert-signals error
    (cl-cc/php:parse-php-source
     "<?php class C { #[Deprecated] public const A = 1, B = 2; }")))

(defun %php-node-attributes (node)
  "Return PHP attribute metadata attached to NODE."
  (getf (cl-cc:ast-imports node) :php-attributes))

(deftest php-parser-attribute-class-metadata
  "#[Attr] class Foo {} attaches attribute metadata to the class AST node."
  (let* ((ast (%php-first "<?php #[Attr] class Foo {}"))
         (attrs (%php-node-attributes ast)))
    (assert-true (cl-cc:ast-defclass-p ast))
    (assert-= 1 (length attrs))
    (assert-string= "Attr" (cl-cc/php:php-attribute-name (first attrs)))
    (assert-eq :class (cl-cc/php:php-attribute-target-type (first attrs)))))

(deftest php-parser-attribute-function-string-arg
  "#[Attr('value')] function foo() {} parses and preserves attribute arguments."
  (let* ((ast (%php-first "<?php #[Attr('value')] function foo() { return 1; }"))
         (attr (first (%php-node-attributes ast)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "Attr" (cl-cc/php:php-attribute-name attr))
    (assert-true (cl-cc:ast-quote-p arg))
    (assert-string= "value" (cl-cc:ast-quote-value arg))))

(deftest php-parser-multiple-attributes-class
  "#[Attr1, Attr2] class Bar {} attaches both attributes in order."
  (let* ((ast (%php-first "<?php #[Attr1, Attr2] class Bar {}"))
         (attrs (%php-node-attributes ast)))
    (assert-equal '("Attr1" "Attr2")
                  (mapcar #'cl-cc/php:php-attribute-name attrs))))

(deftest php-parser-attribute-named-arguments
  "#[Attr(42, name: 'val')] parses positional and named attribute arguments."
  (let* ((ast (%php-first "<?php #[Attr(42, name: 'val')] function bar() { return 1; }"))
         (attr (first (%php-node-attributes ast)))
         (args (cl-cc/php:php-attribute-args attr)))
    (assert-= 2 (length args))
    (assert-true (cl-cc:ast-int-p (first args)))
    (assert-equal "name" (getf (second args) :name))
    (assert-true (cl-cc:ast-quote-p (getf (second args) :value)))
    (assert-string= "val" (cl-cc:ast-quote-value (getf (second args) :value)))))

(deftest php-parser-hash-comment-still-skips
  "A standalone # still starts a PHP line comment, including before declarations."
  (let ((ast (%php-first "<?php # this is a comment
function commented() { return 1; }")))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "COMMENTED" (symbol-name (cl-cc:ast-defun-name ast)))))

(deftest php-parser-constructor-promotion
  "PHP 8.0 constructor property promotion: visibility/readonly modifiers may
precede a parameter's type in __construct."
  (let ((ast (%php-first
              "<?php class P { public function __construct(public int $x, private string $y) {} }")))
    (assert-true (cl-cc:ast-defclass-p ast))))

(deftest php-parser-constructor-promotion-readonly
  "Promoted constructor params accept readonly + nullable + default."
  (let ((ast (%php-first
              "<?php class P { public function __construct(int $a, public readonly ?string $b = null) {} }")))
    (assert-true (cl-cc:ast-defclass-p ast))))

(deftest php-parser-anonymous-class
  "new class { ... } parses to a progn defining and instantiating an anon class."
  (let ((ast (%php-first "<?php $o = new class { public $x = 1; };")))
    (assert-true ast)))

(deftest php-parser-anonymous-class-extends-ctor
  "new class(5) extends Base { __construct(public int $n) {} } parses."
  (let ((ast (%php-first
              "<?php $o = new class(5) extends Base { public function __construct(public int $n) {} };")))
    (assert-true ast)))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
