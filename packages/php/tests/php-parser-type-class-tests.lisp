(in-package :cl-cc/test)

(it-sequential "php-parser-function-type-annotation-preservation"
  (let ((ast (%php-first "<?php function add(int $a, ?string $b = null, int|string $c): bool { return true; }")))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (let ((decls (cl-cc:ast-defun-declarations ast)))
      (expect (getf decls :php-return-type) :to-equal "bool")
      (expect (mapcar (lambda (entry)
                              (cons (symbol-name (car entry)) (cdr entry)))
                            (getf decls :php-param-types)) :to-equal '(("a" . "int") ("b" . "?string") ("c" . "int|string"))))))

(it-sequential "php-parser-function-special-return-types"
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
      (expect (getf (cl-cc:ast-defun-declarations ast) :php-return-type) :to-equal (cdr case)))))

(it-sequential "php-parser-function-return-by-reference-metadata"
  (let* ((ast (%php-first "<?php function &current_item(): mixed { return $item; }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (getf decls :php-return-type) :to-equal "mixed")
    (expect (getf decls :php-returns-by-ref) :to-be-truthy)))

(it-sequential "php-parser-anonymous-function-return-by-reference-metadata"
  (let* ((value (%php-first-binding-value "<?php $f = function &() { return $item; };"))
         (lambda (if (cl-cc:ast-let-p value)
                     (first (cl-cc:ast-let-body value))
                     value))
         (decls (cl-cc:ast-lambda-declarations lambda)))
    (expect (cl-cc:ast-lambda-p lambda) :to-be-truthy)
    (expect (member '(:php-returns-by-ref t) decls :test #'equal) :to-be-truthy)))

(it-sequential "php-parser-dnf-type-annotation-preservation"
  (let* ((fn (%php-first "<?php function f((A&B)|C $x): D|(E&F) { return $x; }"))
         (class (%php-first "<?php class Box { public (A&B)|C $value; const D|(E&F) KIND = 1; }"))
         (decls (cl-cc:ast-defun-declarations fn))
         (slots (cl-cc:ast-defclass-slots class)))
    (expect (mapcar (lambda (entry)
                            (cons (symbol-name (car entry)) (cdr entry)))
                          (getf decls :php-param-types)) :to-equal '(("x" . "(a&b)|c")))
    (expect (getf decls :php-return-type) :to-equal "d|(e&f)")
    (expect (mapcar #'cl-cc:ast-slot-type slots) :to-equal '("(a&b)|c" "d|(e&f)"))))

(it-sequential "php-parser-reference-parameter-and-foreach-are-supported"
  (dolist (src '("<?php function f(&$x) { return $x; }"
                 "<?php foreach ($items as &$item) { echo $item; }"))
    (let ((asts (cl-cc/php:parse-php-source src)))
      (expect (cl-cc/php:php-check-supported-forms asts) :to-be-truthy))))

(it-sequential "php-parser-trait-is-supported-by-check"
  (let ((ast (%php-first "<?php trait T { public $x; }")))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :trait)
    (expect (cl-cc/php:php-check-supported-forms (list ast)) :to-be-truthy)))

(it-sequential "php-parser-interface-is-supported-by-check"
  (let ((ast (%php-first "<?php interface I {}")))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :interface)
    (expect (cl-cc/php:php-check-supported-forms (list ast)) :to-be-truthy)))

(it-sequential "php-parser-unit-enum-cases"
  (let* ((form (%php-first "<?php enum Suit { case Hearts; case Diamonds; }"))
         ;; An enum now lowers to (progn defclass (link-cases)); unwrap the defclass.
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :enum)
    (expect (cl-cc:ast-defclass-php-enum-type ast) :to-be-null)
    (expect (mapcar (lambda (slot) (symbol-name (cl-cc:ast-slot-name slot))) slots) :to-equal '("HEARTS" "DIAMONDS"))
    (expect (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (getf (cl-cc:ast-imports slot) :php-enum-case)))
                        slots) :to-be-truthy)
    (expect (mapcar (lambda (case) (symbol-name (getf case :name)))
                          (cl-cc:ast-defclass-php-enum-cases ast)) :to-equal '("HEARTS" "DIAMONDS"))
    (cl-cc/php:php-check-supported-forms (list ast))))

(it-sequential "php-parser-backed-enum-cases"
  (let* ((form (%php-first "<?php enum Status: int { case Draft = 0; case Published = 1; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :enum)
    (expect (cl-cc:ast-defclass-php-enum-type ast) :to-be :int)
    (expect (= 2 (length slots)) :to-be-truthy)
    (expect (every (lambda (slot)
                          (cl-cc:ast-call-p (cl-cc:ast-slot-initform slot)))
                        slots) :to-be-truthy)))

(it-sequential "php-parser-enum-implements-methods-traits-and-constants"
  (let* ((form (%php-first "<?php enum Status implements JsonSerializable { use HasLabels; const FOO = 'x'; public function label() { return 'ok'; } case Draft = 0; }"))
         (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
         (slot-names (mapcar (lambda (slot) (symbol-name (cl-cc:ast-slot-name slot)))
                             (cl-cc:ast-defclass-slots ast))))
    (expect (member "JSONSERIALIZABLE"
                         (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))
                         :test #'string=) :to-be-truthy)
    (expect (member "FOO" slot-names :test #'string=) :to-be-truthy)
    (expect (member "LABEL" slot-names :test #'string=) :to-be-truthy)
    (expect (member "DRAFT" slot-names :test #'string=) :to-be-truthy)))

(it-sequential "php-parser-enum-static-builtins"
  (let* ((asts (cl-cc/php:parse-php-source "<?php enum Status: int { case Draft = 0; case Published = 1; } $x = Status::from(1); $y = Status::tryFrom(99); $z = Status::cases();"))
         ;; enum defclass is first; let-x is second (wraps y and z in its body chain)
         (let-x  (second asts))
         (let-y  (first (cl-cc:ast-let-body let-x)))
         (let-z  (first (cl-cc:ast-let-body let-y)))
         (from-call     (cdr (first (cl-cc:ast-let-bindings let-x))))
         (try-from-call (cdr (first (cl-cc:ast-let-bindings let-y))))
         (cases-call    (cdr (first (cl-cc:ast-let-bindings let-z)))))
    (expect (%php-call-name from-call) :to-equal "%PHP-ENUM-FROM")
    (expect (%php-call-name try-from-call) :to-equal "%PHP-ENUM-TRY-FROM")
    (expect (%php-call-name cases-call) :to-equal "%PHP-ENUM-CASES")))

(it-sequential "php-parser-reference-syntax-is-supported closure-use-ref"
  (destructuring-bind (src) (list "<?php $fn = function() use (&$x) { return $x; };")
    (let ((asts (cl-cc/php:parse-php-source src)))
    (expect (cl-cc/php:php-check-supported-forms asts) :to-be-truthy))))

(it-sequential "php-parser-reference-syntax-is-supported function-ref-param"
  (destructuring-bind (src) (list "<?php function f(&$x) { return $x; }")
    (let ((asts (cl-cc/php:parse-php-source src)))
    (expect (cl-cc/php:php-check-supported-forms asts) :to-be-truthy))))

(it-sequential "php-parser-reference-syntax-is-supported foreach-ref-value"
  (destructuring-bind (src) (list "<?php foreach ($items as &$item) { echo $item; }")
    (let ((asts (cl-cc/php:parse-php-source src)))
    (expect (cl-cc/php:php-check-supported-forms asts) :to-be-truthy))))

(it-sequential "php-parser-class-typed-properties"
  (let* ((ast (%php-first "<?php class User { public int $id; private ?string $name; readonly public int|float $score; }"))
          (slots (cl-cc:ast-defclass-slots ast)))
    (expect (= 3 (length slots)) :to-be-truthy)
    (expect (mapcar #'cl-cc:ast-slot-type slots) :to-equal '("int" "?string" "int|float"))
    (expect (member :readonly (getf (cl-cc:ast-imports (third slots)) :php-modifiers)) :to-be-truthy)))

(it-sequential "php-parser-readonly-class-marks-instance-properties"
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
        (expect (getf (cl-cc:ast-imports id) :readonly-p) :to-be-truthy)
        (expect (member :readonly (getf (cl-cc:ast-imports id) :php-modifiers)) :to-be-truthy)
        (expect (getf (cl-cc:ast-imports count) :readonly-p) :to-be-falsy)
        (expect (getf (cl-cc:ast-imports name) :readonly-p) :to-be-falsy)))))

(it-sequential "php-parser-class-typed-constants"
  (let* ((ast (%php-first "<?php class C { const int FOO = 1; const BAR = 'x'; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (= 2 (length slots)) :to-be-truthy)
    (expect (mapcar (lambda (slot)
                                             (symbol-name (cl-cc:ast-slot-name slot)))
                                           slots) :to-equal '("FOO" "BAR"))
    (expect (mapcar #'cl-cc:ast-slot-type slots) :to-equal '("int" nil))
     (expect (every (lambda (slot)
                           (and (eq :class (cl-cc:ast-slot-allocation slot))
                                (getf (cl-cc:ast-imports slot) :php-class-constant)))
                         slots) :to-be-truthy)))

(it-sequential "php-parser-multiple-class-constants-in-one-declaration"
  (let* ((ast (%php-first "<?php class C { public const int FOO = 1, BAR = 2; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (= 2 (length slots)) :to-be-truthy)
    (expect (mapcar (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                          slots) :to-equal '("FOO" "BAR"))
    (expect (mapcar #'cl-cc:ast-slot-type slots) :to-equal '("int" "int"))
    (expect (every (lambda (slot)
                          (and (eq :class (cl-cc:ast-slot-allocation slot))
                               (member :public (getf (cl-cc:ast-imports slot) :php-modifiers))
                               (getf (cl-cc:ast-imports slot) :php-class-constant)))
                        slots) :to-be-truthy)))

(it-sequential "php-parser-attribute-grouped-class-constants-signal-error"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/php:parse-php-source
     "<?php class C { #[Deprecated] public const A = 1, B = 2; }")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(defun %php-node-attributes (node)
  "Return PHP attribute metadata attached to NODE."
  (getf (cl-cc:ast-imports node) :php-attributes))

(it-sequential "php-parser-attribute-class-metadata"
  (let* ((ast (%php-first "<?php #[Attr] class Foo {}"))
         (attrs (%php-node-attributes ast)))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (= 1 (length attrs)) :to-be-truthy)
    (expect (cl-cc/php:php-attribute-name (first attrs)) :to-equal "Attr")
    (expect (cl-cc/php:php-attribute-target-type (first attrs)) :to-be :class)))

(it-sequential "php-parser-attribute-function-string-arg"
  (let* ((ast (%php-first "<?php #[Attr('value')] function foo() { return 1; }"))
         (attr (first (%php-node-attributes ast)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (cl-cc/php:php-attribute-name attr) :to-equal "Attr")
    (expect (cl-cc:ast-quote-p arg) :to-be-truthy)
    (expect (cl-cc:ast-quote-value arg) :to-equal "value")))

(it-sequential "php-parser-multiple-attributes-class"
  (let* ((ast (%php-first "<?php #[Attr1, Attr2] class Bar {}"))
         (attrs (%php-node-attributes ast)))
    (expect (mapcar #'cl-cc/php:php-attribute-name attrs) :to-equal '("Attr1" "Attr2"))))

(it-sequential "php-parser-attribute-named-arguments"
  (let* ((ast (%php-first "<?php #[Attr(42, name: 'val')] function bar() { return 1; }"))
         (attr (first (%php-node-attributes ast)))
         (args (cl-cc/php:php-attribute-args attr)))
    (expect (= 2 (length args)) :to-be-truthy)
    (expect (cl-cc:ast-int-p (first args)) :to-be-truthy)
    (expect (getf (second args) :name) :to-equal "name")
    (expect (cl-cc:ast-quote-p (getf (second args) :value)) :to-be-truthy)
    (expect (cl-cc:ast-quote-value (getf (second args) :value)) :to-equal "val")))

(it-sequential "php-parser-hash-comment-still-skips"
  (let ((ast (%php-first "<?php # this is a comment
function commented() { return 1; }")))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-defun-name ast)) :to-equal "COMMENTED")))

(it-sequential "php-parser-constructor-promotion"
  (let ((ast (%php-first
              "<?php class P { public function __construct(public int $x, private string $y) {} }")))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)))

(it-sequential "php-parser-constructor-promotion-readonly"
  (let ((ast (%php-first
              "<?php class P { public function __construct(int $a, public readonly ?string $b = null) {} }")))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)))

(it-sequential "php-parser-anonymous-class"
  (let ((ast (%php-first "<?php $o = new class { public $x = 1; };")))
    (expect ast :to-be-truthy)))

(it-sequential "php-parser-anonymous-class-extends-ctor"
  (let ((ast (%php-first
              "<?php $o = new class(5) extends Base { public function __construct(public int $n) {} };")))
    (expect ast :to-be-truthy)))


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
