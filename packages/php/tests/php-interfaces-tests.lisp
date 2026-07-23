;;;; packages/php/tests/php-interfaces-tests.lisp — PHP Interface tests
;;;;
;;;; Coverage for interface declarations via cl-cc/php:parse-php-source.
;;;; Exercises parser-interface.lisp: definition, single/multiple implements,
;;;; interface extends, interface constants, abstract method signatures.
(in-package :cl-cc/test)

;;; ─── Interface Definition ────────────────────────────────────────────────────

(it-sequential "php-interface-definition-produces-ast-defclass"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php interface Countable { }"))))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :interface)))

(it-sequential "php-interface-name-is-upcased"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php interface JsonSerializable { }"))))
    (expect (symbol-name (cl-cc:ast-defclass-name ast)) :to-equal "JSONSERIALIZABLE")))

(it-sequential "php-interface-is-supported-by-check"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php interface Loggable { }"))))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :interface)
    (expect (cl-cc/php:php-check-supported-forms (list ast)) :to-be-truthy)))

;;; ─── Abstract Method Signatures ─────────────────────────────────────────────

(it-sequential "php-interface-abstract-method-is-registered"
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface HasIdMeth { public function getId(): int; }")))
         ;; Use the exact symbol produced by the parser for registry lookup.
         (iface-sym (cl-cc:ast-defclass-name ast))
         (record (gethash iface-sym cl-cc/php:*php-interface-registry*)))
    (expect record :to-be-truthy)
    (expect (plusp (length (getf record :methods))) :to-be-truthy)))

(it-sequential "php-interface-method-signature-captures-name"
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface RenderableIface { public function render(): string; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods)))
    (expect sigs :to-be-truthy)
    (expect (some (lambda (s) (string= "RENDER" (symbol-name (getf s :name)))) sigs) :to-be-truthy)))

(it-sequential "php-interface-method-signature-captures-return-type"
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface LabeledIface { public function label(): string; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods))
         (sig  (first sigs)))
    (expect (stringp (getf sig :return-type)) :to-be-truthy)))

(it-sequential "php-interface-method-signature-captures-return-by-reference"
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface RefSourceIface { public function &current(): mixed; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods))
         (sig (first sigs)))
    (expect sig :to-be-truthy)
    (expect (symbol-name (getf sig :name)) :to-equal "CURRENT")
    (expect (getf sig :return-type) :to-equal "mixed")
    (expect (getf sig :returns-by-ref) :to-be-truthy)))

(it-sequential "php-interface-method-signature-captures-parameter-metadata"
  (let* ((ast (first (cl-cc/php:parse-php-source
                      "<?php interface SinkIface { #[Trace] public function write(#[Sensitive] string &$message, int $limit = 10, ...$rest): void; }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (sigs (getf (gethash iface-sym cl-cc/php:*php-interface-registry*) :methods))
         (sig (first sigs))
         (params (getf sig :params))
         (param-types (getf sig :param-types))
         (param-defaults (getf sig :param-defaults))
         (default-ast (cdr (assoc (second params) param-defaults :test #'eq))))
    (expect sig :to-be-truthy)
    (expect (getf sig :modifiers) :to-equal '(:public))
    (expect (= 2 (length params)) :to-be-truthy)
    (expect (getf sig :by-ref-indices) :to-equal '(0))
    (expect (symbol-name (first params)) :to-equal "message")
    (expect (symbol-name (getf sig :variadic-param)) :to-equal "rest")
    (expect (cdr (assoc (first params) param-types :test #'eq)) :to-equal "string")
    (expect (cdr (assoc (second params) param-types :test #'eq)) :to-equal "int")
    (expect (cl-cc:ast-int-p default-ast) :to-be-truthy)
    (expect (= 10 (cl-cc:ast-int-value default-ast)) :to-be-truthy)
    (expect (first (first (getf sig :param-attributes))) :to-be (first params))
    (expect (getf (rest (first (getf sig :param-attributes))) :php-attributes) :to-be-truthy)
    (expect (cl-cc/php:php-attribute-name (first (getf sig :attributes))) :to-equal "Trace")))

(it-sequential "php-interface-empty-body-has-no-methods"
  (let* ((ast (first (cl-cc/php:parse-php-source "<?php interface EmptyIfaceX { }")))
         (iface-sym (cl-cc:ast-defclass-name ast))
         (record (gethash iface-sym cl-cc/php:*php-interface-registry*)))
    (expect record :to-be-truthy)
    (expect (getf record :methods) :to-be-null)))

;;; ─── Interface Constants ─────────────────────────────────────────────────────

(it-sequential "php-interface-constant-is-slot-def"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface HasVersion { const VERSION = '1.0'; }")))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :interface)
    (expect (plusp (length slots)) :to-be-truthy)
    (let ((const-slot (first slots)))
      (expect (cl-cc:ast-slot-def-p const-slot) :to-be-truthy)
      (expect (cl-cc:ast-slot-allocation const-slot) :to-be :class)
      (expect (getf (cl-cc:ast-imports const-slot) :php-class-constant) :to-be-truthy))))

(it-sequential "php-interface-constant-name-is-upcased"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface Colors { const RED = 'red'; }")))
         (slot  (first (cl-cc:ast-defclass-slots ast))))
    (expect (symbol-name (cl-cc:ast-slot-name slot)) :to-equal "RED")))

(it-sequential "php-interface-typed-constant"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface Spec { const int LIMIT = 100; }")))
         (slot  (first (cl-cc:ast-defclass-slots ast))))
    (expect (stringp (cl-cc:ast-slot-type slot)) :to-be-truthy)))

(it-sequential "php-interface-multiple-constants-in-one-declaration"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php interface SpecMulti { const int MIN = 1, MAX = 10; }")))
         (slots (cl-cc:ast-defclass-slots ast))
         (record (gethash (cl-cc/php::php-ident-sym "SpecMulti")
                          cl-cc/php:*php-interface-registry*)))
    (expect (= 2 (length slots)) :to-be-truthy)
    (expect (mapcar (lambda (slot)
                            (symbol-name (cl-cc:ast-slot-name slot)))
                          slots) :to-equal '("MIN" "MAX"))
    (expect (mapcar #'cl-cc:ast-slot-type slots) :to-equal '("int" "int"))
    (expect (= 2 (length (getf record :constants))) :to-be-truthy)))

;;; ─── Interface Extends ───────────────────────────────────────────────────────

(it-sequential "php-interface-extends-single-parent"
  (let* ((asts   (cl-cc/php:parse-php-source
                  "<?php interface A { } interface B extends A { }"))
         (iface-b (second asts))
         (supers  (cl-cc:ast-defclass-superclasses iface-b)))
    (expect (cl-cc:ast-defclass-php-kind iface-b) :to-be :interface)
    (expect (= 1 (length supers)) :to-be-truthy)
    (expect (symbol-name (first supers)) :to-equal "A")))

(it-sequential "php-interface-extends-multiple-parents"
  (let* ((asts   (cl-cc/php:parse-php-source
                  "<?php interface A { } interface B { } interface C extends A, B { }"))
         (iface-c (third asts))
         (supers  (cl-cc:ast-defclass-superclasses iface-c)))
    (expect (= 2 (length supers)) :to-be-truthy)
    (expect (find "A" supers :key #'symbol-name :test #'string=) :to-be-truthy)
    (expect (find "B" supers :key #'symbol-name :test #'string=) :to-be-truthy)))

;;; ─── Class Implements ────────────────────────────────────────────────────────

(it-sequential "php-class-implements-single-interface"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php class Foo implements Bar { }")))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (member "BAR" supers :test #'string=) :to-be-truthy)))

(it-sequential "php-class-implements-multiple-interfaces"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php class Foo implements IfaceA, IfaceB { }")))
         (names (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (expect names :to-equal '("IFACEA" "IFACEB"))))

(it-sequential "php-class-extends-and-implements"
  (let* ((ast    (first (cl-cc/php:parse-php-source
                         "<?php class Box extends Base implements Storable, Countable { }")))
         (supers (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (expect (member "BASE"       supers :test #'string=) :to-be-truthy)
    (expect (member "STORABLE"   supers :test #'string=) :to-be-truthy)
    (expect (member "COUNTABLE"  supers :test #'string=) :to-be-truthy)))

(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
