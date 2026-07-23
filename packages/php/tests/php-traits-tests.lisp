;;;; packages/php/tests/php-traits-tests.lisp — PHP Trait tests
;;;;
;;;; Coverage for trait declarations and class-use-trait via cl-cc/php:parse-php-source.
;;;; Exercises parser-trait.lisp: trait bodies, use-trait statements,
;;;; conflict resolution (insteadof), method aliasing (as), visibility change.
(in-package :cl-cc/test)

;;; ─── Helpers (reuse %php-first from php-parser-tests.lisp) ──────────────────

;;; ─── Trait Definition ────────────────────────────────────────────────────────

(it-sequential "php-trait-definition-produces-ast-defclass"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait Greetable { }"))))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :trait)))

(it-sequential "php-trait-name-is-upcased"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait HasTimestamps { }"))))
    (expect (symbol-name (cl-cc:ast-defclass-name ast)) :to-equal "HASTIMESTAMPS")))

(it-sequential "php-trait-with-method-produces-slot-def"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait Greetable { public function greet() { return 'hi'; } }")))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (plusp (length slots)) :to-be-truthy)
    (expect (every #'cl-cc:ast-slot-def-p slots) :to-be-truthy)))

(it-sequential "php-trait-method-name-is-upcased"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait Greetable { public function greet() { return 'hello'; } }")))
         (slot  (first (cl-cc:ast-defclass-slots ast))))
    (expect (symbol-name (cl-cc:ast-slot-name slot)) :to-equal "GREET")))

(it-sequential "php-trait-with-abstract-method"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait HasLogger { abstract public function log(string $msg): void; }")))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :trait)
    (expect (plusp (length slots)) :to-be-truthy)))

(it-sequential "php-trait-no-superclasses"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait Mixin { }"))))
    (expect (cl-cc:ast-defclass-superclasses ast) :to-be-null)))

;;; ─── Class Using a Trait ─────────────────────────────────────────────────────

(it-sequential "php-class-use-single-trait-produces-use-slot"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait Greetable { } class Greeter { use Greetable; }"))
         (class (second asts))
         (slots (cl-cc:ast-defclass-slots class)))
    (expect (cl-cc:ast-defclass-p class) :to-be-truthy)
    (expect (some (lambda (s)
                         (getf (cl-cc:ast-imports s) :php-trait-use))
                       slots) :to-be-truthy)))

(it-sequential "php-class-use-single-trait-records-trait-name"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait Greetable { } class Greeter { use Greetable; }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class))))
    (expect use-slot :to-be-truthy)
    (let ((names (getf (cl-cc:ast-imports use-slot) :php-trait-names)))
      (expect (= 1 (length names)) :to-be-truthy)
      (expect (symbol-name (first names)) :to-equal "GREETABLE"))))

;;; ─── Multiple Traits ─────────────────────────────────────────────────────────

(it-sequential "php-class-use-multiple-traits"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { } trait B { } class C { use A, B; }"))
         (class (third asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (names (getf (cl-cc:ast-imports use-slot) :php-trait-names)))
    (expect (= 2 (length names)) :to-be-truthy)
    (expect (find "A" names :key #'symbol-name :test #'string=) :to-be-truthy)
    (expect (find "B" names :key #'symbol-name :test #'string=) :to-be-truthy)))

;;; ─── Conflict Resolution: insteadof ─────────────────────────────────────────

(it-sequential "php-trait-insteadof-conflict-resolution"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  trait B { public function hello() { return 'B'; } }
                  class C { use A, B { A::hello insteadof B; } }"))
         (class (third asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (insteadof-list (getf (cl-cc:ast-imports use-slot) :php-insteadof)))
    (expect (plusp (length insteadof-list)) :to-be-truthy)
    (let ((entry (first insteadof-list)))
      (expect (symbol-name (getf entry :method)) :to-equal "HELLO")
      (expect (symbol-name (getf entry :from)) :to-equal "A"))))

(it-sequential "php-trait-insteadof-records-excluded-traits"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  trait B { public function hello() { return 'B'; } }
                  class C { use A, B { A::hello insteadof B; } }"))
         (class (third asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (insteadof-list (getf (cl-cc:ast-imports use-slot) :php-insteadof))
         (excluded       (getf (first insteadof-list) :exclude)))
    (expect (find "B" excluded :key #'symbol-name :test #'string=) :to-be-truthy)))

;;; ─── Method Aliasing: as ─────────────────────────────────────────────────────

(it-sequential "php-trait-as-alias-records-alias-name"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { A::hello as hi; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias)))
    (expect (plusp (length alias-list)) :to-be-truthy)
    (let ((entry (first alias-list)))
      (expect (symbol-name (getf entry :alias)) :to-equal "HI"))))

(it-sequential "php-trait-as-alias-records-source-method"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { A::hello as hi; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias))
         (entry (first alias-list)))
    (expect (symbol-name (getf entry :method)) :to-equal "HELLO")))

;;; ─── Visibility Change ───────────────────────────────────────────────────────

(it-sequential "php-trait-as-visibility-change"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { hello as protected; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias))
         (entry      (first alias-list)))
    (expect (getf entry :vis) :to-be :protected)))

(it-sequential "php-trait-as-visibility-and-alias"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait A { public function hello() { return 'A'; } }
                  class C { use A { A::hello as protected greeting; } }"))
         (class (second asts))
         (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
         (alias-list (getf (cl-cc:ast-imports use-slot) :php-alias))
         (entry      (first alias-list)))
    (expect (getf entry :vis) :to-be :protected)
    (expect (symbol-name (getf entry :alias)) :to-equal "GREETING")))

;;; ─── Trait Registry ──────────────────────────────────────────────────────────

(it-sequential "php-trait-definition-registers-methods"
  (cl-cc/php:parse-php-source
   "<?php trait RegisterMe { public function act() { return 1; } }")
  (let ((registry-entry (gethash "REGISTERME" cl-cc/php:*php-trait-registry*)))
    (expect (listp registry-entry) :to-be-truthy)
    (expect (plusp (length registry-entry)) :to-be-truthy)))

(it-sequential "php-trait-is-supported-by-check"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php trait Mixin { }"))))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :trait)
    ;; Must not signal an error — traits are supported
    (expect (cl-cc/php:php-check-supported-forms (list ast)) :to-be-truthy)))

(it-sequential "php-trait-with-property"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait HasName { public string $name; }")))
         (slots (cl-cc:ast-defclass-slots ast))
         (prop  (first slots)))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :trait)
    (expect (cl-cc:ast-slot-def-p prop) :to-be-truthy)
    ;; Property slot names use the SAME symbol convention as member access
    ;; ($obj->name interns via php-ident-sym, upcased), so the declared slot name
    ;; matches how it is read/written. ($name -> NAME.) Without this they mismatched
    ;; and $obj->name raised "slot NAME is missing".
    (expect (symbol-name (cl-cc:ast-slot-name prop)) :to-equal "NAME")))

(it-sequential "php-trait-multiple-methods"
  (let* ((ast   (first (cl-cc/php:parse-php-source
                        "<?php trait Multi {
                           public function foo() { return 1; }
                           public function bar() { return 2; }
                           public function baz() { return 3; }
                         }")))
         (slots (cl-cc:ast-defclass-slots ast))
         (names (mapcar (lambda (s) (symbol-name (cl-cc:ast-slot-name s))) slots)))
    (expect (cl-cc:ast-defclass-php-kind ast) :to-be :trait)
    (expect (= 3 (length slots)) :to-be-truthy)
    (expect (member "FOO" names :test #'string=) :to-be-truthy)
    (expect (member "BAR" names :test #'string=) :to-be-truthy)
    (expect (member "BAZ" names :test #'string=) :to-be-truthy)))

(it-sequential "php-class-use-trait-with-no-conflict-block"
  (let* ((asts  (cl-cc/php:parse-php-source
                 "<?php trait Simple { public function act() { return 1; } }
                  class Worker { use Simple; }"))
         (class (second asts))
        (use-slot (find-if (lambda (s) (getf (cl-cc:ast-imports s) :php-trait-use))
                            (cl-cc:ast-defclass-slots class)))
        (insteadof-list (getf (cl-cc:ast-imports use-slot) :php-insteadof)))
    (expect use-slot :to-be-truthy)
    (expect insteadof-list :to-be-null)))

(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
