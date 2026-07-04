(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(%php85-register-test 'php85-pipe-operator-lowers-to-helper-call
  "The PHP 8.5 pipe operator lowers to the runtime pipe helper."
  (lambda ()
(let ((ast (%php-first "<?php \"  HI  \" |> trim(...);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-eq 'cl-cc/php::%php-pipe
               (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))
    (assert-= 2 (length (cl-cc:ast-call-args ast)))
    (assert-true (cl-cc:ast-lambda-p (second (cl-cc:ast-call-args ast)))))))

(%php85-register-test 'php85-pipe-runtime-applies-callable
  "The pipe helper applies a callable to the piped value."
  (lambda ()
(assert-string= "hi"
                 (cl-cc/php::%php-pipe "HI" #'cl-cc/php::%php-strtolower))))

(%php85-register-test 'php85-pipe-operator-executes-first-class-callable-chain
  "A parsed pipe chain can execute PHP first-class callable RHS expressions."
  (lambda ()
(assert-string= "hi"
                 (%php-run-capture
                  "<?php echo \"  HI  \" |> trim(...) |> strtolower(...);"))))

(%php85-register-test 'php85-array-first-last-runtime-preserves-order
  "array_first() and array_last() return inserted first/last values."
  (lambda ()
(let ((array (cl-cc/php::%php-array)))
    (cl-cc/php::%php-array-set array "a" 10)
    (cl-cc/php::%php-array-set array "b" 20)
    (assert-= 10 (cl-cc/php:%php-array-first array))
    (assert-= 20 (cl-cc/php:%php-array-last array)))))

(%php85-register-test 'php85-array-first-last-empty-arrays-return-null
  "array_first() and array_last() return null for empty arrays."
  (lambda ()
(let ((array (cl-cc/php:%php-array)))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc/php:%php-array-first array))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc/php:%php-array-last array)))))

(%php85-register-test 'php85-array-first-last-execute-as-builtins
  "The PHP 8.5 array_first() and array_last() builtins execute from PHP source."
  (lambda ()
(assert-string= "10:20"
                 (%php-run-capture
                  "<?php $a=['a'=>10,'b'=>20]; echo array_first($a).':'.array_last($a);"))))

(%php85-register-test 'php85-grapheme-levenshtein-counts-combining-cluster
  "grapheme_levenshtein() treats a base character plus combining mark as one cluster."
  (lambda ()
(let ((cluster (format nil "a~C" (code-char #x0301))))
    (assert-= 1 (cl-cc/php::%php-grapheme-levenshtein cluster "")))))

(%php85-register-test 'php85-grapheme-levenshtein-executes-as-builtin
  "The PHP 8.5 grapheme_levenshtein() builtin executes from PHP source."
  (lambda ()
(assert-string= "3"
                 (%php-run-capture
                  "<?php echo grapheme_levenshtein('kitten','sitting');"))))

(%php85-register-test 'php85-locale-is-right-to-left-runtime-detects-rtl-locales
  "PHP 8.5 Locale direction helper detects common RTL locale identifiers."
  (lambda ()
(assert-true (cl-cc/php:%php-locale-is-right-to-left "ar_EG.UTF-8"))
  (assert-true (cl-cc/php:%php-locale-is-right-to-left "fa-IR"))
  (assert-true (cl-cc/php:%php-locale-is-right-to-left "az-Arab"))
  (assert-false (cl-cc/php:%php-locale-is-right-to-left "en_US"))
  (assert-false (cl-cc/php:%php-locale-is-right-to-left "az-Latn"))))

(%php85-register-test 'php85-locale-is-right-to-left-executes-as-builtin
  "The PHP 8.5 locale_is_right_to_left() builtin executes from PHP source."
  (lambda ()
(assert-string= "rtl:ltr"
                 (%php-run-capture
                  "<?php echo (locale_is_right_to_left('he_IL') ? 'rtl' : 'ltr') . ':' . (locale_is_right_to_left('en_US') ? 'rtl' : 'ltr');"))))

(%php85-register-test 'php85-locale-static-is-right-to-left-lowers-to-helper
  "The PHP 8.5 Locale::isRightToLeft() static method lowers to the runtime helper."
  (lambda ()
(assert-string= "rtl:ltr"
                 (%php-run-capture
                  "<?php echo (Locale::isRightToLeft('ur_PK') ? 'rtl' : 'ltr') . ':' . (Locale::isRightToLeft('fr_FR') ? 'rtl' : 'ltr');"))))

(%php85-register-test 'php85-build-metadata-constants-are-predefined
  "PHP_BUILD_DATE and PHP_BUILD_PROVIDER resolve as predefined PHP 8.5 constants."
  (lambda ()
(multiple-value-bind (date date-found)
      (cl-cc/php::%php-lookup-constant "PHP_BUILD_DATE")
    (multiple-value-bind (provider provider-found)
        (cl-cc/php::%php-lookup-constant "PHP_BUILD_PROVIDER")
      (assert-true date-found)
      (assert-true provider-found)
      (assert-string= "1970-01-01T00:00:00+00:00" date)
      (assert-string= "cl-cc" provider)))))

(%php85-register-test 'php85-build-metadata-constants-execute-from-php-source
  "PHP 8.5 build metadata constants are available to parsed PHP code."
  (lambda ()
(assert-string= "cl-cc:date"
                 (%php-run-capture
                  "<?php echo PHP_BUILD_PROVIDER . ':' . (PHP_BUILD_DATE === '' ? 'empty' : 'date');"))))

(%php85-register-test 'php85-no-discard-attribute-preserved-on-function
  "PHP 8.5 #[\\NoDiscard] is preserved as function attribute metadata."
  (lambda ()
(let* ((ast (%php-first "<?php #[\\NoDiscard] function important(): int { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
    (assert-eq :function (cl-cc/php:php-attribute-target-type attr)))))

(%php85-register-test 'php85-no-discard-attribute-preserves-message
  "PHP 8.5 #[NoDiscard('message')] preserves the optional attribute message."
  (lambda ()
(let* ((ast (%php-first "<?php #[NoDiscard('use the return value')] function important() { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
    (assert-true (cl-cc:ast-quote-p arg))
    (assert-string= "use the return value" (cl-cc:ast-quote-value arg)))))

(%php85-register-test 'php85-no-discard-discarded-function-call-triggers-warning
  "Discarding a #[NoDiscard] function result emits E_USER_WARNING."
  (lambda ()
    (assert-string= "512:name:msg:7"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'important()') ? 'name' : 'missing') . ':' . (str_contains($errstr, 'must use it') ? 'msg' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard('must use it')] function important() { echo '7'; return 7; } important(); restore_error_handler();"))))

(%php85-register-test 'php85-no-discard-consumed-function-call-is-silent
  "Using a #[NoDiscard] function result does not emit a warning."
  (lambda ()
    (assert-string= "7"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard] function important(){ return 7; } $x = important(); echo $x; restore_error_handler();"))))

(%php85-register-test 'php85-no-discard-void-cast-suppresses-warning
  "Casting a #[NoDiscard] function result to void suppresses the warning."
  (lambda ()
    (assert-string= "called"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard] function important(){ echo 'called'; return 7; } (void) important(); restore_error_handler();"))))

(%php85-register-test 'php85-no-discard-discarded-method-call-triggers-warning
  "Discarding a #[NoDiscard] method result emits E_USER_WARNING."
  (lambda ()
    (assert-string= "512:name:msg:m"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'label()') ? 'name' : 'missing') . ':' . (str_contains($errstr, 'must use method') ? 'msg' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard('must use method')] public function label(){ echo 'm'; return 'm'; } } $box = new Box(); $box->label(); restore_error_handler();"))))

(%php85-register-test 'php85-no-discard-consumed-method-call-is-silent
  "Using a #[NoDiscard] method result does not emit a warning."
  (lambda ()
    (assert-string= "m"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public function label(){ return 'm'; } } $box = new Box(); $x = $box->label(); echo $x; restore_error_handler();"))))

(%php85-register-test 'php85-no-discard-void-cast-suppresses-method-warning
  "Casting a #[NoDiscard] method result to void suppresses the warning."
  (lambda ()
    (assert-string= "m"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public function label(){ echo 'm'; return 'm'; } } $box = new Box(); (void) $box->label(); restore_error_handler();"))))

(%php85-register-test 'php85-no-discard-discarded-static-method-call-triggers-warning
  "Discarding a #[NoDiscard] static method result emits E_USER_WARNING."
  (lambda ()
    (assert-string= "512:name:s"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'label()') ? 'name' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public static function label(){ echo 's'; return 's'; } } Box::label(); restore_error_handler();"))))

(%php85-register-test 'php85-top-level-const-executes
  "PHP top-level const declarations define readable constants."
  (lambda ()
(assert-string= "42"
                 (%php-run-capture "<?php const ANSWER = 42; echo ANSWER;"))))

(%php85-register-test 'php85-attribute-preserved-on-top-level-constant
  "PHP 8.5 attributes on top-level constants survive as constant metadata."
  (lambda ()
(let* ((ast (%php-first "<?php #[Deprecated('use NEW')] const OLD = 1;"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (assert-true (cl-cc:ast-defvar-p ast))
    (assert-string= "OLD" (symbol-name (cl-cc:ast-defvar-name ast)))
    (assert-string= "Deprecated" (cl-cc/php:php-attribute-name attr))
    (assert-eq :constant (cl-cc/php:php-attribute-target-type attr))
    (assert-true (cl-cc:ast-quote-p arg))
    (assert-string= "use NEW" (cl-cc:ast-quote-value arg)))))

(%php85-register-test 'php85-attribute-grouped-top-level-constants-signal-error
  "PHP 8.5 attributes on grouped top-level const declarations are rejected."
  (lambda ()
    (assert-signals error
      (cl-cc/php:parse-php-source "<?php #[Deprecated] const A = 1, B = 2;"))))

(%php85-register-test 'php85-no-discard-attribute-preserved-on-enum-method
  "PHP 8.5 #[NoDiscard] on enum methods survives enum classlike parsing."
  (lambda ()
    (dolist (source (list
                     "<?php enum Mode { #[NoDiscard] public function label(): string { return 'x'; } case A; } class AfterEnum {}"
                     "<?php class Box { #[NoDiscard] public function label(): string { return 'x'; } }"))
      (let* ((form (%php-first source))
             (ast (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
             (method-slot (find-if (lambda (slot)
                                     (and (cl-cc:ast-slot-def-p slot)
                                          (cl-cc:ast-defun-p (cl-cc:ast-slot-initform slot))))
                                   (append (list nil
                                                 (cl-cc:make-ast-slot-def :name 'probe
                                                                          :allocation :class))
                                           (cl-cc:ast-defclass-slots ast))))
             (method (cl-cc:ast-slot-initform method-slot))
             (attr (first (getf (cl-cc:ast-imports method) :php-attributes))))
        (assert-true (cl-cc:ast-defclass-p ast))
        (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
        (assert-eq :method (cl-cc/php:php-attribute-target-type attr))))))

(%php85-register-test 'php85-override-method-is-validated-against-parent
  "PHP 8.5 #[Override] is accepted on inherited methods."
  (lambda ()
    (assert-true
     (cl-cc:ast-defclass-p
      (%php-first
       "<?php class Base { public function label(): string { return 'x'; } } class Child extends Base { #[Override] public function label(): string { return 'y'; } }")))))

(%php85-register-test 'php85-override-property-is-validated-against-parent
  "PHP 8.5 #[Override] is accepted on inherited properties."
  (lambda ()
    (assert-true
     (cl-cc:ast-defclass-p
      (%php-first
       "<?php class Base { public string $name; } class Child extends Base { #[Override] public string $name; }")))))

(%php85-register-test 'php85-override-private-parent-property-signals-error
  "PHP 8.5 #[Override] does not accept private inherited properties."
  (lambda ()
    (assert-signals error
      (cl-cc/php:parse-php-source
       "<?php class Base { private string $name; } class Child extends Base { #[Override] public string $name; }"))))

(%php85-register-test 'php85-void-cast-lowers-to-discarding-progn
  "The PHP 8.5 (void) statement evaluates its operand and returns PHP null."
  (lambda ()
(let* ((value (%php-first "<?php (void) 123;"))
         (forms (cl-cc:ast-progn-forms value)))
    (assert-true (cl-cc:ast-progn-p value))
    (assert-= 2 (length forms))
    (assert-eq cl-cc/php:+php-null+
               (cl-cc:ast-quote-value (second forms))))))

(%php85-register-test 'php85-void-cast-executes-side-effects
  "The PHP 8.5 (void) statement still evaluates the discarded expression."
  (lambda ()
(assert-string= "1"
                 (%php-run-capture
                  "<?php $x=0; (void)($x=1); echo $x;"))))


(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
