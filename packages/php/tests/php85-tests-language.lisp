(in-package :cl-cc/test)


(%php85-register-test 'php85-pipe-operator-lowers-to-helper-call
  "The PHP 8.5 pipe operator lowers to the runtime pipe helper."
  (lambda ()
(let ((ast (%php-first "<?php \"  HI  \" |> trim(...);")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (cl-cc:ast-var-name (cl-cc:ast-call-func ast)) :to-be 'cl-cc/php::%php-pipe)
    (expect (= 2 (length (cl-cc:ast-call-args ast))) :to-be-truthy)
    (expect (cl-cc:ast-lambda-p (second (cl-cc:ast-call-args ast))) :to-be-truthy))))

(%php85-register-test 'php85-pipe-runtime-applies-callable
  "The pipe helper applies a callable to the piped value."
  (lambda ()
(expect (cl-cc/php::%php-pipe "HI" #'cl-cc/php::%php-strtolower) :to-equal "hi")))

(%php85-register-test 'php85-pipe-operator-executes-first-class-callable-chain
  "A parsed pipe chain can execute PHP first-class callable RHS expressions."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo \"  HI  \" |> trim(...) |> strtolower(...);") :to-equal "hi")))

(%php85-register-test 'php85-array-first-last-runtime-preserves-order
  "array_first() and array_last() return inserted first/last values."
  (lambda ()
(let ((array (cl-cc/php::%php-array)))
    (cl-cc/php::%php-array-set array "a" 10)
    (cl-cc/php::%php-array-set array "b" 20)
    (expect (= 10 (cl-cc/php:%php-array-first array)) :to-be-truthy)
    (expect (= 20 (cl-cc/php:%php-array-last array)) :to-be-truthy))))

(%php85-register-test 'php85-array-first-last-empty-arrays-return-null
  "array_first() and array_last() return null for empty arrays."
  (lambda ()
(let ((array (cl-cc/php:%php-array)))
    (expect (cl-cc/php:%php-array-first array) :to-be cl-cc/php:+php-null+)
    (expect (cl-cc/php:%php-array-last array) :to-be cl-cc/php:+php-null+))))

(%php85-register-test 'php85-array-first-last-execute-as-builtins
  "The PHP 8.5 array_first() and array_last() builtins execute from PHP source."
  (lambda ()
(expect (%php-run-capture
                  "<?php $a=['a'=>10,'b'=>20]; echo array_first($a).':'.array_last($a);") :to-equal "10:20")))

(%php85-register-test 'php85-grapheme-levenshtein-counts-combining-cluster
  "grapheme_levenshtein() treats a base character plus combining mark as one cluster."
  (lambda ()
(let ((cluster (format nil "a~C" (code-char #x0301))))
    (expect (= 1 (cl-cc/php::%php-grapheme-levenshtein cluster "")) :to-be-truthy))))

(%php85-register-test 'php85-grapheme-levenshtein-executes-as-builtin
  "The PHP 8.5 grapheme_levenshtein() builtin executes from PHP source."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo grapheme_levenshtein('kitten','sitting');") :to-equal "3")))

(%php85-register-test 'php85-locale-is-right-to-left-runtime-detects-rtl-locales
  "PHP 8.5 Locale direction helper detects common RTL locale identifiers."
  (lambda ()
(expect (cl-cc/php:%php-locale-is-right-to-left "ar_EG.UTF-8") :to-be-truthy)
  (expect (cl-cc/php:%php-locale-is-right-to-left "fa-IR") :to-be-truthy)
  (expect (cl-cc/php:%php-locale-is-right-to-left "az-Arab") :to-be-truthy)
  (expect (cl-cc/php:%php-locale-is-right-to-left "en_US") :to-be-falsy)
  (expect (cl-cc/php:%php-locale-is-right-to-left "az-Latn") :to-be-falsy)))

(%php85-register-test 'php85-locale-is-right-to-left-executes-as-builtin
  "The PHP 8.5 locale_is_right_to_left() builtin executes from PHP source."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo (locale_is_right_to_left('he_IL') ? 'rtl' : 'ltr') . ':' . (locale_is_right_to_left('en_US') ? 'rtl' : 'ltr');") :to-equal "rtl:ltr")))

(%php85-register-test 'php85-locale-static-is-right-to-left-lowers-to-helper
  "The PHP 8.5 Locale::isRightToLeft() static method lowers to the runtime helper."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo (Locale::isRightToLeft('ur_PK') ? 'rtl' : 'ltr') . ':' . (Locale::isRightToLeft('fr_FR') ? 'rtl' : 'ltr');") :to-equal "rtl:ltr")))

(%php85-register-test 'php85-build-metadata-constants-are-predefined
  "PHP_BUILD_DATE and PHP_BUILD_PROVIDER resolve as predefined PHP 8.5 constants."
  (lambda ()
(multiple-value-bind (date date-found)
      (cl-cc/php::%php-lookup-constant "PHP_BUILD_DATE")
    (multiple-value-bind (provider provider-found)
        (cl-cc/php::%php-lookup-constant "PHP_BUILD_PROVIDER")
      (expect date-found :to-be-truthy)
      (expect provider-found :to-be-truthy)
      (expect date :to-equal "1970-01-01T00:00:00+00:00")
      (expect provider :to-equal "cl-cc")))))

(%php85-register-test 'php85-build-metadata-constants-execute-from-php-source
  "PHP 8.5 build metadata constants are available to parsed PHP code."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo PHP_BUILD_PROVIDER . ':' . (PHP_BUILD_DATE === '' ? 'empty' : 'date');") :to-equal "cl-cc:date")))

(%php85-register-test 'php85-no-discard-attribute-preserved-on-function
  "PHP 8.5 #[\\NoDiscard] is preserved as function attribute metadata."
  (lambda ()
(let* ((ast (%php-first "<?php #[\\NoDiscard] function important(): int { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes))))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (cl-cc/php:php-attribute-name attr) :to-equal "NoDiscard")
    (expect (cl-cc/php:php-attribute-target-type attr) :to-be :function))))

(%php85-register-test 'php85-no-discard-attribute-preserves-message
  "PHP 8.5 #[NoDiscard('message')] preserves the optional attribute message."
  (lambda ()
(let* ((ast (%php-first "<?php #[NoDiscard('use the return value')] function important() { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (expect (cl-cc/php:php-attribute-name attr) :to-equal "NoDiscard")
    (expect (cl-cc:ast-quote-p arg) :to-be-truthy)
    (expect (cl-cc:ast-quote-value arg) :to-equal "use the return value"))))

(%php85-register-test 'php85-no-discard-discarded-function-call-triggers-warning
  "Discarding a #[NoDiscard] function result emits E_USER_WARNING."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'important()') ? 'name' : 'missing') . ':' . (str_contains($errstr, 'must use it') ? 'msg' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard('must use it')] function important() { echo '7'; return 7; } important(); restore_error_handler();") :to-equal "512:name:msg:7")))

(%php85-register-test 'php85-no-discard-consumed-function-call-is-silent
  "Using a #[NoDiscard] function result does not emit a warning."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard] function important(){ return 7; } $x = important(); echo $x; restore_error_handler();") :to-equal "7")))

(%php85-register-test 'php85-no-discard-void-cast-suppresses-warning
  "Casting a #[NoDiscard] function result to void suppresses the warning."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); #[NoDiscard] function important(){ echo 'called'; return 7; } (void) important(); restore_error_handler();") :to-equal "called")))

(%php85-register-test 'php85-no-discard-discarded-method-call-triggers-warning
  "Discarding a #[NoDiscard] method result emits E_USER_WARNING."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'label()') ? 'name' : 'missing') . ':' . (str_contains($errstr, 'must use method') ? 'msg' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard('must use method')] public function label(){ echo 'm'; return 'm'; } } $box = new Box(); $box->label(); restore_error_handler();") :to-equal "512:name:msg:m")))

(%php85-register-test 'php85-no-discard-consumed-method-call-is-silent
  "Using a #[NoDiscard] method result does not emit a warning."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public function label(){ return 'm'; } } $box = new Box(); $x = $box->label(); echo $x; restore_error_handler();") :to-equal "m")))

(%php85-register-test 'php85-no-discard-void-cast-suppresses-method-warning
  "Casting a #[NoDiscard] method result to void suppresses the warning."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public function label(){ echo 'm'; return 'm'; } } $box = new Box(); (void) $box->label(); restore_error_handler();") :to-equal "m")))

(%php85-register-test 'php85-no-discard-discarded-static-method-call-triggers-warning
  "Discarding a #[NoDiscard] static method result emits E_USER_WARNING."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':' . (str_contains($errstr, 'label()') ? 'name' : 'missing') . ':'; return true; } set_error_handler('h', E_USER_WARNING); class Box { #[NoDiscard] public static function label(){ echo 's'; return 's'; } } Box::label(); restore_error_handler();") :to-equal "512:name:s")))

(%php85-register-test 'php85-top-level-const-executes
  "PHP top-level const declarations define readable constants."
  (lambda ()
(expect (%php-run-capture "<?php const ANSWER = 42; echo ANSWER;") :to-equal "42")))

(%php85-register-test 'php85-attribute-preserved-on-top-level-constant
  "PHP 8.5 attributes on top-level constants survive as constant metadata."
  (lambda ()
(let* ((ast (%php-first "<?php #[Deprecated('use NEW')] const OLD = 1;"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes)))
         (arg (first (cl-cc/php:php-attribute-args attr))))
    (expect (cl-cc:ast-defvar-p ast) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-defvar-name ast)) :to-equal "OLD")
    (expect (cl-cc/php:php-attribute-name attr) :to-equal "Deprecated")
    (expect (cl-cc/php:php-attribute-target-type attr) :to-be :constant)
    (expect (cl-cc:ast-quote-p arg) :to-be-truthy)
    (expect (cl-cc:ast-quote-value arg) :to-equal "use NEW"))))

(%php85-register-test 'php85-attribute-grouped-top-level-constants-signal-error
  "PHP 8.5 attributes on grouped top-level const declarations are rejected."
  (lambda ()
    (signals error (cl-cc/php:parse-php-source "<?php #[Deprecated] const A = 1, B = 2;"))))

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
        (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
        (expect (cl-cc/php:php-attribute-name attr) :to-equal "NoDiscard")
        (expect (cl-cc/php:php-attribute-target-type attr) :to-be :method)))))

(%php85-register-test 'php85-override-method-is-validated-against-parent
  "PHP 8.5 #[Override] is accepted on inherited methods."
  (lambda ()
    (expect (cl-cc:ast-defclass-p
      (%php-first
       "<?php class Base { public function label(): string { return 'x'; } } class Child extends Base { #[Override] public function label(): string { return 'y'; } }")) :to-be-truthy)))

(%php85-register-test 'php85-override-property-is-validated-against-parent
  "PHP 8.5 #[Override] is accepted on inherited properties."
  (lambda ()
    (expect (cl-cc:ast-defclass-p
      (%php-first
       "<?php class Base { public string $name; } class Child extends Base { #[Override] public string $name; }")) :to-be-truthy)))

(%php85-register-test 'php85-override-private-parent-property-signals-error
  "PHP 8.5 #[Override] does not accept private inherited properties."
  (lambda ()
    (signals error (cl-cc/php:parse-php-source
       "<?php class Base { private string $name; } class Child extends Base { #[Override] public string $name; }"))))

(%php85-register-test 'php85-void-cast-lowers-to-discarding-progn
  "The PHP 8.5 (void) statement evaluates its operand and returns PHP null."
  (lambda ()
(let* ((value (%php-first "<?php (void) 123;"))
         (forms (cl-cc:ast-progn-forms value)))
    (expect (cl-cc:ast-progn-p value) :to-be-truthy)
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (cl-cc:ast-quote-value (second forms)) :to-be cl-cc/php:+php-null+))))

(%php85-register-test 'php85-void-cast-executes-side-effects
  "The PHP 8.5 (void) statement still evaluates the discarded expression."
  (lambda ()
(expect (%php-run-capture
                  "<?php $x=0; (void)($x=1); echo $x;") :to-equal "1")))


(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
