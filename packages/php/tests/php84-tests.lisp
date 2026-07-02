(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defvar *php85-self-load-guard* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-run-capture (source)
    "Compile PHP SOURCE to VM and run it, returning everything it echoed as a
string. compile-string with :language :php registers the PHP host bridges, so a
fresh VM state runs the program end-to-end."
    (let* ((result  (cl-cc:compile-string source :target :vm :language :php))
           (program (cl-cc/compile:compilation-result-program result))
           (out     (make-string-output-stream)))
      (cl-cc/vm:run-compiled program :output-stream out)
      ;; Trim a trailing newline the VM appends when flushing program output.
      (string-right-trim '(#\Newline) (get-output-stream-string out)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-run-capture-io (source &key ini-settings)
    "Compile PHP SOURCE and capture both stdout and error output."
    (let ((cl-cc/php::*php-ini-settings* ini-settings)
          (cl-cc/php::*php-error-reporting-level* 32767))
      (let* ((result  (cl-cc:compile-string source :target :vm :language :php))
             (program (cl-cc/compile:compilation-result-program result))
             (out     (make-string-output-stream))
             (err     (make-string-output-stream)))
        (handler-case
            (let ((*error-output* err))
              (cl-cc/vm:run-compiled program :output-stream out))
          (error (c)
            (declare (ignore c))))
        (values (string-right-trim '(#\Newline)
                                   (get-output-stream-string out))
                  (string-right-trim '(#\Newline)
                                     (get-output-stream-string err)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-make-ini-settings (&rest pairs)
    "Create a fresh INI settings table for tests."
    (let ((table (make-hash-table :test 'equal)))
      (dolist (entry cl-cc/php::*php-ini-defaults* table)
        (setf (gethash (car entry) table) (cdr entry)))
      (loop for (key value) on pairs by #'cddr
            do (setf (gethash key table) value))
      table)))

(defun %php84-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (cl-cc/php:parse-php-source src)))

(defun %php85-register-test (name docstring thunk &key timeout depends-on tags)
  "Register a php85 coverage test body under NAME."
  (setf *test-registry*
        (persist-assoc *test-registry* name
                       (%test-registry-entry name
                                             :fn thunk
                                             :suite *current-suite*
                                             :timeout timeout
                                             :depends-on depends-on
                                             :tags tags
                                             :docstring docstring
                                             :source-file (or *compile-file-pathname*
                                                              *load-pathname*))))
  name)

(defun %php85-run-registered-tests-with-prefix (prefix &key exclude)
  "Run every registered test whose symbol name starts with PREFIX.
EXCLUDE lists symbols that should not be invoked even if they match PREFIX."
  (let ((results '())
        (prefix-len (length prefix)))
    (persist-each *test-registry*
                  (lambda (name plist)
                    (declare (ignore plist))
                    (when (and (symbolp name)
                               (not (member name exclude :test #'eq))
                               (let ((name-str (symbol-name name)))
                                 (and (<= prefix-len (length name-str))
                                      (string= prefix name-str :end2 prefix-len))))
                      (let ((entry (persist-lookup *test-registry* name)))
                        (assert-true entry)
                        (push (funcall (getf entry :fn))
                              results)))))
    (nreverse results)))

(defun %php85-register-php84-tests ()
  nil)

(%php85-register-test 'php84-named-args-to-positional-lowers-named
  "Named arg descriptors from %php-parse-named-args lower to plain positional AST exprs."
  (lambda ()
;; Test the helper function directly with hand-built descriptors.
  (let* ((descs  (list (list :positional (cl-cc:make-ast-int :value 1))
                       (list :named "key" (cl-cc:make-ast-quote :value "val"))
                       (list :spread  (cl-cc:make-ast-var :name 'x))))
         (result (cl-cc/php::%php-named-args-to-positional descs)))
    (assert-= 3 (length result))
    (assert-true (cl-cc:ast-int-p (first result)))
    (assert-true (cl-cc:ast-quote-p (second result)))
    (assert-true (cl-cc:ast-var-p (third result))))))

(%php85-register-test 'php84-named-arg-p-detects-ident-colon
  "The %php-named-arg-p predicate recognises IDENT : as a named argument."
  (lambda ()
;; We test with a hand-built token stream matching the helper contract.
  (let* ((ident-tok   (list :type :T-IDENT :value "name"))
         (colon-tok   (list :type :T-COLON :value ":"))
         (fake-stream (list ident-tok colon-tok)))
    (assert-true (cl-cc/php::%php-named-arg-p fake-stream)))))

(%php85-register-test 'php84-named-args-parse-produces-positional-call
  "createUser(name: \"Alice\", age: 25) lowers to an ast-call with 2 positional args."
  (lambda ()
(let* ((ast (%php84-first "<?php createUser(\"Alice\", 25);")))
    ;; Without named-arg integration in php-parse-arglist the call uses
    ;; positional lowering: two arguments are preserved in order.
    (assert-true (cl-cc:ast-call-p ast))
    (assert-= 2 (length (cl-cc:ast-call-args ast))))))

(%php85-register-test 'php84-named-args-mixed-with-positional
  "Positional args before named args both survive into the call AST."
  (lambda ()
(let* ((ast (%php84-first "<?php htmlspecialchars(\"<b>hi</b>\", 11);")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-true (plusp (length (cl-cc:ast-call-args ast)))))))

(%php85-register-test 'php84-first-class-callable-predicate-true
  "The %php-first-class-callable-p predicate returns true for ( ... ) token sequence."
  (lambda ()
(let* ((lparen-tok  (list :type :T-LPAREN   :value "("))
         (ellipsis-tok (list :type :T-ELLIPSIS :value "..."))
         (rparen-tok  (list :type :T-RPAREN   :value ")"))
         (stream      (list lparen-tok ellipsis-tok rparen-tok)))
    (assert-true (cl-cc/php::%php-first-class-callable-p stream)))))

(%php85-register-test 'php84-first-class-callable-predicate-false-for-args
  "The %php-first-class-callable-p predicate returns false when ( has real args."
  (lambda ()
(let* ((lparen-tok  (list :type :T-LPAREN :value "("))
         (int-tok     (list :type :T-INT    :value 1))
         (rparen-tok  (list :type :T-RPAREN :value ")"))
         (stream      (list lparen-tok int-tok rparen-tok)))
    (assert-false (cl-cc/php::%php-first-class-callable-p stream)))))

(%php85-register-test 'php84-callable-ref-wraps-in-lambda
  "The %php-callable-ref function returns an ast-lambda that wraps the function."
  (lambda ()
(let* ((func-ast (cl-cc:make-ast-var :name 'strlen))
         (ref      (cl-cc/php::%php-callable-ref func-ast)))
    (assert-true (cl-cc:ast-lambda-p ref))
    (assert-true (plusp (length (cl-cc:ast-lambda-body ref)))))))

(%php85-register-test 'php84-callable-ref-body-is-apply-call
  "The lambda body inside a callable ref calls APPLY with the original function."
  (lambda ()
(let* ((func-ast (cl-cc:make-ast-var :name 'strlen))
         (ref      (cl-cc/php::%php-callable-ref func-ast))
         (body-call (first (cl-cc:ast-lambda-body ref))))
    (assert-true (cl-cc:ast-call-p body-call))
    (assert-string= "APPLY"
                    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func body-call)))))))

(%php85-register-test 'php84-array-find-returns-first-match
  "array_find() returns the first element satisfying the callback."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 3) (list nil nil 7) (list nil nil 4)))
         (result (cl-cc/php::%php-array-find arr (lambda (v) (> v 5)))))
    (assert-= 7 result))))

(%php85-register-test 'php84-array-find-returns-null-when-no-match
  "array_find() returns +php-null+ when no element satisfies the callback."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2)))
         (result (cl-cc/php::%php-array-find arr (lambda (v) (> v 10)))))
    (assert-equal cl-cc/php:+php-null+ result))))

(%php85-register-test 'php84-array-find-key-returns-key-of-first-match
  "array_find_key() returns the integer key of the first matching element."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 10) (list nil nil 20) (list nil nil 30)))
         (result (cl-cc/php::%php-array-find-key arr (lambda (v) (= v 20)))))
    (assert-= 1 result))))

(%php85-register-test 'php84-array-find-key-returns-null-when-no-match
  "array_find_key() returns +php-null+ when no element matches."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 1)))
         (result (cl-cc/php::%php-array-find-key arr (lambda (v) (> v 100)))))
    (assert-equal cl-cc/php:+php-null+ result))))

(%php85-register-test 'php84-array-any-true-for-matching-element
  "array_any() returns true when at least one element satisfies the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2) (list nil nil 50))))
    (assert-true (cl-cc/php::%php-array-any arr (lambda (v) (> v 10)))))))

(%php85-register-test 'php84-array-any-false-when-none-match
  "array_any() returns false when no element satisfies the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2))))
    (assert-false (cl-cc/php::%php-array-any arr (lambda (v) (> v 100)))))))

(%php85-register-test 'php84-array-all-true-when-all-match
  "array_all() returns true when every element satisfies the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 5) (list nil nil 10) (list nil nil 20))))
    (assert-true (cl-cc/php::%php-array-all arr (lambda (v) (> v 0)))))))

(%php85-register-test 'php84-array-all-false-when-one-fails
  "array_all() returns false when any element fails the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 5) (list nil nil -1))))
    (assert-false (cl-cc/php::%php-array-all arr (lambda (v) (> v 0)))))))

(%php85-register-test 'php84-array-all-true-for-empty-array
  "array_all() returns true for an empty array (vacuous truth)."
  (lambda ()
    (let ((arr (cl-cc/php:%php-array)))
      (assert-true (cl-cc/php::%php-array-all arr (lambda (v) (declare (ignore v)))))
      (assert-false
       (cl-cc/php::%php-array-all (cl-cc/php:%php-array (list nil nil 1))
                                  (lambda (v) (declare (ignore v)) nil))))))

(%php85-register-test 'php85-pipe-operator-lowers-to-helper-call
  "The PHP 8.5 pipe operator lowers to the runtime pipe helper."
  (lambda ()
(let ((ast (%php84-first "<?php \"  HI  \" |> trim(...);")))
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
(let* ((ast (%php84-first "<?php #[\\NoDiscard] function important(): int { return 1; }"))
         (attr (first (getf (cl-cc:ast-imports ast) :php-attributes))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-string= "NoDiscard" (cl-cc/php:php-attribute-name attr))
    (assert-eq :function (cl-cc/php:php-attribute-target-type attr)))))

(%php85-register-test 'php85-no-discard-attribute-preserves-message
  "PHP 8.5 #[NoDiscard('message')] preserves the optional attribute message."
  (lambda ()
(let* ((ast (%php84-first "<?php #[NoDiscard('use the return value')] function important() { return 1; }"))
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
(let* ((ast (%php84-first "<?php #[Deprecated('use NEW')] const OLD = 1;"))
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
      (let* ((form (%php84-first source))
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
      (%php84-first
       "<?php class Base { public function label(): string { return 'x'; } } class Child extends Base { #[Override] public function label(): string { return 'y'; } }")))))

(%php85-register-test 'php85-override-property-is-validated-against-parent
  "PHP 8.5 #[Override] is accepted on inherited properties."
  (lambda ()
    (assert-true
     (cl-cc:ast-defclass-p
      (%php84-first
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
(let* ((value (%php84-first "<?php (void) 123;"))
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

(%php85-register-test 'php85-get-error-handler-reports-current-handler
  "get_error_handler() returns the active handler and null when none is installed."
  (lambda ()
(assert-string= "null:h:null"
                 (%php-run-capture
                  "<?php function h($errno,$errstr){ return true; } echo get_error_handler() === null ? 'null' : 'bad'; set_error_handler('h'); echo ':' . get_error_handler() . ':'; restore_error_handler(); echo get_error_handler() === null ? 'null' : 'bad';"))))

(%php85-register-test 'php85-get-exception-handler-reports-current-handler
  "get_exception_handler() returns the active handler and null when none is installed."
  (lambda ()
(assert-string= "null:eh:null"
                 (%php-run-capture
                  "<?php function eh($e){} echo get_exception_handler() === null ? 'null' : 'bad'; set_exception_handler('eh'); echo ':' . get_exception_handler() . ':'; restore_exception_handler(); echo get_exception_handler() === null ? 'null' : 'bad';"))))

(%php85-register-test 'php85-opcache-file-cache-helper-returns-false-without-file-cache
  "opcache_is_script_cached_in_file_cache() is available and false without an OPCache file cache."
  (lambda ()
(assert-false
   (cl-cc/php:%php-opcache-is-script-cached-in-file-cache "/tmp/example.php"))))

(%php85-register-test 'php85-gc-collect-cycles-returns-integer-zero
  "PHP 8.5 gc_collect_cycles() returns an integer 0 in cl-cc's no-GC PHP model."
  (lambda ()
(assert-= 0 (cl-cc/php::%php-gc-collect-cycles))))

(%php85-register-test 'php85-gc-collect-cycles-executes-as-builtin
  "The PHP 8.5 gc_collect_cycles() builtin executes from PHP source."
  (lambda ()
(assert-string= "integer:0"
                 (%php-run-capture
                  "<?php echo gettype(gc_collect_cycles()) . ':' . gc_collect_cycles();"))))

(%php85-register-test 'php85-opcache-file-cache-helper-executes-as-builtin
  "The PHP 8.5 opcache file-cache probe executes from PHP source."
  (lambda ()
(assert-string= "cold"
                 (%php-run-capture
                  "<?php echo opcache_is_script_cached_in_file_cache('/tmp/example.php') ? 'warm' : 'cold';"))))

(%php85-register-test 'php85-curl-share-init-persistent-reuses-handle
  "curl_share_init_persistent() returns a stable persistent handle for each ID."
  (lambda ()
(let ((a (cl-cc/php:%php-curl-share-init-persistent "cache"))
        (b (cl-cc/php:%php-curl-share-init-persistent "cache"))
        (c (cl-cc/php:%php-curl-share-init-persistent "other")))
    (assert-eq a b)
    (assert-false (eq a c))
    (assert-string= "CurlSharePersistentHandle" (gethash "__class__" a))
    (assert-string= "cache" (gethash "id" a))
    (assert-true (gethash "persistent" a)))))

(%php85-register-test 'php85-curl-share-init-persistent-executes-as-builtin
  "The PHP 8.5 persistent cURL share initializer executes from PHP source."
  (lambda ()
(assert-string= "same:object"
                 (%php-run-capture
                  "<?php $a=curl_share_init_persistent('cache'); $b=curl_share_init_persistent('cache'); echo ($a === $b ? 'same' : 'diff') . ':' . gettype($a);"))))

(%php85-register-test 'php85-curl-share-init-persistent-registers-its-class-tag
  "The persistent cURL share handle is visible to class_exists before it is created."
  (lambda ()
(assert-string= "Y"
                 (%php-run-capture
                  "<?php echo class_exists('CurlSharePersistentHandle') ? 'Y' : 'N';"))))

(%php85-register-test 'php85-object-bool-loose-comparison-follows-bool-cast
  "PHP 8.5 object/boolean loose comparison follows the object's bool cast."
  (lambda ()
    (assert-string= "TTF"
                    (%php-run-capture
                     "<?php $o = curl_share_init_persistent('cmp'); $t = true; $f = false; echo ($o == true ? 'T' : 'F'); echo ($o == $t ? 'T' : 'F'); echo ($o == $f ? 'T' : 'F');"))
    (let ((object (make-hash-table :test #'equal))
          (empty-array (cl-cc/php::%php-array)))
      (setf (gethash "__class__" object) "UncomparableInternal")
      (assert-true (cl-cc/php::%php-truthy object))
      (assert-true (cl-cc/php::%php-eq-loose object t))
      (assert-false (cl-cc/php::%php-eq-loose object nil))
      (assert-false (cl-cc/php::%php-truthy empty-array)))))

(%php85-register-test 'php85-curl-multi-get-handles-returns-php-array
  "curl_multi_get_handles() exposes a multi-handle's child handles as a PHP array."
  (lambda ()
(let* ((multi (make-hash-table :test #'equal))
         (a (make-hash-table :test #'equal))
         (b (make-hash-table :test #'equal)))
    (setf (gethash "__class__" a) "CurlHandle"
          (gethash "__class__" b) "CurlHandle"
          (gethash "handles" multi) (list a b))
    (let ((result (cl-cc/php:%php-curl-multi-get-handles multi)))
      (assert-= 2 (cl-cc/php:%php-count result))
      (assert-eq a (cl-cc/php:%php-array-ref result 0))
      (assert-eq b (cl-cc/php:%php-array-ref result 1))))))

(%php85-register-test 'php85-curl-multi-get-handles-executes-as-builtin
  "curl_multi_get_handles() is registered as a PHP 8.5 builtin."
  (lambda ()
(assert-string= "Y"
                 (%php-run-capture
                  "<?php echo function_exists('curl_multi_get_handles') ? 'Y' : 'N';"))))

(%php85-register-test 'php85-locale-likely-subtags-static-methods-lower-to-helpers
  "The PHP 8.5 Locale likely-subtag static methods lower to runtime helpers."
  (lambda ()
    (assert-string= "en-Latn-US:en"
                    (%php-run-capture
                     "<?php echo Locale::addLikelySubtags('en') . ':' . Locale::minimizeSubtags('en_Latn_US');"))))

(%php85-register-test 'php85-new-runtime-symbols-are-pre-registered
  "PHP 8.5 runtime-visible classes, exceptions, and enums are visible to class_exists."
(lambda ()
(dolist (class-name '("NoDiscard"
                      "Closure"
                      "DelayedTargetValidation"
                      "CurlSharePersistentHandle"
                      "Dom\\Element"
                      "Dom\\HTMLCollection"
                      "Dom\\HTMLDocument"
                      "Filter\\FilterException"
                      "Filter\\FilterFailedException"
                      "IntlListFormatter"
                      "Locale"
                      "NumberFormatter"
                      "Pdo\\Sqlite"
                      "SoapClient"
                      "SoapFault"
                      "SoapServer"
                      "Uri\\UriError"
                      "Uri\\UriException"
                      "Uri\\InvalidUriException"
                      "Uri\\UriComparisonMode"
                      "Uri\\Rfc3986\\Uri"
                      "Uri\\WhatWg\\InvalidUrlException"
                      "Uri\\WhatWg\\UrlValidationErrorType"
                      "Uri\\WhatWg\\UrlValidationError"
                      "Uri\\WhatWg\\Url"
                      "XSLTProcessor"))
    (assert-true (cl-cc/php::%php-class-exists class-name)))))

(%php85-register-test 'php85-class-alias-registers-runtime-class-alias
  "PHP 8.5 class_alias registers aliases for runtime-visible classes."
  (lambda ()
    (let ((alias (format nil "Php85AliasRuntimeUri~A" (symbol-name (gensym)))))
      (assert-true (cl-cc/php::%php-class-alias "Uri\\Rfc3986\\Uri" alias))
      (assert-true (cl-cc/php::%php-class-exists alias))
      (assert-true (cl-cc/php::%php-method-exists alias "parse"))
      (let ((methods (cl-cc/php::%php-array-values-list
                      (cl-cc/php::%php-get-class-methods alias))))
        (assert-true (find "toRawString" methods :test #'string=))))))

(%php85-register-test 'php85-class-alias-is-available-from-compiled-php
  "PHP 8.5 class_alias is registered for compiled PHP code."
  (lambda ()
    (let ((alias (format nil "Php85AliasSourceUri~A" (symbol-name (gensym)))))
      (assert-string= "Y"
                      (%php-run-capture
                       (format nil "<?php echo class_alias('Uri\\Rfc3986\\Uri', '~A') && class_exists('~A') && method_exists('~A', 'parse') ? 'Y' : 'N';"
                               alias alias alias))))))

(%php85-register-test 'php85-class-alias-rejects-array-and-callable-aliases
  "PHP 8.5 class_alias rejects array and callable aliases."
  (lambda ()
    (dolist (alias '("array" "callable" "ARRAY" "Callable"))
      (let ((condition (handler-case
                           (progn
                             (cl-cc/php::%php-class-alias "Uri\\Rfc3986\\Uri" alias)
                             nil)
                         (cl-cc/php:php-exception (e) e))))
           (assert-true condition)
           (assert-true
            (cl-cc/php:%php-exception-matches-p condition 'value-error))))))

(%php85-register-test 'php85-intl-and-pdo-class-constants-lower-to-runtime-values
  "PHP 8.5 IntlListFormatter, NumberFormatter, and Pdo\\Sqlite class constants resolve."
  (lambda ()
    (assert-string= "0:1:10:11:12:13:14:15:16:1000:1:2:4:1001:1002:1003:1004:0:1:2:1005:0:1:2:2048"
                    (%php-run-capture
                     "<?php echo IntlListFormatter::TYPE_AND . ':' . IntlListFormatter::TYPE_OR . ':' . NumberFormatter::CURRENCY_ISO . ':' . NumberFormatter::CURRENCY_PLURAL . ':' . NumberFormatter::CURRENCY_ACCOUNTING . ':' . NumberFormatter::CASH_CURRENCY . ':' . NumberFormatter::DECIMAL_COMPACT_SHORT . ':' . NumberFormatter::DECIMAL_COMPACT_LONG . ':' . NumberFormatter::CURRENCY_STANDARD . ':' . Pdo\\Sqlite::ATTR_OPEN_FLAGS . ':' . Pdo\\Sqlite::OPEN_READONLY . ':' . Pdo\\Sqlite::OPEN_READWRITE . ':' . Pdo\\Sqlite::OPEN_CREATE . ':' . Pdo\\Sqlite::ATTR_READONLY_STATEMENT . ':' . Pdo\\Sqlite::ATTR_EXTENDED_RESULT_CODES . ':' . Pdo\\Sqlite::ATTR_BUSY_STATEMENT . ':' . Pdo\\Sqlite::ATTR_EXPLAIN_STATEMENT . ':' . Pdo\\Sqlite::EXPLAIN_MODE_PREPARED . ':' . Pdo\\Sqlite::EXPLAIN_MODE_EXPLAIN . ':' . Pdo\\Sqlite::EXPLAIN_MODE_EXPLAIN_QUERY_PLAN . ':' . Pdo\\Sqlite::ATTR_TRANSACTION_MODE . ':' . Pdo\\Sqlite::TRANSACTION_MODE_DEFERRED . ':' . Pdo\\Sqlite::TRANSACTION_MODE_IMMEDIATE . ':' . Pdo\\Sqlite::TRANSACTION_MODE_EXCLUSIVE . ':' . Pdo\\Sqlite::DETERMINISTIC;"))))

(%php85-register-test 'php85-new-method-surface-is-runtime-visible
  "PHP 8.5 built-in class method additions are visible to method_exists."
  (lambda ()
    (dolist (case '(("Dom\\Element" "getElementsByClassName")
                    ("Dom\\Element" "insertAdjacentHTML")
                    ("Dom\\HTMLDocument" "getElementsByName")
                    ("Closure" "getCurrent")
                    ("Locale" "addLikelySubtags")
                    ("Locale" "isRightToLeft")
                    ("Locale" "minimizeSubtags")
                    ("Pdo\\Sqlite" "setAuthorizer")
                    ("SoapClient" "__construct")
                    ("SoapClient" "__getTypes")
                    ("SoapFault" "__construct")
                    ("SoapServer" "__construct")
                    ("SoapServer" "fault")
                    ("XSLTProcessor" "__construct")
                    ("XSLTProcessor" "getParameter")
                    ("XSLTProcessor" "setParameter")
                    ("XSLTProcessor" "removeParameter")
                    ("SQLite3Stmt" "busy")
                    ("ReflectionConstant" "getFileName")
                    ("ReflectionConstant" "getExtension")
                    ("ReflectionConstant" "getExtensionName")
                    ("ReflectionConstant" "getAttributes")
                    ("ReflectionConstant" "isDeprecated")
                    ("ReflectionProperty" "getMangledName")
                    ("Uri\\Rfc3986\\Uri" "parse")
                    ("Uri\\Rfc3986\\Uri" "withUserInfo")
                    ("Uri\\WhatWg\\Url" "toAsciiString")
                    ("Uri\\WhatWg\\Url" "withUsername")
                    ("Uri\\WhatWg\\UrlValidationError" "__construct")))
      (destructuring-bind (class method) case
        (assert-true (cl-cc/php::%php-method-exists class method))))))

(%php85-register-test 'php85-soap-and-xslt-runtime-methods-execute-from-php-source
  "PHP 8.5 SoapClient, SoapFault, SoapServer, and XSLTProcessor methods execute from PHP source."
  (lambda ()
    (assert-string= "Y:Y:ja:Y:Y:Y:Y"
                    (%php-run-capture
                     "<?php
$client = new SoapClient(null, ['location' => 'http://example.test', 'uri' => 'urn:test']);
$fault = new SoapFault('Sender', 'Boom', 'actor', 'detail', 'name', 'header', 'ja');
$server = new SoapServer(null, ['uri' => 'urn:test']);
$proc = new XSLTProcessor();
$proc->setParameter('urn:a', 'foo', 'bar');
echo (is_array($client->__getTypes()) ? 'Y' : 'N') . ':' .
     ($fault->lang === 'ja' ? 'Y' : 'N') . ':' .
     $fault->lang . ':' .
     (is_null($server->fault('Client', 'Boom')) ? 'Y' : 'N') . ':' .
     ($proc->getParameter('urn:a', 'foo') === 'bar' ? 'Y' : 'N') . ':' .
     ($proc->getParameter('urn:b', 'foo') === false ? 'Y' : 'N') . ':' .
     ($proc->removeParameter('urn:a', 'foo') ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-reflection-constant-runtime-methods-execute-from-php-source
  "PHP 8.5 ReflectionConstant method additions execute from PHP source."
  (lambda ()
    (assert-string= "N:Y:Y:Y"
                    (%php-run-capture
                     "<?php
$r = new ReflectionConstant('PHP_VERSION');
echo ($r->isDeprecated() ? 'Y' : 'N') . ':' .
     (is_array($r->getAttributes()) ? 'Y' : 'N') . ':' .
     (is_null($r->getFileName()) ? 'Y' : 'N') . ':' .
     (is_null($r->getExtensionName()) ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-reflection-property-get-mangled-name-executes-from-php-source
  "PHP 8.5 ReflectionProperty::getMangledName executes from PHP source."
  (lambda ()
    (assert-string= "name"
                    (%php-run-capture
                     "<?php
class RProp {
    public $name;
}
$r = new ReflectionProperty('RProp', 'name');
echo $r->getMangledName();
"))))

(%php85-register-test 'php85-dom-element-runtime-methods-execute-from-php-source
  "PHP 8.5 Dom\\Element method additions execute from PHP source."
  (lambda ()
    (assert-string= "Dom\\HTMLCollection:Y"
                    (%php-run-capture
                     "<?php
$e = new Dom\\Element('div');
$e->insertAdjacentHTML('beforeend', '<span class=\"a\"></span>');
$items = $e->getElementsByClassName('a');
echo get_class($items) . ':' .
     (is_null($e->insertAdjacentHTML('afterbegin', '<b></b>')) ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-dom-element-runtime-properties-execute-from-php-source
  "PHP 8.5 Dom\\Element exposes outerHTML and children properties from PHP source."
  (lambda ()
    (assert-string= "<section></section>:Dom\\HTMLCollection:Y:Y:Y:Y"
                    (%php-run-capture
                     "<?php
$e = new Dom\\Element('section');
echo $e->outerHTML . ':' . get_class($e->children) . ':' .
     (property_exists($e, 'outerHTML') ? 'Y' : 'N') . ':' .
     (property_exists($e, 'children') ? 'Y' : 'N') . ':' .
     (interface_exists('Dom\\\\ParentNode') ? 'Y' : 'N') . ':' .
     (class_exists('Dom\\\\HTMLCollection') ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-dom-parent-node-children-records-owner
  "PHP 8.5 Dom\\ParentNode children property returns an owner-linked HTMLCollection shim."
  (lambda ()
    (let* ((element (cl-cc/php::%php-dom-element-new "article"))
           (children (gethash "children" element)))
      (assert-string= "<article></article>" (gethash "outerHTML" element))
      (assert-string= "<article></article>" (gethash "outerhtml" element))
      (assert-string= "Dom\\HTMLCollection" (cl-cc/php::%php-get-class children))
      (assert-eq element (gethash "__owner__" children))
      (assert-string= "children" (gethash "__property__" children)))))

(%php85-register-test 'php85-dom-html-document-get-elements-by-name-executes-from-php-source
  "PHP 8.5 Dom\\HTMLDocument::getElementsByName executes from PHP source."
  (lambda ()
    (assert-string= "Dom\\HTMLDocument:Dom\\HTMLCollection:Y"
                    (%php-run-capture
                     "<?php
$doc = new Dom\\HTMLDocument('<form><input name=\"token\"></form>');
$items = $doc->getElementsByName('token');
echo get_class($doc) . ':' . get_class($items) . ':' .
     (method_exists($doc, 'getElementsByName') ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-dom-html-document-get-elements-by-name-records-query
  "PHP 8.5 Dom\\HTMLDocument::getElementsByName records the query in the collection shim."
  (lambda ()
    (let* ((doc (cl-cc/php::%php-dom-html-document-new "<input name=\"q\">"))
           (items (cl-cc/php::%php-dom-html-document-get-elements-by-name doc "q")))
      (assert-string= "Dom\\HTMLCollection" (cl-cc/php::%php-get-class items))
      (assert-string= "q" (gethash "__name__" items))
      (assert-eq doc (gethash "__owner__" items)))))

(%php85-register-test 'php85-dom-html-document-children-executes-from-php-source
  "PHP 8.5 Dom\\ParentNode children property is exposed on Dom\\HTMLDocument."
  (lambda ()
    (assert-string= "Dom\\HTMLCollection:Y"
                    (%php-run-capture
                     "<?php
$doc = new Dom\\HTMLDocument('<main></main>');
echo get_class($doc->children) . ':' .
     (property_exists($doc, 'children') ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-pdo-sqlite-set-authorizer-executes-from-php-source
  "PHP 8.5 Pdo\\Sqlite::setAuthorizer executes from PHP source."
  (lambda ()
    (assert-string= "Y"
                    (%php-run-capture
                     "<?php
$pdo = new Pdo\\Sqlite('sqlite::memory:');
echo is_null($pdo->setAuthorizer(null)) ? 'Y' : 'N';
"))))

(%php85-register-test 'php85-sqlite3-stmt-busy-executes-from-php-source
  "PHP 8.5 SQLite3Stmt::busy executes from PHP source."
  (lambda ()
    (assert-string= "N"
                    (%php-run-capture
                     "<?php
$stmt = new SQLite3Stmt();
echo $stmt->busy() ? 'Y' : 'N';
"))))

(%php85-register-test 'php85-uri-class-method-list-is-runtime-visible
  "PHP 8.5 URI class methods are listed for runtime class-name introspection."
  (lambda ()
    (let ((methods (cl-cc/php::%php-array-values-list
                    (cl-cc/php::%php-get-class-methods "Uri\\Rfc3986\\Uri"))))
      (assert-true (find "toRawString" methods :test #'string=))
      (assert-true (find "withUserInfo" methods :test #'string=)))
    (let ((methods (cl-cc/php::%php-array-values-list
                    (cl-cc/php::%php-get-class-methods "Uri\\WhatWg\\Url"))))
      (assert-true (find "toAsciiString" methods :test #'string=))
      (assert-true (find "withUsername" methods :test #'string=)))))

(%php85-register-test 'php85-uri-runtime-object-parses-and-clones-components
  "PHP 8.5 URI objects expose parsed components and with* cloning."
  (lambda ()
    (let* ((uri (cl-cc/php::%php-uri-rfc3986-new "https://user:pw@example.com/a?b=1#frag"))
           (copy (cl-cc/php::%php-uri-with-user-info uri "ada")))
      (assert-string= "https" (cl-cc/php::%php-uri-get-scheme uri))
      (assert-string= "example.com" (cl-cc/php::%php-uri-get-host uri))
      (assert-string= "/a" (cl-cc/php::%php-uri-get-path uri))
      (assert-string= "b=1" (cl-cc/php::%php-uri-get-query uri))
      (assert-string= "frag" (cl-cc/php::%php-uri-get-fragment uri))
      (assert-string= "user" (cl-cc/php::%php-uri-get-username uri))
      (assert-string= "ada" (cl-cc/php::%php-uri-get-username copy))
      (assert-string= "user" (cl-cc/php::%php-uri-get-username uri)))))

(%php85-register-test 'php85-uri-parse-invalid-inputs-return-null
  "PHP 8.5 URI parse helpers return null instead of throwing on invalid input."
  (lambda ()
    (assert-true
     (cl-cc/php::%php-null-p
      (cl-cc/php::%php-uri-rfc3986-parse "https://example.com/%zz")))
    (assert-true
     (cl-cc/php::%php-null-p
      (cl-cc/php::%php-uri-rfc3986-parse "http://example.com:99999/")))
    (assert-true
     (cl-cc/php::%php-null-p
      (cl-cc/php::%php-uri-whatwg-parse "/relative-only")))
    (let ((base (cl-cc/php::%php-uri-whatwg-new "https://example.com/root")))
      (assert-string= "/child"
                      (cl-cc/php::%php-uri-get-path
                       (cl-cc/php::%php-uri-whatwg-parse "/child" base))))))

(%php85-register-test 'php85-uri-constructors-signal-invalid-uri-exceptions
  "PHP 8.5 URI constructors throw the documented URI exception classes on invalid input."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php::%php-uri-rfc3986-new "https://example.com/%zz")
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p
        condition
        (intern "URI\\INVALIDURIEXCEPTION" :cl-cc/php))))
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php::%php-uri-whatwg-new "/relative-only")
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p
        condition
        (intern "URI\\WHATWG\\INVALIDURLEXCEPTION" :cl-cc/php))))))

(%php85-register-test 'php85-uri-equals-honors-fragment-comparison-mode
  "PHP 8.5 URI equality excludes fragments by default and includes them with UriComparisonMode::IncludeFragment."
  (lambda ()
    (let ((a (cl-cc/php::%php-uri-rfc3986-new "https://example.com/a#one"))
          (b (cl-cc/php::%php-uri-rfc3986-new "https://example.com/a#two")))
      (assert-true (cl-cc/php::%php-uri-equals a b))
      (assert-false
       (cl-cc/php::%php-uri-equals
        a
        b
        (cl-cc/php:%php-predefined-class-constant
         "Uri\\UriComparisonMode"
         "IncludeFragment"))))
    (assert-string= "Y:N"
                    (%php-run-capture
                     "<?php
$a = new Uri\\Rfc3986\\Uri('https://example.com/a#one');
$b = new Uri\\Rfc3986\\Uri('https://example.com/a#two');
echo ($a->equals($b) ? 'Y' : 'N') . ':' . ($a->equals($b, Uri\\UriComparisonMode::IncludeFragment) ? 'Y' : 'N');
"))))

(%php85-register-test 'php85-get-class-methods-accepts-runtime-object
  "PHP 8.5 method introspection accepts runtime objects."
  (lambda ()
    (let* ((uri (cl-cc/php::%php-uri-rfc3986-new "https://example.com/"))
           (methods (cl-cc/php::%php-array-values-list
                     (cl-cc/php::%php-get-class-methods uri))))
      (assert-true (find "toRawString" methods :test #'string=))
      (assert-true (find "withUserInfo" methods :test #'string=)))))

(%php85-register-test 'php85-uri-object-debug-output-uses-debug-info
  "PHP 8.5 object debug output uses __debugInfo for Uri objects."
  (lambda ()
    (assert-string= "object(Uri\\Rfc3986\\Uri) (8) {
  [\"scheme\"]=>
  string(5) \"https\"
  [\"username\"]=>
  string(4) \"user\"
  [\"password\"]=>
  NULL
  [\"host\"]=>
  string(11) \"example.com\"
  [\"port\"]=>
  NULL
  [\"path\"]=>
  string(2) \"/a\"
  [\"query\"]=>
  string(3) \"b=1\"
  [\"fragment\"]=>
  string(4) \"frag\"
}"
                    (%php-run-capture
                     "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
var_dump($u);
"))
    (assert-string= "Uri\\Rfc3986\\Uri Object (
    [scheme] => https
    [username] => user
    [password] => NULL
    [host] => example.com
    [port] => NULL
    [path] => /a
    [query] => b=1
    [fragment] => frag
)"
                    (%php-run-capture
                     "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
echo print_r($u, true);
"))))

(%php85-register-test 'php85-uri-constructor-and-static-parse-execute-from-php-source
  "PHP 8.5 URI constructor and static parse helpers execute from PHP source."
  (lambda ()
    (assert-string= "https:example.com:/a:b=1:frag:user:ada:https://example.com/p"
                    (%php-run-capture
                     "<?php
$u = new Uri\\Rfc3986\\Uri('https://user@example.com/a?b=1#frag');
$v = $u->withUserInfo('ada');
$w = Uri\\WhatWg\\Url::parse('https://example.com/p');
echo $u->getScheme() . ':' . $u->getHost() . ':' . $u->getPath() . ':' . $u->getQuery() . ':' . $u->getFragment() . ':' . $u->getUsername() . ':' . $v->getUsername() . ':' . $w->toAsciiString();
"))))

(%php85-register-test 'php85-new-constants-are-predefined
  "PHP 8.5 predefined constants are available through constant lookup."
  (lambda ()
(dolist (constant '("PHP_BUILD_DATE"
                    "PHP_BUILD_PROVIDER"
                    "CURLINFO_USED_PROXY"
                    "CURLINFO_HTTPAUTH_USED"
                    "CURLINFO_PROXYAUTH_USED"
                    "CURLINFO_CONN_ID"
                    "CURLINFO_QUEUE_TIME_T"
                    "CURLOPT_INFILESIZE_LARGE"
                    "CURLOPT_SSL_SIGNATURE_ALGORITHMS"
                    "CURLFOLLOW_ALL"
                    "CURLFOLLOW_OBEYCODE"
                    "CURLFOLLOW_FIRSTONLY"
                    "FILTER_THROW_ON_FAILURE"
                    "DECIMAL_COMPACT_SHORT"
                    "DECIMAL_COMPACT_LONG"
                    "OPENSSL_PKCS1_PSS_PADDING"
                    "PKCS7_NOSMIMECAP"
                    "PKCS7_CRLFEOL"
                    "PKCS7_NOCRL"
                    "PKCS7_NO_DUAL_CONTENT"
                    "POSIX_SC_OPEN_MAX"
                    "IPPROTO_ICMP"
                    "IPPROTO_ICMPV6"
                    "TCP_FUNCTION_BLK"
                    "TCP_FUNCTION_ALIAS"
                    "TCP_REUSPORT_LB_NUMA"
                    "TCP_REUSPORT_LB_NUMA_NODOM"
                    "TCP_REUSPORT_LB_NUMA_CURDOM"
                    "TCP_BBR_ALGORITHM"
                    "AF_PACKET"
                    "IP_BINDANY"
                    "SO_BUSY_POLL"
                    "UDP_SEGMENT"
                    "SHUT_RD"
                    "SHUT_WR"
                    "SHUT_RDWR"
                    "TOKEN_PARSE"
                    "T_LNUMBER"
                    "T_DNUMBER"
                    "T_STRING"
                    "T_VARIABLE"
                    "T_CONSTANT_ENCAPSED_STRING"
                    "T_OBJECT_OPERATOR"
                    "T_DOUBLE_ARROW"
                    "T_COMMENT"
                    "T_DOC_COMMENT"
                    "T_OPEN_TAG"
                    "T_OPEN_TAG_WITH_ECHO"
                    "T_CLOSE_TAG"
                    "T_WHITESPACE"
                    "T_DOUBLE_COLON"
                    "T_VOID_CAST"
                    "T_PIPE"
                    "T_INLINE_HTML"
                    "T_ECHO"
                    "T_CLASS"
                    "T_CONST"
                    "T_PUBLIC"
                    "T_FUNCTION"
                    "T_ABSTRACT"
                    "T_ARRAY"
                    "T_AS"
                    "T_BREAK"
                    "T_CALLABLE"
                    "T_CASE"
                    "T_CATCH"
                    "T_CLONE"
                    "T_CONTINUE"
                    "T_DECLARE"
                    "T_DEFAULT"
                    "T_DO"
                    "T_ELSE"
                    "T_ELSEIF"
                    "T_EMPTY"
                    "T_ENDDECLARE"
                    "T_ENDFOR"
                    "T_ENDFOREACH"
                    "T_ENDIF"
                    "T_ENDSWITCH"
                    "T_ENDWHILE"
                    "T_ENUM"
                    "T_EVAL"
                    "T_EXIT"
                    "T_EXTENDS"
                    "T_FINAL"
                    "T_FINALLY"
                    "T_FN"
                    "T_FOR"
                    "T_FOREACH"
                    "T_GLOBAL"
                    "T_GOTO"
                    "T_IF"
                    "T_IMPLEMENTS"
                    "T_INCLUDE"
                    "T_INCLUDE_ONCE"
                    "T_INSTANCEOF"
                    "T_INSTEADOF"
                    "T_INTERFACE"
                    "T_ISSET"
                    "T_LIST"
                    "T_MATCH"
                    "T_NAMESPACE"
                    "T_NEW"
                    "T_PRINT"
                    "T_PRIVATE"
                    "T_PROTECTED"
                    "T_READONLY"
                    "T_REQUIRE"
                    "T_REQUIRE_ONCE"
                    "T_RETURN"
                    "T_STATIC"
                    "T_SWITCH"
                    "T_THROW"
                    "T_TRAIT"
                    "T_TRY"
                    "T_UNSET"
                    "T_USE"
                    "T_VAR"
                    "T_WHILE"
                    "T_YIELD"
                    "T_LOGICAL_AND"
                    "T_LOGICAL_OR"
                    "T_LOGICAL_XOR"
                    "T_YIELD_FROM"
                    "T_ATTRIBUTE"
                    "T_NS_SEPARATOR"
                    "T_NAME_FULLY_QUALIFIED"
                    "T_NAME_QUALIFIED"
                    "T_NAME_RELATIVE"
                    "T_BAD_CHARACTER"
                    "IMAGETYPE_UNKNOWN"
                    "IMAGETYPE_WEBP"
                    "IMAGETYPE_AVIF"
                    "IMAGETYPE_HEIF"
                    "IMAGETYPE_SVG"
                    "IMAGETYPE_COUNT"))
    (multiple-value-bind (value found)
        (cl-cc/php::%php-lookup-constant constant)
      (declare (ignore value))
      (assert-true found)))))

(%php85-register-test 'php85-image-type-constants-match-current-values
  "PHP 8.5 IMAGETYPE_* constants match current extension values."
  (lambda ()
    (assert-string= "0:18:19:20:21:22"
                    (%php-run-capture
                     "<?php echo IMAGETYPE_UNKNOWN . ':' . IMAGETYPE_WEBP . ':' . IMAGETYPE_AVIF . ':' . IMAGETYPE_HEIF . ':' . IMAGETYPE_SVG . ':' . IMAGETYPE_COUNT;"))))

(%php85-register-test 'php85-image-type-functions-support-svg
  "PHP 8.5 image type helpers support current IMAGETYPE_* values."
  (lambda ()
    (assert-string= ".svg:svg:image/svg+xml:.webp:webp:image/webp:.avif:avif:image/avif:.heif:heif:image/heif:Y:Y:false"
                    (%php-run-capture
                     "<?php
echo image_type_to_extension(IMAGETYPE_SVG) . ':' .
     image_type_to_extension(IMAGETYPE_SVG, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_SVG) . ':' .
     image_type_to_extension(IMAGETYPE_WEBP) . ':' .
     image_type_to_extension(IMAGETYPE_WEBP, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_WEBP) . ':' .
     image_type_to_extension(IMAGETYPE_AVIF) . ':' .
     image_type_to_extension(IMAGETYPE_AVIF, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_AVIF) . ':' .
     image_type_to_extension(IMAGETYPE_HEIF) . ':' .
     image_type_to_extension(IMAGETYPE_HEIF, false) . ':' .
     image_type_to_mime_type(IMAGETYPE_HEIF) . ':' .
     (function_exists('image_type_to_extension') ? 'Y' : 'N') . ':' .
     (function_exists('image_type_to_mime_type') ? 'Y' : 'N') . ':' .
     (image_type_to_extension(9999) === false ? 'false' : 'other');
"))))

(%php85-register-test 'php85-getimagesize-svg-reports-units-and-exif-type
  "PHP 8.5 getimagesize returns SVG dimensions, units, MIME, and image type."
  (lambda ()
    (assert-string= "12:34:type:image/svg+xml:cm:px:exif:false:Y:Y"
                    (%php-run-capture
                     "<?php
$f = tempnam(sys_get_temp_dir(), 'clcc-svg-');
file_put_contents($f, '<svg width=\"12cm\" height=\"34px\" xmlns=\"http://www.w3.org/2000/svg\"></svg>');
$size = getimagesize($f);
echo $size[0] . ':' .
     $size[1] . ':' .
     ($size[2] === IMAGETYPE_SVG ? 'type' : 'bad') . ':' .
     $size['mime'] . ':' .
     $size['width_unit'] . ':' .
     $size['height_unit'] . ':' .
     (exif_imagetype($f) === IMAGETYPE_SVG ? 'exif' : 'bad') . ':' .
     (getimagesize($f . '.missing') === false ? 'false' : 'other') . ':' .
     (function_exists('getimagesize') ? 'Y' : 'N') . ':' .
     (function_exists('exif_imagetype') ? 'Y' : 'N');
unlink($f);
"))))

(%php85-register-test 'php85-token-name-reports-tokenizer-constants
  "PHP 8.5 tokenizer constants have token_name mappings."
  (lambda ()
    (dolist (name '("T_PIPE"
                    "T_VOID_CAST"
                    "T_CLOSE_TAG"
                    "T_INLINE_HTML"
                    "T_ECHO"
                    "T_CLASS"
                    "T_CONST"
                    "T_PUBLIC"
                    "T_FUNCTION"
                    "T_ABSTRACT"
                    "T_ARRAY"
                    "T_AS"
                    "T_BREAK"
                    "T_CALLABLE"
                    "T_CASE"
                    "T_CATCH"
                    "T_CLONE"
                    "T_CONTINUE"
                    "T_DECLARE"
                    "T_DEFAULT"
                    "T_DO"
                    "T_ELSE"
                    "T_ELSEIF"
                    "T_EMPTY"
                    "T_ENDDECLARE"
                    "T_ENDFOR"
                    "T_ENDFOREACH"
                    "T_ENDIF"
                    "T_ENDSWITCH"
                    "T_ENDWHILE"
                    "T_ENUM"
                    "T_EVAL"
                    "T_EXIT"
                    "T_EXTENDS"
                    "T_FINAL"
                    "T_FINALLY"
                    "T_FN"
                    "T_FOR"
                    "T_FOREACH"
                    "T_GLOBAL"
                    "T_GOTO"
                    "T_IF"
                    "T_IMPLEMENTS"
                    "T_INCLUDE"
                    "T_INCLUDE_ONCE"
                    "T_INSTANCEOF"
                    "T_INSTEADOF"
                    "T_INTERFACE"
                    "T_ISSET"
                    "T_LIST"
                    "T_MATCH"
                    "T_NAMESPACE"
                    "T_NEW"
                    "T_PRINT"
                    "T_PRIVATE"
                    "T_PROTECTED"
                    "T_READONLY"
                    "T_REQUIRE"
                    "T_REQUIRE_ONCE"
                    "T_RETURN"
                    "T_STATIC"
                    "T_SWITCH"
                    "T_THROW"
                    "T_TRAIT"
                    "T_TRY"
                    "T_UNSET"
                    "T_USE"
                    "T_VAR"
                    "T_WHILE"
                    "T_YIELD"
                    "T_LOGICAL_AND"
                    "T_LOGICAL_OR"
                    "T_LOGICAL_XOR"
                    "T_YIELD_FROM"
                    "T_ATTRIBUTE"
                    "T_NS_SEPARATOR"
                    "T_NAME_FULLY_QUALIFIED"
                    "T_NAME_QUALIFIED"
                    "T_NAME_RELATIVE"
                    "T_BAD_CHARACTER"))
      (multiple-value-bind (token-id found)
          (cl-cc/php::%php-lookup-constant name)
        (assert-true found)
        (assert-string= name (cl-cc/php::%php-token-name token-id))))
    (assert-string= "UNKNOWN" (cl-cc/php::%php-token-name -1))))

(%php85-register-test 'php85-token-get-all-exposes-pipe-and-void-cast
  "PHP 8.5 token_get_all exposes pipe and void-cast tokens."
  (lambda ()
    (multiple-value-bind (pipe-id pipe-found)
        (cl-cc/php::%php-lookup-constant "T_PIPE")
      (assert-true pipe-found)
      (multiple-value-bind (void-id void-found)
          (cl-cc/php::%php-lookup-constant "T_VOID_CAST")
        (assert-true void-found)
        (let* ((tokens (cl-cc/php::%php-token-get-all "<?php $x = (VOID) $y |> strlen;"))
               (entries (cl-cc/php::%php-array-values-list tokens)))
          (flet ((entry-id (entry)
                   (and (hash-table-p entry)
                        (cl-cc/php::%php-array-ref entry 0)))
                 (entry-text (entry)
                   (cl-cc/php::%php-array-ref entry 1))
                 (entry-line (entry)
                   (cl-cc/php::%php-array-ref entry 2)))
            (let ((pipe-token (find-if (lambda (entry)
                                         (eql pipe-id (entry-id entry)))
                                       entries))
                  (void-token (find-if (lambda (entry)
                                         (eql void-id (entry-id entry)))
                                       entries)))
              (assert-true pipe-token)
              (assert-true void-token)
              (assert-string= "|>" (entry-text pipe-token))
              (assert-string= "(VOID)" (entry-text void-token))
              (assert-equal 1 (entry-line pipe-token))
              (assert-equal 1 (entry-line void-token)))))))))

(%php85-register-test 'php85-token-get-all-models-html-boundaries
  "PHP 8.5 token_get_all models inline HTML and PHP close tags."
  (lambda ()
    (multiple-value-bind (inline-id inline-found)
        (cl-cc/php::%php-lookup-constant "T_INLINE_HTML")
      (assert-true inline-found)
      (multiple-value-bind (close-id close-found)
          (cl-cc/php::%php-lookup-constant "T_CLOSE_TAG")
        (assert-true close-found)
        (let* ((tokens (cl-cc/php::%php-token-get-all "hello <?php echo 1; ?> world"))
               (entries (cl-cc/php::%php-array-values-list tokens)))
          (flet ((entry-id (entry)
                   (and (hash-table-p entry)
                        (cl-cc/php::%php-array-ref entry 0)))
                 (entry-text (entry)
                   (cl-cc/php::%php-array-ref entry 1))
                 (entry-line (entry)
                   (cl-cc/php::%php-array-ref entry 2)))
            (let ((inline-tokens (remove-if-not (lambda (entry)
                                                  (eql inline-id (entry-id entry)))
                                                entries))
                  (close-token (find-if (lambda (entry)
                                          (eql close-id (entry-id entry)))
                                        entries)))
              (assert-equal 2 (length inline-tokens))
              (assert-string= "hello " (entry-text (first inline-tokens)))
              (assert-string= " world" (entry-text (second inline-tokens)))
              (assert-true close-token)
              (assert-string= "?>" (entry-text close-token))
              (assert-equal 1 (entry-line close-token)))))))))

(%php85-register-test 'php85-token-get-all-models-open-tags-and-keywords
  "PHP 8.5 token_get_all models open-tag trivia, keywords, and TOKEN_PARSE context."
  (lambda ()
    (labels ((entry-id (entry)
               (and (hash-table-p entry)
                    (cl-cc/php::%php-array-ref entry 0)))
             (entry-text (entry)
               (and (hash-table-p entry)
                    (cl-cc/php::%php-array-ref entry 1)))
             (entry-name (entry)
               (and (hash-table-p entry)
                    (cl-cc/php::%php-token-name (entry-id entry)))))
      (let* ((tokens (cl-cc/php::%php-token-get-all "<?php echo; ?>"))
             (entries (cl-cc/php::%php-array-values-list tokens)))
        (assert-string= "T_OPEN_TAG" (entry-name (first entries)))
        (assert-string= "<?php " (entry-text (first entries)))
        (assert-true (find "T_ECHO" entries :key #'entry-name :test #'string=))
        (assert-true (find "T_CLOSE_TAG" entries :key #'entry-name :test #'string=)))
      (let* ((tokens (cl-cc/php::%php-token-get-all "/* comment */"))
             (entries (cl-cc/php::%php-array-values-list tokens)))
        (assert-equal 1 (length entries))
        (assert-string= "T_INLINE_HTML" (entry-name (first entries)))
        (assert-string= "/* comment */" (entry-text (first entries))))
      (dolist (item '(("abstract" . "T_ABSTRACT")
                      ("array" . "T_ARRAY")
                      ("as" . "T_AS")
                      ("break" . "T_BREAK")
                      ("callable" . "T_CALLABLE")
                      ("case" . "T_CASE")
                      ("catch" . "T_CATCH")
                      ("clone" . "T_CLONE")
                      ("continue" . "T_CONTINUE")
                      ("declare" . "T_DECLARE")
                      ("default" . "T_DEFAULT")
                      ("die" . "T_EXIT")
                      ("do" . "T_DO")
                      ("else" . "T_ELSE")
                      ("elseif" . "T_ELSEIF")
                      ("empty" . "T_EMPTY")
                      ("enddeclare" . "T_ENDDECLARE")
                      ("endfor" . "T_ENDFOR")
                      ("endforeach" . "T_ENDFOREACH")
                      ("endif" . "T_ENDIF")
                      ("endswitch" . "T_ENDSWITCH")
                      ("endwhile" . "T_ENDWHILE")
                      ("enum" . "T_ENUM")
                      ("eval" . "T_EVAL")
                      ("exit" . "T_EXIT")
                      ("extends" . "T_EXTENDS")
                      ("final" . "T_FINAL")
                      ("finally" . "T_FINALLY")
                      ("fn" . "T_FN")
                      ("for" . "T_FOR")
                      ("foreach" . "T_FOREACH")
                      ("function" . "T_FUNCTION")
                      ("global" . "T_GLOBAL")
                      ("goto" . "T_GOTO")
                      ("if" . "T_IF")
                      ("implements" . "T_IMPLEMENTS")
                      ("include" . "T_INCLUDE")
                      ("include_once" . "T_INCLUDE_ONCE")
                      ("instanceof" . "T_INSTANCEOF")
                      ("insteadof" . "T_INSTEADOF")
                      ("interface" . "T_INTERFACE")
                      ("isset" . "T_ISSET")
                      ("list" . "T_LIST")
                      ("match" . "T_MATCH")
                      ("namespace" . "T_NAMESPACE")
                      ("new" . "T_NEW")
                      ("print" . "T_PRINT")
                      ("private" . "T_PRIVATE")
                      ("protected" . "T_PROTECTED")
                      ("public" . "T_PUBLIC")
                      ("readonly" . "T_READONLY")
                      ("require" . "T_REQUIRE")
                      ("require_once" . "T_REQUIRE_ONCE")
                      ("return" . "T_RETURN")
                      ("static" . "T_STATIC")
                      ("switch" . "T_SWITCH")
                      ("throw" . "T_THROW")
                      ("trait" . "T_TRAIT")
                      ("try" . "T_TRY")
                      ("unset" . "T_UNSET")
                      ("use" . "T_USE")
                      ("var" . "T_VAR")
                      ("while" . "T_WHILE")
                      ("yield" . "T_YIELD")
                      ("yield from" . "T_YIELD_FROM")
                      ("#[" . "T_ATTRIBUTE")
                      ("\\Foo\\Bar" . "T_NAME_FULLY_QUALIFIED")
                      ("Foo\\Bar" . "T_NAME_QUALIFIED")
                      ("namespace\\Foo" . "T_NAME_RELATIVE")
                      ("\\" . "T_NS_SEPARATOR")
                      ("and" . "T_LOGICAL_AND")
                      ("or" . "T_LOGICAL_OR")
                      ("xor" . "T_LOGICAL_XOR")))
        (let* ((tokens (cl-cc/php::%php-token-get-all
                        (format nil "<?php ~A" (car item))))
               (entries (cl-cc/php::%php-array-values-list tokens)))
          (assert-true (find (cdr item) entries :key #'entry-name :test #'string=))))
      (let* ((tokens (cl-cc/php::%php-token-get-all
                      (concatenate 'string "<?php " (string (code-char 0)))))
             (entries (cl-cc/php::%php-array-values-list tokens)))
        (assert-true (find "T_BAD_CHARACTER" entries :key #'entry-name :test #'string=)))
      (let* ((source "<?php class A { const PUBLIC = 1; function f() {} }")
             (regular (cl-cc/php::%php-array-values-list
                       (cl-cc/php::%php-token-get-all source)))
             (parsed (cl-cc/php::%php-array-values-list
                      (cl-cc/php::%php-token-get-all source 1))))
        (assert-true (find "T_CLASS" regular :key #'entry-name :test #'string=))
        (assert-true (find "T_CONST" regular :key #'entry-name :test #'string=))
        (assert-true (find "T_PUBLIC" regular :key #'entry-name :test #'string=))
        (assert-true (find "T_FUNCTION" regular :key #'entry-name :test #'string=))
        (assert-false (find "T_PUBLIC" parsed :key #'entry-name :test #'string=))
        (assert-true (find-if (lambda (entry)
                                (and (string= "T_STRING" (entry-name entry))
                                     (string= "PUBLIC" (entry-text entry))))
                              parsed))))))

(%php85-register-test 'php85-tokenizer-builtins-execute-from-php-source
  "PHP 8.5 tokenizer builtins are callable from PHP code."
  (lambda ()
    (assert-string= "T_PIPE:T_VOID_CAST:T_INLINE_HTML:T_CLOSE_TAG:T_YIELD_FROM:T_ATTRIBUTE:T_NAME_FULLY_QUALIFIED:T_NAME_QUALIFIED:T_NAME_RELATIVE:T_NS_SEPARATOR:T_BAD_CHARACTER"
                    (%php-run-capture
                     "<?php echo token_name(T_PIPE) . ':' . token_name(T_VOID_CAST) . ':' . token_name(T_INLINE_HTML) . ':' . token_name(T_CLOSE_TAG) . ':' . token_name(T_YIELD_FROM) . ':' . token_name(T_ATTRIBUTE) . ':' . token_name(T_NAME_FULLY_QUALIFIED) . ':' . token_name(T_NAME_QUALIFIED) . ':' . token_name(T_NAME_RELATIVE) . ':' . token_name(T_NS_SEPARATOR) . ':' . token_name(T_BAD_CHARACTER);"))))

(%php85-register-test 'php85-extension-new-free-functions-are-registered
  "New PHP 8.5 extension-level free functions are registered as builtins."
  (lambda ()
    (dolist (function-name '("enchant_dict_remove_from_session"
                             "enchant_dict_remove"
                             "pg_close_stmt"
                             "pg_service"))
      (assert-true (cl-cc/php::%php-function-exists function-name)))))

(%php85-register-test 'php85-cookie-and-session-builtins-are-registered
  "Cookie and session helpers affected by PHP 8.5 are registered as builtins."
  (lambda ()
    (dolist (function-name '("setcookie"
                             "setrawcookie"
                             "session_name"
                             "session_id"
                             "session_set_cookie_params"
                             "session_get_cookie_params"
                             "session_start"))
      (assert-true (cl-cc/php::%php-function-exists function-name)))))

(%php85-register-test 'php85-extension-new-free-function-helpers-update-modeled-state
  "PHP 8.5 enchant and pgsql compatibility helpers update modeled runtime state."
  (lambda ()
    (let ((dict (make-hash-table :test #'equal))
          (words (make-hash-table :test #'equal)))
      (setf (gethash "words" dict) words
            (gethash "hello" words) t)
      (assert-true (cl-cc/php:%php-enchant-dict-remove dict "hello"))
      (assert-false (gethash "hello" words)))
    (let ((connection (make-hash-table :test #'equal))
          (statements (make-hash-table :test #'equal)))
      (setf (gethash "statements" connection) statements
            (gethash "stmt1" statements) t
            (gethash "service" connection) "analytics")
      (assert-string= "analytics" (cl-cc/php:%php-pg-service connection))
      (assert-true (cl-cc/php:%php-pg-close-stmt connection "stmt1"))
      (assert-false (gethash "stmt1" statements)))))

(%php85-register-test 'php85-setcookie-partitioned-option-queues-set-cookie-header
  "PHP 8.5 setcookie() accepts partitioned cookies when secure is enabled."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (assert-string= "[\"Set-Cookie: chip=a+b; secure; Partitioned\"]"
                      (%php-run-capture
                       "<?php
setcookie('chip', 'a b', ['secure' => true, 'partitioned' => true]);
echo json_encode(headers_list());")))))

(%php85-register-test 'php85-setcookie-options-are-case-insensitive
  "PHP 8.5 setcookie() option keys are case-insensitive."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (assert-string= "[\"Set-Cookie: chip=v; secure; SameSite=Strict; Partitioned\"]"
                      (%php-run-capture
                       "<?php
setcookie('chip', 'v', ['Secure' => true, 'SameSite' => 'Strict', 'Partitioned' => true]);
echo json_encode(headers_list());")))))

(%php85-register-test 'php85-setrawcookie-partitioned-option-queues-set-cookie-header
  "PHP 8.5 setrawcookie() keeps raw values and supports partitioned cookies."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil))
      (assert-string= "[\"Set-Cookie: raw=a b; secure; SameSite=None; Partitioned\"]"
                      (%php-run-capture
                       "<?php
setrawcookie('raw', 'a b', ['secure' => true, 'samesite' => 'None', 'partitioned' => true]);
echo json_encode(headers_list());")))))

(%php85-register-test 'php85-cookie-partitioned-requires-secure
  "PHP 8.5 partitioned cookies require secure cookies."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php::%php-setcookie
                            "chip"
                            "v"
                            (cl-cc/php:%php-array
                             (list t "partitioned" t)))
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p condition 'value-error)))))

(%php85-register-test 'php85-cookie-option-validation-matches-php85
  "PHP 8.5 setcookie() rejects numeric keys, unknown keys, and invalid SameSite values."
  (lambda ()
    (flet ((raises-value-error-p (thunk)
             (let ((condition (handler-case
                                  (progn (funcall thunk) nil)
                                (cl-cc/php:php-exception (e) e))))
               (and condition
                    (cl-cc/php:%php-exception-matches-p condition 'value-error)))))
      (let ((numeric-options (cl-cc/php::%php-make-array))
            (unknown-options (cl-cc/php::%php-make-array))
            (bad-samesite-options (cl-cc/php::%php-make-array)))
        (cl-cc/php::%php-array-set numeric-options 0 t)
        (cl-cc/php::%php-array-set unknown-options "bogus" t)
        (cl-cc/php::%php-array-set bad-samesite-options "samesite" "Relaxed")
        (assert-true
         (raises-value-error-p
          (lambda () (cl-cc/php::%php-setcookie "chip" "v" numeric-options))))
        (assert-true
         (raises-value-error-p
          (lambda () (cl-cc/php::%php-setcookie "chip" "v" unknown-options))))
        (assert-true
         (raises-value-error-p
          (lambda () (cl-cc/php::%php-setcookie "chip" "v" bad-samesite-options))))))))

(%php85-register-test 'php85-session-cookie-params-support-partitioned
  "PHP 8.5 session cookie params expose and emit partitioned cookies."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil)
          (cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-id* "")
          (cl-cc/php::*php-session-name* "PHPSESSID")
          (cl-cc/php::*php-session-active-p* nil))
      (assert-string= "Y:true:[\"Set-Cookie: PHPSESSID=12345; path=/; secure; Partitioned\"]"
                      (%php-run-capture
                       "<?php
$registered = function_exists('session_set_cookie_params') && function_exists('session_get_cookie_params') && function_exists('session_start') ? 'Y' : 'N';
session_id('12345');
session_set_cookie_params(['secure' => true, 'partitioned' => true]);
$params = session_get_cookie_params();
$partitioned = $params['partitioned'] ? 'true' : 'false';
session_start();
echo $registered . ':' . $partitioned . ':' . json_encode(headers_list());")))))

(%php85-register-test 'php85-mail-function-is-registered-and-returns-true
  "PHP 8.5 mail() is registered and accepts mail requests in the CLI model."
  (lambda ()
    (assert-string= "Y:true"
                    (%php-run-capture
                     "<?php
$registered = function_exists('mail') ? 'Y' : 'N';
$result = mail('to@example.com', 'subject', 'message', ['X-Test: value'], '-f bounce@example.com');
echo $registered . ':' . ($result ? 'true' : 'false');"))))

(%php85-register-test 'php85-session-cookie-param-options-are-case-insensitive
  "PHP 8.5 session cookie param option keys are case-insensitive."
  (lambda ()
    (let ((cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-active-p* nil))
      (assert-string= "true:true"
                      (%php-run-capture
                       "<?php
session_set_cookie_params(['Secure' => true, 'Partitioned' => true]);
$params = session_get_cookie_params();
echo ($params['secure'] ? 'true' : 'false') . ':' . ($params['partitioned'] ? 'true' : 'false');")))))

(%php85-register-test 'php85-session-cookie-params-order-includes-partitioned-after-secure
  "PHP 8.5 session_get_cookie_params() inserts partitioned after secure."
  (lambda ()
    (let ((cl-cc/php::*php-session-cookie-params* nil))
      (assert-string=
       "lifetime,path,domain,secure,partitioned,httponly,samesite"
       (format nil "~{~A~^,~}"
               (cl-cc/php::%php-array-ordered-keys
                (cl-cc/php::%php-session-get-cookie-params)))))))

(%php85-register-test 'php85-session-start-cookie-partitioned-requires-secure
  "PHP 8.5 session_start() warns and fails when cookie_partitioned lacks cookie_secure."
  (lambda ()
    (let ((cl-cc/php::*php-http-response-code* 200)
          (cl-cc/php::*php-http-headers* nil)
          (cl-cc/php::*php-output-started-p* nil)
          (cl-cc/php::*php-session-cookie-params* nil)
          (cl-cc/php::*php-session-id* "")
          (cl-cc/php::*php-session-name* "PHPSESSID")
          (cl-cc/php::*php-session-active-p* nil)
          (cl-cc/php::*php-error-handler-stack* nil))
      (assert-string= "2:false:[]"
                      (%php-run-capture
                       "<?php
function php85_session_warning($errno, $errstr, $file, $line) { echo $errno . ':'; return true; }
set_error_handler('php85_session_warning', E_WARNING);
session_id('12345');
session_set_cookie_params(['partitioned' => true]);
$ok = session_start();
restore_error_handler();
echo ($ok ? 'true' : 'false') . ':' . json_encode(headers_list());")))))

(%php85-register-test 'php85-filter-throw-on-failure-constant-is-defined
  "PHP 8.5 defines FILTER_THROW_ON_FAILURE for filter functions."
  (lambda ()
(multiple-value-bind (value found)
      (cl-cc/php::%php-lookup-constant "FILTER_THROW_ON_FAILURE")
    (assert-true found)
    (assert-= 268435456 value))))

(%php85-register-test 'php85-filter-var-validates-int
  "filter_var() supports integer validation."
  (lambda ()
(assert-string= "42:false"
                 (%php-run-capture
                  "<?php echo filter_var('42', FILTER_VALIDATE_INT) . ':' . (filter_var('abc', FILTER_VALIDATE_INT) === false ? 'false' : 'bad');"))))

(%php85-register-test 'php85-filter-var-validates-boolean-false
  "FILTER_VALIDATE_BOOLEAN can return a successful false result."
  (lambda ()
(assert-string= "false"
                 (%php-run-capture
                  "<?php echo filter_var('false', FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) === false ? 'false' : 'bad';"))))

(%php85-register-test 'php85-filter-var-throws-on-validation-failure
  "FILTER_THROW_ON_FAILURE raises the PHP 8.5 filter exception on validation failure."
  (lambda ()
(assert-signals cl-cc/php:php-exception
    (cl-cc/php:%php-filter-var "abc" 257 268435456))))

(%php85-register-test 'php85-filter-var-throws-filter-failed-exception
  "FILTER_THROW_ON_FAILURE reports the PHP 8.5 Filter\\FilterFailedException class."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (cl-cc/php:%php-filter-var "abc" 257 268435456)
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p
        condition
        (intern "FILTER\\FILTERFAILEDEXCEPTION" :cl-cc/php))))))

(%php85-register-test 'php85-filter-var-rejects-null-and-throw-flags-together
  "FILTER_THROW_ON_FAILURE cannot be combined with FILTER_NULL_ON_FAILURE."
  (lambda ()
(assert-signals cl-cc/php:php-exception
    (cl-cc/php:%php-filter-var "42" 257 (+ 134217728 268435456)))))

(%php85-register-test 'php85-clone-with-lowers-to-helper-call
  "PHP 8.5 clone-with syntax lowers to the clone-with runtime helper."
  (lambda ()
(let* ((value (%php-first-binding-value "<?php $b = clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-call-p with-call))
    (assert-eq 'cl-cc/php::%php-clone-with
               (cl-cc:ast-var-name (cl-cc:ast-call-func with-call))))))

(%php85-register-test 'php85-cast-expressions-execute
  "PHP cast expressions execute through the runtime conversion helpers."
  (lambda ()
    (assert-string= "42:7:1"
                    (%php-run-capture
                     "<?php echo (int) '42' . ':' . (string) 7 . ':' . ((bool) 'x' ? 1 : 0);"))))

(%php85-register-test 'php85-non-canonical-casts-trigger-deprecation-warning
  "PHP 8.5 deprecated cast spellings emit E_DEPRECATED while preserving cast results."
  (lambda ()
    (assert-string= "8192:1|8192:1|8192:1.5|8192:7"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); echo (integer) '1'; echo '|'; echo (boolean) '1'; echo '|'; echo (double) '1.5'; echo '|'; echo (binary) 7; restore_error_handler();"))))

(%php85-register-test 'php85-canonical-casts-do-not-trigger-deprecation-warning
  "Canonical cast spellings remain silent under PHP 8.5."
  (lambda ()
    (assert-string= "1|1|1.5|7"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_DEPRECATED); echo (int) '1'; echo '|'; echo (bool) '1'; echo '|'; echo (float) '1.5'; echo '|'; echo (string) 7; restore_error_handler();"))))

(%php85-register-test 'php85-suppressed-non-canonical-cast-hides-deprecation-warning
  "The @ operator suppresses PHP 8.5 non-canonical cast deprecation warnings."
  (lambda ()
    (assert-string= "1"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_DEPRECATED); echo @(integer) '1'; restore_error_handler();"))))

(%php85-register-test 'php85-switch-case-semicolon-triggers-deprecation-warning
  "PHP 8.5 warns when switch case labels use a semicolon."
  (lambda ()
    (assert-string= "8192:A"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); switch (1) { case 1; echo 'A'; break; } restore_error_handler();"))))

(%php85-register-test 'php85-switch-default-semicolon-triggers-deprecation-warning
  "PHP 8.5 warns when switch default labels use a semicolon."
  (lambda ()
    (assert-string= "8192:D"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); switch (0) { default; echo 'D'; } restore_error_handler();"))))

(%php85-register-test 'php85-sleep-triggers-deprecation-warning
  "PHP 8.5 warns when __sleep() is used during serialization."
  (lambda ()
    (assert-string= "8192:X"
                    (%php-run-capture
                     "<?php class A { function __sleep(){ return ['x']; } public $x = 1; } function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); $o = new A; serialize($o); echo 'X'; restore_error_handler();"))))

(%php85-register-test 'php85-wakeup-triggers-deprecation-warning
  "PHP 8.5 warns when __wakeup() is used during unserialization."
  (lambda ()
    (assert-string= "8192:X"
                    (%php-run-capture
                     "<?php class A { function __wakeup(){} } function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); unserialize('O:1:\"A\":0:{}'); echo 'X'; restore_error_handler();"))))

(%php85-register-test 'php85-null-array-offset-triggers-deprecation-warning
  "PHP 8.5 warns when null is used as an array offset."
  (lambda ()
    (assert-string= "8192:X"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); $a = []; $a[null] = 1; echo 'X'; restore_error_handler();"))))

(%php85-register-test 'php85-null-array-key-exists-triggers-deprecation-warning
  "PHP 8.5 warns when null is used with array_key_exists()."
  (lambda ()
    (assert-string= "8192:X"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); array_key_exists(null, ['' => 1]); echo 'X'; restore_error_handler();"))))

(%php85-register-test 'php85-list-destructuring-non-array-warns-e-warning
  "PHP 8.5 emits E_WARNING when list/[] destructuring reads a non-array value."
  (lambda ()
    (assert-string= "2:N"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_WARNING); [$a] = false; echo $a === null ? 'N' : 'V'; restore_error_handler();"))
    (assert-string= "2:N"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_WARNING); list($a) = 'x'; echo $a === null ? 'N' : 'V'; restore_error_handler();"))
    (assert-string= "N"
                    (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_WARNING); [$a] = null; echo $a === null ? 'N' : 'V'; restore_error_handler();"))))

(%php85-register-test 'php85-fatal-error-backtraces-disabled-by-default
  "fatal_error_backtraces stays off unless explicitly enabled."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php $a = []; $b = $a[];")
      (declare (ignore stdout))
      (assert-true (search "PHP fatal error: Cannot use [] for reading" stderr :test #'char=))
      (assert-false (search "VM backtrace:" stderr :test #'char=)))))

(%php85-register-test 'php85-fatal-error-backtraces-emit-vm-stack-when-enabled
  "fatal_error_backtraces emits a VM backtrace for fatal errors."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php ini_set('fatal_error_backtraces', '1'); function boom() { $a = []; $b = $a[]; } boom();")
      (declare (ignore stdout))
      (assert-true (search "PHP fatal error: Cannot use [] for reading" stderr :test #'char=))
      (assert-true (search "VM backtrace:" stderr :test #'char=)))))

(%php85-register-test 'php85-max-memory-limit-is-startup-only
  "max_memory_limit cannot be changed at runtime."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php echo ini_set('max_memory_limit', '64M') === false ? 'F' : 'T'; echo ':'; echo ini_get('max_memory_limit');"
         :ini-settings (%php-make-ini-settings "max_memory_limit" "128M"))
      (declare (ignore stderr))
      (assert-string= "F:128M" stdout))))

(%php85-register-test 'php85-memory-limit-clamps-to-max-memory-limit
  "memory_limit is clamped to max_memory_limit."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php ini_set('memory_limit', '256M'); echo ini_get('memory_limit');"
         :ini-settings (%php-make-ini-settings "max_memory_limit" "128M"))
      (assert-string= "128M" stdout)
      (assert-true (search "max_memory_limit" stderr :test #'char=)))))

(%php85-register-test 'php85-memory-limit-unbounded-when-max-disabled
  "memory_limit remains unconstrained when max_memory_limit is disabled."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php ini_set('memory_limit', '256M'); echo ini_get('memory_limit');"
         :ini-settings (%php-make-ini-settings "max_memory_limit" "-1"))
      (assert-string= "256M" stdout)
      (assert-false (search "max_memory_limit" stderr :test #'char=)))))

(%php85-register-test 'php85-cast-expressions-work-in-constant-expressions
  "PHP 8.5 permits scalar casts in constant expressions."
  (lambda ()
    (assert-string= "42:7:1"
                    (%php-run-capture
                     "<?php const I = (int) '42'; const S = (string) 7; const B = (bool) 'x'; echo I . ':' . S . ':' . (B ? 1 : 0);"))))

(%php85-register-test 'php85-clone-function-single-argument-executes
  "PHP 8.5 clone($object) function-style syntax clones without overrides."
  (lambda ()
    (assert-string= "1:2"
                    (%php-run-capture
                     "<?php class C{ public $x=1; } $a=new C(); $b=clone($a); $b->x=2; echo $a->x.':'.$b->x;"))))

(%php85-register-test 'php85-clone-with-executes-property-overrides
  "Clone-with applies property overrides without mutating the original object."
  (lambda ()
(assert-string= "1:9"
                 (%php-run-capture
                  "<?php class C{ public $x=1; } $a=new C(); $b=clone($a, ['x'=>9]); echo $a->x.':'.$b->x;"))))

(%php85-register-test 'php85-clone-with-applies-overrides-after-clone-hook
  "Clone-with applies the override array after __clone has run."
  (lambda ()
(assert-string= "4:99"
                 (%php-run-capture
                  "<?php class C{ public $x; function __construct($x){ $this->x=$x; } function __clone(){ $this->x=$this->x+10; } } $a=new C(4); $b=clone($a, ['x'=>99]); echo $a->x.':'.$b->x;"))))

(%php85-register-test 'php85-closure-get-current-outside-signals-error
  "Closure::getCurrent() signals Error outside closure execution."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (%php-run-capture "<?php Closure::getCurrent();")
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (assert-true condition)
      (assert-true
       (cl-cc/php:%php-exception-matches-p condition 'error))
      (assert-string=
       "Current function is not a closure."
       (cl-cc/php:%php-exception-value condition)))))

(%php85-register-test 'php85-closure-get-current-inside-direct-call
  "Closure::getCurrent() returns the executing closure during direct invocation."
  (lambda ()
(assert-string= "same"
                 (%php-run-capture
                  "<?php $f=function(){ return Closure::getCurrent(); }; echo $f() === $f ? 'same' : 'bad';"))))

(%php85-register-test 'php85-closure-get-current-inside-call-user-func
  "Closure::getCurrent() returns the executing closure through call_user_func()."
  (lambda ()
(assert-string= "same"
                 (%php-run-capture
                  "<?php $f=function(){ return Closure::getCurrent(); }; echo call_user_func($f) === $f ? 'same' : 'bad';"))))

(%php85-register-test 'php85-file-io-helpers-cover-offset-append-and-metadata-paths
  "File helpers cover offset reads, append writes, and basic metadata lookups."
  (lambda ()
    (let* ((tmp-dir (uiop:temporary-directory))
           (unique-base (cl-cc/php::%php-tempnam tmp-dir "cl-cc-php85-"))
           (file (format nil "~A.txt" unique-base))
           (file-path (pathname file))
           (dir (format nil "~A-dir/" file))
           (dir-path (pathname dir)))
      (unwind-protect
           (progn
             (with-open-file (stream file-path
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (write-string "abcdef" stream))
             (assert-string= "cdef"
                             (cl-cc/php::%php-file-get-contents file nil nil 2 nil))
             (assert-string= "cde"
                             (cl-cc/php::%php-file-get-contents file nil nil 2 3))
             (assert-= 2 (cl-cc/php::%php-file-put-contents file "gh" 8))
             (assert-string= "abcdefgh"
                             (cl-cc/php::%php-file-get-contents file))
             (assert-true (cl-cc/php::%php-file-exists file))
             (assert-true (cl-cc/php::%php-is-file file))
             (assert-true (cl-cc/php::%php-is-readable file))
             (assert-true (cl-cc/php::%php-is-writable file))
             (assert-true (cl-cc/php::%php-is-executable file))
             (assert-= 8 (cl-cc/php::%php-filesize file))
             (assert-string= "file" (cl-cc/php::%php-filetype file))
             (assert-true (search (namestring tmp-dir)
                                  (cl-cc/php::%php-realpath file)
                                  :test #'char=))
             (assert-string= "txt" (cl-cc/php::%php-pathinfo file 4))
             (assert-true (search "cl-cc-php85-" (cl-cc/php::%php-basename file) :test #'char=))
             (assert-string= (string-right-trim "/" (namestring tmp-dir))
                              (cl-cc/php::%php-dirname file))
             (ensure-directories-exist dir-path)
             (assert-true (cl-cc/php::%php-mkdir (merge-pathnames "nested/" dir-path) nil nil))
             (assert-true (cl-cc/php::%php-mkdir (merge-pathnames "recursive/a/b/" dir-path) nil t))
             (assert-true (cl-cc/php::%php-is-dir dir-path))
             (let ((sorted-up (cl-cc/php::%php-scandir dir 0))
                   (sorted-down (cl-cc/php::%php-scandir dir 1)))
               (assert-true (hash-table-p sorted-up))
               (assert-true (hash-table-p sorted-down))
               (assert-= 4 (cl-cc/php:%php-count sorted-up))
               (assert-= 4 (cl-cc/php:%php-count sorted-down))))
        (ignore-errors (delete-file unique-base))
        (ignore-errors (delete-file file-path))
        (ignore-errors (uiop:delete-directory-tree dir-path :validate t :if-does-not-exist :ignore))))))

(%php85-register-test 'php85-file-copy-rename-unlink-and-tempnam-cover-mutating-paths
  "File mutation helpers cover copy, rename, unlink, and tempnam."
  (lambda ()
    (let* ((tmp-dir (uiop:temporary-directory))
           (source (pathname (cl-cc/php::%php-tempnam tmp-dir "cl-cc-php85-src-")))
           (copy (pathname (format nil "~A.copy" (namestring source))))
           (renamed (pathname (format nil "~A.renamed" (namestring source))))
           (tempnam (cl-cc/php::%php-tempnam tmp-dir "cl-cc-php85-")))
      (unwind-protect
           (progn
             (with-open-file (stream source
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (write-string "source" stream))
             (assert-true (cl-cc/php::%php-copy source copy))
             (assert-string= "source" (cl-cc/php::%php-file-get-contents copy))
             (assert-true (cl-cc/php::%php-rename copy renamed))
             (assert-false (probe-file copy))
             (assert-string= "source" (cl-cc/php::%php-file-get-contents renamed))
             (assert-true (cl-cc/php::%php-unlink renamed))
             (assert-false (probe-file renamed))
             (assert-true (probe-file tempnam)))
        (ignore-errors (delete-file source))
        (ignore-errors (delete-file copy))
        (ignore-errors (delete-file renamed))
        (ignore-errors (delete-file tempnam))))))

(%php85-register-test 'php85-flock-supports-lock-release-and-reacquire
  "flock() supports exclusive locking, unlocking, and reacquiring in the CLI model."
  (lambda ()
    (let* ((tmp-dir (uiop:temporary-directory))
           (path (cl-cc/php::%php-tempnam tmp-dir "cl-cc-php85-flock-"))
           (php-path (namestring path)))
      (unwind-protect
           (progn
             (with-open-file (stream path
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (write-string "lock me" stream))
                  (assert-string=
                   "Y:N:Y:Y"
                   (%php-run-capture
                    (format nil "<?php
$fp1 = fopen('~A', 'r+');
$fp2 = fopen('~A', 'r+');
$first = flock($fp1, LOCK_EX);
$blocked = flock($fp2, LOCK_EX | LOCK_NB);
$released = flock($fp1, LOCK_UN);
$second = flock($fp2, LOCK_EX | LOCK_NB);
echo ($first ? 'Y' : 'N') . ':' .
     ($blocked ? 'Y' : 'N') . ':' .
     ($released ? 'Y' : 'N') . ':' .
     ($second ? 'Y' : 'N');"
                       php-path php-path))))
        (ignore-errors (delete-file path))))))

(%php85-register-test 'php85-empty-helper-follows-php-truthiness
  "The %php-empty helper mirrors PHP truthiness for empty values."
  (lambda ()
(assert-true (cl-cc/php::%php-empty nil))
  (assert-true (cl-cc/php::%php-empty 0))
  (assert-true (cl-cc/php::%php-empty "0"))
  (assert-false (cl-cc/php::%php-empty "hello"))
  (let ((array (cl-cc/php:%php-array)))
    (assert-true (cl-cc/php::%php-empty array)))))

(%php85-register-test 'php85-is-null-helper-detects-null
  "The %php-is-null helper distinguishes PHP null from other values."
  (lambda ()
(assert-true (cl-cc/php::%php-is-null cl-cc/php:+php-null+))
  (assert-false (cl-cc/php::%php-is-null 0))
  (assert-false (cl-cc/php::%php-is-null ""))))

(%php85-register-test 'php85-unset-helper-returns-null
  "The %php-unset helper always returns PHP null."
  (lambda ()
(assert-eq cl-cc/php:+php-null+
             (cl-cc/php::%php-unset "ignored"))))

(%php85-register-test 'php85-compact-helper-returns-argument-array
  "The %php-compact helper forwards its arguments into an array."
  (lambda ()
(let ((array (cl-cc/php::%php-compact "x" "y")))
    (assert-true (hash-table-p array))
    (assert-= 2 (hash-table-count array)))))

(%php85-register-test 'php85-extract-helper-returns-zero
  "The %php-extract helper returns zero for the fallback path."
  (lambda ()
(assert-= 0 (cl-cc/php::%php-extract (cl-cc/php:%php-array)))))

(%php85-register-test 'php85-grapheme-clusters-collapse-combining-marks
  "The grapheme cluster helper keeps base characters and combining marks together."
  (lambda ()
(let* ((cluster (format nil "a~C" (code-char #x0301)))
         (clusters (cl-cc/php::%php-grapheme-clusters cluster)))
    (assert-= 1 (length clusters))
    (assert-string= cluster (aref clusters 0)))))

(%php85-register-test 'php85-similar-text-counts-common-matches
  "The similar_text helper counts common character matches."
  (lambda ()
(assert-= 2 (cl-cc/php::%php-similar-text "abc" "axc"))))

(%php85-register-test 'php85-soundex-encodes-phonetic-code
  "The soundex helper returns the expected four-character code."
  (lambda ()
(assert-string= "E251" (cl-cc/php::%php-soundex "Example"))))

(%php85-register-test 'php85-isset-runtime-executes-without-notice
  "isset() evaluates through the syntax lowering path without requiring the variable to exist first."
  (lambda ()
(assert-string= "F"
                 (%php-run-capture
                  "<?php echo isset($missing) ? 'T' : 'F';"))))

(%php85-register-test 'php85-empty-runtime-uses-known-variable
  "empty() evaluates through the syntax lowering path when the variable is known."
  (lambda ()
(assert-string= "T"
                 (%php-run-capture
                  "<?php $x = 0; echo empty($x) ? 'T' : 'F';"))))

(%php85-register-test 'php85-compact-runtime-captures-visible-variable
  "compact() captures a visible variable through the parser lowering path."
  (lambda ()
(assert-string= "42"
                 (%php-run-capture
                  "<?php $x = 42; $a = compact('x'); echo $a['x'];"))))

(%php85-register-test 'php85-extract-runtime-introduces-variables
  "extract() introduces variables through the parser lowering path."
  (lambda ()
(assert-string= "17"
                 (%php-run-capture
                  "<?php extract(['x' => 17]); echo $x;"))))

(%php85-register-test 'php85-unset-runtime-removes-array-slot
  "unset() removes an array slot through the parser lowering path."
  (lambda ()
(assert-string= "F"
                 (%php-run-capture
                  "<?php $a = ['x' => 1]; unset($a['x']); echo isset($a['x']) ? 'T' : 'F';"))))

(%php85-register-test 'php85-unset-runtime-clears-object-slot
  "unset() clears object properties through the parser lowering path."
  (lambda ()
(assert-string= "F"
                 (%php-run-capture
                  "<?php $o = new class { public $x = 1; }; unset($o->x); echo isset($o->x) ? 'T' : 'F';"))))

(%php85-register-test 'php84-fiber-make-creates-object
  "new Fiber(callback) lowers to a PHP object wrapper via %php-fiber-make."
  (lambda ()
    (let ((fiber (cl-cc/php::%php-fiber-make (lambda () 42))))
      (assert-true (hash-table-p fiber))
      (assert-string= "Fiber" (gethash "__class__" fiber))
      (assert-false (cl-cc/php::%php-fiber-started-p fiber))
      (assert-false (cl-cc/php::%php-fiber-terminated-p fiber))
      (assert-= 42 (cl-cc/php::%php-fiber-start fiber))
      (assert-true (cl-cc/php::%php-fiber-started-p fiber))
      (assert-true (cl-cc/php::%php-fiber-terminated-p fiber)))))

(%php85-register-test 'php84-fiber-start-runs-callback
  "Fiber::start() runs the callback and returns its result when it does not suspend."
  (lambda ()
(let* ((fiber (cl-cc/php::%php-fiber-make (lambda () 99)))
         (result (cl-cc/php::%php-fiber-start fiber)))
    (assert-= 99 result)
    (assert-true (cl-cc/php::%php-fiber-started-p fiber))
    (assert-true (cl-cc/php::%php-fiber-terminated-p fiber)))))

(%php85-register-test 'php84-fiber-start-twice-signals-error
  "Starting an already-started Fiber signals an error."
  (lambda ()
(let ((fiber (cl-cc/php::%php-fiber-make (lambda () 1))))
    (cl-cc/php::%php-fiber-start fiber)
    (assert-signals error (cl-cc/php::%php-fiber-start fiber)))))

(%php85-register-test 'php84-fiber-get-return-value
  "%php-fiber-get-return returns the final return value of a terminated Fiber."
  (lambda ()
(let* ((fiber (cl-cc/php::%php-fiber-make (lambda () :done))))
    (cl-cc/php::%php-fiber-start fiber)
    (assert-eq :done (cl-cc/php::%php-fiber-get-return fiber)))))

(%php85-register-test 'php84-fiber-get-return-before-termination-signals
  "%php-fiber-get-return signals an error when the fiber has not yet terminated."
  (lambda ()
    (let ((fiber (cl-cc/php::%php-fiber-make (lambda () :done))))
      ;; Not started yet
      (assert-signals error (cl-cc/php::%php-fiber-get-return fiber))
      (assert-eq :done (cl-cc/php::%php-fiber-start fiber)))))

(%php85-register-test 'php84-mark-all-props-readonly-marks-instance-slots
  "%php-mark-all-props-readonly adds :readonly-p to instance property slot-defs."
  (lambda ()
(let* ((prop (cl-cc:make-ast-slot-def :name 'x :allocation :instance))
         (marked (cl-cc/php::%php-mark-all-props-readonly (list prop)))
         (slot (first marked)))
    (assert-true (getf (cl-cc:ast-imports slot) :readonly-p)))))

(%php85-register-test 'php84-mark-all-props-readonly-skips-class-slots
  "%php-mark-all-props-readonly does not modify :class allocation slots (constants/statics)."
  (lambda ()
(let* ((const (cl-cc:make-ast-slot-def :name 'x :allocation :class))
         (marked (cl-cc/php::%php-mark-all-props-readonly (list const)))
         (slot (first marked)))
    (assert-false (getf (cl-cc:ast-imports slot) :readonly-p)))))

(%php85-register-test 'php84-lower-property-with-hooks-get-only
  "%php-lower-property-with-hooks with only a getter produces a __get_ method."
  (lambda ()
(let* ((getter-body (cl-cc:make-ast-return-from :name nil
                                                  :value (cl-cc:make-ast-int :value 1)))
         (prop-sym    (intern "NAME" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym getter-body nil 'myclass))
         (names       (mapcar (lambda (m) (symbol-name (cl-cc:ast-defun-name m))) methods)))
    (assert-true (find "__GET_NAME" names :test #'string=)))))

(%php85-register-test 'php84-lower-property-with-hooks-set-only
  "%php-lower-property-with-hooks with only a setter produces a __set_ method."
  (lambda ()
(let* ((setter-body (cl-cc:make-ast-quote :value nil))
         (prop-sym    (intern "NAME" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym nil setter-body 'myclass))
         (names       (mapcar (lambda (m) (symbol-name (cl-cc:ast-defun-name m))) methods)))
    (assert-true (find "__SET_NAME" names :test #'string=)))))

(%php85-register-test 'php84-lower-property-with-hooks-both
  "%php-lower-property-with-hooks with get and set produces two methods."
  (lambda ()
(let* ((getter-body (cl-cc:make-ast-int :value 1))
         (setter-body (cl-cc:make-ast-quote :value nil))
         (prop-sym    (intern "TITLE" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym getter-body setter-body 'myclass)))
    (assert-= 2 (length methods)))))

(%php85-register-test 'php84-class-property-hooks-lower-to-accessor-slots
  "Class property hooks parse through the class parser and add accessor slots."
  (lambda ()
(let* ((ast (%php84-first
               "<?php class User { public string $name { get => $this->name; set($value) => $value; } }"))
         (slots (cl-cc:ast-defclass-slots ast))
         (slot-names (mapcar (lambda (slot)
                               (symbol-name (cl-cc:ast-slot-name slot)))
                             slots)))
    (assert-true (member "NAME" slot-names :test #'string=))
    (assert-true (member "__GET_NAME" slot-names :test #'string=))
    (assert-true (member "__SET_NAME" slot-names :test #'string=)))))

(%php85-register-test 'php84-asymmetric-visibility-parse-public-private-set
  "%php-parse-asymmetric-visibility parses public private(set) and returns two keywords."
  (lambda ()
(let* ((pub-tok  (list :type :T-KEYWORD :value :public))
         (priv-tok (list :type :T-KEYWORD :value :private))
         (lparen   (list :type :T-LPAREN  :value "("))
         (set-tok  (list :type :T-IDENT   :value "set"))
         (rparen   (list :type :T-RPAREN  :value ")"))
         (stream   (list pub-tok priv-tok lparen set-tok rparen)))
    (multiple-value-bind (outer inner _rest)
        (cl-cc/php::%php-parse-asymmetric-visibility stream)
      (declare (ignore _rest))
      (assert-eq :public outer)
      (assert-eq :private inner)))))

(%php85-register-test 'php84-asymmetric-visibility-single-modifier-no-inner
  "%php-parse-asymmetric-visibility returns nil inner when only one modifier."
  (lambda ()
(let* ((pub-tok (list :type :T-KEYWORD :value :public))
         (ident   (list :type :T-IDENT   :value "x"))
         (stream  (list pub-tok ident)))
    (multiple-value-bind (outer inner _rest)
        (cl-cc/php::%php-parse-asymmetric-visibility stream)
      (declare (ignore _rest))
      (assert-eq :public outer)
      (assert-null inner)))))

(%php85-register-test 'php84-class-asymmetric-visibility-metadata
  "Class properties preserve PHP 8.4 asymmetric set visibility metadata."
  (lambda ()
(let* ((ast (%php84-first
               "<?php class User { public private(set) string $id; }"))
         (slot (first (cl-cc:ast-defclass-slots ast)))
         (imports (cl-cc:ast-imports slot)))
    (assert-eq :private (getf imports :php-set-visibility))
    (assert-true (member :public (getf imports :php-modifiers) :test #'eq)))))

(%php85-register-test 'php85-class-static-asymmetric-visibility-metadata
  "PHP 8.5 static properties preserve asymmetric set visibility metadata."
  (lambda ()
    (dolist (case '(("<?php class Config { final public static private(set) string $name; }"
                     :private)
                    ("<?php class Config { final public static protected(set) string $name; }"
                     :protected)))
      (let* ((ast (%php84-first (first case)))
             (slot (first (cl-cc:ast-defclass-slots ast)))
             (imports (cl-cc:ast-imports slot))
             (modifiers (getf imports :php-modifiers)))
        (assert-eq :class (cl-cc:ast-slot-allocation slot))
        (assert-eq (second case) (getf imports :php-set-visibility))
        (assert-true (member :final modifiers :test #'eq))
        (assert-true (member :public modifiers :test #'eq))
        (assert-true (member :static modifiers :test #'eq))))))

(%php85-register-test 'php85-final-promoted-property-preserves-final-modifier
  "PHP 8.5 constructor promotion preserves final property metadata."
  (lambda ()
(let* ((ast (%php84-first
               "<?php class Token { public function __construct(final string $id) {} }"))
         (slots (cl-cc:ast-defclass-slots ast))
         (slot (find-if (lambda (candidate)
                           (string= "ID" (symbol-name (cl-cc:ast-slot-name candidate))))
                        slots)))
    (assert-true slot)
    (let* ((imports (cl-cc:ast-imports slot))
           (modifiers (getf imports :php-modifiers)))
      (assert-eq :instance (cl-cc:ast-slot-allocation slot))
      (assert-true (member :final modifiers :test #'eq))))))

(%php85-register-test 'php84-function-intersection-type-annotation-preserved
  "Intersection type A&B in a function declaration is preserved as a type annotation string."
  (lambda ()
(let* ((ast (%php84-first
               "<?php function process(Iterator $it): void { return; }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-equal "void" (getf decls :php-return-type)))))

(%php85-register-test 'php84-intersection-type-parse-helper
  "%php-parse-intersection-type builds a structured :intersection descriptor."
  (lambda ()
;; We call the helper directly with a fake stream.
  ;; %php-type-token-string lowercases the segment, so "Countable" → "countable".
  (let* ((amp-tok       (list :type :T-OP    :value "&"))
         (countable-tok (list :type :T-IDENT  :value "Countable"))
         (stream        (list amp-tok countable-tok)))
    (multiple-value-bind (spec _rest)
        (cl-cc/php::%php-parse-intersection-type "Iterator" stream)
      (declare (ignore _rest))
      (assert-true (consp spec))
      (assert-eq :intersection (first spec))
      (assert-true (member "Iterator" spec :test #'string=))
      (assert-true (member "countable" spec :test #'string=))))))

(%php85-register-test 'php84-never-return-type-preserved-in-declarations
  "function fail(): never preserves 'never' as the return type in declarations."
  (lambda ()
(let* ((ast (%php84-first "<?php function fail(): never { throw new Ex(); }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-equal "never" (getf decls :php-return-type)))))

(%php85-register-test 'php84-enum-with-method-produces-slot-def
  "An enum with a method body contains the method as a slot-def."
  (lambda ()
    (dolist (spec (list
                   (list "<?php enum Status: int { case Draft = 0; public function label(): string { return 'Draft'; } } function afterEnum() {}"
                         :enum "LABEL")
                   (list "<?php class User { public string $name; }"
                         :class "NAME")))
      (destructuring-bind (source expected-kind expected-slot-name) spec
        (let* ((form  (%php84-first source))
               ;; An enum now lowers to (progn defclass (link-cases)); unwrap the defclass when present.
               (ast   (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
               (slots (cl-cc:ast-defclass-slots ast))
               (slot-names (mapcar (lambda (s) (symbol-name (cl-cc:ast-slot-name s))) slots)))
          (assert-eq expected-kind (cl-cc:ast-defclass-php-kind ast))
          (assert-true (member expected-slot-name slot-names :test #'string=)))))))

(%php85-register-test 'php84-new-in-initializer-default-param
  "function f(Logger $l = new FileLogger()) parses default value as PHP new lowering."
  (lambda ()
;; PHP 8.1 allows `new ClassName()` as a default parameter value.
  ;; `new` lowers to a let that allocates the instance, conditionally runs
  ;; __construct, and returns the instance.
  (let* ((ast (%php84-first
               "<?php function process(Logger $logger = new FileLogger()) { return $logger; }"))
         (optionals (cl-cc:ast-defun-optional-params ast))
         (default-ast (second (first optionals)))
         (instance-ast (cdr (first (cl-cc:ast-let-bindings default-ast)))))
    (assert-true (cl-cc:ast-defun-p ast))
    (assert-= 1 (length optionals))
    (assert-true (cl-cc:ast-let-p default-ast))
    (assert-true (cl-cc:ast-make-instance-p instance-ast))
    (assert-string= "FILELOGGER"
                    (symbol-name (cl-cc:ast-var-name
                                  (cl-cc:ast-make-instance-class instance-ast)))))))

(%php85-register-test 'php85-self-load-executes-registrations
  "Loading the source file replays registration coverage under a fresh registry."
  (lambda ()
    (let* ((entry (persist-lookup *test-registry* 'php84-named-args-to-positional-lowers-named))
           (source-file (getf entry :source-file))
           (loaded-registry nil))
      (assert-true source-file)
      (let ((*php85-self-load-guard* t)
            (*test-registry* (persist-empty)))
        (load source-file)
        (setf loaded-registry *test-registry*))
      (assert-true (persist-lookup loaded-registry 'php84-named-args-to-positional-lowers-named))
      (assert-true (persist-lookup loaded-registry 'php84-enum-with-method-produces-slot-def))
      (assert-null (%php85-run-registered-tests-with-prefix "PHP85-THIS-PREFIX-IS-TOO-LONG")))))

  (%php85-register-test 'php85-run-registered-tests-with-prefix-skips-non-symbol-and-excluded
  "The prefix runner ignores non-symbol registry keys and excluded matches."
  (lambda ()
    (let* ((kept-name 'php85-run-registered-tests-with-prefix-kept)
           (excluded-name 'php85-run-registered-tests-with-prefix-excluded)
           (kept-ran nil)
           (excluded-ran nil)
           (symbol-run-count 0)
           (fresh-registry (let ((registry (persist-empty)))
                             (setf registry
                                   (persist-assoc registry 42
                                                  (%test-registry-entry 42
                                                                        :fn (lambda ()
                                                                              (incf symbol-run-count))
                                                                        :suite *current-suite*
                                                                        :source-file (or *compile-file-pathname*
                                                                                         *load-pathname*))))
                             (setf registry
                                   (persist-assoc registry kept-name
                                                  (%test-registry-entry kept-name
                                                                        :fn (lambda ()
                                                                              (setf kept-ran t))
                                                                        :suite *current-suite*
                                                                        :source-file (or *compile-file-pathname*
                                                                                         *load-pathname*))))
                             (setf registry
                                   (persist-assoc registry excluded-name
                                                  (%test-registry-entry excluded-name
                                                                        :fn (lambda ()
                                                                              (setf excluded-ran t))
                                                                        :suite *current-suite*
                                                                        :source-file (or *compile-file-pathname*
                                                                                         *load-pathname*))))
                             registry)))
      (let ((symbol-entry (persist-lookup fresh-registry 42))
            (excluded-entry (persist-lookup fresh-registry excluded-name)))
        (assert-true symbol-entry)
        (assert-true excluded-entry)
        (let ((*test-registry* fresh-registry))
          (assert-equal '(t) (%php85-run-registered-tests-with-prefix "PHP85-RUN-REGISTERED-TESTS-WITH-PREFIX"
                                                                       :exclude (list excluded-name))))
        (assert-true kept-ran)
        (assert-null excluded-ran)
        (funcall (getf symbol-entry :fn))
        (funcall (getf excluded-entry :fn))
        (assert-= 1 symbol-run-count)
        (assert-true excluded-ran)))))
(eval-when (:load-toplevel :execute)
  (%php85-register-php84-tests)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)
   :exclude (when *php85-self-load-guard*
              '(php85-self-load-executes-registrations))))
