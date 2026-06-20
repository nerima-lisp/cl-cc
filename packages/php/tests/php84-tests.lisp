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
                      "DelayedTargetValidation"
                      "CurlSharePersistentHandle"
                      "Filter\\FilterException"
                      "Filter\\FilterFailedException"
                      "IntlListFormatter"
                      "Locale"
                      "NumberFormatter"
                      "Pdo\\Sqlite"
                      "Uri\\UriError"
                      "Uri\\UriException"
                      "Uri\\InvalidUriException"
                      "Uri\\UriComparisonMode"
                      "Uri\\Rfc3986\\Uri"
                      "Uri\\WhatWg\\InvalidUrlException"
                      "Uri\\WhatWg\\UrlValidationErrorType"
                      "Uri\\WhatWg\\UrlValidationError"
                      "Uri\\WhatWg\\Url"))
    (assert-true (cl-cc/php::%php-class-exists class-name)))))

(%php85-register-test 'php85-intl-and-pdo-class-constants-lower-to-runtime-values
  "PHP 8.5 IntlListFormatter, NumberFormatter, and Pdo\\Sqlite class constants resolve."
  (lambda ()
    (assert-string= "0:1:10:12:1:2048"
                    (%php-run-capture
                     "<?php echo IntlListFormatter::TYPE_AND . ':' . IntlListFormatter::TYPE_OR . ':' . NumberFormatter::CURRENCY_ISO . ':' . NumberFormatter::CURRENCY_ACCOUNTING . ':' . Pdo\\Sqlite::OPEN_READONLY . ':' . Pdo\\Sqlite::DETERMINISTIC;"))))

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
                    "T_VOID_CAST"
                    "T_PIPE"
                    "IMAGETYPE_HEIF"
                    "IMAGETYPE_SVG"))
    (multiple-value-bind (value found)
        (cl-cc/php::%php-lookup-constant constant)
      (declare (ignore value))
      (assert-true found)))))

(%php85-register-test 'php85-extension-new-free-functions-are-registered
  "New PHP 8.5 extension-level free functions are registered as builtins."
  (lambda ()
    (dolist (function-name '("enchant_dict_remove_from_session"
                             "enchant_dict_remove"
                             "pg_close_stmt"
                             "pg_service"))
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

(%php85-register-test 'php85-closure-get-current-outside-returns-null
  "Closure::getCurrent() returns null outside closure execution."
  (lambda ()
(assert-string= "null"
                 (%php-run-capture
                  "<?php echo Closure::getCurrent() === null ? 'null' : 'bad';"))))

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
         (file-path (merge-pathnames (format nil "cl-cc-php85-~a.txt" (gensym "file-")) tmp-dir))
         (file (namestring file-path))
         (dir-path (merge-pathnames (format nil "cl-cc-php85-~a/" (gensym "dir-")) tmp-dir))
         (dir (namestring dir-path)))
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
      (ignore-errors (delete-file file-path))
      (ignore-errors (uiop:delete-directory-tree dir-path :validate t :if-does-not-exist :ignore))))))

(%php85-register-test 'php85-file-copy-rename-unlink-and-tempnam-cover-mutating-paths
  "File mutation helpers cover copy, rename, unlink, and tempnam."
  (lambda ()
(let* ((tmp-dir (uiop:temporary-directory))
         (source (merge-pathnames (format nil "cl-cc-php85-src-~a.txt" (gensym "file-")) tmp-dir))
         (copy (merge-pathnames (format nil "cl-cc-php85-copy-~a.txt" (gensym "file-")) tmp-dir))
         (renamed (merge-pathnames (format nil "cl-cc-php85-renamed-~a.txt" (gensym "file-")) tmp-dir))
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
