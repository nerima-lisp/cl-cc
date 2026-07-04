(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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


(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
