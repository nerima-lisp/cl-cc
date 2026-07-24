(in-package :cl-cc/test)


(%php85-register-test 'php85-clone-with-lowers-to-helper-call
  "PHP 8.5 clone-with syntax lowers to the clone-with runtime helper."
  (lambda ()
(let* ((value (%php-first-binding-value "<?php $b = clone($a, ['x' => 9]);"))
         (body (cl-cc:ast-let-body value))
         (with-call (second body)))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (cl-cc:ast-call-p with-call) :to-be-truthy)
    (expect (cl-cc:ast-var-name (cl-cc:ast-call-func with-call)) :to-be 'cl-cc/php::%php-clone-with))))

(%php85-register-test 'php85-cast-expressions-execute
  "PHP cast expressions execute through the runtime conversion helpers."
  (lambda ()
    (expect (%php-run-capture
                     "<?php echo (int) '42' . ':' . (string) 7 . ':' . ((bool) 'x' ? 1 : 0);") :to-equal "42:7:1")))

(%php85-register-test 'php85-non-canonical-casts-trigger-deprecation-warning
  "PHP 8.5 deprecated cast spellings emit E_DEPRECATED while preserving cast results."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); echo (integer) '1'; echo '|'; echo (boolean) '1'; echo '|'; echo (double) '1.5'; echo '|'; echo (binary) 7; restore_error_handler();") :to-equal "8192:1|8192:1|8192:1.5|8192:7")))

(%php85-register-test 'php85-canonical-casts-do-not-trigger-deprecation-warning
  "Canonical cast spellings remain silent under PHP 8.5."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_DEPRECATED); echo (int) '1'; echo '|'; echo (bool) '1'; echo '|'; echo (float) '1.5'; echo '|'; echo (string) 7; restore_error_handler();") :to-equal "1|1|1.5|7")))

(%php85-register-test 'php85-suppressed-non-canonical-cast-hides-deprecation-warning
  "The @ operator suppresses PHP 8.5 non-canonical cast deprecation warnings."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_DEPRECATED); echo @(integer) '1'; restore_error_handler();") :to-equal "1")))

(%php85-register-test 'php85-switch-case-semicolon-triggers-deprecation-warning
  "PHP 8.5 warns when switch case labels use a semicolon."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); switch (1) { case 1; echo 'A'; break; } restore_error_handler();") :to-equal "8192:A")))

(%php85-register-test 'php85-switch-default-semicolon-triggers-deprecation-warning
  "PHP 8.5 warns when switch default labels use a semicolon."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); switch (0) { default; echo 'D'; } restore_error_handler();") :to-equal "8192:D")))

(%php85-register-test 'php85-sleep-triggers-deprecation-warning
  "PHP 8.5 warns when __sleep() is used during serialization."
  (lambda ()
    (expect (%php-run-capture
                     "<?php class A { function __sleep(){ return ['x']; } public $x = 1; } function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); $o = new A; serialize($o); echo 'X'; restore_error_handler();") :to-equal "8192:X")))

(%php85-register-test 'php85-wakeup-triggers-deprecation-warning
  "PHP 8.5 warns when __wakeup() is used during unserialization."
  (lambda ()
    (expect (%php-run-capture
                     "<?php class A { function __wakeup(){} } function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); unserialize('O:1:\"A\":0:{}'); echo 'X'; restore_error_handler();") :to-equal "8192:X")))

(%php85-register-test 'php85-null-array-offset-triggers-deprecation-warning
  "PHP 8.5 warns when null is used as an array offset."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); $a = []; $a[null] = 1; echo 'X'; restore_error_handler();") :to-equal "8192:X")))

(%php85-register-test 'php85-null-array-key-exists-triggers-deprecation-warning
  "PHP 8.5 warns when null is used with array_key_exists()."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_DEPRECATED); array_key_exists(null, ['' => 1]); echo 'X'; restore_error_handler();") :to-equal "8192:X")))

(%php85-register-test 'php85-list-destructuring-non-array-warns-e-warning
  "PHP 8.5 emits E_WARNING when list/[] destructuring reads a non-array value."
  (lambda ()
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_WARNING); [$a] = false; echo $a === null ? 'N' : 'V'; restore_error_handler();") :to-equal "2:N")
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo $errno . ':'; return true; } set_error_handler('h', E_WARNING); list($a) = 'x'; echo $a === null ? 'N' : 'V'; restore_error_handler();") :to-equal "2:N")
    (expect (%php-run-capture
                     "<?php function h($errno,$errstr){ echo 'warn:'; return true; } set_error_handler('h', E_WARNING); [$a] = null; echo $a === null ? 'N' : 'V'; restore_error_handler();") :to-equal "N")))

(%php85-register-test 'php85-fatal-error-backtraces-disabled-by-default
  "fatal_error_backtraces stays off unless explicitly enabled."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php $a = []; $b = $a[];")
      (declare (ignore stdout))
      (expect (search "PHP fatal error: Cannot use [] for reading" stderr :test #'char=) :to-be-truthy)
      (expect (search "VM backtrace:" stderr :test #'char=) :to-be-falsy))))

(%php85-register-test 'php85-fatal-error-backtraces-emit-vm-stack-when-enabled
  "fatal_error_backtraces emits a VM backtrace for fatal errors."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php ini_set('fatal_error_backtraces', '1'); function boom() { $a = []; $b = $a[]; } boom();")
      (declare (ignore stdout))
      (expect (search "PHP fatal error: Cannot use [] for reading" stderr :test #'char=) :to-be-truthy)
      (expect (search "VM backtrace:" stderr :test #'char=) :to-be-truthy))))

(%php85-register-test 'php85-max-memory-limit-is-startup-only
  "max_memory_limit cannot be changed at runtime."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php echo ini_set('max_memory_limit', '64M') === false ? 'F' : 'T'; echo ':'; echo ini_get('max_memory_limit');"
         :ini-settings (%php-make-ini-settings "max_memory_limit" "128M"))
      (declare (ignore stderr))
      (expect stdout :to-equal "F:128M"))))

(%php85-register-test 'php85-memory-limit-clamps-to-max-memory-limit
  "memory_limit is clamped to max_memory_limit."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php ini_set('memory_limit', '256M'); echo ini_get('memory_limit');"
         :ini-settings (%php-make-ini-settings "max_memory_limit" "128M"))
      (expect stdout :to-equal "128M")
      (expect (search "max_memory_limit" stderr :test #'char=) :to-be-truthy))))

(%php85-register-test 'php85-memory-limit-unbounded-when-max-disabled
  "memory_limit remains unconstrained when max_memory_limit is disabled."
  (lambda ()
    (multiple-value-bind (stdout stderr)
        (%php-run-capture-io
         "<?php ini_set('memory_limit', '256M'); echo ini_get('memory_limit');"
         :ini-settings (%php-make-ini-settings "max_memory_limit" "-1"))
      (expect stdout :to-equal "256M")
      (expect (search "max_memory_limit" stderr :test #'char=) :to-be-falsy))))

(%php85-register-test 'php85-cast-expressions-work-in-constant-expressions
  "PHP 8.5 permits scalar casts in constant expressions."
  (lambda ()
    (expect (%php-run-capture
                     "<?php const I = (int) '42'; const S = (string) 7; const B = (bool) 'x'; echo I . ':' . S . ':' . (B ? 1 : 0);") :to-equal "42:7:1")))

(%php85-register-test 'php85-clone-function-single-argument-executes
  "PHP 8.5 clone($object) function-style syntax clones without overrides."
  (lambda ()
    (expect (%php-run-capture
                     "<?php class C{ public $x=1; } $a=new C(); $b=clone($a); $b->x=2; echo $a->x.':'.$b->x;") :to-equal "1:2")))

(%php85-register-test 'php85-clone-with-executes-property-overrides
  "Clone-with applies property overrides without mutating the original object."
  (lambda ()
(expect (%php-run-capture
                  "<?php class C{ public $x=1; } $a=new C(); $b=clone($a, ['x'=>9]); echo $a->x.':'.$b->x;") :to-equal "1:9")))

(%php85-register-test 'php85-clone-with-applies-overrides-after-clone-hook
  "Clone-with applies the override array after __clone has run."
  (lambda ()
(expect (%php-run-capture
                  "<?php class C{ public $x; function __construct($x){ $this->x=$x; } function __clone(){ $this->x=$this->x+10; } } $a=new C(4); $b=clone($a, ['x'=>99]); echo $a->x.':'.$b->x;") :to-equal "4:99")))

(%php85-register-test 'php85-closure-get-current-outside-signals-error
  "Closure::getCurrent() signals Error outside closure execution."
  (lambda ()
    (let ((condition (handler-case
                         (progn
                           (%php-run-capture "<?php Closure::getCurrent();")
                           nil)
                       (cl-cc/php:php-exception (e) e))))
      (expect condition :to-be-truthy)
      (expect (cl-cc/php:%php-exception-matches-p condition 'error) :to-be-truthy)
      (expect (cl-cc/php:%php-exception-value condition) :to-equal "Current function is not a closure."))))

(%php85-register-test 'php85-closure-get-current-inside-direct-call
  "Closure::getCurrent() returns the executing closure during direct invocation."
  (lambda ()
(expect (%php-run-capture
                  "<?php $f=function(){ return Closure::getCurrent(); }; echo $f() === $f ? 'same' : 'bad';") :to-equal "same")))

(%php85-register-test 'php85-closure-get-current-inside-call-user-func
  "Closure::getCurrent() returns the executing closure through call_user_func()."
  (lambda ()
(expect (%php-run-capture
                  "<?php $f=function(){ return Closure::getCurrent(); }; echo call_user_func($f) === $f ? 'same' : 'bad';") :to-equal "same")))

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
             (expect (cl-cc/php::%php-file-get-contents file nil nil 2 nil) :to-equal "cdef")
             (expect (cl-cc/php::%php-file-get-contents file nil nil 2 3) :to-equal "cde")
             (expect (= 2 (cl-cc/php::%php-file-put-contents file "gh" 8)) :to-be-truthy)
             (expect (cl-cc/php::%php-file-get-contents file) :to-equal "abcdefgh")
             (expect (cl-cc/php::%php-file-exists file) :to-be-truthy)
             (expect (cl-cc/php::%php-is-file file) :to-be-truthy)
             (expect (cl-cc/php::%php-is-readable file) :to-be-truthy)
             (expect (cl-cc/php::%php-is-writable file) :to-be-truthy)
             (expect (cl-cc/php::%php-is-executable file) :to-be-truthy)
             (expect (= 8 (cl-cc/php::%php-filesize file)) :to-be-truthy)
             (expect (cl-cc/php::%php-filetype file) :to-equal "file")
             (expect (search (namestring tmp-dir)
                                  (cl-cc/php::%php-realpath file)
                                  :test #'char=) :to-be-truthy)
             (expect (cl-cc/php::%php-pathinfo file 4) :to-equal "txt")
             (expect (search "cl-cc-php85-" (cl-cc/php::%php-basename file) :test #'char=) :to-be-truthy)
             (expect (cl-cc/php::%php-dirname file) :to-equal (string-right-trim "/" (namestring tmp-dir)))
             (ensure-directories-exist dir-path)
             (expect (cl-cc/php::%php-mkdir (merge-pathnames "nested/" dir-path) nil nil) :to-be-truthy)
             (expect (cl-cc/php::%php-mkdir (merge-pathnames "recursive/a/b/" dir-path) nil t) :to-be-truthy)
             (expect (cl-cc/php::%php-is-dir dir-path) :to-be-truthy)
             (let ((sorted-up (cl-cc/php::%php-scandir dir 0))
                   (sorted-down (cl-cc/php::%php-scandir dir 1)))
               (expect (hash-table-p sorted-up) :to-be-truthy)
               (expect (hash-table-p sorted-down) :to-be-truthy)
               (expect (= 4 (cl-cc/php:%php-count sorted-up)) :to-be-truthy)
               (expect (= 4 (cl-cc/php:%php-count sorted-down)) :to-be-truthy)))
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
             (expect (cl-cc/php::%php-copy source copy) :to-be-truthy)
             (expect (cl-cc/php::%php-file-get-contents copy) :to-equal "source")
             (expect (cl-cc/php::%php-rename copy renamed) :to-be-truthy)
             (expect (probe-file copy) :to-be-falsy)
             (expect (cl-cc/php::%php-file-get-contents renamed) :to-equal "source")
             (expect (cl-cc/php::%php-unlink renamed) :to-be-truthy)
             (expect (probe-file renamed) :to-be-falsy)
             (expect (probe-file tempnam) :to-be-truthy))
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
                  (expect (%php-run-capture
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
                       php-path php-path)) :to-equal "Y:N:Y:Y"))
        (ignore-errors (delete-file path))))))

(%php85-register-test 'php85-empty-helper-follows-php-truthiness
  "The %php-empty helper mirrors PHP truthiness for empty values."
  (lambda ()
(expect (cl-cc/php::%php-empty nil) :to-be-truthy)
  (expect (cl-cc/php::%php-empty 0) :to-be-truthy)
  (expect (cl-cc/php::%php-empty "0") :to-be-truthy)
  (expect (cl-cc/php::%php-empty "hello") :to-be-falsy)
  (let ((array (cl-cc/php:%php-array)))
    (expect (cl-cc/php::%php-empty array) :to-be-truthy))))

(%php85-register-test 'php85-is-null-helper-detects-null
  "The %php-is-null helper distinguishes PHP null from other values."
  (lambda ()
(expect (cl-cc/php::%php-is-null cl-cc/php:+php-null+) :to-be-truthy)
  (expect (cl-cc/php::%php-is-null 0) :to-be-falsy)
  (expect (cl-cc/php::%php-is-null "") :to-be-falsy)))

(%php85-register-test 'php85-unset-helper-returns-null
  "The %php-unset helper always returns PHP null."
  (lambda ()
(expect (cl-cc/php::%php-unset "ignored") :to-be cl-cc/php:+php-null+)))

(%php85-register-test 'php85-compact-helper-returns-argument-array
  "The %php-compact helper forwards its arguments into an array."
  (lambda ()
(let ((array (cl-cc/php::%php-compact "x" "y")))
    (expect (hash-table-p array) :to-be-truthy)
    (expect (= 2 (hash-table-count array)) :to-be-truthy))))

(%php85-register-test 'php85-extract-helper-returns-zero
  "The %php-extract helper returns zero for the fallback path."
  (lambda ()
(expect (= 0 (cl-cc/php::%php-extract (cl-cc/php:%php-array))) :to-be-truthy)))

(%php85-register-test 'php85-grapheme-clusters-collapse-combining-marks
  "The grapheme cluster helper keeps base characters and combining marks together."
  (lambda ()
(let* ((cluster (format nil "a~C" (code-char #x0301)))
         (clusters (cl-cc/php::%php-grapheme-clusters cluster)))
    (expect (= 1 (length clusters)) :to-be-truthy)
    (expect (aref clusters 0) :to-equal cluster))))

(%php85-register-test 'php85-similar-text-counts-common-matches
  "The similar_text helper counts common character matches."
  (lambda ()
(expect (= 2 (cl-cc/php::%php-similar-text "abc" "axc")) :to-be-truthy)))

(%php85-register-test 'php85-soundex-encodes-phonetic-code
  "The soundex helper returns the expected four-character code."
  (lambda ()
(expect (cl-cc/php::%php-soundex "Example") :to-equal "E251")))

(%php85-register-test 'php85-isset-runtime-executes-without-notice
  "isset() evaluates through the syntax lowering path without requiring the variable to exist first."
  (lambda ()
(expect (%php-run-capture
                  "<?php echo isset($missing) ? 'T' : 'F';") :to-equal "F")))

(%php85-register-test 'php85-empty-runtime-uses-known-variable
  "empty() evaluates through the syntax lowering path when the variable is known."
  (lambda ()
(expect (%php-run-capture
                  "<?php $x = 0; echo empty($x) ? 'T' : 'F';") :to-equal "T")))

(%php85-register-test 'php85-compact-runtime-captures-visible-variable
  "compact() captures a visible variable through the parser lowering path."
  (lambda ()
(expect (%php-run-capture
                  "<?php $x = 42; $a = compact('x'); echo $a['x'];") :to-equal "42")))

(%php85-register-test 'php85-extract-runtime-introduces-variables
  "extract() introduces variables through the parser lowering path."
  (lambda ()
(expect (%php-run-capture
                  "<?php extract(['x' => 17]); echo $x;") :to-equal "17")))

(%php85-register-test 'php85-unset-runtime-removes-array-slot
  "unset() removes an array slot through the parser lowering path."
  (lambda ()
(expect (%php-run-capture
                  "<?php $a = ['x' => 1]; unset($a['x']); echo isset($a['x']) ? 'T' : 'F';") :to-equal "F")))

(%php85-register-test 'php85-unset-runtime-clears-object-slot
  "unset() clears object properties through the parser lowering path."
  (lambda ()
(expect (%php-run-capture
                  "<?php $o = new class { public $x = 1; }; unset($o->x); echo isset($o->x) ? 'T' : 'F';") :to-equal "F")))


(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
