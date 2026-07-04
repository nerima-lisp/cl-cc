;;;; PHP file I/O, system, and miscellaneous builtins.

(in-package :cl-cc/php)

;;; ─── Shared Utilities ────────────────────────────────────────────────────────

(defun %php-path-string (value)
  (if (pathnamep value)
      (namestring value)
      (%php-stringify value)))

;;; ─── Image Metadata ─────────────────────────────────────────────────────────
;;; Image metadata builtins live in runtime-builtins-io-image.lisp.

;;; ─── File I/O ────────────────────────────────────────────────────────────────
;;; File and stream resource builtins live in runtime-builtins-io-files.lisp.

;;; ─── System / environment ────────────────────────────────────────────────────

(defun %php-getenv (varname &optional local-only)
  "PHP getenv: get environment variable."
  (declare (ignore local-only))
  (or (sb-ext:posix-getenv (%php-stringify varname)) nil))

(defun %php-putenv (setting)
  "PHP putenv: set environment variable."
  (let* ((s (%php-stringify setting))
         (eq-pos (position #\= s)))
    (if eq-pos
        (handler-case
            (progn
              (when (find-package :sb-posix)
                (funcall (intern "SETENV" :sb-posix) (subseq s 0 eq-pos) (subseq s (1+ eq-pos)) 1))
              t)
          (error () t))
        nil)))

(defun %php-php-uname (&optional (mode "a"))
  "PHP php_uname: OS information."
  (declare (ignore mode))
  "Darwin")

(defun %php-php-sapi-name ()
  "PHP php_sapi_name: return SAPI name."
  "cli")

(defun %php-php-version ()
  "PHP phpversion: return PHP version string."
  "8.5.0")

(defun %php-php-int-size ()
  "PHP PHP_INT_SIZE constant."
  8)

(defun %php-php-int-max ()
  "PHP PHP_INT_MAX constant."
  9223372036854775807)

(defun %php-php-int-min ()
  "PHP PHP_INT_MIN constant."
  -9223372036854775808)

(defun %php-php-float-max ()
  "PHP PHP_FLOAT_MAX constant."
  most-positive-double-float)

(defun %php-php-float-epsilon ()
  "PHP PHP_FLOAT_EPSILON constant."
  2.220446049250313d-16)

(defun %php-memory-limit-unit-multiplier (unit)
  "Return the multiplier for a PHP memory_limit suffix."
  (case (char-upcase unit)
    (#\K 1024)
    (#\M (expt 1024 2))
    (#\G (expt 1024 3))
    (#\T (expt 1024 4))
    (#\P (expt 1024 5))
    (#\E (expt 1024 6))
    (#\Z (expt 1024 7))
    (#\Y (expt 1024 8))
    (t 1)))

(defun %php-parse-memory-limit (value)
  "Parse a PHP memory_limit-style value into bytes or :unlimited."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (%php-stringify value))))
    (cond
      ((string= text "-1")
       (values nil t))
      ((zerop (length text))
       (values nil nil))
      (t
       (let* ((len (length text))
              (unit (and (> len 0)
                         (find (char-upcase (char text (1- len)))
                               "KMGTPEZY"
                               :test #'char=)))
              (number-text (if unit (subseq text 0 (1- len)) text)))
         (handler-case
             (let ((number (parse-integer (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                       number-text)
                                          :junk-allowed nil)))
               (values (if unit
                           (* number (%php-memory-limit-unit-multiplier
                                      (char text (1- len))))
                           number)
                       nil))
           (error ()
             (values nil nil))))))))

(defun %php-memory-limit-exceeds-p (value max-value)
  "Return true when VALUE is larger than MAX-VALUE."
  (multiple-value-bind (value-bytes value-unlimited-p)
      (%php-parse-memory-limit value)
    (multiple-value-bind (max-bytes max-unlimited-p)
        (%php-parse-memory-limit max-value)
      (cond
        (max-unlimited-p nil)
        (value-unlimited-p t)
        ((or (null value-bytes) (null max-bytes)) nil)
        (t (> value-bytes max-bytes))))))

(defvar *php-ini-settings* nil
  "Mutable PHP INI settings for the current Lisp image.")

(defvar *php-error-reporting-level* 32767
  "Current PHP error_reporting() level.")

(defvar *php-error-handler-stack* nil
  "Stack of active PHP error handlers as (CALLBACK . ERROR-MASK).")

(defvar *php-exception-handler-stack* nil
  "Stack of active PHP exception handlers.")

(defun %php-current-error-handler ()
  "Return the active PHP error handler entry, or NIL."
  (first *php-error-handler-stack*))

(defun %php-current-exception-handler ()
  "Return the active PHP exception handler, or NIL."
  (first *php-exception-handler-stack*))

(defun %php-get-error-handler ()
  "PHP get_error_handler: return the active custom error handler, or null."
  (let ((entry (%php-current-error-handler)))
    (if entry
        (car entry)
        +php-null+)))

(defun %php-get-exception-handler ()
  "PHP get_exception_handler: return the active custom exception handler, or null."
  (or (%php-current-exception-handler) +php-null+))

(defun %php-valid-callback-or-throw (callback function-name)
  "Return CALLBACK when it is callable, otherwise signal a PHP TypeError."
  (if (%php-callable-function callback)
      callback
      (%php-throw 'type-error
                  (format nil "~A(): Argument #1 ($callback) must be a valid callback"
                          function-name))))

(defun %php-ensure-ini-settings ()
  (or *php-ini-settings*
      (setf *php-ini-settings*
            (let ((table (make-hash-table :test 'equal)))
              (dolist (entry *php-ini-defaults* table)
                (setf (gethash (car entry) table) (cdr entry)))))))

(defun %php-ini-key (varname)
  (string-downcase (%php-stringify varname)))

(defun %php-parse-integer-setting (value fallback)
  (handler-case
      (parse-integer (%php-stringify value) :junk-allowed t)
    (error () fallback)))

(defun %php-ini-get (varname)
  "PHP ini_get: get an INI configuration value."
  (let* ((key (%php-ini-key varname))
         (table (%php-ensure-ini-settings)))
    (if (string= key "date.timezone")
        (%php-date-default-timezone-get)
        (multiple-value-bind (value present-p) (gethash key table)
          (if present-p value nil)))))

(defun %php-ini-set (varname newvalue)
  "PHP ini_set: set an INI configuration value and return the previous value."
  (let* ((key (%php-ini-key varname))
         (table (%php-ensure-ini-settings))
         (old (%php-ini-get key)))
    (cond
      ((string= key "max_memory_limit")
       nil)
      ((string= key "date.timezone")
       (when (%php-date-default-timezone-set newvalue)
         (setf (gethash key table) (%php-date-default-timezone-get))
         old))
      ((string= key "memory_limit")
       (let ((max-memory-limit (%php-ini-get "max_memory_limit")))
         (when (and max-memory-limit
                    (%php-memory-limit-exceeds-p newvalue max-memory-limit))
           (format *error-output*
                   "memory_limit exceeds max_memory_limit (~A); clamping to ~A~%"
                   max-memory-limit
                   max-memory-limit)
           (setf newvalue max-memory-limit))
         (setf (gethash key table) (%php-stringify newvalue))
         old))
      (t
       (setf (gethash key table) (%php-stringify newvalue))
       (when (string= key "error_reporting")
         (setf *php-error-reporting-level*
               (%php-parse-integer-setting newvalue *php-error-reporting-level*)))
       old))))

(defun %php-set-error-handler (callback &optional error-types)
  "PHP set_error_handler: install CALLBACK and return the previous handler."
  (let* ((old (car (%php-current-error-handler)))
         (mask (%php-parse-integer-setting
                (if (and error-types (not (%php-null-p error-types)))
                    error-types
                    *php-error-reporting-level*)
                *php-error-reporting-level*))
         (cb (%php-valid-callback-or-throw callback "set_error_handler")))
    (push (cons cb mask) *php-error-handler-stack*)
    old))

(defun %php-restore-error-handler ()
  "PHP restore_error_handler: restore the previous custom error handler."
  (when *php-error-handler-stack*
    (pop *php-error-handler-stack*))
  t)

(defun %php-set-exception-handler (callback)
  "PHP set_exception_handler: install CALLBACK and return the previous handler."
  (let ((old (%php-current-exception-handler))
        (cb (%php-valid-callback-or-throw callback "set_exception_handler")))
    (push cb *php-exception-handler-stack*)
    old))

(defun %php-restore-exception-handler ()
  "PHP restore_exception_handler: restore the previous custom exception handler."
  (when *php-exception-handler-stack*
    (pop *php-exception-handler-stack*))
  t)

(defun %php-error-reporting (&optional level)
  "PHP error_reporting: set/get error reporting level."
  (let ((old *php-error-reporting-level*))
    (when level
      (setf *php-error-reporting-level*
            (%php-parse-integer-setting level *php-error-reporting-level*))
      (setf (gethash "error_reporting" (%php-ensure-ini-settings))
            (%php-stringify *php-error-reporting-level*)))
    old))

(defun %php-trigger-error (message &optional (error-type 256))
  "PHP trigger_error: generate a user-level error."
  (let* ((errno (%php-parse-integer-setting error-type 256))
         (handler-entry (%php-current-error-handler))
         (reportable-p (logtest errno *php-error-reporting-level*))
         (handled-p nil))
    (when (and reportable-p handler-entry (logtest errno (cdr handler-entry)))
      (let ((fn (%php-callable-function (car handler-entry))))
        (when fn
          (setf handled-p
                (%php-truthy
                 (funcall fn errno (%php-stringify message) nil nil))))))
    (unless (or handled-p (not reportable-p))
      ;; User-triggered errors (E_USER_*) report to stdout so trigger_error() is
      ;; observable in program output; engine-generated diagnostics (E_DEPRECATED,
      ;; E_WARNING, …) go to stderr as PHP's CLI SAPI does, keeping them out of
      ;; captured program output (e.g. serialize() using __sleep()).
      (if (logtest errno (logior 256 512 1024 16384)) ; E_USER_ERROR/WARNING/NOTICE/DEPRECATED
          (format t "~A~%" (%php-stringify message))
          (format *error-output* "~A~%" (%php-stringify message)))))
  t)

(defun %php-exit (&optional (status 0))
  "PHP exit/die: terminate execution."
  (when (stringp status) (%php-output-write status))
  (sb-ext:exit :code (if (numberp status) status 0)))

(defun %php-die (&optional (status 0))
  "PHP die: alias for exit."
  (%php-exit status))

(defun %php-sleep (seconds)
  "PHP sleep: delay execution."
  (sleep seconds)
  0)

(defun %php-usleep (microseconds)
  "PHP usleep: delay in microseconds."
  (sleep (/ microseconds 1000000.0))
  nil)

(defun %php-nl-langinfo (item)
  "PHP nl_langinfo: return deterministic locale metadata for ITEM."
  (let* ((entry (if (stringp item)
                    (find (string-upcase item) +php-nl-langinfo-items+
                          :test #'string= :key #'first)
                    (find (%php-to-integer item) +php-nl-langinfo-items+
                          :test #'= :key #'second))))
    (if entry
        (third entry)
        "")))

(defun %php-locale-normalized-id (locale)
  "Return a lowercase BCP-47-ish locale id without encoding/modifier suffixes."
  (let* ((raw (%php-stringify locale))
         (cut (reduce #'min
                      (remove nil (list (position #\. raw) (position #\@ raw)))
                      :initial-value (length raw))))
    (string-downcase (substitute #\- #\_ (subseq raw 0 cut)))))

(defun %php-locale-subtags (locale-id)
  (loop with start = 0
        for pos = (position #\- locale-id :start start)
        collect (subseq locale-id start pos)
        while pos
        do (setf start (1+ pos))))

(defun %php-locale-add-likely-subtags (locale)
  "PHP 8.5 Locale::addLikelySubtags compatibility helper."
  (let ((id (%php-locale-normalized-id locale)))
    (or (cdr (assoc id *php-locale-likely-subtags* :test #'string=))
        (if (position #\- id)
            id
            (format nil "~A-Latn-US" id)))))

(defun %php-locale-minimize-subtags (locale)
  "PHP 8.5 Locale::minimizeSubtags compatibility helper."
  (let* ((id (%php-locale-normalized-id locale))
         (likely (rassoc id *php-locale-likely-subtags* :test #'string-equal)))
    (or (car likely)
        (first (%php-locale-subtags id))
        id)))

(defun %php-locale-is-right-to-left (locale)
  "PHP 8.5 locale_is_right_to_left / Locale::isRightToLeft helper."
  (let* ((id (%php-locale-normalized-id locale))
         (subtags (%php-locale-subtags id))
         (primary (first subtags)))
    (and primary
         (or (member primary
                     '("ar" "arc" "ckb" "dv" "fa" "he" "iw" "ks" "lrc"
                       "mzn" "nqo" "pnb" "ps" "sd" "syr" "ug" "ur" "yi")
                     :test #'string=)
             (some (lambda (subtag)
                     (member subtag '("arab" "hebr" "nkoo" "syrc" "thaa")
                             :test #'string=))
                   subtags)))))

;;; ─── Output buffering and headers ───────────────────────────────────────────
;;; Output buffering and response-model builtins live in runtime-builtins-io-output.lisp.

;;; ─── Cookie / Session ─────────────────────────────────────────────────────────
;;; Cookie and session response-model builtins live in runtime-builtins-io-cookie-session.lisp.

;;; ─── String misc ─────────────────────────────────────────────────────────────

(defun %php-parse-str (str &optional result)
  "PHP parse_str: parse URL query string into variables."
  (let ((parts (%php-explode "&" str))
        (target (or result (%php-make-array))))
    (dolist (pair (%php-array-values-list parts))
      (let ((eq-pos (search "=" pair)))
        (when eq-pos
          (%php-array-set target
                          (%php-urldecode (subseq pair 0 eq-pos))
                          (%php-urldecode (subseq pair (1+ eq-pos)))))))
    target))

(defun %php-http-build-query (data &optional numeric-prefix arg-separator enc-type)
  "PHP http_build_query: generate URL-encoded query string."
  (declare (ignore numeric-prefix enc-type))
  (let ((sep (if (and arg-separator (not (%php-null-p arg-separator)))
                 (%php-stringify arg-separator) "&"))
        (parts nil))
    (when (hash-table-p data)
      (dolist (pair (%php-array-pairs data))
        (push (concatenate 'string
                           (%php-urlencode (%php-stringify (car pair)))
                           "="
                           (%php-urlencode (%php-stringify (cdr pair))))
              parts)))
    (format nil "~{~A~^~A~}" (list (car (nreverse parts)) sep))))

;;; ─── Type / class helpers ─────────────────────────────────────────────────────
;;; Object-model, SPL, and reflection builtins live in runtime-builtins-io-objects.lisp.

;;; ─── Misc utility ────────────────────────────────────────────────────────────

(defun %php-array-map-keys (callback array)
  "PHP array_map with keys support (2-arg form with null callback)."
  (when (null callback)
    (let ((result (%php-make-array)))
      (dolist (pair (%php-array-pairs array))
        (%php-array-set result (%php-array-next-auto-index result)
                        (let ((row (%php-make-array)))
                          (%php-array-set row 0 (car pair))
                          (%php-array-set row 1 (cdr pair))
                          row)))
      (return-from %php-array-map-keys result)))
  (%php-array-map callback array))


(defun %php-list-assign (&rest values)
  "PHP list() — returns the values as a PHP array for destructuring."
  (%php-list-to-array values))

(defun %php-reset (array)
  "PHP reset: reset array pointer to first element."
  (when (hash-table-p array)
    (let ((vals (%php-array-values-list array)))
      (if vals (first vals) nil))))

(defun %php-end (array)
  "PHP end: advance array pointer to last element."
  (when (hash-table-p array)
    (let ((vals (%php-array-values-list array)))
      (if vals (car (last vals)) nil))))

(defun %php-current (array)
  "PHP current: return current element."
  (%php-reset array))

(defun %php-next (array)
  "PHP next: advance internal pointer."
  (when (hash-table-p array)
    (let ((vals (%php-array-values-list array)))
      (if (> (length vals) 1) (second vals) nil))))

(defun %php-prev (array)
  "PHP prev: rewind internal pointer."
  (%php-reset array))

(defun %php-key (array)
  "PHP key: return current key."
  (when (hash-table-p array)
    (let ((keys (%php-array-ordered-keys array)))
      (if keys (first keys) nil))))

(defun %php-microtime-float ()
  "PHP microtime(true): return current time as float."
  (coerce (- (get-internal-real-time) 0) 'double-float))

;;; ─── Non-CL-named builtins that must be registered by SYMBOL ────────────────
;;; A PHP builtin call lowers to a function symbol the VM resolves only if it is
;;; fbound; a lambda registration leaves no fbound symbol, so these all hit
;;; "Undefined function: <NAME>".  Named helpers fix that.

(defun %php-class-implements (class &optional autoload)
  "PHP class_implements: return implemented interfaces keyed by interface name."
  (declare (ignore autoload))
  (%php-reflection-symbols-to-array
   (%php-reflection-class-interface-symbols class)))
(defun %php-class-parents (class &optional autoload)
  "PHP class_parents: return parent classes keyed by class name."
  (declare (ignore autoload))
  (%php-reflection-symbols-to-array
   (%php-reflection-class-parent-symbols class)))
(defun %php-class-uses (class &optional autoload)
  "PHP class_uses: return traits used directly by a class keyed by trait name."
  (declare (ignore autoload))
  (%php-reflection-symbols-to-array
   (%php-reflection-class-trait-symbols class)))

(defvar *php-spl-autoload-functions* nil
  "Registered PHP SPL autoload callbacks in call order.")

(defun %php-spl-autoload-callback-equal-p (left right)
  "Return true when two PHP callable values identify the same autoload entry."
  (or (eq left right)
      (equal left right)
      (and (stringp left) (stringp right) (string= left right))))

(defun %php-spl-autoload-callback-valid-p (callback)
  "Return true when CALLBACK is acceptable for spl_autoload_register."
  (or (stringp callback)
      (functionp callback)
      (cl-cc/vm::%vm-closure-object-p callback)
      (and (hash-table-p callback)
           (plusp (%php-count callback)))))

(defun %php-spl-autoload-register (&optional callback throw prepend)
  "PHP spl_autoload_register: add CALLBACK to the SPL autoload queue.

The runtime does not model file loading yet, but it now preserves PHP-visible
autoload state for spl_autoload_functions/unregister and class_exists($c, true)
plumbing."
  (let ((entry (if (or (null callback) (%php-null-p callback))
                   "spl_autoload"
                   callback)))
    (cond
      ((not (%php-spl-autoload-callback-valid-p entry))
       (if (%php-truthy throw)
           (%php-throw 'type-error
                       "spl_autoload_register(): Argument #1 ($callback) must be a valid callback")
           nil))
      ((some (lambda (registered)
               (%php-spl-autoload-callback-equal-p registered entry))
             *php-spl-autoload-functions*)
       t)
      ((%php-truthy prepend)
       (push entry *php-spl-autoload-functions*)
       t)
      (t
       (setf *php-spl-autoload-functions*
             (append *php-spl-autoload-functions* (list entry)))
       t))))

(defun %php-spl-autoload-unregister (&optional callback)
  "PHP spl_autoload_unregister: remove CALLBACK from the SPL autoload queue."
  (let* ((entry (if (or (null callback) (%php-null-p callback))
                    "spl_autoload"
                    callback))
         (before *php-spl-autoload-functions*))
    (setf *php-spl-autoload-functions*
          (remove-if (lambda (registered)
                       (%php-spl-autoload-callback-equal-p registered entry))
                     *php-spl-autoload-functions*))
    (not (= (length before) (length *php-spl-autoload-functions*)))))

(defun %php-spl-autoload-functions ()
  "PHP spl_autoload_functions: return registered autoload callbacks."
  (%php-list-to-array *php-spl-autoload-functions*))

(defun %php-lcg-value ()
  "PHP lcg_value: a pseudo-random float in [0,1)."
  (random 1.0d0))

(defun %php-settype-array-value (value)
  "Return VALUE converted with PHP's `(array)` cast shape."
  (let ((result (%php-make-array)))
    (unless (%php-null-p value)
      (%php-array-set result 0 value))
    result))

(defun %php-settype-object-value (value)
  "Return VALUE converted with PHP's `(object)` cast shape."
  (let ((object (%php-make-array)))
    (%php-array-set object "__class__" "stdClass")
    (cond
      ((%php-null-p value))
      ((hash-table-p value)
       (dolist (pair (%php-array-pairs value))
         (%php-array-set object (car pair) (cdr pair))))
      (t
       (%php-array-set object "scalar" value)))
    object))

(defun %php-settype (v &optional type)
  "PHP settype: mutate the referenced value and return success."
  (let* ((target (%php-deref v))
         (type-name (string-downcase (%php-stringify type)))
         (converted
           (cond ((or (string= type-name "boolean") (string= type-name "bool"))
                  (%php-boolval target))
                 ((or (string= type-name "integer") (string= type-name "int"))
                  (%php-intval target))
                 ((or (string= type-name "float") (string= type-name "double"))
                  (%php-floatval target))
                 ((string= type-name "string")
                  (%php-strval target))
                 ((string= type-name "array")
                  (if (hash-table-p target) target (%php-settype-array-value target)))
                 ((string= type-name "object")
                  (if (or (%php-object-table-p target)
                          (and (not (%php-null-p target))
                               (not (null target))
                               (not (eq target t))
                               (not (numberp target))
                               (not (stringp target))
                               (not (hash-table-p target))))
                      target
                      (%php-settype-object-value target)))
                 ((string= type-name "null")
                  +php-null+)
                 (t :php-invalid-settype))))
    (if (eq converted :php-invalid-settype)
        nil
        (progn
          (%php-ref-set! v converted)
          t))))
(defun %php-scan-whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %php-sscanf-digit-value (ch)
  (cond ((and (char>= ch #\0) (char<= ch #\9))
         (- (char-code ch) (char-code #\0)))
        ((and (char>= ch #\a) (char<= ch #\f))
         (+ 10 (- (char-code ch) (char-code #\a))))
        ((and (char>= ch #\A) (char<= ch #\F))
         (+ 10 (- (char-code ch) (char-code #\A))))
        (t nil)))

(defun %php-sscanf-skip-ws (s pos)
  (loop while (and (< pos (length s))
                   (%php-scan-whitespace-char-p (char s pos)))
        do (incf pos)
        finally (return pos)))

(defun %php-sscanf-scan-int (s pos width radix &key auto-radix unsigned)
  (let* ((start-limit pos)
         (end-limit (if width (min (length s) (+ pos width)) (length s)))
         (sign 1)
         (base radix))
    (when (and (< pos end-limit)
               (not unsigned)
               (member (char s pos) '(#\+ #\-) :test #'char=))
      (when (char= (char s pos) #\-) (setf sign -1))
      (incf pos))
    (when auto-radix
      (cond
        ((and (<= (+ pos 2) end-limit)
              (char= (char s pos) #\0)
              (member (char s (1+ pos)) '(#\x #\X) :test #'char=))
         (setf base 16)
         (incf pos 2))
        ((and (< pos end-limit) (char= (char s pos) #\0))
         (setf base 8))
        (t (setf base 10))))
    (let ((digits-start pos)
          (value 0))
      (loop while (< pos end-limit)
            for digit = (%php-sscanf-digit-value (char s pos))
            while (and digit (< digit base))
            do (setf value (+ (* value base) digit))
               (incf pos))
      (if (= pos digits-start)
          (values nil start-limit nil)
          (values (* sign value) pos t)))))

(defun %php-sscanf-scan-float (s pos width)
  (let* ((start pos)
         (end-limit (if width (min (length s) (+ pos width)) (length s)))
         (saw-digit nil))
    (when (and (< pos end-limit) (member (char s pos) '(#\+ #\-) :test #'char=))
      (incf pos))
    (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
          do (setf saw-digit t)
             (incf pos))
    (when (and (< pos end-limit) (char= (char s pos) #\.))
      (incf pos)
      (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
            do (setf saw-digit t)
               (incf pos)))
    (when (and saw-digit (< pos end-limit) (member (char s pos) '(#\e #\E) :test #'char=))
      (let ((exp-start pos)
            (exp-digits nil))
        (incf pos)
        (when (and (< pos end-limit) (member (char s pos) '(#\+ #\-) :test #'char=))
          (incf pos))
        (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
              do (setf exp-digits t)
                 (incf pos))
        (unless exp-digits
          (setf pos exp-start))))
    (if saw-digit
        (handler-case
            (let ((*read-default-float-format* 'double-float))
              (values (coerce (read-from-string (subseq s start pos)) 'double-float)
                      pos t))
          (error () (values nil start nil)))
        (values nil start nil))))

(defun %php-sscanf-values (str fmt)
  "Return sscanf parsed values as a PHP array."
  (let* ((input (%php-stringify str))
         (format-string (%php-stringify fmt))
         (i 0)
         (j 0)
         (values nil)
         (failed nil))
    (labels ((emit (value)
               (push value values))
             (parse-width ()
               (let ((start j))
                 (loop while (and (< j (length format-string))
                                  (digit-char-p (char format-string j)))
                       do (incf j))
                 (when (< start j)
                   (parse-integer format-string :start start :end j)))))
      (loop while (and (< j (length format-string)) (not failed))
            for fch = (char format-string j)
            do (cond
                 ((%php-scan-whitespace-char-p fch)
                  (setf i (%php-sscanf-skip-ws input i))
                  (loop while (and (< j (length format-string))
                                   (%php-scan-whitespace-char-p
                                    (char format-string j)))
                        do (incf j)))
                 ((char/= fch #\%)
                  (if (and (< i (length input)) (char= (char input i) fch))
                      (progn (incf i) (incf j))
                      (setf failed t)))
                 (t
                  (incf j)
                  (cond
                    ((>= j (length format-string))
                     (setf failed t))
                    ((char= (char format-string j) #\%)
                     (if (and (< i (length input)) (char= (char input i) #\%))
                         (progn (incf i) (incf j))
                         (setf failed t)))
                    (t
                     (let ((suppress nil))
                       (when (and (< j (length format-string))
                                  (char= (char format-string j) #\*))
                         (setf suppress t)
                         (incf j))
                       (let ((width (parse-width)))
                         (when (>= j (length format-string))
                           (setf failed t))
                         (unless failed
                           (let ((spec (char-downcase (char format-string j))))
                             (incf j)
                             (multiple-value-bind (value new-pos ok)
                                 (case spec
                                   ((#\d #\u)
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 10
                                     :unsigned (char= spec #\u)))
                                   (#\i
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 10
                                     :auto-radix t))
                                   (#\x
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 16
                                     :unsigned t))
                                   (#\o
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 8
                                     :unsigned t))
                                   ((#\f #\e #\g)
                                    (%php-sscanf-scan-float
                                     input (%php-sscanf-skip-ws input i) width))
                                   (#\s
                                    (let* ((start (%php-sscanf-skip-ws input i))
                                           (end-limit (if width
                                                          (min (length input)
                                                               (+ start width))
                                                          (length input)))
                                           (end start))
                                      (loop while (and (< end end-limit)
                                                       (not (%php-scan-whitespace-char-p
                                                             (char input end))))
                                            do (incf end))
                                      (if (= start end)
                                          (values nil i nil)
                                          (values (subseq input start end) end t))))
                                   (#\c
                                    (let* ((count (or width 1))
                                           (end (+ i count)))
                                      (if (<= end (length input))
                                          (values (subseq input i end) end t)
                                          (values nil i nil))))
                                   (otherwise
                                    (values nil i nil)))
                               (if ok
                                   (progn
                                     (setf i new-pos)
                                     (unless suppress (emit value)))
                                   (setf failed t))))))))))))
      (%php-list-to-array (nreverse values)))))

(defun %php-sscanf (str fmt &rest ignored)
  "PHP sscanf: parse STR with FMT. Without out-params this returns a PHP array;
the parser lowers out-param calls through `%php-sscanf-values` and returns the
number of assigned values."
  (let ((values (%php-sscanf-values str fmt)))
    (if ignored (%php-count values) values)))

(defun %php-read-standard-input-string ()
  "Read the remaining dynamically bound standard input as a string."
  (with-output-to-string (out)
    (loop for ch = (read-char *standard-input* nil nil)
          while ch
          do (write-char ch out))))

(defun %php-scanf-values (fmt)
  "Return scanf parsed values from standard input as a PHP array."
  (%php-sscanf-values (%php-read-standard-input-string) fmt))

(defun %php-scanf (fmt &rest ignored)
  "PHP scanf: parse the remaining standard input with FMT."
  (let ((values (%php-scanf-values fmt)))
    (if ignored (%php-count values) values)))

(defun %php-iterator-to-array (iter &optional (preserve-keys t))
  "PHP iterator_to_array: drain a Generator into a PHP array."
  (if (php-generator-p iter)
      (let ((result (%php-make-array)))
        (loop for i from 0
              while (%php-generator-valid iter)
              do (if (%php-truthy preserve-keys)
                     (%php-array-set result i (%php-generator-next iter))
                     (%php-array-set result (%php-array-next-auto-index result)
                                     (%php-generator-next iter))))
        result)
      iter))

(defun %php-iterator-count (iter)
  "PHP iterator_count: number of elements a Generator yields."
  (if (php-generator-p iter)
      (length (php-gen-values iter))
      (%php-count iter)))
