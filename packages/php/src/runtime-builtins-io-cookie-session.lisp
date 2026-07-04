;;;; PHP cookie and session response-model builtins.

(in-package :cl-cc/php)

;;; --- Cookie / session response model ---------------------------------------

(defvar *php-session-name* "PHPSESSID"
  "Current PHP session cookie name for the CLI response model.")

(defvar *php-session-id* ""
  "Current PHP session identifier for the CLI response model.")

(defvar *php-session-active-p* nil
  "Whether session_start() has activated the modeled session.")

(defvar *php-session-cookie-params* nil
  "Modeled session cookie params as a PHP ordered array.")

(defun %php-cookie-option (options key default)
  "Read KEY from PHP option array OPTIONS, preserving false as a value."
  (if (hash-table-p options)
      (let ((result default))
        (dolist (present-key (%php-array-ordered-keys options))
          (when (and (stringp present-key)
                     (string-equal present-key key))
            (let ((value (gethash present-key options)))
              (setf result (if (%php-null-p value) default value)))))
        result)
      default))

(defun %php-cookie-option-key-name (key)
  (if (stringp key)
      key
      (%php-stringify key)))

(defun %php-cookie-valid-option-key-p (key allowed-keys)
  (and (stringp key)
       (member key allowed-keys :test #'string-equal)))

(defun %php-cookie-valid-option-count (options allowed-keys)
  (let ((count 0))
    (when (hash-table-p options)
      (dolist (key (%php-array-ordered-keys options))
        (when (%php-cookie-valid-option-key-p key allowed-keys)
          (incf count))))
    count))

(defun %php-cookie-validate-option-keys
    (function-name options allowed-keys &key warn)
  "Validate cookie option array keys like PHP's cookie APIs."
  (when (hash-table-p options)
    (dolist (key (%php-array-ordered-keys options))
      (unless (%php-cookie-valid-option-key-p key allowed-keys)
        (if warn
            (%php-trigger-error
             (if (stringp key)
                 (format nil "~A(): Argument #1 ($lifetime_or_options) contains an unrecognized key \"~A\""
                         function-name
                         key)
                 (format nil "~A(): Argument #1 ($lifetime_or_options) cannot contain numeric keys"
                         function-name))
             2)
            (%php-throw 'value-error
                        (if (stringp key)
                            (format nil "~A(): option \"~A\" is invalid"
                                    function-name
                                    (%php-cookie-option-key-name key))
                            (format nil "~A(): option array cannot have numeric keys"
                                    function-name))))))))

(defun %php-cookie-string (value &optional (default ""))
  "Coerce cookie option VALUE to a string, using DEFAULT for null/omitted."
  (if (or (null value) (%php-null-p value))
      default
      (%php-stringify value)))

(defun %php-cookie-integer (value &optional (default 0))
  "Coerce cookie option VALUE to an integer, using DEFAULT for null/omitted."
  (if (or (null value) (%php-null-p value))
      default
      (%php-to-integer value)))

(defun %php-cookie-bool (value)
  "Coerce cookie option VALUE to a PHP boolean."
  (and (not (%php-null-p value))
       (%php-truthy value)))

(defun %php-cookie-samesite (function-name value &optional (default ""))
  "Normalize and validate SameSite cookie option values."
  (let ((samesite (%php-cookie-string value default)))
    (when (and (plusp (length samesite))
               (not (member samesite '("None" "Lax" "Strict")
                            :test #'string-equal)))
      (%php-throw 'value-error
                  (format nil "~A(): \"samesite\" option must be \"Strict\", \"Lax\", \"None\", or \"\""
                          function-name)))
    samesite))

(defun %php-cookie-params (&key (expires 0) (path "") (domain "")
                                secure httponly (samesite "") partitioned)
  "Return normalized cookie params as a plist."
  (list :expires expires
        :path path
        :domain domain
        :secure secure
        :httponly httponly
        :samesite samesite
        :partitioned partitioned))

(defun %php-cookie-validate-partitioned (function-name params)
  "PHP 8.5 CHIPS requires Partitioned cookies to also be Secure."
  (when (and (getf params :partitioned)
             (not (getf params :secure)))
    (%php-throw 'value-error
                (format nil "~A(): \"partitioned\" option cannot be used without \"secure\" option"
                        function-name)))
  params)

(defun %php-cookie-params-from-call
    (function-name expires-or-options path domain secure httponly)
  "Normalize setcookie()/setrawcookie() positional or options-array params."
  (when (hash-table-p expires-or-options)
    (%php-cookie-validate-option-keys
     function-name
     expires-or-options
     +php-cookie-option-keys+))
  (%php-cookie-validate-partitioned
   function-name
   (if (hash-table-p expires-or-options)
       (%php-cookie-params
        :expires (%php-cookie-integer
                  (%php-cookie-option expires-or-options "expires" 0))
        :path (%php-cookie-string
               (%php-cookie-option expires-or-options "path" ""))
        :domain (%php-cookie-string
                 (%php-cookie-option expires-or-options "domain" ""))
        :secure (%php-cookie-bool
                 (%php-cookie-option expires-or-options "secure" nil))
        :httponly (%php-cookie-bool
                   (%php-cookie-option expires-or-options "httponly" nil))
        :samesite (%php-cookie-samesite
                   function-name
                   (%php-cookie-option expires-or-options "samesite" ""))
        :partitioned (%php-cookie-bool
                      (%php-cookie-option expires-or-options "partitioned" nil)))
       (%php-cookie-params
        :expires (%php-cookie-integer expires-or-options)
        :path (%php-cookie-string path)
        :domain (%php-cookie-string domain)
        :secure (%php-cookie-bool secure)
        :httponly (%php-cookie-bool httponly)))))

(defun %php-cookie-weekday-name (index)
  (nth index '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun %php-cookie-month-name (index)
  (nth (1- index)
       '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
         "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun %php-cookie-expires-gmt (unix-seconds)
  "Format a Unix timestamp as an HTTP-date for cookie Expires."
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time (+ (%php-to-integer unix-seconds) 2208988800) 0)
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
            (%php-cookie-weekday-name day)
            date
            (%php-cookie-month-name month)
            year
            hour
            minute
            second)))

(defun %php-cookie-header (name value params &key raw-value)
  "Build a Set-Cookie header from normalized PARAMS."
  (let ((path (getf params :path))
        (domain (getf params :domain))
        (samesite (getf params :samesite))
        (expires (getf params :expires)))
    (with-output-to-string (out)
      (format out "Set-Cookie: ~A=~A"
              (%php-stringify name)
              (if raw-value
                  (%php-stringify value)
                  (%php-urlencode (%php-stringify value))))
      (when (> (%php-to-integer expires) 0)
        (format out "; expires=~A" (%php-cookie-expires-gmt expires)))
      (when (plusp (length path))
        (format out "; path=~A" path))
      (when (plusp (length domain))
        (format out "; domain=~A" domain))
      (when (getf params :secure)
        (write-string "; secure" out))
      (when (getf params :httponly)
        (write-string "; HttpOnly" out))
      (when (plusp (length samesite))
        (format out "; SameSite=~A" samesite))
      (when (getf params :partitioned)
        (write-string "; Partitioned" out)))))

(defun %php-queue-cookie-header (header)
  "Queue HEADER unless response headers have already been sent."
  (if *php-output-started-p*
      nil
      (progn
        (%php-header header nil)
        t)))

(defun %php-setcookie
    (name &optional value expires-or-options path domain secure httponly)
  "PHP setcookie: queue a URL-encoded Set-Cookie header."
  (let ((params (%php-cookie-params-from-call
                 "setcookie" expires-or-options path domain secure httponly)))
    (%php-queue-cookie-header (%php-cookie-header name value params))))

(defun %php-setrawcookie
    (name &optional value expires-or-options path domain secure httponly)
  "PHP setrawcookie: queue an unencoded Set-Cookie header."
  (let ((params (%php-cookie-params-from-call
                 "setrawcookie" expires-or-options path domain secure httponly)))
    (%php-queue-cookie-header
     (%php-cookie-header name value params :raw-value t))))

(defun %php-default-session-cookie-params ()
  "Return default modeled session cookie params."
  (let ((params (%php-make-array)))
    (%php-array-set params "lifetime" 0)
    (%php-array-set params "path" "/")
    (%php-array-set params "domain" "")
    (%php-array-set params "secure" nil)
    (%php-array-set params "partitioned" nil)
    (%php-array-set params "httponly" nil)
    (%php-array-set params "samesite" "")
    params))

(defun %php-copy-array (array)
  "Return a shallow copy of a PHP ordered array."
  (let ((copy (%php-make-array)))
    (when (hash-table-p array)
      (dolist (key (%php-array-ordered-keys array))
        (%php-array-set copy key (gethash key array))))
    copy))

(defun %php-ensure-session-cookie-params ()
  "Ensure the session cookie param array exists."
  (unless (hash-table-p *php-session-cookie-params*)
    (setf *php-session-cookie-params* (%php-default-session-cookie-params)))
  *php-session-cookie-params*)

(defun %php-session-cookie-params-plist (params)
  "Convert session cookie param array PARAMS to normalized cookie plist."
  (%php-cookie-params
   :expires 0
   :path (%php-cookie-string (%php-cookie-option params "path" "/") "/")
   :domain (%php-cookie-string (%php-cookie-option params "domain" ""))
   :secure (%php-cookie-bool (%php-cookie-option params "secure" nil))
   :httponly (%php-cookie-bool (%php-cookie-option params "httponly" nil))
   :samesite (%php-cookie-samesite
              "session_start"
              (%php-cookie-option params "samesite" ""))
   :partitioned (%php-cookie-bool
                 (%php-cookie-option params "partitioned" nil))))

(defun %php-session-partitioned-without-secure-p (params)
  "Return true when modeled session params violate CHIPS Secure requirements."
  (let ((cookie-params (%php-session-cookie-params-plist params)))
    (and (getf cookie-params :partitioned)
         (not (getf cookie-params :secure)))))

(defun %php-session-warn-partitioned-without-secure ()
  (%php-trigger-error
   "session_start(): Partitioned session cookie cannot be used without also configuring it as secure"
   2))

(defun %php-session-name (&optional name)
  "PHP session_name: get or set the modeled session cookie name."
  (if (or (null name) (%php-null-p name))
      *php-session-name*
      (let ((old *php-session-name*))
        (setf *php-session-name* (%php-stringify name))
        old)))

(defun %php-session-id (&optional id)
  "PHP session_id: get or set the modeled session id."
  (if (or (null id) (%php-null-p id))
      *php-session-id*
      (let ((old *php-session-id*))
        (setf *php-session-id* (%php-stringify id))
        old)))

(defun %php-session-get-cookie-params ()
  "PHP session_get_cookie_params: return current session cookie params."
  (%php-copy-array (%php-ensure-session-cookie-params)))

(defun %php-session-set-cookie-params
    (lifetime-or-options &optional path domain secure httponly)
  "PHP session_set_cookie_params with PHP 8.5 partitioned support."
  (let ((params (%php-copy-array (%php-ensure-session-cookie-params))))
    (if (hash-table-p lifetime-or-options)
        (progn
          (%php-cookie-validate-option-keys
           "session_set_cookie_params"
           lifetime-or-options
           +php-session-cookie-option-keys+
           :warn t)
          (when (zerop (%php-cookie-valid-option-count
                        lifetime-or-options
                        +php-session-cookie-option-keys+))
            (%php-throw
             'value-error
             "session_set_cookie_params(): Argument #1 ($lifetime_or_options) must contain at least 1 valid key"))
          (%php-array-set params "lifetime"
                          (%php-cookie-integer
                           (%php-cookie-option lifetime-or-options
                                               "lifetime"
                                               (%php-cookie-option params
                                                                   "lifetime"
                                                                   0))))
          (%php-array-set params "path"
                          (%php-cookie-string
                           (%php-cookie-option lifetime-or-options
                                               "path"
                                               (%php-cookie-option params
                                                                   "path"
                                                                   "/"))))
          (%php-array-set params "domain"
                          (%php-cookie-string
                           (%php-cookie-option lifetime-or-options
                                               "domain"
                                               (%php-cookie-option params
                                                                   "domain"
                                                                   ""))))
          (%php-array-set params "secure"
                          (%php-cookie-bool
                           (%php-cookie-option lifetime-or-options
                                               "secure"
                                               (%php-cookie-option params
                                                                   "secure"
                                                                   nil))))
          (%php-array-set params "httponly"
                          (%php-cookie-bool
                           (%php-cookie-option lifetime-or-options
                                               "httponly"
                                               (%php-cookie-option params
                                                                   "httponly"
                                                                   nil))))
          (%php-array-set params "samesite"
                          (%php-cookie-samesite
                           "session_set_cookie_params"
                           (%php-cookie-option lifetime-or-options
                                               "samesite"
                                               (%php-cookie-option params
                                                                   "samesite"
                                                                   ""))))
          (%php-array-set params "partitioned"
                          (%php-cookie-bool
                           (%php-cookie-option lifetime-or-options
                                               "partitioned"
                                               (%php-cookie-option params
                                                                   "partitioned"
                                                                   nil)))))
        (progn
          (%php-array-set params "lifetime"
                          (%php-cookie-integer lifetime-or-options))
          (%php-array-set params "path"
                          (%php-cookie-string path
                                              (%php-cookie-option params
                                                                  "path"
                                                                  "/")))
          (%php-array-set params "domain"
                          (%php-cookie-string domain
                                              (%php-cookie-option params
                                                                  "domain"
                                                                  "")))
          (%php-array-set params "secure" (%php-cookie-bool secure))
          (%php-array-set params "httponly" (%php-cookie-bool httponly))))
    (setf *php-session-cookie-params* params)
    t))

(defun %php-session-apply-start-options (options)
  "Apply session_start() options, including PHP 8.5 cookie_partitioned."
  (when (hash-table-p options)
    (let ((params (%php-copy-array (%php-ensure-session-cookie-params))))
      (multiple-value-bind (name present-p) (gethash "name" options)
        (when (and present-p (not (%php-null-p name)))
          (setf *php-session-name* (%php-stringify name))))
      (%php-array-set params "lifetime"
                      (%php-cookie-integer
                       (%php-cookie-option options
                                           "cookie_lifetime"
                                           (%php-cookie-option params
                                                               "lifetime"
                                                               0))))
      (%php-array-set params "path"
                      (%php-cookie-string
                       (%php-cookie-option options
                                           "cookie_path"
                                           (%php-cookie-option params
                                                               "path"
                                                               "/"))))
      (%php-array-set params "domain"
                      (%php-cookie-string
                       (%php-cookie-option options
                                           "cookie_domain"
                                           (%php-cookie-option params
                                                               "domain"
                                                               ""))))
      (%php-array-set params "secure"
                      (%php-cookie-bool
                       (%php-cookie-option options
                                           "cookie_secure"
                                           (%php-cookie-option params
                                                               "secure"
                                                               nil))))
      (%php-array-set params "httponly"
                      (%php-cookie-bool
                       (%php-cookie-option options
                                           "cookie_httponly"
                                           (%php-cookie-option params
                                                               "httponly"
                                                               nil))))
      (%php-array-set params "samesite"
                      (%php-cookie-samesite
                       "session_start"
                       (%php-cookie-option options
                                           "cookie_samesite"
                                           (%php-cookie-option params
                                                               "samesite"
                                                               ""))))
      (%php-array-set params "partitioned"
                      (%php-cookie-bool
                       (%php-cookie-option options
                                           "cookie_partitioned"
                                           (%php-cookie-option params
                                                               "partitioned"
                                                               nil))))
      (setf *php-session-cookie-params* params))))

(defun %php-session-effective-id ()
  "Return the current session id, creating a deterministic one when absent."
  (when (or (null *php-session-id*)
            (string= *php-session-id* ""))
    (setf *php-session-id* "clccsession"))
  *php-session-id*)

(defun %php-session-start (&optional options)
  "PHP session_start: mark a session active and queue the modeled cookie."
  (%php-session-apply-start-options options)
  (cond
    (*php-output-started-p* nil)
    ((%php-session-partitioned-without-secure-p
      (%php-ensure-session-cookie-params))
     (%php-session-warn-partitioned-without-secure)
     nil)
    (t
     (let* ((params (%php-ensure-session-cookie-params))
            (cookie-params (%php-session-cookie-params-plist params))
            (header (%php-cookie-header *php-session-name*
                                        (%php-session-effective-id)
                                        cookie-params
                                        :raw-value t)))
       (%php-queue-cookie-header header)
       (setf *php-session-active-p* t)
       t))))
