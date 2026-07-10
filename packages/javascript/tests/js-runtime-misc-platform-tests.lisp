;;;; packages/javascript/tests/js-runtime-misc-platform-tests.lisp
;;;;
;;;; Unit tests for platform-oriented runtime functions:
;;;; URLSearchParams, TextEncoder/TextDecoder, Intl.*, crypto.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list),
;;;;             js-runtime-misc-tests.lisp (%jr-assert-string-props)

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-suite)

;;; ─── URLSearchParams ─────────────────────────────────────────────────────────

(deftest js-rt-url-search-params-basic-operations
  "%js-make-url-search-params supports ordered query param operations."
  (let* ((params (cl-cc/javascript::%js-make-url-search-params "?a=1&b=two&a=3"))
         (get-fn (gethash "get" params))
         (get-all-fn (gethash "getAll" params))
         (has-fn (gethash "has" params))
         (set-fn (gethash "set" params))
         (append-fn (gethash "append" params))
         (to-string-fn (gethash "toString" params)))
    (assert-string= "1" (funcall get-fn "a"))
    (assert-true (funcall has-fn "b"))
    (let ((all-a (funcall get-all-fn "a")))
      (assert-= 2 (length all-a))
      (assert-string= "1" (aref all-a 0))
      (assert-string= "3" (aref all-a 1)))
    (funcall set-fn "a" "9")
    (assert-string= "a=9&b=two" (funcall to-string-fn))
    (funcall append-fn "space" "a b")
    (assert-string= "a=9&b=two&space=a+b" (funcall to-string-fn))))

(deftest js-rt-url-search-params-updates-url-search-and-href
  "URL.searchParams mutators update the owning URL search and href fields."
  (let* ((url (cl-cc/javascript::%js-make-url "https://example.com/path?a=1"))
         (params (gethash "searchParams" url))
         (set-fn (gethash "set" params))
         (delete-fn (gethash "delete" params)))
    (funcall set-fn "a" "2")
    (assert-string= "?a=2" (gethash "search" url))
    (assert-string= "https://example.com/path?a=2" (gethash "href" url))
    (funcall delete-fn "a")
    (assert-string= "" (gethash "search" url))
    (assert-string= "https://example.com/path" (gethash "href" url))))

(deftest js-rt-url-search-params-sort-is-stable-and-updates-url
  "URLSearchParams.sort orders by key, preserves duplicate order, and updates URL."
  (let* ((url (cl-cc/javascript::%js-make-url "https://example.com/path?b=2&a=1&a=0&c=3"))
         (params (gethash "searchParams" url))
         (sort-fn (gethash "sort" params))
         (to-string-fn (gethash "toString" params))
         (get-all-fn (gethash "getAll" params)))
    (assert-eq cl-cc/javascript::+js-undefined+ (funcall sort-fn))
    (assert-string= "a=1&a=0&b=2&c=3" (funcall to-string-fn))
    (assert-string= "?a=1&a=0&b=2&c=3" (gethash "search" url))
    (assert-string= "https://example.com/path?a=1&a=0&b=2&c=3"
                    (gethash "href" url))
    (let ((all-a (funcall get-all-fn "a")))
      (assert-= 2 (length all-a))
      (assert-string= "1" (aref all-a 0))
      (assert-string= "0" (aref all-a 1)))))

;;; ─── TextEncoder / TextDecoder ──────────────────────────────────────────────

(deftest js-rt-text-encoder-encode-utf8
  "TextEncoder.encode returns UTF-8 bytes in a Uint8Array."
  (let* ((encoder (cl-cc/javascript::%js-make-text-encoder))
         (text (format nil "A~C~C" (code-char #x00E9) (code-char #x20AC)))
         (encoded (funcall (gethash "encode" encoder) text)))
    (assert-true (cl-cc/javascript::js-typed-array-p encoded))
    (assert-string= "Uint8Array" (cl-cc/javascript::js-ta-type-name encoded))
    (assert-= 6 (cl-cc/javascript::js-ta-length encoded))
    (assert-equal '(65 195 169 226 130 172)
                  (loop for i below (cl-cc/javascript::js-ta-length encoded)
                        collect (cl-cc/javascript::%js-ta-get encoded i)))))

(deftest js-rt-text-decoder-decode-utf8
  "TextDecoder.decode reads UTF-8 bytes from a typed array."
  (let* ((decoder (cl-cc/javascript::%js-make-text-decoder "utf8"))
         (bytes (cl-cc/javascript::%js-make-typed-array "Uint8Array" 6))
         (expected (format nil "A~C~C" (code-char #x00E9) (code-char #x20AC))))
    (loop for b in '(65 195 169 226 130 172)
          for i from 0
          do (cl-cc/javascript::%js-ta-set bytes i b))
    (assert-string= "utf-8" (gethash "encoding" decoder))
    (assert-string= expected (funcall (gethash "decode" decoder) bytes))
    (assert-string= "" (funcall (gethash "decode" decoder)))))

(deftest js-rt-text-decoder-decode-subarray
  "TextDecoder.decode reads bytes through the TypedArray accessor path."
  (let* ((decoder (cl-cc/javascript::%js-make-text-decoder))
         (bytes (cl-cc/javascript::%js-make-typed-array "Uint8Array" 6))
         (expected (format nil "~C~C" (code-char #x00E9) (code-char #x20AC))))
    (loop for b in '(88 195 169 226 130 0)
          for i from 0
          do (cl-cc/javascript::%js-ta-set bytes i b))
    (let ((view (cl-cc/javascript::%js-ta-subarray bytes 1)))
      (cl-cc/javascript::%js-ta-set view 4 172)
      (assert-string= expected (funcall (gethash "decode" decoder) view)))))

(deftest js-rt-text-encoder-encode-into
  "TextEncoder.encodeInto writes complete UTF-8 characters into destination."
  (let* ((encoder (cl-cc/javascript::%js-make-text-encoder))
         (dest (cl-cc/javascript::%js-make-typed-array "Uint8Array" 4))
         (text (format nil "A~C~C" (code-char #x00E9) (code-char #x20AC)))
         (result (funcall (gethash "encodeInto" encoder) text dest)))
    (assert-= 2 (gethash "read" result))
    (assert-= 3 (gethash "written" result))
    (assert-equal '(65 195 169 0)
                  (loop for i below (cl-cc/javascript::js-ta-length dest)
                        collect (cl-cc/javascript::%js-ta-get dest i)))))

;;; ─── Intl.NumberFormat ──────────────────────────────────────────────────────

(deftest js-rt-intl-number-format-fraction-options
  "Intl.NumberFormat honors basic minimum/maximum fraction digit options."
  (let* ((formatter (cl-cc/javascript::%js-make-intl-number-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "minimumFractionDigits" 1
                              "maximumFractionDigits" 2)))
         (format-fn (gethash "format" formatter)))
    (assert-string= "12.35" (funcall format-fn 12.345d0))
    (assert-string= "12.0" (funcall format-fn 12))))

(deftest js-rt-intl-number-format-grouping-option
  "Intl.NumberFormat applies grouping by default and allows disabling it."
  (let* ((grouped (cl-cc/javascript::%js-make-intl-number-format "en-US"))
         (plain (cl-cc/javascript::%js-make-intl-number-format
                 "en-US" (cl-cc/javascript::%js-make-object
                          "useGrouping" nil))))
    (assert-string= "12,345" (funcall (gethash "format" grouped) 12345))
    (assert-string= "12345" (funcall (gethash "format" plain) 12345))))

(deftest js-rt-intl-number-format-percent-and-parts
  "Intl.NumberFormat supports a basic percent style and exposes simple parts."
  (let* ((formatter (cl-cc/javascript::%js-make-intl-number-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "style" "percent"
                              "minimumFractionDigits" 1
                              "maximumFractionDigits" 1)))
         (format-fn (gethash "format" formatter))
         (parts-fn (gethash "formatToParts" formatter))
         (parts (funcall parts-fn 0.1234d0)))
    (assert-string= "12.3%" (funcall format-fn 0.1234d0))
    (assert-string= "integer" (gethash "type" (aref parts 0)))
    (assert-string= "12" (gethash "value" (aref parts 0)))
    (assert-string= "decimal" (gethash "type" (aref parts 1)))
    (assert-string= "." (gethash "value" (aref parts 1)))
    (assert-string= "fraction" (gethash "type" (aref parts 2)))
    (assert-string= "3" (gethash "value" (aref parts 2)))
    (assert-string= "percentSign" (gethash "type" (aref parts 3)))
    (assert-string= "%" (gethash "value" (aref parts 3)))))

;;; ─── Intl.Collator ──────────────────────────────────────────────────────────

(deftest js-rt-intl-collator-numeric-option
  "Intl.Collator({ numeric: true }) compares digit runs numerically."
  (let* ((collator (cl-cc/javascript::%js-make-intl-collator
                    "en-US" (cl-cc/javascript::%js-make-object "numeric" t)))
         (compare (gethash "compare" collator)))
    (assert-true (< (funcall compare "item2" "item10") 0))
    (assert-true (> (funcall compare "item11" "item2") 0))))

(deftest js-rt-intl-collator-sensitivity-base
  "Intl.Collator({ sensitivity: 'base' }) ignores case and basic accents."
  (let* ((collator (cl-cc/javascript::%js-make-intl-collator
                    "en-US" (cl-cc/javascript::%js-make-object
                             "sensitivity" "base")))
         (compare (gethash "compare" collator)))
    (assert-= 0 (funcall compare "Résumé" "resume"))
    (assert-= 0 (funcall compare "Alpha" "alpha"))))

(deftest js-rt-intl-collator-resolved-options
  "Intl.Collator.resolvedOptions exposes the lightweight option state."
  (let* ((collator (cl-cc/javascript::%js-make-intl-collator
                    "en-US" (cl-cc/javascript::%js-make-object
                             "numeric" t
                             "sensitivity" "accent")))
         (resolved (funcall (gethash "resolvedOptions" collator))))
    (assert-string= "en-US" (gethash "locale" resolved))
    (assert-string= "sort" (gethash "usage" resolved))
    (assert-string= "accent" (gethash "sensitivity" resolved))
    (assert-true (gethash "numeric" resolved))))

;;; ─── Intl.DateTimeFormat ───────────────────────────────────────────────────

(deftest js-rt-intl-date-time-format-default-and-parts
  "Intl.DateTimeFormat defaults to deterministic UTC month/day/year output."
  (let* ((formatter (cl-cc/javascript::%js-make-intl-date-time-format "en-US"))
         (date (cl-cc/javascript::%js-make-date 97445000))
         (format-fn (gethash "format" formatter))
         (parts (funcall (gethash "formatToParts" formatter) date)))
    (assert-string= "1/2/1970" (funcall format-fn date))
    (assert-= 5 (length parts))
    (assert-string= "month" (gethash "type" (aref parts 0)))
    (assert-string= "1" (gethash "value" (aref parts 0)))
    (assert-string= "literal" (gethash "type" (aref parts 1)))
    (assert-string= "/" (gethash "value" (aref parts 1)))
    (assert-string= "day" (gethash "type" (aref parts 2)))
    (assert-string= "2" (gethash "value" (aref parts 2)))
    (assert-string= "year" (gethash "type" (aref parts 4)))
    (assert-string= "1970" (gethash "value" (aref parts 4)))))

(deftest js-rt-intl-date-time-format-options-and-resolved
  "Intl.DateTimeFormat honors basic component options and resolvedOptions."
  (let* ((formatter (cl-cc/javascript::%js-make-intl-date-time-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "year" "2-digit"
                              "month" "short"
                              "day" "2-digit"
                              "hour" "2-digit"
                              "minute" "2-digit"
                              "second" "2-digit"
                              "hour12" t)))
         (date (cl-cc/javascript::%js-make-date 97445000))
         (resolved (funcall (gethash "resolvedOptions" formatter))))
    (assert-string= "Jan/02/70, 03:04:05 AM"
                    (funcall (gethash "format" formatter) date))
    (assert-string= "en-US" (gethash "locale" resolved))
    (assert-string= "UTC" (gethash "timeZone" resolved))
    (assert-string= "short" (gethash "month" resolved))
    (assert-string= "2-digit" (gethash "day" resolved))
    (assert-string= "2-digit" (gethash "hour" resolved))
    (assert-true (gethash "hour12" resolved))))

(deftest js-rt-intl-date-time-format-style-options
  "Intl.DateTimeFormat maps dateStyle/timeStyle to stable English UTC output."
  (let* ((formatter (cl-cc/javascript::%js-make-intl-date-time-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "dateStyle" "medium"
                              "timeStyle" "short")))
         (date (cl-cc/javascript::%js-make-date 97445000))
         (resolved (funcall (gethash "resolvedOptions" formatter))))
    (assert-string= "Jan/2/1970, 3:04"
                    (funcall (gethash "format" formatter) date))
    (assert-string= "medium" (gethash "dateStyle" resolved))
    (assert-string= "short" (gethash "timeStyle" resolved))
    (assert-string= "short" (gethash "month" resolved))
    (assert-string= "2-digit" (gethash "minute" resolved))))

;;; ─── Intl.ListFormat ────────────────────────────────────────────────────────

(deftest js-rt-intl-list-format-conjunction-and-disjunction
  "Intl.ListFormat formats conjunction and disjunction lists."
  (let* ((conjunction (cl-cc/javascript::%js-make-intl-list-format "en-US"))
         (disjunction (cl-cc/javascript::%js-make-intl-list-format
                       "en-US" (cl-cc/javascript::%js-make-object
                                "type" "disjunction")))
         (items (%jr-arr "red" "green" "blue")))
    (assert-string= "red, green, and blue"
                    (funcall (gethash "format" conjunction) items))
    (assert-string= "red, green, or blue"
                    (funcall (gethash "format" disjunction) items))))

(deftest js-rt-intl-list-format-to-parts-and-resolved-options
  "Intl.ListFormat exposes parts and resolved option state."
  (let* ((formatter (cl-cc/javascript::%js-make-intl-list-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "type" "unit"
                              "style" "short")))
         (parts (funcall (gethash "formatToParts" formatter)
                         (%jr-arr "1 km" "2 min")))
         (resolved (funcall (gethash "resolvedOptions" formatter))))
    (assert-= 3 (length parts))
    (assert-string= "element" (gethash "type" (aref parts 0)))
    (assert-string= "1 km" (gethash "value" (aref parts 0)))
    (assert-string= "literal" (gethash "type" (aref parts 1)))
    (assert-string= ", " (gethash "value" (aref parts 1)))
    (assert-string= "element" (gethash "type" (aref parts 2)))
    (assert-string= "2 min" (gethash "value" (aref parts 2)))
    (assert-string= "en-US" (gethash "locale" resolved))
    (assert-string= "unit" (gethash "type" resolved))
    (assert-string= "short" (gethash "style" resolved))))

;;; ─── Intl.PluralRules ───────────────────────────────────────────────────────

(deftest js-rt-intl-plural-rules-cardinal-and-ordinal
  "Intl.PluralRules selects basic English cardinal and ordinal categories."
  (let* ((cardinal (cl-cc/javascript::%js-make-intl-plural-rules "en-US"))
         (ordinal (cl-cc/javascript::%js-make-intl-plural-rules
                   "en-US" (cl-cc/javascript::%js-make-object
                            "type" "ordinal"))))
    (assert-string= "one" (funcall (gethash "select" cardinal) 1))
    (assert-string= "other" (funcall (gethash "select" cardinal) 2))
    (assert-string= "one" (funcall (gethash "select" ordinal) 21))
    (assert-string= "two" (funcall (gethash "select" ordinal) 22))
    (assert-string= "few" (funcall (gethash "select" ordinal) 23))
    (assert-string= "other" (funcall (gethash "select" ordinal) 11))))

(deftest js-rt-intl-plural-rules-select-range-and-resolved-options
  "Intl.PluralRules exposes selectRange and resolved option categories."
  (let* ((rules (cl-cc/javascript::%js-make-intl-plural-rules
                 "en-US" (cl-cc/javascript::%js-make-object
                          "type" "ordinal")))
         (resolved (funcall (gethash "resolvedOptions" rules)))
         (categories (gethash "pluralCategories" resolved)))
    (assert-string= "few" (funcall (gethash "selectRange" rules) 1 3))
    (assert-string= "en-US" (gethash "locale" resolved))
    (assert-string= "ordinal" (gethash "type" resolved))
    (assert-equal '("one" "two" "few" "other") (%jr-list categories))))

;;; ─── crypto ──────────────────────────────────────────────────────────────────

(deftest js-rt-crypto-random-uuid
  "%js-make-crypto randomUUID returns a string matching UUID format."
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (uuid   (funcall (gethash "randomUUID" crypto))))
    (assert-true (stringp uuid))
    (assert-= 36 (length uuid))
    (assert-string= uuid (string-downcase uuid))
    (assert-equal #\- (char uuid 8))
    (assert-equal #\- (char uuid 13))
    (assert-equal #\- (char uuid 18))
    (assert-equal #\- (char uuid 23))
    (assert-equal #\4 (char uuid 14))
    (assert-true (member (char uuid 19) '(#\8 #\9 #\a #\b) :test #'char=))))

(deftest js-rt-crypto-get-random-values
  "%js-make-crypto getRandomValues fills and returns the same integer TypedArray."
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (ta     (cl-cc/javascript::%js-make-typed-array "Uint8Array" 4))
         (ret    (funcall (gethash "getRandomValues" crypto) ta))
         (buffer (cl-cc/javascript::js-ta-buffer ta)))
    (assert-eq ta ret)
    (assert-= 4 (cl-cc/javascript::js-ta-length ret))
    (loop for i below (cl-cc/javascript::js-ta-length ret)
          do (progn
               (assert-true (integerp (aref buffer i)))
               (assert-true (<= 0 (aref buffer i) 255))))))

(deftest js-rt-crypto-get-random-values-typed-array
  "%js-make-crypto getRandomValues fills integer TypedArrays."
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (ta     (cl-cc/javascript::%js-make-typed-array "Uint8Array" 8))
         (ret    (funcall (gethash "getRandomValues" crypto) ta))
         (buffer (cl-cc/javascript::js-ta-buffer ta)))
    (assert-eq ta ret)
    (assert-= 8 (cl-cc/javascript::js-ta-length ta))
    (loop for i below (cl-cc/javascript::js-ta-length ta)
          do (progn
               (assert-true (integerp (aref buffer i)))
               (assert-true (<= 0 (aref buffer i) 255))))))

(deftest js-rt-crypto-get-random-values-rejects-invalid-inputs
  "%js-make-crypto getRandomValues rejects non-integer views and oversized arrays."
  (let ((crypto (cl-cc/javascript::%js-make-crypto)))
    (assert-signals error
      (funcall (gethash "getRandomValues" crypto) (make-array 4 :initial-element 0)))
    (assert-signals error
      (funcall (gethash "getRandomValues" crypto)
               (cl-cc/javascript::%js-make-typed-array "Float32Array" 4)))
    (assert-signals error
      (funcall (gethash "getRandomValues" crypto)
               (cl-cc/javascript::%js-make-typed-array "Uint8Array" 65537)))))
