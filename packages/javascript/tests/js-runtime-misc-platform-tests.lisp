;;;; packages/javascript/tests/js-runtime-misc-platform-tests.lisp
;;;;
;;;; Unit tests for platform-oriented runtime functions:
;;;; URLSearchParams, TextEncoder/TextDecoder, Intl.*, crypto.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list),
;;;;             js-runtime-misc-tests.lisp (%jr-assert-string-props)

(in-package :cl-cc/test)

;;; ─── URLSearchParams ─────────────────────────────────────────────────────────

(it-sequential "js-rt-url-search-params-basic-operations"
  (let* ((params (cl-cc/javascript::%js-make-url-search-params "?a=1&b=two&a=3"))
         (get-fn (gethash "get" params))
         (get-all-fn (gethash "getAll" params))
         (has-fn (gethash "has" params))
         (set-fn (gethash "set" params))
         (append-fn (gethash "append" params))
         (to-string-fn (gethash "toString" params)))
    (expect (funcall get-fn "a") :to-equal "1")
    (expect (funcall has-fn "b") :to-be-truthy)
    (let ((all-a (funcall get-all-fn "a")))
      (expect (= 2 (length all-a)) :to-be-truthy)
      (expect (aref all-a 0) :to-equal "1")
      (expect (aref all-a 1) :to-equal "3"))
    (funcall set-fn "a" "9")
    (expect (funcall to-string-fn) :to-equal "a=9&b=two")
    (funcall append-fn "space" "a b")
    (expect (funcall to-string-fn) :to-equal "a=9&b=two&space=a+b")))

(it-sequential "js-rt-url-search-params-updates-url-search-and-href"
  (let* ((url (cl-cc/javascript::%js-make-url "https://example.com/path?a=1"))
         (params (gethash "searchParams" url))
         (set-fn (gethash "set" params))
         (delete-fn (gethash "delete" params)))
    (funcall set-fn "a" "2")
    (expect (gethash "search" url) :to-equal "?a=2")
    (expect (gethash "href" url) :to-equal "https://example.com/path?a=2")
    (funcall delete-fn "a")
    (expect (gethash "search" url) :to-equal "")
    (expect (gethash "href" url) :to-equal "https://example.com/path")))

(it-sequential "js-rt-url-search-params-sort-is-stable-and-updates-url"
  (let* ((url (cl-cc/javascript::%js-make-url "https://example.com/path?b=2&a=1&a=0&c=3"))
         (params (gethash "searchParams" url))
         (sort-fn (gethash "sort" params))
         (to-string-fn (gethash "toString" params))
         (get-all-fn (gethash "getAll" params)))
    (expect (funcall sort-fn) :to-be cl-cc/javascript::+js-undefined+)
    (expect (funcall to-string-fn) :to-equal "a=1&a=0&b=2&c=3")
    (expect (gethash "search" url) :to-equal "?a=1&a=0&b=2&c=3")
    (expect (gethash "href" url) :to-equal "https://example.com/path?a=1&a=0&b=2&c=3")
    (let ((all-a (funcall get-all-fn "a")))
      (expect (= 2 (length all-a)) :to-be-truthy)
      (expect (aref all-a 0) :to-equal "1")
      (expect (aref all-a 1) :to-equal "0"))))

;;; ─── TextEncoder / TextDecoder ──────────────────────────────────────────────

(it-sequential "js-rt-text-encoder-encode-utf8"
  (let* ((encoder (cl-cc/javascript::%js-make-text-encoder))
         (text (format nil "A~C~C" (code-char #x00E9) (code-char #x20AC)))
         (encoded (funcall (gethash "encode" encoder) text)))
    (expect (cl-cc/javascript::js-typed-array-p encoded) :to-be-truthy)
    (expect (cl-cc/javascript::js-ta-type-name encoded) :to-equal "Uint8Array")
    (expect (= 6 (cl-cc/javascript::js-ta-length encoded)) :to-be-truthy)
    (expect (loop for i below (cl-cc/javascript::js-ta-length encoded)
                        collect (cl-cc/javascript::%js-ta-get encoded i)) :to-equal '(65 195 169 226 130 172))))

(it-sequential "js-rt-text-decoder-decode-utf8"
  (let* ((decoder (cl-cc/javascript::%js-make-text-decoder "utf8"))
         (bytes (cl-cc/javascript::%js-make-typed-array "Uint8Array" 6))
         (expected (format nil "A~C~C" (code-char #x00E9) (code-char #x20AC))))
    (loop for b in '(65 195 169 226 130 172)
          for i from 0
          do (cl-cc/javascript::%js-ta-set bytes i b))
    (expect (gethash "encoding" decoder) :to-equal "utf-8")
    (expect (funcall (gethash "decode" decoder) bytes) :to-equal expected)
    (expect (funcall (gethash "decode" decoder)) :to-equal "")))

(it-sequential "js-rt-text-decoder-decode-subarray"
  (let* ((decoder (cl-cc/javascript::%js-make-text-decoder))
         (bytes (cl-cc/javascript::%js-make-typed-array "Uint8Array" 6))
         (expected (format nil "~C~C" (code-char #x00E9) (code-char #x20AC))))
    (loop for b in '(88 195 169 226 130 0)
          for i from 0
          do (cl-cc/javascript::%js-ta-set bytes i b))
    (let ((view (cl-cc/javascript::%js-ta-subarray bytes 1)))
      (cl-cc/javascript::%js-ta-set view 4 172)
      (expect (funcall (gethash "decode" decoder) view) :to-equal expected))))

(it-sequential "js-rt-text-encoder-encode-into"
  (let* ((encoder (cl-cc/javascript::%js-make-text-encoder))
         (dest (cl-cc/javascript::%js-make-typed-array "Uint8Array" 4))
         (text (format nil "A~C~C" (code-char #x00E9) (code-char #x20AC)))
         (result (funcall (gethash "encodeInto" encoder) text dest)))
    (expect (= 2 (gethash "read" result)) :to-be-truthy)
    (expect (= 3 (gethash "written" result)) :to-be-truthy)
    (expect (loop for i below (cl-cc/javascript::js-ta-length dest)
                        collect (cl-cc/javascript::%js-ta-get dest i)) :to-equal '(65 195 169 0))))

;;; ─── Intl.NumberFormat ──────────────────────────────────────────────────────

(it-sequential "js-rt-intl-number-format-fraction-options"
  (let* ((formatter (cl-cc/javascript::%js-make-intl-number-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "minimumFractionDigits" 1
                              "maximumFractionDigits" 2)))
         (format-fn (gethash "format" formatter)))
    (expect (funcall format-fn 12.345d0) :to-equal "12.35")
    (expect (funcall format-fn 12) :to-equal "12.0")))

(it-sequential "js-rt-intl-number-format-grouping-option"
  (let* ((grouped (cl-cc/javascript::%js-make-intl-number-format "en-US"))
         (plain (cl-cc/javascript::%js-make-intl-number-format
                 "en-US" (cl-cc/javascript::%js-make-object
                          "useGrouping" nil))))
    (expect (funcall (gethash "format" grouped) 12345) :to-equal "12,345")
    (expect (funcall (gethash "format" plain) 12345) :to-equal "12345")))

(it-sequential "js-rt-intl-number-format-percent-and-parts"
  (let* ((formatter (cl-cc/javascript::%js-make-intl-number-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "style" "percent"
                              "minimumFractionDigits" 1
                              "maximumFractionDigits" 1)))
         (format-fn (gethash "format" formatter))
         (parts-fn (gethash "formatToParts" formatter))
         (parts (funcall parts-fn 0.1234d0)))
    (expect (funcall format-fn 0.1234d0) :to-equal "12.3%")
    (expect (gethash "type" (aref parts 0)) :to-equal "integer")
    (expect (gethash "value" (aref parts 0)) :to-equal "12")
    (expect (gethash "type" (aref parts 1)) :to-equal "decimal")
    (expect (gethash "value" (aref parts 1)) :to-equal ".")
    (expect (gethash "type" (aref parts 2)) :to-equal "fraction")
    (expect (gethash "value" (aref parts 2)) :to-equal "3")
    (expect (gethash "type" (aref parts 3)) :to-equal "percentSign")
    (expect (gethash "value" (aref parts 3)) :to-equal "%")))

;;; ─── Intl.Collator ──────────────────────────────────────────────────────────

(it-sequential "js-rt-intl-collator-numeric-option"
  (let* ((collator (cl-cc/javascript::%js-make-intl-collator
                    "en-US" (cl-cc/javascript::%js-make-object "numeric" t)))
         (compare (gethash "compare" collator)))
    (expect (< (funcall compare "item2" "item10") 0) :to-be-truthy)
    (expect (> (funcall compare "item11" "item2") 0) :to-be-truthy)))

(it-sequential "js-rt-intl-collator-sensitivity-base"
  (let* ((collator (cl-cc/javascript::%js-make-intl-collator
                    "en-US" (cl-cc/javascript::%js-make-object
                             "sensitivity" "base")))
         (compare (gethash "compare" collator)))
    (expect (= 0 (funcall compare "Résumé" "resume")) :to-be-truthy)
    (expect (= 0 (funcall compare "Alpha" "alpha")) :to-be-truthy)))

(it-sequential "js-rt-intl-collator-resolved-options"
  (let* ((collator (cl-cc/javascript::%js-make-intl-collator
                    "en-US" (cl-cc/javascript::%js-make-object
                             "numeric" t
                             "sensitivity" "accent")))
         (resolved (funcall (gethash "resolvedOptions" collator))))
    (expect (gethash "locale" resolved) :to-equal "en-US")
    (expect (gethash "usage" resolved) :to-equal "sort")
    (expect (gethash "sensitivity" resolved) :to-equal "accent")
    (expect (gethash "numeric" resolved) :to-be-truthy)))

;;; ─── Intl.DateTimeFormat ───────────────────────────────────────────────────

(it-sequential "js-rt-intl-date-time-format-default-and-parts"
  (let* ((formatter (cl-cc/javascript::%js-make-intl-date-time-format "en-US"))
         (date (cl-cc/javascript::%js-make-date 97445000))
         (format-fn (gethash "format" formatter))
         (parts (funcall (gethash "formatToParts" formatter) date)))
    (expect (funcall format-fn date) :to-equal "1/2/1970")
    (expect (= 5 (length parts)) :to-be-truthy)
    (expect (gethash "type" (aref parts 0)) :to-equal "month")
    (expect (gethash "value" (aref parts 0)) :to-equal "1")
    (expect (gethash "type" (aref parts 1)) :to-equal "literal")
    (expect (gethash "value" (aref parts 1)) :to-equal "/")
    (expect (gethash "type" (aref parts 2)) :to-equal "day")
    (expect (gethash "value" (aref parts 2)) :to-equal "2")
    (expect (gethash "type" (aref parts 4)) :to-equal "year")
    (expect (gethash "value" (aref parts 4)) :to-equal "1970")))

(it-sequential "js-rt-intl-date-time-format-options-and-resolved"
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
    (expect (funcall (gethash "format" formatter) date) :to-equal "Jan/02/70, 03:04:05 AM")
    (expect (gethash "locale" resolved) :to-equal "en-US")
    (expect (gethash "timeZone" resolved) :to-equal "UTC")
    (expect (gethash "month" resolved) :to-equal "short")
    (expect (gethash "day" resolved) :to-equal "2-digit")
    (expect (gethash "hour" resolved) :to-equal "2-digit")
    (expect (gethash "hour12" resolved) :to-be-truthy)))

(it-sequential "js-rt-intl-date-time-format-style-options"
  (let* ((formatter (cl-cc/javascript::%js-make-intl-date-time-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "dateStyle" "medium"
                              "timeStyle" "short")))
         (date (cl-cc/javascript::%js-make-date 97445000))
         (resolved (funcall (gethash "resolvedOptions" formatter))))
    (expect (funcall (gethash "format" formatter) date) :to-equal "Jan/2/1970, 3:04")
    (expect (gethash "dateStyle" resolved) :to-equal "medium")
    (expect (gethash "timeStyle" resolved) :to-equal "short")
    (expect (gethash "month" resolved) :to-equal "short")
    (expect (gethash "minute" resolved) :to-equal "2-digit")))

;;; ─── Intl.ListFormat ────────────────────────────────────────────────────────

(it-sequential "js-rt-intl-list-format-conjunction-and-disjunction"
  (let* ((conjunction (cl-cc/javascript::%js-make-intl-list-format "en-US"))
         (disjunction (cl-cc/javascript::%js-make-intl-list-format
                       "en-US" (cl-cc/javascript::%js-make-object
                                "type" "disjunction")))
         (items (%jr-arr "red" "green" "blue")))
    (expect (funcall (gethash "format" conjunction) items) :to-equal "red, green, and blue")
    (expect (funcall (gethash "format" disjunction) items) :to-equal "red, green, or blue")))

(it-sequential "js-rt-intl-list-format-to-parts-and-resolved-options"
  (let* ((formatter (cl-cc/javascript::%js-make-intl-list-format
                     "en-US" (cl-cc/javascript::%js-make-object
                              "type" "unit"
                              "style" "short")))
         (parts (funcall (gethash "formatToParts" formatter)
                         (%jr-arr "1 km" "2 min")))
         (resolved (funcall (gethash "resolvedOptions" formatter))))
    (expect (= 3 (length parts)) :to-be-truthy)
    (expect (gethash "type" (aref parts 0)) :to-equal "element")
    (expect (gethash "value" (aref parts 0)) :to-equal "1 km")
    (expect (gethash "type" (aref parts 1)) :to-equal "literal")
    (expect (gethash "value" (aref parts 1)) :to-equal ", ")
    (expect (gethash "type" (aref parts 2)) :to-equal "element")
    (expect (gethash "value" (aref parts 2)) :to-equal "2 min")
    (expect (gethash "locale" resolved) :to-equal "en-US")
    (expect (gethash "type" resolved) :to-equal "unit")
    (expect (gethash "style" resolved) :to-equal "short")))

;;; ─── Intl.PluralRules ───────────────────────────────────────────────────────

(it-sequential "js-rt-intl-plural-rules-cardinal-and-ordinal"
  (let* ((cardinal (cl-cc/javascript::%js-make-intl-plural-rules "en-US"))
         (ordinal (cl-cc/javascript::%js-make-intl-plural-rules
                   "en-US" (cl-cc/javascript::%js-make-object
                            "type" "ordinal"))))
    (expect (funcall (gethash "select" cardinal) 1) :to-equal "one")
    (expect (funcall (gethash "select" cardinal) 2) :to-equal "other")
    (expect (funcall (gethash "select" ordinal) 21) :to-equal "one")
    (expect (funcall (gethash "select" ordinal) 22) :to-equal "two")
    (expect (funcall (gethash "select" ordinal) 23) :to-equal "few")
    (expect (funcall (gethash "select" ordinal) 11) :to-equal "other")))

(it-sequential "js-rt-intl-plural-rules-select-range-and-resolved-options"
  (let* ((rules (cl-cc/javascript::%js-make-intl-plural-rules
                 "en-US" (cl-cc/javascript::%js-make-object
                          "type" "ordinal")))
         (resolved (funcall (gethash "resolvedOptions" rules)))
         (categories (gethash "pluralCategories" resolved)))
    (expect (funcall (gethash "selectRange" rules) 1 3) :to-equal "few")
    (expect (gethash "locale" resolved) :to-equal "en-US")
    (expect (gethash "type" resolved) :to-equal "ordinal")
    (expect (%jr-list categories) :to-equal '("one" "two" "few" "other"))))

;;; ─── crypto ──────────────────────────────────────────────────────────────────

(it-sequential "js-rt-crypto-random-uuid"
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (uuid   (funcall (gethash "randomUUID" crypto))))
    (expect (stringp uuid) :to-be-truthy)
    (expect (= 36 (length uuid)) :to-be-truthy)
    (expect (string-downcase uuid) :to-equal uuid)
    (expect (char uuid 8) :to-equal #\-)
    (expect (char uuid 13) :to-equal #\-)
    (expect (char uuid 18) :to-equal #\-)
    (expect (char uuid 23) :to-equal #\-)
    (expect (char uuid 14) :to-equal #\4)
    (expect (member (char uuid 19) '(#\8 #\9 #\a #\b) :test #'char=) :to-be-truthy)))

(it-sequential "js-rt-crypto-get-random-values"
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (ta     (cl-cc/javascript::%js-make-typed-array "Uint8Array" 4))
         (ret    (funcall (gethash "getRandomValues" crypto) ta))
         (buffer (cl-cc/javascript::js-ta-buffer ta)))
    (expect ret :to-be ta)
    (expect (= 4 (cl-cc/javascript::js-ta-length ret)) :to-be-truthy)
    (loop for i below (cl-cc/javascript::js-ta-length ret)
          do (progn
               (expect (integerp (aref buffer i)) :to-be-truthy)
               (expect (<= 0 (aref buffer i) 255) :to-be-truthy)))))

(it-sequential "js-rt-crypto-get-random-values-typed-array"
  (let* ((crypto (cl-cc/javascript::%js-make-crypto))
         (ta     (cl-cc/javascript::%js-make-typed-array "Uint8Array" 8))
         (ret    (funcall (gethash "getRandomValues" crypto) ta))
         (buffer (cl-cc/javascript::js-ta-buffer ta)))
    (expect ret :to-be ta)
    (expect (= 8 (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (loop for i below (cl-cc/javascript::js-ta-length ta)
          do (progn
               (expect (integerp (aref buffer i)) :to-be-truthy)
               (expect (<= 0 (aref buffer i) 255) :to-be-truthy)))))

(it-sequential "js-rt-crypto-get-random-values-rejects-invalid-inputs"
  (let ((crypto (cl-cc/javascript::%js-make-crypto)))
    (let ((%%signaled1 nil)) (handler-case (progn (funcall (gethash "getRandomValues" crypto) (make-array 4 :initial-element 0))) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))
    (let ((%%signaled2 nil)) (handler-case (progn (funcall (gethash "getRandomValues" crypto)
               (cl-cc/javascript::%js-make-typed-array "Float32Array" 4))) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))
    (let ((%%signaled3 nil)) (handler-case (progn (funcall (gethash "getRandomValues" crypto)
               (cl-cc/javascript::%js-make-typed-array "Uint8Array" 65537))) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))
