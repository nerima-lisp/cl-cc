;;;; wasm-binary-tests.lisp — Tests for wasm-binary, wasm-output, wasm-binary-debug
;;;
;;; Covers: LEB128 encoding, value-type table, name writing, export name handling,
;;; local decls, function body bytes, base64 encoding, hex-to-bytes round-trip,
;;; debug name cleaning, human local names, WAT/JSON string escaping, and SRI hash
;;; structure. All functions exercised live in the cl-cc/codegen package.

(in-package :cl-cc/test)



;;; ─────────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %make-test-buffer ()
  "Return a fresh byte buffer for testing."
  (cl-cc/binary::make-byte-buffer 64))

(defun %buffer-bytes (buf)
  "Extract bytes from BUF as a list."
  (coerce (cl-cc/binary::buffer-get-bytes buf) 'list))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 1: wasm-binary-value-type-byte — all 9 known types + error path
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-binary-value-type-byte-known i32"
  (destructuring-bind (keyword expected-byte) (list :i32 cl-cc/codegen::+wasm-i32+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known i64"
  (destructuring-bind (keyword expected-byte) (list :i64 cl-cc/codegen::+wasm-i64+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known f32"
  (destructuring-bind (keyword expected-byte) (list :f32 cl-cc/codegen::+wasm-f32+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known f64"
  (destructuring-bind (keyword expected-byte) (list :f64 cl-cc/codegen::+wasm-f64+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known f16"
  (destructuring-bind (keyword expected-byte) (list :f16 cl-cc/codegen::+wasm-f16+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known funcref"
  (destructuring-bind (keyword expected-byte) (list :funcref cl-cc/codegen::+wasm-funcref+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known externref"
  (destructuring-bind (keyword expected-byte) (list :externref cl-cc/codegen::+wasm-externref+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known stringref"
  (destructuring-bind (keyword expected-byte) (list :stringref cl-cc/codegen::+wasm-stringref+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-known eqref"
  (destructuring-bind (keyword expected-byte) (list :eqref cl-cc/codegen::+wasm-eqref+)
    (expect (cl-cc/codegen::wasm-binary-value-type-byte keyword) :to-equal expected-byte)))

(it-sequential "wasm-binary-value-type-byte-unknown-signals-error"
  (signals error (cl-cc/codegen::wasm-binary-value-type-byte :not-a-type)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 2: wasm-binary-write-name — byte-length prefix + content bytes
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-binary-write-name-layout"
  (let ((buf (%make-test-buffer)))
    (cl-cc/codegen::wasm-binary-write-name buf "abc")
    (expect (%buffer-bytes buf) :to-equal '(3 97 98 99))))

(it-sequential "wasm-binary-write-name-empty"
  (let ((buf (%make-test-buffer)))
    (cl-cc/codegen::wasm-binary-write-name buf "")
    (expect (%buffer-bytes buf) :to-equal '(0))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 3: wasm-binary-export-name-for-function — $-prefix stripping / gating
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %make-test-wasm-func (&key exported-p export-name wat-name index)
  "Construct a minimal wasm-func for export-name tests."
  (let ((f (cl-cc/codegen::make-wasm-func)))
    (setf (cl-cc/codegen::wasm-func-exported-p f) exported-p
          (cl-cc/codegen::wasm-func-export-name f) export-name
          (cl-cc/codegen::wasm-func-wat-name f) wat-name
          (cl-cc/codegen::wasm-func-index f) index)
    f))

(it-sequential "wasm-binary-export-name-strips-dollar-prefix"
  (let ((func (%make-test-wasm-func :exported-p t :export-name nil
                                     :wat-name "$my_func" :index 0)))
    (expect (cl-cc/codegen::wasm-binary-export-name-for-function func) :to-equal "my_func")))

(it-sequential "wasm-binary-export-name-uses-explicit-export-name"
  (let ((func (%make-test-wasm-func :exported-p t :export-name "main"
                                     :wat-name "$some_internal" :index 0)))
    (expect (cl-cc/codegen::wasm-binary-export-name-for-function func) :to-equal "main")))

(it-sequential "wasm-binary-export-name-nil-when-not-exported"
  (let ((func (%make-test-wasm-func :exported-p nil :export-name nil
                                     :wat-name "$hidden" :index 0)))
    (expect (cl-cc/codegen::wasm-binary-export-name-for-function func) :to-equal nil)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 4: wasm-binary-write-local-decls — count + type pairs
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-binary-write-local-decls-empty"
  (let ((buf (%make-test-buffer)))
    (cl-cc/codegen::wasm-binary-write-local-decls buf nil)
    (expect (%buffer-bytes buf) :to-equal '(0))))

(it-sequential "wasm-binary-write-local-decls-two-groups"
  (let ((buf (%make-test-buffer)))
    ;; 2 groups: (3 :i32) and (1 :i64)
    (cl-cc/codegen::wasm-binary-write-local-decls buf (list (list 3 :i32) (list 1 :i64)))
    (let ((bytes (%buffer-bytes buf)))
      ;; First byte: 2 groups.
      (expect (first bytes) :to-equal 2)
      ;; Group 0: count=3 + type-byte for :i32.
      (expect (second bytes) :to-equal 3)
      (expect (third bytes) :to-equal cl-cc/codegen::+wasm-i32+)
      ;; Group 1: count=1 + type-byte for :i64.
      (expect (fourth bytes) :to-equal 1)
      (expect (fifth bytes) :to-equal cl-cc/codegen::+wasm-i64+))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 5: wasm-binary-function-body-bytes — stack-neutral empty body
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-binary-function-body-bytes-minimal"
  (let* ((dummy-func (cl-cc/codegen::make-wasm-func))
         (body (cl-cc/codegen::wasm-binary-function-body-bytes dummy-func))
         (body-list (coerce body 'list)))
    (expect (typep body '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    ;; Minimal body: 1 byte for local-count (0) + 1 byte for end (0x0b).
    (expect (length body-list) :to-equal 2)
    (expect (first body-list) :to-equal 0)
    (expect (second body-list) :to-equal cl-cc/codegen::+wasm-end+)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 6: %wasm-base64-encode — RFC 4648 vectors
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-base64-encode-rfc4648-vectors empty"
  (destructuring-bind (bytes expected) (list #() "")
    (expect (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8)))) :to-equal expected)))

(it-sequential "wasm-base64-encode-rfc4648-vectors one-byte"
  (destructuring-bind (bytes expected) (list #(77) "TQ==")
    (expect (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8)))) :to-equal expected)))

(it-sequential "wasm-base64-encode-rfc4648-vectors two-bytes"
  (destructuring-bind (bytes expected) (list #(77 97) "TWE=")
    (expect (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8)))) :to-equal expected)))

(it-sequential "wasm-base64-encode-rfc4648-vectors three-bytes"
  (destructuring-bind (bytes expected) (list #(77 97 110) "TWFu")
    (expect (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8)))) :to-equal expected)))

(it-sequential "wasm-base64-encode-rfc4648-vectors four-bytes"
  (destructuring-bind (bytes expected) (list #(77 97 110 32) "TWFuIA==")
    (expect (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8)))) :to-equal expected)))

(it-sequential "wasm-base64-encode-rfc4648-vectors hello"
  (destructuring-bind (bytes expected) (list #(104 101 108 108 111) "aGVsbG8=")
    (expect (cl-cc/codegen::%wasm-base64-encode (coerce bytes '(vector (unsigned-byte 8)))) :to-equal expected)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 7: %wasm-hex-to-bytes — round-trip
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-hex-to-bytes-round-trip"
  (let ((bytes (cl-cc/codegen::%wasm-hex-to-bytes "deadbeef")))
    (expect (coerce bytes 'list) :to-equal '(#xde #xad #xbe #xef))))

(it-sequential "wasm-hex-to-bytes-with-spaces-and-colons"
  (let ((bytes (cl-cc/codegen::%wasm-hex-to-bytes "de:ad be:ef")))
    (expect (coerce bytes 'list) :to-equal '(#xde #xad #xbe #xef))))

(it-sequential "wasm-hex-to-bytes-empty"
  (let ((bytes (cl-cc/codegen::%wasm-hex-to-bytes "")))
    (expect (length bytes) :to-equal 0)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 8: %wasm-clean-debug-name — $-stripping and nil input
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-clean-debug-name-cases dollar-prefix"
  (destructuring-bind (input expected) (list "$my_func" "my_func")
    (expect (cl-cc/codegen::%wasm-clean-debug-name input) :to-equal expected)))

(it-sequential "wasm-clean-debug-name-cases no-dollar"
  (destructuring-bind (input expected) (list "plain" "plain")
    (expect (cl-cc/codegen::%wasm-clean-debug-name input) :to-equal expected)))

(it-sequential "wasm-clean-debug-name-cases double-dollar"
  (destructuring-bind (input expected) (list "$$weird" "$weird")
    (expect (cl-cc/codegen::%wasm-clean-debug-name input) :to-equal expected)))

(it-sequential "wasm-clean-debug-name-cases empty-string"
  (destructuring-bind (input expected) (list "" "")
    (expect (cl-cc/codegen::%wasm-clean-debug-name input) :to-equal expected)))

(it-sequential "wasm-clean-debug-name-cases nil-input"
  (destructuring-bind (input expected) (list nil "anonymous")
    (expect (cl-cc/codegen::%wasm-clean-debug-name input) :to-equal expected)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 9: %wasm-human-local-name — :R0 temp-result, :R1 temp-r1, non-numeric raw
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-human-local-name-cases r0"
  (destructuring-bind (reg expected) (list :R0 "temp-result")
    (expect (cl-cc/codegen::%wasm-human-local-name reg) :to-equal expected)))

(it-sequential "wasm-human-local-name-cases r1"
  (destructuring-bind (reg expected) (list :R1 "temp-r1")
    (expect (cl-cc/codegen::%wasm-human-local-name reg) :to-equal expected)))

(it-sequential "wasm-human-local-name-cases r5"
  (destructuring-bind (reg expected) (list :R5 "temp-r5")
    (expect (cl-cc/codegen::%wasm-human-local-name reg) :to-equal expected)))

(it-sequential "wasm-human-local-name-cases r10"
  (destructuring-bind (reg expected) (list :R10 "temp-r10")
    (expect (cl-cc/codegen::%wasm-human-local-name reg) :to-equal expected)))

(it-sequential "wasm-human-local-name-cases non-numeric"
  (destructuring-bind (reg expected) (list :RAX "rax")
    (expect (cl-cc/codegen::%wasm-human-local-name reg) :to-equal expected)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 10: %wasm-wat-string — quote/backslash/newline escaping
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-wat-string-escaping plain"
  (destructuring-bind (input expected) (list "hello" "\"hello\"")
    (when expected
    (expect (cl-cc/codegen::%wasm-wat-string input) :to-equal expected))))

(it-sequential "wasm-wat-string-escaping double-q"
  (destructuring-bind (input expected) (list "say \"hi\"" "\"say \\\"hi\\\"\"")
    (when expected
    (expect (cl-cc/codegen::%wasm-wat-string input) :to-equal expected))))

(it-sequential "wasm-wat-string-escaping backslash"
  (destructuring-bind (input expected) (list "a\\b" "\"a\\\\b\"")
    (when expected
    (expect (cl-cc/codegen::%wasm-wat-string input) :to-equal expected))))

(it-sequential "wasm-wat-string-escaping newline"
  (destructuring-bind (input expected) (list (format nil "x~%y") "\"x\\0ay\"")
    (when expected
    (expect (cl-cc/codegen::%wasm-wat-string input) :to-equal expected))))

(it-sequential "wasm-wat-string-escaping return"
  (destructuring-bind (input expected) (list (format nil "x~Cy" #\Return) "\"x\\0dy\"")
    (when expected
    (expect (cl-cc/codegen::%wasm-wat-string input) :to-equal expected))))

(it-sequential "wasm-wat-string-escaping tab"
  (destructuring-bind (input expected) (list (format nil "x~Cy" #\Tab) nil)
    (when expected
    (expect (cl-cc/codegen::%wasm-wat-string input) :to-equal expected))))

(it-sequential "wasm-wat-string-tab-escape"
  (let ((result (cl-cc/codegen::%wasm-wat-string (string #\Tab))))
    (expect result :to-equal "\"\\09\"")))

(it-sequential "wasm-wat-string-nil-input"
  (expect (cl-cc/codegen::%wasm-wat-string nil) :to-equal "\"\""))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 11: %wasm-json-string — \n vs \0a difference from wat
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-json-string-newline-uses-backslash-n"
  (let ((result (cl-cc/codegen::%wasm-json-string (format nil "line1~%line2"))))
    (expect (search "\\n" result :test #'char=) :to-be-truthy)
    (expect (search "\\0a" result :test #'char=) :to-equal nil)))

(it-sequential "wasm-json-string-tab-uses-backslash-t"
  (let ((result (cl-cc/codegen::%wasm-json-string (string #\Tab))))
    (expect result :to-equal "\"\\t\"")))

(it-sequential "wasm-json-string-double-quote-escaping"
  (let ((result (cl-cc/codegen::%wasm-json-string "say \"hello\"")))
    (expect (search "\\\"" result :test #'char=) :to-be-truthy)))

(it-sequential "wasm-json-string-vs-wat-string-newline-differ"
  (let* ((input (format nil "~%"))
         (wat (cl-cc/codegen::%wasm-wat-string input))
         (json (cl-cc/codegen::%wasm-json-string input)))
    (expect wat :to-equal "\"\\0a\"")
    (expect json :to-equal "\"\\n\"")
    (expect (string= wat json) :to-be-falsy)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Section 12: wasm-file-sri-hash — sha384- prefix and base64 body structure
;;; ─────────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-file-sri-hash-structure"
  (let* ((tmp (merge-pathnames (make-pathname :name (format nil "cl-cc-test-sri-~A" (gensym))
                                              :type "bin")
                               (uiop:temporary-directory)))
         (content (make-array 8 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4 5 6 7 8))))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp)
           (with-open-file (out tmp :direction :output :if-exists :supersede
                                    :if-does-not-exist :create
                                    :element-type '(unsigned-byte 8))
             (write-sequence content out))
           (let ((sri (cl-cc/codegen:wasm-file-sri-hash tmp)))
             (expect (typep sri 'string) :to-be-truthy)
             ;; Must begin with the algorithm prefix.
             (expect (subseq sri 0 7) :to-equal "sha384-")
             ;; The base64 body must be non-empty and consist of valid base64 chars.
             (let ((b64-body (subseq sri 7)))
               (expect (> (length b64-body) 0) :to-be-truthy)
               (expect (every (lambda (c)
                                     (or (alphanumericp c)
                                         (member c '(#\+ #\/ #\=))))
                                   b64-body) :to-be-truthy))))
      (ignore-errors (delete-file tmp)))))
