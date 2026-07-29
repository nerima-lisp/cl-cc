;;;; packages/runtime/tests/runtime-io-tests.lisp — Runtime I/O Unit Tests
;;;;
;;;; Tests for packages/runtime/src/runtime-io.lisp:
;;;; define-rt-stream-op macro, format/read-line/write-line/peek-char,
;;;; string-output-stream helpers, stream predicates, and pathname utilities.

(in-package :cl-cc/test)


;;; ─── rt-format ──────────────────────────────────────────────────────────────

(it-sequential "rt-format-nil-stream-returns-string"
  (expect (cl-cc/runtime:rt-format nil "~A" 42) :to-equal "42"))

(it-sequential "rt-format-explicit-stream-writes-to-it"
  (let ((s (make-string-output-stream)))
    (cl-cc/runtime:rt-format s "~A" "hello")
    (expect (get-output-stream-string s) :to-equal "hello")))

;;; ─── String output stream helpers ──────────────────────────────────────────

(it-sequential "rt-make-string-output-stream-creates-stream-and-get-empty"
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (expect (streamp s) :to-be-truthy)
    (expect (cl-cc/runtime:rt-get-output-stream-string s) :to-equal "")))

(it-sequential "rt-get-output-stream-string-after-write-returns-content"
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (write-string "hello world" s)
    (expect (cl-cc/runtime:rt-get-output-stream-string s) :to-equal "hello world")))

(it-sequential "rt-get-output-stream-string-works-with-standard-stream"
  (let ((s (make-string-output-stream)))
    (write-string "test" s)
    (expect (cl-cc/runtime:rt-get-output-stream-string s) :to-equal "test")))

(it-sequential "rt-stream-op-output-wrappers-write-in-order"
  (let ((s (make-string-output-stream)))
    (cl-cc/runtime:rt-write-char #\A s)
    (cl-cc/runtime:rt-write-string "BC" s)
    (cl-cc/runtime:rt-write-line "D" s)
    (expect (get-output-stream-string s) :to-equal (format nil "ABCD~%"))))

(it-sequential "rt-stream-op-input-wrappers-read-and-peek"
  (let ((s (make-string-input-stream "xyz")))
    (expect (cl-cc/runtime:rt-read-char s) :to-equal #\x)
    (expect (cl-cc/runtime:rt-peek-char s) :to-equal #\y)
    (expect (cl-cc/runtime:rt-read-char s) :to-equal #\y)))

;;; ─── rt-read-line ───────────────────────────────────────────────────────────

(it-sequential "rt-read-line-from-string-stream"
  (let ((s (make-string-input-stream "hello")))
    (expect (cl-cc/runtime:rt-read-line s) :to-equal "hello")))

(it-sequential "rt-read-byte-write-byte-roundtrip-through-file"
  (let* ((path (merge-pathnames "cl-cc-runtime-io-bytes.bin" (uiop:temporary-directory)))
         (out (open path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (cl-cc/runtime:rt-write-byte 65 out)
           (cl-cc/runtime:rt-write-byte 66 out)
           (close out)
           (let ((in (open path :direction :input :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (progn
                    (expect (cl-cc/runtime:rt-read-byte in) :to-equal 65)
                    (expect (cl-cc/runtime:rt-read-byte in) :to-equal 66))
               (close in))))
      (when (probe-file path)
        (delete-file path)))))

;;; ─── rt-write-line ──────────────────────────────────────────────────────────

(it-sequential "rt-write-line-to-string"
  (let ((s (make-string-output-stream)))
    (cl-cc/runtime:rt-write-line "test" s)
    (expect (get-output-stream-string s) :to-equal (format nil "test~%"))))

;;; ─── Package wrappers ───────────────────────────────────────────────────────

(it-sequential "rt-make-package-creates-package"
  (let* ((pkg-name (gensym "RT-PKG-"))
         (pkg (cl-cc/runtime:rt-make-package pkg-name :use '(:cl))))
    (expect (hash-table-p pkg) :to-be-truthy)
    (expect (gethash :name pkg) :to-equal (string pkg-name))
    (expect (gethash (string pkg-name) cl-cc/runtime:*rt-package-registry*) :to-be pkg)))

(it-sequential "rt-find-package-locates-created-package"
  (let* ((pkg-name (gensym "RT-PKG-"))
         (pkg (cl-cc/runtime:rt-make-package pkg-name :use '(:cl))))
    (expect (cl-cc/runtime:rt-find-package pkg-name) :to-be pkg)))

(it-sequential "rt-find-package-prefers-runtime-registry"
  (let* ((pkg-name (gensym "RT-PKG-"))
         (pkg (cl-cc/runtime:rt-make-package pkg-name :use '(:cl))))
    (expect (gethash (string pkg-name) cl-cc/runtime:*rt-package-registry*) :to-be pkg)
    (expect (cl-cc/runtime:rt-find-package pkg-name) :to-be pkg)))

(it-sequential "rt-export-records-symbol"
  (let* ((pkg-name (gensym "RT-PKG-"))
         (pkg (cl-cc/runtime:rt-make-package pkg-name :use '(:cl)))
         (sym (cl-cc/runtime:rt-intern "FOO" pkg)))
    (cl-cc/runtime:rt-export sym pkg)
    (expect (member sym (gethash :exports pkg) :test #'eq) :to-be-truthy)))

(it-sequential "rt-use-unuse-package-maintains-inherited-symbols"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal)))
    (let* ((lib (cl-cc/runtime:rt-make-package "RT-USE-LIB"))
           (user (cl-cc/runtime:rt-make-package "RT-USE-USER"))
           (sym (cl-cc/runtime:rt-intern "EXPORTED" lib)))
      (cl-cc/runtime:rt-export sym lib)
      (expect (cl-cc/runtime::rt-use-package lib user) :to-be-truthy)
      (multiple-value-bind (found status)
          (cl-cc/runtime::rt-find-symbol "EXPORTED" user)
        (expect found :to-be sym)
        (expect status :to-be :inherited))
      (expect (cl-cc/runtime::rt-use-package lib user) :to-be-truthy)
      (expect (= 1 (length (gethash :use-list user))) :to-be-truthy)
      (cl-cc/runtime::rt-unuse-package lib user)
      (multiple-value-bind (found status)
          (cl-cc/runtime::rt-find-symbol "EXPORTED" user)
        (expect found :to-be nil)
        (expect status :to-be nil)))))

;;; ─── rt-peek-char ───────────────────────────────────────────────────────────

(it-sequential "rt-peek-char-does-not-advance"
  (let ((s (make-string-input-stream "abc")))
    (let ((first-peek  (cl-cc/runtime:rt-peek-char s))
          (second-peek (cl-cc/runtime:rt-peek-char s)))
      (expect first-peek :to-equal #\a)
      (expect second-peek :to-equal #\a))))

(it-sequential "rt-make-string-stream-input-direction"
  (let ((in (cl-cc/runtime:rt-make-string-stream "abc" :direction :input)))
    (expect (read-char in) :to-equal #\a)))

(it-sequential "rt-make-string-stream-output-direction"
  (let ((out (cl-cc/runtime:rt-make-string-stream "ignored" :direction :output)))
    (write-string "ok" out)
    (expect (cl-cc/runtime:rt-get-output-stream-string out) :to-equal "ok")))

;;; ─── Stream predicates ──────────────────────────────────────────────────────

(it-sequential "rt-stream-element-type-is-character"
  (expect (cl-cc/runtime:rt-stream-element-type *standard-input*) :to-equal 'character))

(it-sequential "rt-interactive-stream-p-is-zero-for-string-stream"
  (let ((s (make-string-input-stream "x")))
    (expect (= 0 (cl-cc/runtime:rt-interactive-stream-p s)) :to-be-truthy)))
