;;;; tests/unit/parse/diagnostics-tests.lisp — Diagnostic System Tests
;;;;
;;;; Tests for src/parse/diagnostics.lisp:
;;;; byte-offset-to-line-col, source-line-at, format-diagnostic,
;;;; format-diagnostic-list, make-parse-error, make-parse-warning,
;;;; parse-failure condition.

(in-package :cl-cc/test)



;;; ─── byte-offset-to-line-col ──────────────────────────────────────────────────

(it-sequential "diag-byte-offset-to-line-col-cases simple"
  (destructuring-bind (src offset expected-line expected-col) (list "hello" 0 1 1)
    (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (expect line :to-equal expected-line)
     (expect col :to-equal expected-col))))

(it-sequential "diag-byte-offset-to-line-col-cases with-space"
  (destructuring-bind (src offset expected-line expected-col) (list "hello world" 5 1 6)
    (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (expect line :to-equal expected-line)
     (expect col :to-equal expected-col))))

(it-sequential "diag-byte-offset-to-line-col-cases with-format-1"
  (destructuring-bind (src offset expected-line expected-col) (list (format nil "abc~%def") 4 2 1)
    (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (expect line :to-equal expected-line)
     (expect col :to-equal expected-col))))

(it-sequential "diag-byte-offset-to-line-col-cases with-format-2"
  (destructuring-bind (src offset expected-line expected-col) (list (format nil "a~%b~%cde") 4 3 1)
    (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (expect line :to-equal expected-line)
     (expect col :to-equal expected-col))))

(it-sequential "diag-byte-offset-to-line-col-cases late"
  (destructuring-bind (src offset expected-line expected-col) (list "hi" 100 1 3)
    (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (expect line :to-equal expected-line)
     (expect col :to-equal expected-col))))

(it-sequential "diag-byte-offset-to-line-col-cases empty"
  (destructuring-bind (src offset expected-line expected-col) (list "" 0 1 1)
    (multiple-value-bind (line col)
       (cl-cc/parse:byte-offset-to-line-col src offset)
     (expect line :to-equal expected-line)
     (expect col :to-equal expected-col))))

;;; ─── source-line-at ───────────────────────────────────────────────────────────

(it-sequential "diag-source-line-at-cases offset-zero"
  (destructuring-bind (src offset expected) (list (format nil "hello~%world") 0 "hello")
    (expect (cl-cc/parse:source-line-at src offset) :to-equal expected)))

(it-sequential "diag-source-line-at-cases offset-middle"
  (destructuring-bind (src offset expected) (list (format nil "hello~%world") 6 "world")
    (expect (cl-cc/parse:source-line-at src offset) :to-equal expected)))

(it-sequential "diag-source-line-at-cases no-format"
  (destructuring-bind (src offset expected) (list "foobar" 3 "foobar")
    (expect (cl-cc/parse:source-line-at src offset) :to-equal expected)))

;;; ─── diagnostic struct ────────────────────────────────────────────────────────

(it-sequential "diag-struct-creation"
  (let ((d (cl-cc/parse:make-diagnostic)))
    (expect (cl-cc/parse:diagnostic-severity d) :to-be :error)
    (expect (cl-cc/parse:diagnostic-message d) :to-equal "")
    (expect (cl-cc/parse::diagnostic-hints d) :to-be-null)
    (expect (cl-cc/parse::diagnostic-notes d) :to-be-null))
  (let ((d (cl-cc/parse:make-parse-error "bad syntax" '(5 . 10))))
    (expect (cl-cc/parse:diagnostic-severity d) :to-be :error)
    (expect (cl-cc/parse:diagnostic-message d) :to-equal "bad syntax")
    (expect (cl-cc/parse:diagnostic-span d) :to-equal '(5 . 10)))
  (let ((d (cl-cc/parse:make-parse-warning "deprecated" '(0 . 3))))
    (expect (cl-cc/parse:diagnostic-severity d) :to-be :warning)
    (expect (cl-cc/parse:diagnostic-message d) :to-equal "deprecated")))

(it-sequential "diag-error-with-hints-and-notes"
  (let ((d (cl-cc/parse:make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))
             :notes '("see docs"))))
    (expect (length (cl-cc/parse::diagnostic-hints d)) :to-equal 1)
    (expect (length (cl-cc/parse::diagnostic-notes d)) :to-equal 1)))

(it-sequential "diag-structured-fields"
  (let* ((fix-it (cl-cc/parse:make-fix-it :text "insert )" :span '(5 . 5)))
         (d (cl-cc/parse:make-parse-error "missing )" '(5 . 5)
              :error-code "E0001"
              :fix-it fix-it)))
    (expect (cl-cc/parse:diagnostic-error-code d) :to-equal "E0001")
    (expect (cl-cc/parse:diagnostic-fix-it d) :to-be fix-it)
    (expect (cl-cc/parse:fix-it-text fix-it) :to-equal "insert )")
    (expect (cl-cc/parse:fix-it-span fix-it) :to-equal '(5 . 5))))

;;; ─── format-diagnostic ────────────────────────────────────────────────────────

(it-sequential "diag-format-diagnostic-content"
  (let* ((d (cl-cc/parse:make-parse-error "unexpected )" '(5 . 6)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "hello)" out))))
    (expect (search "error" s) :to-be-truthy))
  (let* ((d (cl-cc/parse:make-parse-error "bad token" '(0 . 3)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "foo" out))))
    (expect (search "bad token" s) :to-be-truthy))
  (let* ((d (cl-cc/parse:make-parse-error "err" '(0 . 1) :source-file "test.lisp"))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "x" out))))
    (expect (search "test.lisp" s) :to-be-truthy)
    (expect (search "1:1" s) :to-be-truthy))
  (let* ((src "(defun bad)")
         (d (cl-cc/parse:make-parse-error "err" '(7 . 10)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d src out))))
    (expect (search "(defun bad)" s) :to-be-truthy))
  (let* ((d (cl-cc/parse:make-parse-error "err" '(0 . 1)
             :hints '(("try this" . (0 . 1)))))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "x" out))))
    (expect (search "hint: try this" s) :to-be-truthy))
  (let* ((d (cl-cc/parse:make-parse-error "err" '(0 . 1)
              :notes '("see documentation")))
         (s (with-output-to-string (out)
               (cl-cc/parse:format-diagnostic d "x" out))))
    (expect (search "note: see documentation" s) :to-be-truthy))
  (let* ((fix-it (cl-cc/parse:make-fix-it :text "insert )" :span '(5 . 5)))
         (d (cl-cc/parse:make-parse-error "missing )" '(5 . 5)
              :error-code "E0001"
              :fix-it fix-it))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic d "(foo" out))))
    (expect (search "code: E0001" s) :to-be-truthy)
    (expect (search "fix-it: replace" s) :to-be-truthy)
    (expect (search "insert )" s) :to-be-truthy)))

;;; ─── format-diagnostic-list ───────────────────────────────────────────────────

(it-sequential "diag-format-diagnostic-list"
  (let* ((d1 (cl-cc/parse:make-parse-error "err1" '(0 . 1)))
         (d2 (cl-cc/parse:make-parse-warning "warn1" '(2 . 3)))
         (s (with-output-to-string (out)
              (cl-cc/parse:format-diagnostic-list (list d1 d2) "abcdef" out))))
    (expect (search "1 error" s) :to-be-truthy)
    (expect (search "1 warning" s) :to-be-truthy))
  (let ((s (with-output-to-string (out)
             (cl-cc/parse:format-diagnostic-list nil "x" out))))
    (expect (search "0 errors" s) :to-be-truthy)
    (expect (search "0 warnings" s) :to-be-truthy)))

;;; ─── parse-failure condition ──────────────────────────────────────────────────

(it-sequential "diag-parse-failure-behavior"
  (let* ((d (cl-cc/parse:make-parse-error "oops" '(0 . 1)))
         (caught nil))
    (handler-case
      (error 'cl-cc/parse:parse-failure :diagnostics (list d))
      (cl-cc/parse:parse-failure (c)
        (setf caught c)))
    (expect caught :to-be-truthy)
    (expect (length (cl-cc/parse::parse-failure-diagnostics caught)) :to-equal 1))
  (let* ((d1 (cl-cc/parse:make-parse-error "e1" '(0 . 1)))
         (d2 (cl-cc/parse:make-parse-error "e2" '(2 . 3)))
         (msg (handler-case
                (error 'cl-cc/parse:parse-failure :diagnostics (list d1 d2))
                (cl-cc/parse:parse-failure (c) (format nil "~A" c)))))
    (expect (search "2 error" msg) :to-be-truthy)))
