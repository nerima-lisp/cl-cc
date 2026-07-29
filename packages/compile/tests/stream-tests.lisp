;;;; tests/integration/stream-tests.lisp — Stream I/O Integration Tests
;;;; Tests for Phase 2 (Streams) of self-hosting: stream predicates,
;;;; string streams, binary I/O, write-line, with-open-file, etc.

(in-package :cl-cc/test)



;;; ─── Stream Predicates ──────────────────────────────────────────────────

(it-sequential "stream-predicates streamp-on-string-output"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream))) (streamp s))" t)
    (assert-run= expected expr)))

(it-sequential "stream-predicates streamp-on-number"
  (destructuring-bind (expr expected) (list "(streamp 42)" nil)
    (assert-run= expected expr)))

(it-sequential "stream-predicates streamp-on-string"
  (destructuring-bind (expr expected) (list "(streamp \"hello\")" nil)
    (assert-run= expected expr)))

(it-sequential "stream-predicates input-stream-p-on-input"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-input-stream \"hello\"))) (input-stream-p s))" t)
    (assert-run= expected expr)))

(it-sequential "stream-predicates input-stream-p-on-output"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream))) (input-stream-p s))" nil)
    (assert-run= expected expr)))

(it-sequential "stream-predicates output-stream-p-on-output"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream))) (output-stream-p s))" t)
    (assert-run= expected expr)))

(it-sequential "stream-predicates output-stream-p-on-input"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-input-stream \"hi\"))) (output-stream-p s))" nil)
    (assert-run= expected expr)))

(it-sequential "stream-predicates open-stream-p-open"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream))) (open-stream-p s))" t)
    (assert-run= expected expr)))

;;; ─── String Streams (with-output-to-string / with-input-from-string) ────

(it-sequential "string-stream-ops with-output-to-string-basic"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s) (write-string \"hello\" s))" "hello")
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "string-stream-ops with-output-to-string-multiple"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))" "ab")
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "string-stream-ops with-output-to-string-initial-string"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s \"prefix:\") (write-string \"body\" s))" "prefix:body")
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "string-stream-ops with-input-from-string-read-char"
  (destructuring-bind (expr expected) (list "(with-input-from-string (s \"abc\") (read-char s))" #\a)
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "string-stream-ops with-input-from-string-read-line"
  (destructuring-bind (expr expected) (list "(with-input-from-string (s \"hello\") (read-line s))" "hello")
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "string-stream-ops make-string-output-stream-get"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream)))
               (write-string \"test\" s)
               (get-output-stream-string s))" "test")
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "string-stream-ops make-string-input-stream-read-char"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-input-stream \"xyz\"))) (read-char s))" #\x)
    (with-reset-repl-state
     (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-constructors make-synonym-stream"
  (destructuring-bind (expr expected) (list "(let ((*terminal-io* (make-string-output-stream)))
               (write-char #\\A (make-synonym-stream '*terminal-io*))
               (get-output-stream-string *terminal-io*))" "A")
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-constructors make-broadcast-stream"
  (destructuring-bind (expr expected) (list "(let ((a (make-string-output-stream))
                  (b (make-string-output-stream)))
              (let ((s (make-broadcast-stream a b)))
                (write-string \"hi\" s)
                (list (get-output-stream-string a)
                      (get-output-stream-string b))))" '("hi" "hi"))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-constructors make-two-way-stream"
  (destructuring-bind (expr expected) (list "(let* ((in (make-string-input-stream \"abc\"))
                   (out (make-string-output-stream))
                   (s (make-two-way-stream in out)))
              (write-char #\\Z s)
              (list (read-char s) (get-output-stream-string out)))" '(#\a "Z"))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-constructors make-echo-stream"
  (destructuring-bind (expr expected) (list "(let* ((in (make-string-input-stream \"Q\"))
                   (out (make-string-output-stream))
                   (s (make-echo-stream in out)))
              (list (read-char s) (get-output-stream-string out)))" '(#\Q "Q"))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-constructors make-concatenated-stream"
  (destructuring-bind (expr expected) (list "(let ((s (make-concatenated-stream
                      (make-string-input-stream \"ab\")
                      (make-string-input-stream \"cd\"))))
              (list (read-char s) (read-char s) (read-char s) (read-char s)))" '(#\a #\b #\c #\d))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-accessors broadcast-stream-streams"
  (destructuring-bind (expr expected) (list "(let ((a (make-string-output-stream))
                  (b (make-string-output-stream)))
              (let ((s (make-broadcast-stream a b)))
                (list (eq a (first (broadcast-stream-streams s)))
                      (eq b (second (broadcast-stream-streams s))))))" '(t t))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-accessors two-way-stream-accessors"
  (destructuring-bind (expr expected) (list "(let ((in (make-string-input-stream \"a\"))
                  (out (make-string-output-stream)))
              (let ((s (make-two-way-stream in out)))
                (list (eq in (two-way-stream-input-stream s))
                      (eq out (two-way-stream-output-stream s)))))" '(t t))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-accessors echo-stream-accessors"
  (destructuring-bind (expr expected) (list "(let ((in (make-string-input-stream \"a\"))
                  (out (make-string-output-stream)))
              (let ((s (make-echo-stream in out)))
                (list (eq in (echo-stream-input-stream s))
                      (eq out (echo-stream-output-stream s)))))" '(t t))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "compound-stream-accessors concatenated-stream-streams"
  (destructuring-bind (expr expected) (list "(let ((a (make-string-input-stream \"ab\"))
                  (b (make-string-input-stream \"cd\")))
              (let ((s (make-concatenated-stream a b)))
                (list (eq a (first (concatenated-stream-streams s)))
                      (eq b (second (concatenated-stream-streams s))))))" '(t t))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

;;; ─── write-line ─────────────────────────────────────────────────────────

(it-sequential "write-line-ops write-line-to-string-stream"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s) (write-line \"hello\" s))" (format nil "hello~%"))
    (assert-run-string= expected expr)))

(it-sequential "write-line-ops write-line-returns-string"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream)))
              (write-line \"test\" s))" "test")
    (assert-run-string= expected expr)))

;;; ─── read-char / read-line with optional stream ─────────────────────────

(it-sequential "read-char-optional-stream read-char-from-string-stream"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-input-stream \"hello\"))) (read-char s))" #\h)
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "read-char-optional-stream read-char-sequence"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-input-stream \"ab\"))) (read-char s))" #\a)
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "read-line-optional-stream read-line-from-string-stream"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-input-stream \"hello world\")))
              (read-line s))" "hello world")
    (assert-run-string= expected expr)))

;;; ─── write-char / write-string to stream ────────────────────────────────

(it-sequential "write-char-to-stream write-char-to-string-stream"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s) (write-char #\\A s))" "A")
    (assert-run-string= expected expr)))

(it-sequential "write-char-to-stream write-char-sequence"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s)
              (write-char #\\H s) (write-char #\\i s))" "Hi")
    (assert-run-string= expected expr)))

(it-sequential "write-string-to-stream write-string-to-output-stream"
  (destructuring-bind (expr expected) (list "(with-output-to-string (s) (write-string \"hello\" s))" "hello")
    (assert-run-string= expected expr)))

(it-sequential "write-string-to-stream write-string-returns-string"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream)))
              (write-string \"test\" s))" "test")
    (assert-run-string= expected expr)))

;;; ─── Stream element type ────────────────────────────────────────────────

(it-sequential "stream-element-type-basic"
  (let ((result (run-string
                 "(let ((s (make-string-output-stream)))
                    (stream-element-type s))")))
    (expect (not (null result)) :to-be-truthy)))

;;; ─── force-output / finish-output ───────────────────────────────────────

(it-sequential "stream-output-control force-output"
  (destructuring-bind (flush-fn) (list "force-output")
    (with-reset-repl-state
    (let ((result (ignore-errors
                    (run-string-repl
                     (format nil "(let ((s (make-string-output-stream)))
                        (write-string \"test\" s)
                        (~A s)
                        (get-output-stream-string s))" flush-fn)))))
      (expect (stringp result) :to-be-truthy)
      (expect result :to-equal "test")))))

(it-sequential "stream-output-control finish-output"
  (destructuring-bind (flush-fn) (list "finish-output")
    (with-reset-repl-state
    (let ((result (ignore-errors
                    (run-string-repl
                     (format nil "(let ((s (make-string-output-stream)))
                        (write-string \"test\" s)
                        (~A s)
                        (get-output-stream-string s))" flush-fn)))))
      (expect (stringp result) :to-be-truthy)
      (expect result :to-equal "test")))))

;;; ─── with-open-file (file I/O) ──────────────────────────────────────────

(it-sequential "with-open-file-write"
  (let* ((tmpfile (format nil "/tmp/cl-cc-stream-test-~A.txt" (get-universal-time)))
         (write-expr (format nil
                             "(progn
                                (with-open-file (s ~S :direction :output)
                                  (write-string \"hello world\" s))
                                t)"
                             tmpfile))
         (write-result (ignore-errors (run-string write-expr)))
         (host-result (ignore-errors
                        (with-open-file (in tmpfile :direction :input)
                          (read-line in nil nil)))))
    (ignore-errors (delete-file tmpfile))
    (expect write-result :to-be-truthy)
    (expect (and (stringp host-result) (string= host-result "hello world")) :to-be-truthy)))

(it-sequential "with-open-file-read"
  (let* ((tmpfile (format nil "/tmp/cl-cc-stream-read-test-~A.txt" (get-universal-time)))
         (read-expr (format nil
                            "(with-open-file (s ~S :direction :input)
                               (read-line s))"
                            tmpfile))
         (result nil))
    (unwind-protect
         (progn
           (with-open-file (out tmpfile :direction :output :if-exists :supersede)
             (write-string "hello world" out))
           (setf result (ignore-errors (run-string read-expr)))
           (expect (and (stringp result) (string= result "hello world")) :to-be-truthy))
      (ignore-errors (delete-file tmpfile)))))

;;; ─── Standard stream variables accessible ───────────────────────────────

(it-sequential "standard-stream-vars standard-output-bound"
  (destructuring-bind (expr expected) (list "(streamp *standard-output*)" t)
    (assert-run= expected expr)))

(it-sequential "standard-stream-vars standard-input-bound"
  (destructuring-bind (expr expected) (list "(streamp *standard-input*)" t)
    (assert-run= expected expr)))

(it-sequential "standard-stream-vars error-output-bound"
  (destructuring-bind (expr expected) (list "(streamp *error-output*)" t)
    (assert-run= expected expr)))

(it-sequential "query-io-prompt-bridges y-or-n-p-accepts-y"
  (destructuring-bind (expr expected) (list "(let* ((in (make-string-input-stream (format nil \"Y~%\")))
                   (out (make-string-output-stream))
                   (*query-io* (make-two-way-stream in out)))
              (list (y-or-n-p \"Proceed? \") (get-output-stream-string out)))" '(t "Proceed? "))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "query-io-prompt-bridges y-or-n-p-accepts-yes"
  (destructuring-bind (expr expected) (list "(let* ((in (make-string-input-stream (format nil \"YES~%\")))
                   (out (make-string-output-stream))
                   (*query-io* (make-two-way-stream in out)))
              (list (y-or-n-p \"Proceed? \") (get-output-stream-string out)))" '(t "Proceed? "))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "query-io-prompt-bridges yes-or-no-p-rejects-short-answer"
  (destructuring-bind (expr expected) (list "(let* ((in (make-string-input-stream (format nil \"y~%no~%\")))
                   (out (make-string-output-stream))
                   (*query-io* (make-two-way-stream in out)))
              (list (yes-or-no-p \"Continue? \")
                    (> (length (get-output-stream-string out)) 0)))" '(nil t))
    (with-reset-repl-state
    (expect (run-string-repl expr) :to-equal expected))))

(it-sequential "file-string-length-uses-utf-8-byte-count character-on-stream"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream)))
              (file-string-length s (code-char 233)))" 2)
    (assert-run= expected expr)))

(it-sequential "file-string-length-uses-utf-8-byte-count string-on-stream"
  (destructuring-bind (expr expected) (list "(let ((s (make-string-output-stream)))
              (file-string-length s \"hé\"))" 3)
    (assert-run= expected expr)))

(it-sequential "file-string-length-supports-vm-file-handles"
  (let ((tmpfile (format nil "/tmp/cl-cc-file-string-length-~A.txt" (get-universal-time))))
    (unwind-protect
         (expect (= 3 (run-string
           (format nil "(let ((h (open ~S :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (prog1 (file-string-length h \"hé\")
    (close h)))"
                    tmpfile))) :to-be-truthy)
      (ignore-errors (delete-file tmpfile)))))

(it-sequential "open-external-format-utf-16-roundtrip"
  (let ((tmpfile (format nil "/tmp/cl-cc-utf16-stream-~A.txt" (get-universal-time))))
    (unwind-protect
         (expect (= 128512 (run-string
           (format nil "(let ((out (open ~S :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-16)))
  (write-char (code-char 128512) out)
  (close out)
  (let ((in (open ~S :direction :input :external-format :utf-16)))
    (prog1 (char-code (read-char in))
      (close in))))"
                   tmpfile tmpfile))) :to-be-truthy)
      (ignore-errors (delete-file tmpfile)))))

;;; ─── Peek-char ──────────────────────────────────────────────────────────

(it-sequential "peek-char-basic"
  (assert-run= #\a
    "(let ((s (make-string-input-stream \"abc\")))
       (let ((peeked (peek-char nil s)))
         (let ((actual (read-char s)))
           (if (eql peeked actual) peeked nil))))"))

;;; ─── with-open-stream ──────────────────────────────────────────────────

(it-sequential "with-open-stream-basic"
  (assert-run-string= "hello"
    "(with-open-stream (s (make-string-input-stream \"hello\"))
       (read-line s))"))

;;; ─── Load ───────────────────────────────────────────────────────────────

(it-sequential "load-file-basic"
  (let* ((tmpfile (format nil "/tmp/cl-cc-load-test-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           ;; Write the source file from host CL
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defun load-test-fn-42 (x) (* x 3))" s))
           ;; Use REPL pipeline: load defines the function, then call it
            (with-reset-repl-state
              (run-string-repl (format nil "(load ~S)" tmpfile))
              (let ((result (run-string-repl "(load-test-fn-42 14)")))
                (expect 42 :to-be result))))
       (ignore-errors (delete-file tmpfile))
       (reset-repl-state))))
