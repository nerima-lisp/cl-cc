;;;; tests/conformance/native-io-conformance-tests.lisp
;;;; ANSI CL Native I/O / Stream / Pathname Conformance Tests
;;;;
;;;; Tests I/O operations that must work across VM and native execution. These
;;;; are tagged :e2e and remain outside the canonical fast plan while native
;;;; binary parity is validated end-to-end.

(in-package :cl-cc/test)



;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun io-run (form-string)
  "Run FORM-STRING through cl-cc pipeline and return result."
  (cl-cc:run-string form-string))

;;; ──────────────────────────────────────────────────────────────────────
;;; String Streams
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-string-output-stream"
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-string \"hello\" s)
                    (get-output-stream-string s))")))
    (expect result :to-equal "hello")))

(it-sequential "io-with-output-to-string"
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (princ 42 s)
                    (write-string \"abc\" s))")))
    (expect result :to-equal "42abc")))

(it-sequential "io-with-input-from-string"
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (read-char s))")))
    (expect result :to-be #\h)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Character I/O
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-read-char-write-char"
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-char #\\X out)
                    (write-char #\\Y out)
                    (get-output-stream-string out))")))
    (expect result :to-equal "XY")))

(it-sequential "io-peek-char"
  (let ((result (io-run
                 "(with-input-from-string (s \"ABC\")
                    (list (peek-char nil s) (read-char s) (read-char s)))")))
    (expect result :to-equal '(#\A #\A #\B))))

(it-sequential "io-unread-char"
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (let ((c (read-char s)))
                      (unread-char c s)
                      (list (read-char s) (read-char s))))")))
    (expect result :to-equal '(#\h #\e))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Line I/O
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-read-line"
  (let ((result (io-run
                 "(with-input-from-string (s (format nil \"hello~%world\"))
                    (list (read-line s) (read-line s)))")))
    (expect result :to-equal '("hello" "world"))))

(it-sequential "io-write-line"
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-line \"hello\" out)
                    (get-output-stream-string out))")))
    (expect result :to-equal (format nil "hello~%"))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Print Functions
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-print-prin1-princ"
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (prin1 \"hello\" s)
                    (princ #\\space s)
                    (princ 42 s))")))
    (expect result :to-equal "\"hello\" 42")))

(it-sequential "io-write-to-string"
  (let ((result (io-run "(write-to-string 42 :base 16)")))
    (expect result :to-equal "2A")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Stream Predicates
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-stream-predicates"
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (list (streamp s)
                          (output-stream-p s)
                          (input-stream-p s)
                          (open-stream-p s)))")))
    (expect result :to-equal '(t t nil t))))

;;; ──────────────────────────────────────────────────────────────────────
;;; File I/O
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-open-close"
  (let ((result (io-run
                 "(let ((s (open \"/tmp/cl-cc-conformance-test.txt\"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)))
                    (write-line \"test\" s)
                    (close s)
                    :ok)")))
    (expect result :to-equal :ok)))

(it-sequential "io-with-open-file"
  (let ((result (io-run
                 "(with-open-file (s \"/tmp/cl-cc-conformance-test2.txt\"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                    (write-line \"hello\" s))
                  (with-open-file (s \"/tmp/cl-cc-conformance-test2.txt\"
                                     :direction :input)
                    (read-line s))")))
    (expect result :to-equal "hello")))

(it-sequential "io-file-position-length"
  (let ((result (io-run
                 "(with-open-file (s \"/tmp/cl-cc-conformance-test3.txt\"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                    (write-string \"abcdef\" s)
                    (file-position s 3)
                    (write-string \"XYZ\" s)
                    (file-length s))")))
    (expect (= 6 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Stream Control
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-force-finish-output"
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-char #\\X s)
                    (finish-output s)
                    (force-output s)
                    :ok)")))
    (expect result :to-equal :ok)))

(it-sequential "io-listen"
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (list (listen s) (read-char s) (listen s)))")))
    (expect result :to-equal '(t #\h t))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Pathname Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-make-pathname"
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"txt\")))
                    (list (pathnamep p) (pathname-name p) (pathname-type p)))")))
    (expect result :to-equal '(t "test" "txt"))))

(it-sequential "io-namestring"
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"lisp\")))
                    (namestring p))")))
    (expect (search "test.lisp" result :test #'char-equal) :to-be-truthy)))

(it-sequential "io-merge-pathnames"
  (let ((result (io-run
                 "(let* ((d (make-pathname :name \"base\" :type \"lisp\"))
                         (m (merge-pathnames (make-pathname :type \"txt\") d)))
                    (pathname-type m))")))
    (expect result :to-equal "txt")))

(it-sequential "io-probe-file"
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-probe-test.txt\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"data\" s))
                    (if (probe-file \"/tmp/cl-cc-probe-test.txt\")
                        :exists
                        :not-found))")))
    (expect result :to-equal :exists)))

(it-sequential "io-delete-file"
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-delete-test.txt\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"x\" s))
                    (delete-file \"/tmp/cl-cc-delete-test.txt\")
                    (if (probe-file \"/tmp/cl-cc-delete-test.txt\")
                        :still-there
                        :deleted))")))
    (expect result :to-equal :deleted)))

;;; ──────────────────────────────────────────────────────────────────────
;;; File System Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-directory-listing"
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-dir-test-a.txt\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"a\" s))
                    (length (directory \"/tmp/cl-cc-dir-test-*.txt\")))")))
    (expect (>= result 1) :to-be-truthy)))

(it-sequential "io-ensure-directories-exist"
  (let ((result (io-run
                 "(let ((dir \"/tmp/cl-cc-ensure-dir-test/\"))
                    (ensure-directories-exist dir)
                    (probe-file dir))")))
    (expect result :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; LOAD (loading files at runtime)
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-load-file"
  (let ((result (io-run
                 "(progn
                    (with-open-file (s \"/tmp/cl-cc-load-test.lisp\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"(defparameter *load-test-var* 42)\" s))
                    (load \"/tmp/cl-cc-load-test.lisp\")
                    *load-test-var*)")))
    (expect (= 42 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Compound Streams
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-broadcast-stream"
  (let ((result (io-run
                 "(let* ((a (make-string-output-stream))
                         (b (make-string-output-stream))
                         (bc (make-broadcast-stream a b)))
                    (write-string \"hello\" bc)
                    (list (get-output-stream-string a)
                          (get-output-stream-string b)))")))
    (expect result :to-equal '("hello" "hello"))))

(it-sequential "io-concatenated-stream"
  (let ((result (io-run
                 "(let* ((a (make-string-input-stream \"ABC\"))
                         (b (make-string-input-stream \"DEF\"))
                         (cc (make-concatenated-stream a b)))
                    (list (read-char cc) (read-char cc) (read-char cc)
                          (read-char cc) (read-char cc) (read-char cc)))")))
    (expect result :to-equal '(#\A #\B #\C #\D #\E #\F))))

(it-sequential "io-echo-stream"
  (let ((result (io-run
                 "(let* ((in (make-string-input-stream \"hello\"))
                         (out (make-string-output-stream))
                         (ec (make-echo-stream in out)))
                    (list (read-char ec) (read-char ec)
                          (get-output-stream-string out)))")))
    ;; Reading 'h' then 'e' echoes both to OUT, so OUT holds "he" (verified
    ;; against host SBCL: (#\h #\e "he")). The previous "hh" expectation was wrong.
    (expect result :to-equal '(#\h #\e "he"))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Sequence I/O
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-read-sequence"
  (let ((result (io-run
                 "(with-input-from-string (s \"ABCDEF\")
                    (let ((v (make-string 3)))
                      (read-sequence v s)
                      v))")))
    (expect result :to-equal "ABC")))

(it-sequential "io-write-sequence"
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-sequence \"hello\" out)
                    (get-output-stream-string out))")))
    (expect result :to-equal "hello")))
