;;;; tests/conformance/native-io-conformance-tests.lisp
;;;; ANSI CL Native I/O / Stream / Pathname Conformance Tests
;;;;
;;;; Tests I/O operations that must work across VM and native execution. These
;;;; are tagged :e2e and remain outside the canonical fast plan while native
;;;; binary parity is validated end-to-end.

(in-package :cl-cc/test)

(defsuite ansi-conformance-native-io-suite
  :description "ANSI CL Native I/O / Stream / Pathname Conformance Tests"
  :parent cl-cc-conformance-suite
  :parallel nil)

(in-suite ansi-conformance-native-io-suite)

;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun io-run (form-string)
  "Run FORM-STRING through cl-cc pipeline and return result."
  (cl-cc:run-string form-string))

(defun io-scratch-directory ()
  "Return a scratch directory private to this test process, creating it if
needed.

The file tests below deliberately touch the real filesystem: exercising OPEN,
LOAD, DIRECTORY and friends against real files is the whole point of a native
I/O conformance suite, and TEST_STANDARD.md allows real FS access for exactly
this case. What it does not allow is a fixed shared path. These tests used to
write to hard-coded /tmp/cl-cc-*.txt names, so a developer running the suite
directly and the `nix build` user running it afterwards collided on files
owned by the other account and every OPEN failed with EACCES. Deriving the
directory from TMPDIR and the process id makes each run self-contained, which
also stops the suite from leaving litter in /tmp."
  (let ((directory (merge-pathnames
                    (format nil "cl-cc-io-conformance-~D/" (sb-unix:unix-getpid))
                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defun io-scratch-path (name)
  "Namestring for NAME inside IO-SCRATCH-DIRECTORY.

Returned as a string because callers splice it into the cl-cc source text they
hand to IO-RUN, where a #P literal would have to survive another round of
reader escaping."
  (namestring (merge-pathnames name (io-scratch-directory))))

;;; ──────────────────────────────────────────────────────────────────────
;;; String Streams
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-string-output-stream
  "make-string-output-stream and get-output-stream-string should work."
  :timeout 30
  :tags '(:io :string-stream :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-string \"hello\" s)
                    (get-output-stream-string s))")))
    (assert-equal "hello" result)))

(deftest io-with-output-to-string
  "with-output-to-string should capture output."
  :timeout 30
  :tags '(:io :with-output-to-string :native :e2e)
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (princ 42 s)
                    (write-string \"abc\" s))")))
    (assert-equal "42abc" result)))

(deftest io-with-input-from-string
  "with-input-from-string should provide string as input."
  :timeout 30
  :tags '(:io :with-input-from-string :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (read-char s))")))
    (assert-eql #\h result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Character I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-read-char-write-char
  "read-char and write-char should work with string streams."
  :timeout 30
  :tags '(:io :read-char :write-char :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-char #\\X out)
                    (write-char #\\Y out)
                    (get-output-stream-string out))")))
    (assert-equal "XY" result)))

(deftest io-peek-char
  "peek-char should look ahead without consuming."
  :timeout 30
  :tags '(:io :peek-char :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"ABC\")
                    (list (peek-char nil s) (read-char s) (read-char s)))")))
    (assert-equal '(#\A #\A #\B) result)))

(deftest io-unread-char
  "unread-char should push character back."
  :timeout 30
  :tags '(:io :unread-char :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (let ((c (read-char s)))
                      (unread-char c s)
                      (list (read-char s) (read-char s))))")))
    (assert-equal '(#\h #\e) result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Line I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-read-line
  "read-line should read until newline."
  :timeout 30
  :tags '(:io :read-line :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s (format nil \"hello~%world\"))
                    (list (read-line s) (read-line s)))")))
    (assert-equal '("hello" "world") result)))

(deftest io-write-line
  "write-line should append newline."
  :timeout 30
  :tags '(:io :write-line :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-line \"hello\" out)
                    (get-output-stream-string out))")))
    (assert-equal (format nil "hello~%") result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Print Functions
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-print-prin1-princ
  "print/princ/prin1 should work via native code."
  :timeout 30
  :tags '(:io :print :princ :prin1 :native :e2e)
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (prin1 \"hello\" s)
                    (princ #\\space s)
                    (princ 42 s))")))
    (assert-equal "\"hello\" 42" result)))

(deftest io-write-to-string
  "write-to-string should return string representation."
  :timeout 30
  :tags '(:io :write-to-string :native :e2e)
  (let ((result (io-run "(write-to-string 42 :base 16)")))
    (assert-equal "2A" result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Stream Predicates
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-stream-predicates
  "streamp/input-stream-p/output-stream-p should work."
  :timeout 30
  :tags '(:io :streamp :stream-predicates :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (list (streamp s)
                          (output-stream-p s)
                          (input-stream-p s)
                          (open-stream-p s)))")))
    (assert-equal '(t t nil t) result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; File I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-open-close
  "open and close should work with file streams."
  :timeout 30
  :tags '(:io :open :close :file :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(let ((s (open \"~A\"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)))
                    (write-line \"test\" s)
                    (close s)
                    :ok)"
                         (io-scratch-path "open-close.txt")))))
    (assert-equal :ok result)))

(deftest io-with-open-file
  "with-open-file should handle file open/close automatically."
  :timeout 30
  :tags '(:io :with-open-file :native :e2e)
  (let* ((path (io-scratch-path "with-open-file.txt"))
         (result (io-run
                  (format nil
                          "(with-open-file (s \"~A\"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                    (write-line \"hello\" s))
                  (with-open-file (s \"~:*~A\"
                                     :direction :input)
                    (read-line s))"
                          path))))
    (assert-equal "hello" result)))

(deftest io-file-position-length
  "file-position and file-length should work."
  :timeout 30
  :tags '(:io :file-position :file-length :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(with-open-file (s \"~A\"
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
                    (write-string \"abcdef\" s)
                    (file-position s 3)
                    (write-string \"XYZ\" s)
                    (file-length s))"
                         (io-scratch-path "file-position.txt")))))
    (assert-= 6 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Stream Control
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-force-finish-output
  "force-output and finish-output should not error."
  :timeout 30
  :tags '(:io :force-output :finish-output :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-char #\\X s)
                    (finish-output s)
                    (force-output s)
                    :ok)")))
    (assert-equal :ok result)))

(deftest io-listen
  "listen should detect available input."
  :timeout 30
  :tags '(:io :listen :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (list (listen s) (read-char s) (listen s)))")))
    (assert-equal '(t #\h t) result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Pathname Operations
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-make-pathname
  "make-pathname should construct pathnames."
  :timeout 30
  :tags '(:io :make-pathname :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"txt\")))
                    (list (pathnamep p) (pathname-name p) (pathname-type p)))")))
    (assert-equal '(t "test" "txt") result)))

(deftest io-namestring
  "namestring should convert pathname to string."
  :timeout 30
  :tags '(:io :namestring :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"lisp\")))
                    (namestring p))")))
    (assert-true (search "test.lisp" result :test #'char-equal))))

(deftest io-merge-pathnames
  "merge-pathnames should fill in defaults."
  :timeout 30
  :tags '(:io :merge-pathnames :pathname :native :e2e)
  (let ((result (io-run
                 "(let* ((d (make-pathname :name \"base\" :type \"lisp\"))
                         (m (merge-pathnames (make-pathname :type \"txt\") d)))
                    (pathname-type m))")))
    (assert-equal "txt" result)))

(deftest io-probe-file
  "probe-file should detect file existence."
  :timeout 30
  :tags '(:io :probe-file :pathname :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(progn
                    (with-open-file (s \"~A\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"data\" s))
                    (if (probe-file \"~:*~A\")
                        :exists
                        :not-found))"
                         (io-scratch-path "probe.txt")))))
    (assert-equal :exists result)))

(deftest io-delete-file
  "delete-file should remove a file."
  :timeout 30
  :tags '(:io :delete-file :pathname :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(progn
                    (with-open-file (s \"~A\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"x\" s))
                    (delete-file \"~:*~A\")
                    (if (probe-file \"~:*~A\")
                        :still-there
                        :deleted))"
                         (io-scratch-path "delete.txt")))))
    (assert-equal :deleted result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; File System Operations
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-directory-listing
  "directory should list files matching pattern."
  :timeout 30
  :tags '(:io :directory :pathname :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(progn
                    (with-open-file (s \"~A\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"a\" s))
                    (length (directory \"~A\")))"
                         (io-scratch-path "dir-test-a.txt")
                         (io-scratch-path "dir-test-*.txt")))))
    (assert-true (>= result 1))))

(deftest io-ensure-directories-exist
  "ensure-directories-exist should create directories."
  :timeout 30
  :tags '(:io :ensure-directories-exist :pathname :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(let ((dir \"~A\"))
                    (ensure-directories-exist dir)
                    (probe-file dir))"
                         (io-scratch-path "ensure-dir-test/")))))
    (assert-true result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; LOAD (loading files at runtime)
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-load-file
  "load should evaluate forms from a file."
  :timeout 30
  :tags '(:io :load :native :e2e)
  ;; Write a file, then load it
  (let ((result (io-run
                 (format nil
                         "(progn
                    (with-open-file (s \"~A\"
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                      (write-string \"(defparameter *load-test-var* 42)\" s))
                    (load \"~:*~A\")
                    *load-test-var*)"
                         (io-scratch-path "load-test.lisp")))))
    (assert-= 42 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Compound Streams
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-broadcast-stream
  "make-broadcast-stream should create broadcast stream."
  :timeout 30
  :tags '(:io :broadcast-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((a (make-string-output-stream))
                         (b (make-string-output-stream))
                         (bc (make-broadcast-stream a b)))
                    (write-string \"hello\" bc)
                    (list (get-output-stream-string a)
                          (get-output-stream-string b)))")))
    (assert-equal '("hello" "hello") result)))

(deftest io-concatenated-stream
  "make-concatenated-stream should concatenate input streams."
  :timeout 30
  :tags '(:io :concatenated-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((a (make-string-input-stream \"ABC\"))
                         (b (make-string-input-stream \"DEF\"))
                         (cc (make-concatenated-stream a b)))
                    (list (read-char cc) (read-char cc) (read-char cc)
                          (read-char cc) (read-char cc) (read-char cc)))")))
    (assert-equal '(#\A #\B #\C #\D #\E #\F) result)))

(deftest io-echo-stream
  "make-echo-stream should echo input to output."
  :timeout 30
  :tags '(:io :echo-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((in (make-string-input-stream \"hello\"))
                         (out (make-string-output-stream))
                         (ec (make-echo-stream in out)))
                    (list (read-char ec) (read-char ec)
                          (get-output-stream-string out)))")))
    ;; Reading 'h' then 'e' echoes both to OUT, so OUT holds "he" (verified
    ;; against host SBCL: (#\h #\e "he")). The previous "hh" expectation was wrong.
    (assert-equal '(#\h #\e "he") result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Sequence I/O
;;; ──────────────────────────────────────────────────────────────────────

(deftest io-read-sequence
  "read-sequence should fill a sequence with stream input."
  :timeout 30
  :tags '(:io :read-sequence :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"ABCDEF\")
                    (let ((v (make-string 3)))
                      (read-sequence v s)
                      v))")))
    (assert-equal "ABC" result)))

(deftest io-write-sequence
  "write-sequence should write sequence elements to stream."
  :timeout 30
  :tags '(:io :write-sequence :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-sequence \"hello\" out)
                    (get-output-stream-string out))")))
    (assert-equal "hello" result)))
