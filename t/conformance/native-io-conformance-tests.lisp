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

(it-sequential "io-string-output-stream"
  :timeout
  30
  :tags
  '(:io :string-stream :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-string \"hello\" s)
                    (get-output-stream-string s))")))
    (expect result :to-equal "hello")))

(it-sequential "io-with-output-to-string"
  :timeout
  30
  :tags
  '(:io :with-output-to-string :native :e2e)
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (princ 42 s)
                    (write-string \"abc\" s))")))
    (expect result :to-equal "42abc")))

(it-sequential "io-with-input-from-string"
  :timeout
  30
  :tags
  '(:io :with-input-from-string :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (read-char s))")))
    (expect result :to-be #\h)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Character I/O
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-read-char-write-char"
  :timeout
  30
  :tags
  '(:io :read-char :write-char :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-char #\\X out)
                    (write-char #\\Y out)
                    (get-output-stream-string out))")))
    (expect result :to-equal "XY")))

(it-sequential "io-peek-char"
  :timeout
  30
  :tags
  '(:io :peek-char :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"ABC\")
                    (list (peek-char nil s) (read-char s) (read-char s)))")))
    (expect result :to-equal '(#\A #\A #\B))))

(it-sequential "io-unread-char"
  :timeout
  30
  :tags
  '(:io :unread-char :native :e2e)
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
  :timeout
  30
  :tags
  '(:io :read-line :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s (format nil \"hello~%world\"))
                    (list (read-line s) (read-line s)))")))
    (expect result :to-equal '("hello" "world"))))

(it-sequential "io-write-line"
  :timeout
  30
  :tags
  '(:io :write-line :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-line \"hello\" out)
                    (get-output-stream-string out))")))
    (expect result :to-equal (format nil "hello~%"))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Print Functions
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-print-prin1-princ"
  :timeout
  30
  :tags
  '(:io :print :princ :prin1 :native :e2e)
  (let ((result (io-run
                 "(with-output-to-string (s)
                    (prin1 \"hello\" s)
                    (princ #\\space s)
                    (princ 42 s))")))
    (expect result :to-equal "\"hello\" 42")))

(it-sequential "io-write-to-string"
  :timeout
  30
  :tags
  '(:io :write-to-string :native :e2e)
  (let ((result (io-run "(write-to-string 42 :base 16)")))
    (expect result :to-equal "2A")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Stream Predicates
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-stream-predicates"
  :timeout
  30
  :tags
  '(:io :streamp :stream-predicates :native :e2e)
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
  :timeout
  30
  :tags
  '(:io :open :close :file :native :e2e)
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
    (expect result :to-equal :ok)))

(it-sequential "io-with-open-file"
  :timeout
  30
  :tags
  '(:io :with-open-file :native :e2e)
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
    (expect result :to-equal "hello")))

(it-sequential "io-file-position-length"
  :timeout
  30
  :tags
  '(:io :file-position :file-length :native :e2e)
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
    (expect (= 6 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Stream Control
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-force-finish-output"
  :timeout
  30
  :tags
  '(:io :force-output :finish-output :native :e2e)
  (let ((result (io-run
                 "(let ((s (make-string-output-stream)))
                    (write-char #\\X s)
                    (finish-output s)
                    (force-output s)
                    :ok)")))
    (expect result :to-equal :ok)))

(it-sequential "io-listen"
  :timeout
  30
  :tags
  '(:io :listen :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"hello\")
                    (list (listen s) (read-char s) (listen s)))")))
    (expect result :to-equal '(t #\h t))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Pathname Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-make-pathname"
  :timeout
  30
  :tags
  '(:io :make-pathname :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"txt\")))
                    (list (pathnamep p) (pathname-name p) (pathname-type p)))")))
    (expect result :to-equal '(t "test" "txt"))))

(it-sequential "io-namestring"
  :timeout
  30
  :tags
  '(:io :namestring :pathname :native :e2e)
  (let ((result (io-run
                 "(let ((p (make-pathname :name \"test\" :type \"lisp\")))
                    (namestring p))")))
    (expect (search "test.lisp" result :test #'char-equal) :to-be-truthy)))

(it-sequential "io-merge-pathnames"
  :timeout
  30
  :tags
  '(:io :merge-pathnames :pathname :native :e2e)
  (let ((result (io-run
                 "(let* ((d (make-pathname :name \"base\" :type \"lisp\"))
                         (m (merge-pathnames (make-pathname :type \"txt\") d)))
                    (pathname-type m))")))
    (expect result :to-equal "txt")))

(it-sequential "io-probe-file"
  :timeout
  30
  :tags
  '(:io :probe-file :pathname :native :e2e)
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
    (expect result :to-equal :exists)))

(it-sequential "io-delete-file"
  :timeout
  30
  :tags
  '(:io :delete-file :pathname :native :e2e)
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
    (expect result :to-equal :deleted)))

;;; ──────────────────────────────────────────────────────────────────────
;;; File System Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-directory-listing"
  :timeout
  30
  :tags
  '(:io :directory :pathname :native :e2e)
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
    (expect (>= result 1) :to-be-truthy)))

(it-sequential "io-ensure-directories-exist"
  :timeout
  30
  :tags
  '(:io :ensure-directories-exist :pathname :native :e2e)
  (let ((result (io-run
                 (format nil
                         "(let ((dir \"~A\"))
                    (ensure-directories-exist dir)
                    (probe-file dir))"
                         (io-scratch-path "ensure-dir-test/")))))
    (expect result :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; LOAD (loading files at runtime)
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-load-file"
  :timeout
  30
  :tags
  '(:io :load :native :e2e)
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
    (expect (= 42 result) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Compound Streams
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "io-broadcast-stream"
  :timeout
  30
  :tags
  '(:io :broadcast-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((a (make-string-output-stream))
                         (b (make-string-output-stream))
                         (bc (make-broadcast-stream a b)))
                    (write-string \"hello\" bc)
                    (list (get-output-stream-string a)
                          (get-output-stream-string b)))")))
    (expect result :to-equal '("hello" "hello"))))

(it-sequential "io-concatenated-stream"
  :timeout
  30
  :tags
  '(:io :concatenated-stream :native :e2e)
  (let ((result (io-run
                 "(let* ((a (make-string-input-stream \"ABC\"))
                         (b (make-string-input-stream \"DEF\"))
                         (cc (make-concatenated-stream a b)))
                    (list (read-char cc) (read-char cc) (read-char cc)
                          (read-char cc) (read-char cc) (read-char cc)))")))
    (expect result :to-equal '(#\A #\B #\C #\D #\E #\F))))

(it-sequential "io-echo-stream"
  :timeout
  30
  :tags
  '(:io :echo-stream :native :e2e)
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
  :timeout
  30
  :tags
  '(:io :read-sequence :native :e2e)
  (let ((result (io-run
                 "(with-input-from-string (s \"ABCDEF\")
                    (let ((v (make-string 3)))
                      (read-sequence v s)
                      v))")))
    (expect result :to-equal "ABC")))

(it-sequential "io-write-sequence"
  :timeout
  30
  :tags
  '(:io :write-sequence :native :e2e)
  (let ((result (io-run
                 "(let ((out (make-string-output-stream)))
                    (write-sequence \"hello\" out)
                    (get-output-stream-string out))")))
    (expect result :to-equal "hello")))
