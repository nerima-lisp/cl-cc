;;;; tests/unit/cli/main-tests.lisp — CLI main help tests
(in-package :cl-cc/test)




(defmacro %with-cli-function-overrides (bindings &body body)
  "Temporarily override global function bindings used by CLI unit tests."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (list ,@(mapcar (lambda (binding)
                                     `(cons ',(first binding)
                                            (symbol-function ',(first binding))))
                                   bindings))))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (binding)
                          `(setf (symbol-function ',(first binding)) ,(second binding)))
                        bindings)
              ,@body)
         (dolist (entry ,saved)
           (setf (symbol-function (car entry)) (cdr entry)))))))

(defmacro %with-captured-quit (&body body)
  "Run BODY with uiop:quit modeled as a non-local exit carrying the exit code."
  `(%with-cli-function-overrides
       ((uiop:quit (lambda (&optional code)
                     (throw 'cl-cc-cli-quit code))))
     (catch 'cl-cc-cli-quit
       ,@body)))

(it-sequential "cli-print-global-help usage"
  (destructuring-bind (expected-str) (list "Usage: cl-cc <command>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help debug"
  (destructuring-bind (expected-str) (list "--debug")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help opt-remarks"
  (destructuring-bind (expected-str) (list "--opt-remarks <mode>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help time-passes"
  (destructuring-bind (expected-str) (list "--time-passes")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help trace-json"
  (destructuring-bind (expected-str) (list "--trace-json <file>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help flamegraph"
  (destructuring-bind (expected-str) (list "--flamegraph <file>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help stats"
  (destructuring-bind (expected-str) (list "--stats")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help trace-emit"
  (destructuring-bind (expected-str) (list "--trace-emit")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help timeout"
  (destructuring-bind (expected-str) (list "--timeout <seconds>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help no-timeout"
  (destructuring-bind (expected-str) (list "--no-timeout")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-global-help version"
  (destructuring-bind (expected-str) (list "Version: 0.1.0")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help usage"
  (destructuring-bind (expected-str) (list "Usage: cl-cc run")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help opt-remarks"
  (destructuring-bind (expected-str) (list "--opt-remarks <mode>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help time-passes"
  (destructuring-bind (expected-str) (list "--time-passes")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help trace-json"
  (destructuring-bind (expected-str) (list "--trace-json <file>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help flamegraph"
  (destructuring-bind (expected-str) (list "--flamegraph <file>")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help stats"
  (destructuring-bind (expected-str) (list "--stats")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help trace-emit"
  (destructuring-bind (expected-str) (list "--trace-emit")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help timeout"
  (destructuring-bind (expected-str) (list "default: 30 seconds")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help no-timeout"
  (destructuring-bind (expected-str) (list "--no-timeout")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-command-help stdlib"
  (destructuring-bind (expected-str) (list "Prepend standard library")
    (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (expect (search expected-str out) :to-be-truthy))))

(it-sequential "cli-print-compile-help-includes-debug-flag"
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "compile")))))
    (expect (search "--debug" out) :to-be-truthy)))

(it-sequential "cli-print-repl-help-is-language-neutral"
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "repl")))))
    (expect (search "ANSI Common Lisp" out) :to-be-falsy)
    (expect (search "--lang lisp|elisp|php|js|javascript" out) :to-be-truthy)))

(it-sequential "cli-print-eval-help-documents-elisp-language"
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "eval")))))
    (expect (search "--lang lisp|elisp|php|js|javascript" out) :to-be-truthy)))

(it-sequential "cli-print-unknown-command-falls-back"
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (cl-cc/cli::%print-command-help "not-a-command"))
    (let ((stdout (get-output-stream-string out))
          (stderr (get-output-stream-string err)))
      (expect (search "Usage: cl-cc <command>" stdout) :to-be-truthy)
      (expect (search "Unknown command: not-a-command" stderr) :to-be-truthy))))

(it-sequential "cli-main-shows-global-help-with-no-command"
  (%with-cli-function-overrides
      ((uiop:command-line-arguments (lambda () '())))
    (let ((out (with-output-to-string (s)
                 (let ((*standard-output* s)
                       (*error-output* s))
                   (expect (%with-captured-quit (cl-cc/cli:main)) :to-equal 0)))))
      (expect (search "Usage: cl-cc <command>" out) :to-be-truthy))))

(it-sequential "cli-main-top-level-help-uses-command-help"
  (let ((printed-command :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("run" "--help")))
         (cl-cc/cli::%print-help (lambda (&optional command) (setf printed-command command))))
      (expect (%with-captured-quit (cl-cc/cli:main)) :to-equal 0)
      (expect printed-command :to-equal "run"))))

(it-sequential "cli-main-help-subcommand-prints-positional-help"
  (let ((printed-command :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("help" "compile")))
         (cl-cc/cli::%print-help (lambda (&optional command) (setf printed-command command))))
      (expect (%with-captured-quit (cl-cc/cli:main)) :to-equal 0)
      (expect printed-command :to-equal "compile"))))

(it-sequential "cli-main-unknown-command-prints-error-and-exits-2"
  (let ((printed-help nil))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("bogus")))
         (cl-cc/cli::%print-global-help (lambda () (setf printed-help t))))
      (let ((err (with-output-to-string (s)
                   (let ((*standard-output* s)
                         (*error-output* s))
                     (expect (%with-captured-quit (cl-cc/cli:main)) :to-equal 2)))))
        (expect printed-help :to-be-truthy)
        (expect (search "Unknown command: bogus" err) :to-be-truthy)))))

(it-sequential "cli-main-parse-error-prints-message-and-exits-2"
  (let ((printed-help nil))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("--bad")))
         (cl-cc/cli:parse-args (lambda (argv)
                                 (declare (ignore argv))
                                 (error 'cl-cc/cli:arg-parse-error :message "boom")))
         (cl-cc/cli::%print-global-help (lambda () (setf printed-help t))))
      (let ((err (with-output-to-string (s)
                   (let ((*standard-output* s)
                         (*error-output* s))
                     (expect (%with-captured-quit (cl-cc/cli:main)) :to-equal 2)))))
        (expect printed-help :to-be-truthy)
        (expect (search "boom" err) :to-be-truthy)))))

(it-sequential "cli-do-repl-forwards-elisp-language"
  (let ((captured-language :unset)
        (captured-source :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("repl" "--lang" "elisp")))
         (cl-cc:reset-repl-state (lambda () nil))
         (cl-cc:%ensure-repl-state (lambda () nil))
         (cl-cc/cli::%initialize-repl-completeness-globals (lambda () nil))
         (cl-cc/cli::%load-repl-history-file (lambda () nil))
         (cl-cc/cli::%save-repl-history-file (lambda () nil))
         (cl-cc/cli::%update-repl-completeness-globals (lambda (&rest args) (declare (ignore args)) nil))
         (cl-cc:%repl-record-history (lambda (&rest args) (declare (ignore args)) nil))
         (cl-cc:repl-edit-input-line (lambda (line) (values line nil nil)))
         (cl-cc/vm:vm-values-list (lambda (&rest args) (declare (ignore args)) nil))
         (cl-cc:run-string-repl (lambda (source &key language)
                                  (setf captured-source source
                                        captured-language language)
                                  42)))
      (let ((out (with-output-to-string (s)
                    (let ((*standard-input* (make-string-input-stream (format nil "(+ 1 2)~%")))
                         (*standard-output* s)
                         (*error-output* s))
                     (expect (%with-captured-quit (cl-cc/cli:main)) :to-equal 0)))))
        (expect captured-language :to-equal :elisp)
        (expect captured-source :to-equal "(+ 1 2)")
        (expect (search "Emacs Lisp" out) :to-be-truthy)
        (expect (search "=> 42" out) :to-be-truthy)))))

(it-sequential "cli-do-eval-forwards-elisp-language"
  (let ((captured-language :unset)
        (captured-source :unset)
        (run-called nil))
    (%with-cli-function-overrides
        ((cl-cc/cli::%call-with-runtime-sanitizer-flags (lambda (opts thunk &rest args)
                                                          (declare (ignore opts args))
                                                          (funcall thunk)))
         (cl-cc:compile-string (lambda (source &rest args)
                                 (setf captured-source source
                                       captured-language (getf args :language))
                                 (cl-cc/compile:make-compilation-result :program :dummy)))
         (cl-cc:run-compiled (lambda (program &rest args)
                               (declare (ignore args))
                               (setf run-called program)
                               99)))
      (let ((result (cl-cc/cli::%compile-and-run-eval-form "(+ 1 2)" nil nil :elisp nil nil nil)))
        (expect result :to-equal 99)
        (expect captured-source :to-equal "(+ 1 2)")
        (expect captured-language :to-equal :elisp)
        (expect run-called :to-equal :dummy)))))

(it-sequential "cli-do-compile-dump-ir-forwards-elisp-language"
  (let ((captured-language :unset)
        (captured-source :unset))
    (labels ((%run-dump-ir-compile-path (file language)
               (let* ((source (cl-cc/cli::%read-command-source file))
                      (result (cl-cc:compile-string source
                                                    :target :vm
                                                    :language language
                                                    :source-file file))
                      (stream (make-string-output-stream)))
                 (cl-cc/cli::%dump-ir-phase :vm result stream t)
                 (get-output-stream-string stream))))
      (%with-cli-function-overrides
          ((cl-cc/cli::%read-command-source (lambda (file)
                                              (setf captured-source file)
                                              "(+ 1 2)"))
         (cl-cc/cli::%dump-ir-phase (lambda (&rest args)
                                      (declare (ignore args))
                                      nil))
         (cl-cc:compile-string (lambda (source &rest args)
                                 (setf captured-source source
                                       captured-language (getf args :language))
                                 (cl-cc:make-compilation-result :program :dummy))))
        (%run-dump-ir-compile-path "input.el" :elisp)
      (expect captured-source :to-equal "(+ 1 2)")
      (expect captured-language :to-equal :elisp)))))

(it-sequential "cli-wasm-helper-forwards-elisp-language"
  (let ((captured-language :unset)
        (captured-source :unset))
    (%with-cli-function-overrides
        ((cl-cc:compile-string (lambda (source &rest args)
                                 (setf captured-source source
                                       captured-language (getf args :language))
                                 (cl-cc:make-compilation-result :program :dummy))))
      (let ((result (cl-cc/cli::%wasm-compile-source-to-vm-result "(+ 1 2)" "input.el" :elisp nil)))
        (expect (cl-cc:compilation-result-program result) :to-equal :dummy)
        (expect captured-source :to-equal "(+ 1 2)")
        (expect captured-language :to-equal :elisp)))))
