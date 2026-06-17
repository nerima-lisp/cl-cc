;;;; tests/unit/cli/main-tests.lisp — CLI main help tests
(in-package :cl-cc/test)

(defsuite cl-cc-cli-serial-suite
  :description "Serial CLI unit tests that temporarily override process-wide quit hooks"
  :parent cl-cc-unit-suite
  :parallel nil)

(defsuite cl-cc-cli-pure-suite
  :description "Parallel CLI unit tests (pure helpers, no function replacement)"
  :parent cl-cc-unit-suite)

(in-suite cl-cc-cli-serial-suite)

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

(deftest-each cli-print-global-help
  "Global help text includes the command usage banner, shared flags, and version."
  :cases (("usage"       "Usage: cl-cc <command>")
          ("debug"       "--debug")
          ("opt-remarks" "--opt-remarks <mode>")
          ("time-passes" "--time-passes")
          ("trace-json"  "--trace-json <file>")
           ("flamegraph"  "--flamegraph <file>")
           ("stats"       "--stats")
           ("trace-emit"  "--trace-emit")
          ("timeout"     "--timeout <seconds>")
          ("no-timeout"  "--no-timeout")
           ("version"     "Version: 0.1.0"))
  (expected-str)
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (assert-true (search expected-str out))))

(deftest-each cli-print-command-help
  "Command help for 'run' includes shared flags and its own description."
  :cases (("usage"       "Usage: cl-cc run")
          ("opt-remarks" "--opt-remarks <mode>")
          ("time-passes" "--time-passes")
          ("trace-json"  "--trace-json <file>")
          ("flamegraph"  "--flamegraph <file>")
           ("stats"       "--stats")
           ("trace-emit"  "--trace-emit")
          ("timeout"     "default: 30 seconds")
          ("no-timeout"  "--no-timeout")
          ("stdlib"      "Prepend standard library"))
  (expected-str)
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (assert-true (search expected-str out))))

(deftest cli-print-compile-help-includes-debug-flag
  "Compile help text documents the --debug frame-pointer opt-out flag."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "compile")))))
    (assert-true (search "--debug" out))))

(deftest cli-print-repl-help-is-language-neutral
  "REPL help no longer claims ANSI Common Lisp only and documents --lang."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "repl")))))
    (assert-false (search "ANSI Common Lisp" out))
    (assert-true (search "--lang lisp|elisp|php|js|javascript" out))))

(deftest cli-print-eval-help-documents-elisp-language
  "Eval help documents the expanded language selector."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "eval")))))
    (assert-true (search "--lang lisp|elisp|php|js|javascript" out))))

(deftest cli-print-unknown-command-falls-back
  "Unknown command help prints an error and falls back to global help."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (cl-cc/cli::%print-command-help "not-a-command"))
    (let ((stdout (get-output-stream-string out))
          (stderr (get-output-stream-string err)))
      (assert-true (search "Usage: cl-cc <command>" stdout))
      (assert-true (search "Unknown command: not-a-command" stderr)))))

(deftest cli-main-shows-global-help-with-no-command
  "main prints global help and exits 0 when no command is supplied."
  (%with-cli-function-overrides
      ((uiop:command-line-arguments (lambda () '())))
    (let ((out (with-output-to-string (s)
                 (let ((*standard-output* s)
                       (*error-output* s))
                   (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))))))
      (assert-true (search "Usage: cl-cc <command>" out)))))

(deftest cli-main-top-level-help-uses-command-help
  "main dispatches top-level --help through %print-help and exits 0."
  (let ((printed-command :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("run" "--help")))
         (cl-cc/cli::%print-help (lambda (&optional command) (setf printed-command command))))
      (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))
      (assert-string= "run" printed-command))))

(deftest cli-main-help-subcommand-prints-positional-help
  "main handles the explicit help subcommand and forwards its positional command."
  (let ((printed-command :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("help" "compile")))
         (cl-cc/cli::%print-help (lambda (&optional command) (setf printed-command command))))
      (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))
      (assert-string= "compile" printed-command))))

(deftest cli-main-unknown-command-prints-error-and-exits-2
  "main reports unknown commands, prints global help, and exits 2."
  (let ((printed-help nil))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("bogus")))
         (cl-cc/cli::%print-global-help (lambda () (setf printed-help t))))
      (let ((err (with-output-to-string (s)
                   (let ((*standard-output* s)
                         (*error-output* s))
                     (assert-equal 2 (%with-captured-quit (cl-cc/cli:main)))))))
        (assert-true printed-help)
        (assert-true (search "Unknown command: bogus" err))))))

(deftest cli-main-parse-error-prints-message-and-exits-2
  "main catches arg-parse-error, prints the message and help, and exits 2."
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
                     (assert-equal 2 (%with-captured-quit (cl-cc/cli:main)))))))
        (assert-true printed-help)
        (assert-true (search "boom" err))))))

(deftest cli-do-repl-forwards-elisp-language
  "repl passes the selected language through to run-string-repl and prints it."
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
                     (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))))))
        (assert-equal :elisp captured-language)
        (assert-string= "(+ 1 2)" captured-source)
        (assert-true (search "Emacs Lisp" out))
        (assert-true (search "=> 42" out))))))

(deftest cli-do-eval-forwards-elisp-language
  "eval forwards --lang elisp to compile-string and run-compiled."
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
        (assert-equal 99 result)
        (assert-string= "(+ 1 2)" captured-source)
        (assert-equal :elisp captured-language)
        (assert-equal :dummy run-called)))))

(deftest cli-do-compile-dump-ir-forwards-elisp-language
  "compile passes --lang elisp through the dump-ir path."
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
      (assert-string= "(+ 1 2)" captured-source)
      (assert-equal :elisp captured-language)))))

(deftest cli-wasm-helper-forwards-elisp-language
  "wasm helper preserves elisp instead of downcasting it."
  (let ((captured-language :unset)
        (captured-source :unset))
    (%with-cli-function-overrides
        ((cl-cc:compile-string (lambda (source &rest args)
                                 (setf captured-source source
                                       captured-language (getf args :language))
                                 (cl-cc:make-compilation-result :program :dummy))))
      (let ((result (cl-cc/cli::%wasm-compile-source-to-vm-result "(+ 1 2)" "input.el" :elisp nil)))
        (assert-equal :dummy (cl-cc:compilation-result-program result))
        (assert-string= "(+ 1 2)" captured-source)
        (assert-equal :elisp captured-language)))))
