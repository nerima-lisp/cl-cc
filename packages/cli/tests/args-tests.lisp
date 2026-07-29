;;;; tests/unit/cli/args-tests.lisp — CLI argument parser unit tests
;;;;
;;;; Covers: parse-args command detection, boolean flags, string flags
;;;;         (long form, short form, --key=value inline form), error cases,
;;;;         and combined multi-flag invocations.

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun %flags (parsed key)
  "Return value of KEY in the parsed-args flag hash-table."
  (gethash key (cl-cc/cli:parsed-args-flags parsed)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Command detection
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-empty-argv-yields-nil-command-and-positional"
  (let ((p (cl-cc/cli:parse-args '())))
    (expect (cl-cc/cli:parsed-args-command p) :to-be-null)
    (expect (cl-cc/cli:parsed-args-positional p) :to-be-null)))

(it-sequential "cli-args-command-only-has-no-positionals"
  (let ((p (cl-cc/cli:parse-args '("run"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "run")
    (expect (cl-cc/cli:parsed-args-positional p) :to-be-null)))

(it-sequential "cli-args-command-with-file-puts-file-in-positionals"
  (let ((p (cl-cc/cli:parse-args '("run" "foo.lisp"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "run")
    (expect (cl-cc/cli:parsed-args-positional p) :to-equal '("foo.lisp"))))

(it-sequential "cli-args-multiple-positionals-after-command-all-collected"
  (let ((p (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "extra"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "eval")
    (expect (cl-cc/cli:parsed-args-positional p) :to-equal '("(+ 1 2)" "extra"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Boolean flags
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-bool-flags stdlib"
  (destructuring-bind (argv flag-key) (list '("run" "f.lisp" "--stdlib") "--stdlib")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags debug"
  (destructuring-bind (argv flag-key) (list '("compile" "f.lisp" "--debug") "--debug")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags verbose"
  (destructuring-bind (argv flag-key) (list '("run" "f.lisp" "--verbose") "--verbose")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags pass timings"
  (destructuring-bind (argv flag-key) (list '("eval" "(+ 1 2)" "--print-pass-timings") "--print-pass-timings")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags time-passes alias"
  (destructuring-bind (argv flag-key) (list '("eval" "(+ 1 2)" "--time-passes") "--time-passes")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags stats"
  (destructuring-bind (argv flag-key) (list '("eval" "(+ 1 2)" "--stats") "--stats")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags optimization report"
  (destructuring-bind (argv flag-key) (list '("eval" "(+ 1 2)" "--optimization-report") "--optimization-report")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags trace-emit"
  (destructuring-bind (argv flag-key) (list '("eval" "(+ 1 2)" "--trace-emit") "--trace-emit")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags verify-transforms"
  (destructuring-bind (argv flag-key) (list '("compile" "f.lisp" "--verify-transforms") "--verify-transforms")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags deterministic"
  (destructuring-bind (argv flag-key) (list '("compile" "f.lisp" "--deterministic") "--deterministic")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags no-timeout"
  (destructuring-bind (argv flag-key) (list '("eval" "(+ 1 2)" "--no-timeout") "--no-timeout")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags strict"
  (destructuring-bind (argv flag-key) (list '("check" "f.lisp" "--strict") "--strict")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flags help"
  (destructuring-bind (argv flag-key) (list '("--help") "--help")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-be-truthy))))

(it-sequential "cli-args-bool-flag-absent"
  (let ((p (cl-cc/cli:parse-args '("run" "f.lisp"))))
    (expect (%flags p "--stdlib") :to-be-null)
    (expect (%flags p "--verbose") :to-be-null)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; String flags — --key value form
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-output-long-and-short long form"
  (destructuring-bind (argv flag-key expected) (list '("compile" "f.lisp" "--output" "out") "--output" "out")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-output-long-and-short short form"
  (destructuring-bind (argv flag-key expected) (list '("compile" "f.lisp" "-o" "mybin") "-o" "mybin")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags arch arm64"
  (destructuring-bind (argv flag-key expected) (list '("compile" "f.lisp" "--arch" "arm64") "--arch" "arm64")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags lang php"
  (destructuring-bind (argv flag-key expected) (list '("run" "f.php"  "--lang" "php") "--lang" "php")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags lang lisp"
  (destructuring-bind (argv flag-key expected) (list '("run" "f.lisp" "--lang" "lisp") "--lang" "lisp")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags pass pipeline"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--pass-pipeline" "fold,dce") "--pass-pipeline" "fold,dce")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags opt remarks"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--opt-remarks" "changed") "--opt-remarks" "changed")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags trace json"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--trace-json" "trace.json") "--trace-json" "trace.json")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags build id"
  (destructuring-bind (argv flag-key expected) (list '("compile" "f.lisp" "--build-id" "auto") "--build-id" "auto")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags tier"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--tier" "1") "--tier" "1")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-string-flags flamegraph"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--flamegraph" "flame.svg") "--flamegraph" "flame.svg")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-timeout-string-flag"
  (let ((p (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--timeout" "7"))))
    (expect (%flags p "--timeout") :to-equal "7")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; String flags — --key=value inline form
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-equals-form output=mybin"
  (destructuring-bind (argv flag-key expected) (list '("compile" "f.lisp" "--output=mybin") "--output" "mybin")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-equals-form arch=arm64"
  (destructuring-bind (argv flag-key expected) (list '("compile" "f.lisp" "--arch=arm64") "--arch" "arm64")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-equals-form lang=php"
  (destructuring-bind (argv flag-key expected) (list '("run"     "f.php"  "--lang=php") "--lang" "php")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-equals-form pipeline=fold,dce"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--pass-pipeline=fold,dce") "--pass-pipeline" "fold,dce")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-equals-form opt-remarks=missed"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--opt-remarks=missed") "--opt-remarks" "missed")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-equals-form trace-json=trace.json"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--trace-json=trace.json") "--trace-json" "trace.json")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

(it-sequential "cli-args-equals-form flamegraph=flame.svg"
  (destructuring-bind (argv flag-key expected) (list '("eval" "(+ 1 2)" "--flamegraph=flame.svg") "--flamegraph" "flame.svg")
    (let ((p (cl-cc/cli:parse-args argv)))
    (expect (%flags p flag-key) :to-equal expected))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; flag / flag-or accessor helpers
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-flag-helper"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--arch" "x86-64"))))
    (expect (cl-cc/cli:flag p "--arch") :to-equal "x86-64")
    (expect (cl-cc/cli:flag p "--output") :to-be-null)))

(it-sequential "cli-flag-or-long-form-wins-when-present"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output" "long-out"))))
    (expect (cl-cc/cli:flag-or p "--output" "-o") :to-equal "long-out")))

(it-sequential "cli-flag-or-falls-back-to-short-form-when-long-absent"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "-o" "short-out"))))
    (expect (cl-cc/cli:flag-or p "--output" "-o") :to-equal "short-out")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Edge cases — special tokens
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-single-dash-signals-unexpected"
  (signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args '("-"))))

(it-sequential "cli-args-empty-inline-value"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output="))))
    (expect (%flags p "--output") :to-equal "")))

(it-sequential "cli-args-duplicate-flag-last-wins"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--arch" "arm64"
                                    "--arch" "x86-64"))))
    (expect (%flags p "--arch") :to-equal "x86-64")))

(it-sequential "cli-args-value-resembles-flag"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output" "--verbose"))))
    (expect (%flags p "--output") :to-equal "--verbose")
    (expect (%flags p "--verbose") :to-be-null)))

(it-sequential "cli-args-flags-interleaved-with-positionals"
  (let ((p (cl-cc/cli:parse-args '("compile" "foo.lisp" "--arch" "arm64"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "compile")
    (expect (cl-cc/cli:parsed-args-positional p) :to-equal '("foo.lisp"))
    (expect (%flags p "--arch") :to-equal "arm64")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Error cases
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-error-cases unknown-flag"
  (destructuring-bind (argv) (list '("run" "--totally-unknown"))
    (signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args argv))))

(it-sequential "cli-args-error-cases bool-with-value"
  (destructuring-bind (argv) (list '("run" "--stdlib=true"))
    (signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args argv))))

(it-sequential "cli-args-error-cases triple-dash"
  (destructuring-bind (argv) (list '("run" "---foo"))
    (signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args argv))))

(it-sequential "cli-args-error-cases missing-output-long"
  (destructuring-bind (argv) (list '("compile" "f.lisp" "--output"))
    (signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args argv))))

(it-sequential "cli-args-error-cases missing-output-short"
  (destructuring-bind (argv) (list '("compile" "f.lisp" "-o"))
    (signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args argv))))

(it-sequential "cli-args-double-dash-separator"
  (let ((p (cl-cc/cli:parse-args '("--"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-be-null)
    (expect (cl-cc/cli:parsed-args-positional p) :to-be-null)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Combined invocations
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cli-args-full-compile-invocation"
  (let ((p (cl-cc/cli:parse-args
            '("compile" "foo.lisp" "--arch" "arm64" "--output=mybin" "--verbose"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "compile")
    (expect (cl-cc/cli:parsed-args-positional p) :to-equal '("foo.lisp"))
    (expect (%flags p "--arch") :to-equal "arm64")
    (expect (%flags p "--output") :to-equal "mybin")
    (expect (%flags p "--verbose") :to-be-truthy)))

(it-sequential "cli-args-run-with-php-flags"
  (let ((p (cl-cc/cli:parse-args
            '("run" "script.php" "--lang=php" "--stdlib" "--verbose"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "run")
    (expect (cl-cc/cli:parsed-args-positional p) :to-equal '("script.php"))
    (expect (%flags p "--lang") :to-equal "php")
    (expect (%flags p "--stdlib") :to-be-truthy)
    (expect (%flags p "--verbose") :to-be-truthy)))

(it-sequential "cli-args-flags-before-command-are-parsed"
  (let ((p (cl-cc/cli:parse-args '("--verbose" "run" "foo.lisp"))))
    (expect (cl-cc/cli:parsed-args-command p) :to-equal "run")
    (expect (cl-cc/cli:parsed-args-positional p) :to-equal '("foo.lisp"))
    (expect (%flags p "--verbose") :to-be-truthy)))
