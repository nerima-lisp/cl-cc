(in-package :cl-cc/test)



(it-sequential "script-mode-cli-system-loads"
  (expect (asdf:find-system :cl-cc-cli nil) :to-be-truthy))

(it-sequential "script-mode-parse-args-accepts-script-flag"
  (let ((parsed (cl-cc/cli:parse-args '("--script" "tool.lisp" "a" "--" "--literal"))))
    (expect (cl-cc/cli:flag parsed "--script") :to-be-truthy)
    (expect (cl-cc/cli:parsed-args-command parsed) :to-equal "tool.lisp")
    (expect (cl-cc/cli:parsed-args-positional parsed) :to-equal '("a" "--literal"))))

(it-sequential "script-mode-getopt-separates-positionals"
  (multiple-value-bind (opts positionals)
      (cl-cc/cli:getopt "my-script"
                         '((:verbose #\v "verbose" "Enable verbose output"))
                         '("-v" "--" "--not-a-flag"))
    (expect (getf opts :verbose) :to-be-truthy)
    (expect positionals :to-equal '("--not-a-flag"))))

(it-sequential "script-mode-source-wrapper-installs-argv-and-main-call"
  (let ((wrapped (cl-cc/cli::%source-with-script-bindings "(defun cl-cc:main (args) args)" '("x" "y"))))
    ;; The script wrapper installs the argv into cl-cc:*command-line-arguments*
    ;; (renamed from the old *script-argv*) and calls CL-CC:MAIN when bound.
    (expect (search "*COMMAND-LINE-ARGUMENTS*" (string-upcase wrapped)) :to-be-truthy)
    (expect (search "CL-CC:MAIN" (string-upcase wrapped)) :to-be-truthy)))

(it-sequential "script-mode-parse-args-accepts-reproducible-alias"
  (let ((parsed (cl-cc/cli:parse-args '("compile" "tool.lisp" "--reproducible"))))
    (expect (cl-cc/cli:flag parsed "--reproducible") :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-deterministic
                  (cl-cc/cli::%parse-compile-opts parsed)) :to-be-truthy)))

(it-sequential "cli-parse-args-accepts-gc-heap-flags"
  (let* ((parsed (cl-cc/cli:parse-args '("compile" "tool.lisp"
                                         "--gc-min-heap" "1048576"
                                         "--gc-max-heap=4194304")))
         (opts (cl-cc/cli::%parse-compile-opts parsed)))
    (expect (cl-cc/cli::compile-opts-gc-min-heap opts) :to-be 131072)
    (expect (cl-cc/cli::compile-opts-gc-max-heap opts) :to-be 524288)))
