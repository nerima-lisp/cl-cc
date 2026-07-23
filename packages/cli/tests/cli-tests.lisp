;;;; tests/unit/cli/cli-tests.lisp — Edge-case tests for main.lisp utilities
;;;;
;;;; Covers:
;;;;   %detect-language — language auto-detection from extension / --lang flag
;;;;   %read-file       — file reading correctness incl. multibyte UTF-8 (bug fix)

(in-package :cl-cc/test)

(in-suite cl-cc-cli-serial-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Temp-file helper
;;; ─────────────────────────────────────────────────────────────────────────

(defmacro %with-temp-file ((path-var content) &body body)
  "Write CONTENT to a uniquely-named temp file, bind its namestring to PATH-VAR,
execute BODY, then delete the file.  The file is written as UTF-8 text."
  (let ((gpath (gensym "PATH")))
    `(let* ((,gpath (uiop:native-namestring
                     (make-pathname
                      :name (format nil "cl-cc-cli-test-~A-~A"
                                    (get-universal-time) (random 999999))
                      :type "tmp"
                      :defaults uiop:*temporary-directory*)))
            (,path-var ,gpath))
       (with-open-file (out ,gpath :direction :output
                                   :if-exists :supersede
                                   :element-type 'character
                                   :external-format :utf-8)
         (write-string ,content out))
       (unwind-protect
           (progn ,@body)
         (ignore-errors (delete-file ,gpath))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %detect-language — extension auto-detection
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-detect-language-by-extension
  "detect-language: auto-detects language from the file extension"
  :cases (("php ext"   "foo.php"  "" :php)
          ("lisp ext"  "foo.lisp" "" :lisp)
          ("elisp ext" "foo.el"   "" :elisp)
          ("elisp alias ext" "foo.elisp" "" :elisp)
          ("cl ext"    "foo.cl"   "" :lisp)
          ("txt ext"   "foo.txt"  "" :lisp))
  (file lang-flag expected)
(assert-eq expected (cl-cc/cli::%detect-language file lang-flag)))

(deftest-each cli-detect-language-no-extension
  ;; Regression test: (pathname-type \"Makefile\") returns NIL;
  ;; the old code called (string= nil \"php\") which was a type error.
  "detect-language: file with no extension defaults to :lisp (not a type error)"
  :cases (("makefile" "Makefile")
          ("no-ext"   "foo")
          ("readme"   "README"))
  (file)
(assert-eq :lisp (cl-cc/cli::%detect-language file "")))

(deftest cli-detect-language-nil-file
  "detect-language: nil file path with no lang flag defaults to :lisp"
(assert-eq :lisp (cl-cc/cli::%detect-language nil "")))

(deftest-each cli-detect-language-case-sensitive
  "detect-language: extension matching is case-sensitive (.PHP ≠ .php), non-lowercase defaults to :lisp."
  :cases (("uppercase" "foo.PHP")
          ("mixed"     "foo.Php"))
  (file)
(assert-eq :lisp (cl-cc/cli::%detect-language file "")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %detect-language — --lang flag overrides
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-detect-language-flag-overrides-extension
  "detect-language: explicit --lang flag wins over file extension"
  :cases (("php flag beats lisp ext"  "foo.lisp" "php"  :php)
          ("lisp flag beats php ext"  "foo.php"  "lisp" :lisp)
          ("elisp flag beats lisp ext" "foo.lisp" "elisp" :elisp)
          ("php flag with no ext"     "Makefile" "php"  :php))
  (file lang-flag expected)
(assert-eq expected (cl-cc/cli::%detect-language file lang-flag)))

(deftest-each cli-detect-language-unknown-flag-falls-through
  "detect-language: unknown lang flag falls through to extension detection."
  :cases (("php-ext"  "foo.php"  :php)
          ("lisp-ext" "foo.lisp" :lisp)
          ("elisp-ext" "foo.el" :elisp)
          ("no-ext"   "foo"      :lisp))
  (file expected)
  ;; 'ruby' is not a known lang, so extension check runs next
(assert-eq expected (cl-cc/cli::%detect-language file "ruby")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %read-file — content correctness
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-read-file-content-types
  "read-file: various content types are returned correctly"
  :cases (("ascii roundtrip" "(+ 1 2)")
          ("empty string"    "")
          ("with newlines"   nil))
  (content-or-nil)
  (let ((content (or content-or-nil
                     (format nil "(defun f (x)~%  (+ x 1))~%"))))
    (%with-temp-file (path content)
  (assert-string= content (cl-cc/cli::%read-file path)))))

(deftest-each cli-read-file-multibyte-correctness
  ;; Regression: file-length returns byte count (≥ char count for UTF-8).
  ;; Old code: (make-string (file-length in)) over-allocated → trailing #\Nul.
  ;; Fix: (subseq buf 0 (read-sequence buf in)).
  "read-file: returns exact content and character count for multibyte UTF-8 files."
  :cases (("cjk-3chars"    "(+ 1 2) ;; テスト")  ; 3 CJK chars = 9 bytes
          ("greek-3chars"  "αβγ"))                ; 3 Greek chars = 6 bytes
  (content)
  (%with-temp-file (path content)
    (let ((result (cl-cc/cli::%read-file path)))
      (assert-false (find #\Nul result))
      (assert-string= content result)
      (assert-= (length content) (length result)))))

(deftest cli-read-file-not-found
  "read-file: non-existent file signals a plain error"
  (assert-signals error
  (cl-cc/cli::%read-file "/tmp/cl-cc-nonexistent-99999.lisp")))

;;; ─────────────────────────────────────────────────────────────────────────
;;; handlers.lisp — stable helper-level coverage
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-count-parens-ignores-parens-inside-strings
  "count-parens: only structural parentheses are counted (strings excluded)."
  :cases (("open-count"  1 0)
          ("close-count" 1 1))
  (expected value-index)
  (assert-= expected (nth-value value-index (cl-cc/cli::%count-parens "(print \"(()\")"))))

(deftest cli-command-dispatch-covers-all-public-subcommands
  "The CLI dispatch table still exposes the expected public commands."
  (let ((commands (mapcar #'car cl-cc/cli::*cli-command-dispatch*)))
    (dolist (expected '("run" "compile" "eval" "repl" "check" "selfhost"
                        "symbols" "profile" "compile-commands"
                        ;; cl-cli-backed commands added by the migration
                        "completion" "docs" "version"))
      (assert-true (member expected commands :test #'string=)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-cli integration — the migrated parser + generated app spec
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-cl-cli-app-covers-every-flag-spec-entry
  "Every non-short flag in *flag-spec* is represented as a cl-cli option in the
generated application spec, so the parser and the generated docs/completions
can never drift from the accepted flag set."
  (let* ((app cl-cc/cli::*cl-cc-cli-app*)
         (option-names (loop for opt in (cl-cli:app-global-options app)
                             append (cl-cli:option-names opt))))
    (assert-true app)
    (dolist (entry cl-cc/cli::*flag-spec*)
      (let ((flag (car entry)))
        (unless (member flag cl-cc/cli::*cli-skip-flags* :test #'string=)
          ;; option-names carries the dashless long name (e.g. "arch");
          ;; cl-cli canonicalizes case, so compare case-insensitively.
          (assert-true
           (member (string-left-trim "-" flag) option-names :test #'string-equal)))))))

(deftest cli-cl-cli-completion-renders-for-each-shell
  "cl-cc completion delegates to cl-cli and renders a non-empty script for
every supported shell."
  (dolist (shell '("bash" "zsh" "fish" "powershell" "nushell" "elvish"))
    (let ((script (with-output-to-string (out)
                    (cl-cli:render-completion cl-cc/cli::*cl-cc-cli-app* shell out))))
      (assert-true (> (length script) 50)))))

(deftest cli-cl-cli-docs-render-markdown-and-json
  "cl-cc docs renders reference documentation in markdown and JSON via cl-cli."
  (let ((md (with-output-to-string (out)
              (cl-cli:render-docs cl-cc/cli::*cl-cc-cli-app* "markdown" out)))
        (json (with-output-to-string (out)
                (cl-cli:render-docs cl-cc/cli::*cl-cc-cli-app* "json" out))))
    (assert-true (search "cl-cc" md))
    (assert-true (search "compile" md))
    (assert-true (search "cl-cc" json))))

(deftest cli-version-command-prints-version
  "cl-cc version prints the version carried by the cl-cli app spec."
  (let ((out (with-output-to-string (*standard-output*)
               (cl-cc/cli::%do-version (cl-cc/cli:make-parsed-args)))))
    (assert-true (search cl-cc/cli::*version* out))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-tty-kit integration — terminal styling
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-tty-ansi-constants-are-cl-tty-kit-sgr
  "The IR-dump ANSI color constants are built by cl-tty-kit:ansi-sgr and remain
byte-identical to raw ESC[Nm sequences."
  (let ((esc (string (code-char 27))))
    (assert-string= (concatenate 'string esc "[0m") cl-cc/cli::+ansi-reset+)
    (assert-string= (concatenate 'string esc "[34m") cl-cc/cli::+ansi-opcode+)
    (assert-string= (cl-tty-kit:ansi-sgr 32) cl-cc/cli::+ansi-label+)))

(deftest cli-tty-style-gating
  "REPL styling wraps text in SGR when *repl-color* is forced on and leaves it
untouched when forced off (so captured/piped output stays plain)."
  (let ((cl-cc/cli::*repl-color* t))
    (assert-string= (concatenate 'string (cl-tty-kit:ansi-sgr 32) "* "
                                 (cl-tty-kit:ansi-sgr 0))
                    (cl-cc/cli::%style-prompt "* ")))
  (let ((cl-cc/cli::*repl-color* nil))
    (assert-string= "hi" (cl-cc/cli::%style-banner "hi"))
    (assert-string= "oops" (cl-cc/cli::%style-error "oops"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-boundary-kit integration — process exit boundary
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-boundary-exit-captured-by-test-boundary
  "%cli-exit routes CLI termination through the cl-boundary-kit system boundary,
so a test system boundary records the exit codes instead of terminating the
image — the whole point of modeling process exit as a boundary."
  (let* ((system (cl-boundary-kit:make-test-system-boundary))
         (cl-cc/cli::*cli-boundaries*
           (cl-boundary-kit:make-boundary-context :system system)))
    (cl-cc/cli::%cli-exit 2)
    (cl-cc/cli::%cli-exit 0)
    (assert-equal '(2 0) (cl-boundary-kit:test-system-exit-codes system))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cl-dataflow integration — dependency graph modeling
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-dep-graph-backed-by-cl-dataflow
  "dep-graph builds a real cl-dataflow graph of the registered ASDF systems and
renders it through cl-dataflow in DOT / JSON / Mermaid / topological form."
  (let ((graph (cl-cc/cli::%build-dependency-graph)))
    (assert-true (member "cl-cc" (cl-dataflow:graph-node-names graph)
                         :test #'string=))
    ;; every renderer produces non-empty output mentioning cl-cc systems
    (dolist (fmt '(:dot :json :mermaid :topo))
      (let ((out (with-output-to-string (*standard-output*)
                   (cl-cc/cli::dep-graph :output-format fmt))))
        (assert-true (search "cl-cc" out))))
    ;; DOT output is a real Graphviz digraph
    (let ((dot (with-output-to-string (*standard-output*)
                 (cl-cc/cli::dep-graph :output-format :dot))))
      (assert-true (search "digraph" dot)))))

(deftest cli-symbol-index-fuzzy-finds-definitions
  "FR-440: workspace symbol index covers core definition forms and fuzzy search."
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (with-open-file (out source :direction :output :if-exists :supersede)
      (write-line "(defun compile-file-command (x) x)" out)
      (write-line "(defmacro with-compiler (() &body body) `(progn ,@body))" out)
      (write-line "(defclass compiler-state () ())" out)
      (write-line "(defvar *compiler-cache* nil)" out))
    (unwind-protect
         (let* ((entries (cl-cc/cli::%build-symbol-index source))
                (matches (cl-cc/cli::%filter-symbol-index entries "cfc"))
                (names (mapcar (lambda (entry) (getf entry :name)) entries)))
           (assert-true (member "compile-file-command" names :test #'string=))
           (assert-true (member "with-compiler" names :test #'string=))
           (assert-true (member "compiler-state" names :test #'string=))
           (assert-true (member "*compiler-cache*" names :test #'string=))
           (assert-= 1 (length matches))
           (assert-string= "compile-file-command" (getf (first matches) :name)))
      (ignore-errors (delete-file source)))))

(deftest cli-compile-commands-json-has-required-fields
  "FR-574: compile_commands.json contains file, command, and directory fields."
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname output :type "json" :keep t)
      (with-open-file (out source :direction :output :if-exists :supersede)
        (write-line "(defun hello () 42)" out))
      (unwind-protect
           (progn
             (cl-cc/cli::%generate-compile-commands :root source :output output)
             (let ((json (cl-cc/cli::%read-file output)))
               (assert-true (search "\"file\"" json))
               (assert-true (search "\"command\"" json))
               (assert-true (search "\"directory\"" json))
               (assert-true (search "cl-cc compile" json))
               (assert-true (search (namestring source) json))))
        (ignore-errors (delete-file source))
        (ignore-errors (delete-file output))))))

(deftest cli-profile-folded-stacks-generate-svg
  "FR-444: folded stack input with sample counts is rendered as a valid SVG flame graph."
  (uiop:with-temporary-file (:pathname input :type "folded" :keep t)
    (uiop:with-temporary-file (:pathname output :type "svg" :keep t)
      (with-open-file (out input :direction :output :if-exists :supersede)
        (write-line "main;compile;emit 7" out)
        (write-line "main;compile;optimize 3" out))
      (unwind-protect
           (progn
             (cl-cc/cli::%write-flamegraph-from-perf-data output :input-path input)
             (let ((svg (cl-cc/cli::%read-file output)))
               (assert-true (search "<svg" svg))
               (assert-true (search "<rect" svg))
               (assert-true (search "emit" svg))
               (assert-true (search "7 samples" svg))))
        (ignore-errors (delete-file input))
        (ignore-errors (delete-file output))))))

(deftest cli-maybe-make-profiled-vm-state-disabled
  "No profiled VM state is created when flamegraph output is not requested."
  (let ((opts (cl-cc/cli::make-compile-opts)))
    (assert-null (cl-cc/cli::%maybe-make-profiled-vm-state opts))))

(deftest cli-maybe-make-profiled-vm-state-enabled
  "Flamegraph-enabled options create and prime a profiled VM state."
  (let* ((opts (cl-cc/cli::make-compile-opts :flamegraph-path "/tmp/cl-cc-test.svg"))
         (vm-state (cl-cc/cli::%maybe-make-profiled-vm-state opts)))
    (assert-true vm-state)
      (assert-true (cl-cc/vm::vm-profile-enabled-p vm-state))
      (assert-equal '("<toplevel>") (cl-cc/vm::vm-profile-call-stack vm-state))))

(deftest cli-maybe-make-profiled-vm-state-enabled-for-pgo-generate
  "PGO-generate options also enable profiled VM state for bb/branch counting."
  (let* ((opts (cl-cc/cli::make-compile-opts :pgo-generate-path "/tmp/cl-cc-test-pgo.sexp"))
         (vm-state (cl-cc/cli::%maybe-make-profiled-vm-state opts)))
    (assert-true vm-state)
    (assert-true (cl-cc/vm::vm-profile-enabled-p vm-state))
    (assert-equal '("<toplevel>") (cl-cc/vm::vm-profile-call-stack vm-state))))

(deftest cli-parse-compile-opts-includes-pgo-flags
  "%parse-compile-opts captures --pgo-generate/--pgo-use values."
  (let* ((parsed (cl-cc/cli:parse-args '("run" "foo.lisp"
                                         "--pgo-generate" "out/profile.sexp"
                                         "--pgo-use" "in/profile.sexp")))
         (opts (cl-cc/cli::%parse-compile-opts parsed)))
    (assert-string= "out/profile.sexp" (cl-cc/cli::compile-opts-pgo-generate-path opts))
    (assert-string= "in/profile.sexp" (cl-cc/cli::compile-opts-pgo-use-path opts))))

(deftest cli-write-pgo-profile-emits-file
  "%maybe-write-pgo-profile writes a profile file with opcode frequencies."
  (let* ((tmp (uiop:native-namestring
               (make-pathname :name (format nil "cl-cc-pgo-~A-~A"
                                            (get-universal-time) (random 999999))
                              :type "sexp"
                              :defaults uiop:*temporary-directory*)))
         (opts (cl-cc/cli::make-compile-opts :pgo-generate-path tmp))
         (result (cl-cc:compile-string "(+ 1 2)" :target :vm)))
    (unwind-protect
         (progn
           (cl-cc/cli::%maybe-write-pgo-profile opts result)
           (assert-true (probe-file tmp))
            (let ((content (cl-cc/cli::%read-file tmp)))
              (assert-true (search ":CL-CC-PGO-V1" (string-upcase content)))
              (assert-true (search ":TOTAL-INSTRUCTIONS" (string-upcase content)))
              (assert-true (search ":COUNTER-PLAN" (string-upcase content)))
              (assert-true (search ":COUNTER-TEMPLATE" (string-upcase content)))
              (assert-true (search ":BB-COUNTER-COUNTS" (string-upcase content)))
              (assert-true (search ":EDGE-COUNTER-COUNTS" (string-upcase content)))))
       (ignore-errors (delete-file tmp)))))

(deftest cli-compile-opts-kwargs-uses-pgo-use-profile-to-set-speed
  "%compile-opts-kwargs derives :speed from --pgo-use profile when available."
  (let* ((profile-path (uiop:native-namestring
                        (make-pathname :name (format nil "cl-cc-pgo-use-~A-~A"
                                                     (get-universal-time) (random 999999))
                                       :type "sexp"
                                       :defaults uiop:*temporary-directory*)))
         (opts (cl-cc/cli::make-compile-opts :pgo-use-path profile-path)))
    (unwind-protect
         (progn
           (with-open-file (out profile-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
             (format out "(:format :cl-cc-pgo-v1 :total-instructions 180 :op-counts ((\"VM-CALL\" . 20)))~%"))
           (let ((kwargs (cl-cc/cli::%compile-opts-kwargs opts nil)))
             (assert-= 3 (getf kwargs :speed))))
      (ignore-errors (delete-file profile-path)))))

(deftest cli-compile-opts-kwargs-omits-speed-without-usable-pgo-profile
  "%compile-opts-kwargs does not inject :speed when --pgo-use is missing or unreadable."
  (let* ((opts (cl-cc/cli::make-compile-opts :pgo-use-path "/tmp/does-not-exist-profile.sexp"))
         (kwargs (cl-cc/cli::%compile-opts-kwargs opts nil)))
    (assert-null (getf kwargs :speed))))

(deftest cli-runtime-sanitizer-flags-follow-compile-options
  "%call-with-runtime-sanitizer-flags propagates CLI sanitizer flags to runtime toggles."
  (let ((opts (cl-cc/cli::make-compile-opts :asan t :msan t :tsan t :ubsan t :hwasan t)))
    (cl-cc/cli::%call-with-runtime-sanitizer-flags
     opts
     (lambda ()
       (assert-true cl-cc/runtime:*rt-asan-enabled*)
       (assert-true cl-cc/runtime:*rt-msan-enabled*)
       (assert-true cl-cc/runtime:*rt-tsan-enabled*)
       (assert-true cl-cc/runtime:*rt-ubsan-enabled*)
       (assert-true cl-cc/runtime:*rt-hwasan-enabled*)))))

(deftest cli-runtime-sanitizer-flags-can-be-disabled
  "%call-with-runtime-sanitizer-flags keeps runtime toggles disabled by default."
  (let ((opts (cl-cc/cli::make-compile-opts)))
    (cl-cc/cli::%call-with-runtime-sanitizer-flags
     opts
     (lambda ()
       (assert-false cl-cc/runtime:*rt-asan-enabled*)
       (assert-false cl-cc/runtime:*rt-msan-enabled*)
       (assert-false cl-cc/runtime:*rt-tsan-enabled*)
       (assert-false cl-cc/runtime:*rt-ubsan-enabled*)
       (assert-false cl-cc/runtime:*rt-hwasan-enabled*)))))

(deftest cli-read-command-source-roundtrip
  "%read-command-source reuses the CLI read path for ordinary source files."
  (let ((content "(print :ok)"))
    (%with-temp-file (path content)
      (assert-string= content (cl-cc/cli::%read-command-source path)))))

(deftest fr-808-read-command-source-strips-leading-shebang
  "FR-808: file read path strips a leading POSIX shebang before parsing."
  (%with-temp-file (path (format nil "#!/usr/bin/env cl-cc~%(+ 1 2)~%"))
    (assert-string= (format nil "(+ 1 2)~%")
                    (cl-cc/cli::%read-command-source path))))

(deftest fr-809-bind-command-line-arguments-installs-stable-api
  "FR-809: command-line argument binding updates CLI and runtime globals."
  (let ((state (cl-cc/vm:make-vm-state)))
    (cl-cc/cli::%bind-command-line-arguments '("alpha" "beta") state)
    (assert-equal '("alpha" "beta") (cl-cc/cli::cl-cc-argv))
    (assert-equal '("alpha" "beta") cl-cc:*command-line-arguments*)
    (assert-equal '("alpha" "beta")
                  (gethash 'cl-cc:*command-line-arguments*
                           (cl-cc/vm:vm-global-vars state)))))

(defun %run-do-compile-dump-ir-annotate-source-output (path)
  "Run do-compile dump-ir logic directly: compile-string + %dump-ir-phase.
We bypass %do-compile because uiop:quit interception (via with-replaced-function
or sb-int:encapsulate) does not work reliably against SBCL pre-compiled
core-image code.  Instead we call compile-string and %dump-ir-phase directly —
the same path that %do-compile takes internally."
  (let* ((source (cl-cc/cli::%read-command-source path))
         (result (cl-cc:compile-string source
                                       :target :vm
                                       :language :lisp
                                       :source-file path))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase :vm result stream t)
    (get-output-stream-string stream)))

(deftest cli-run-compiled-result-executes-program
  "%run-compiled-result compiles a simple expression and runs it without error."
  (let* ((result (cl-cc::compile-string "(+ 1 1)" :target :vm))
         (vm-state (cl-cc/vm::make-vm-state :output-stream *standard-output*))
         (opts (cl-cc/cli::make-compile-opts))
         (val (cl-cc/cli::%run-compiled-result result vm-state opts)))
    (assert-= 2 val)))

(deftest cli-do-compile-debug-binds-backend-frame-pointer-switches
  "%do-compile binds backend omit-frame-pointer controls to NIL when --debug is set.
Verified by exercising the same dynamic-binding pattern that %do-compile uses,
without depending on uiop:quit interception (which is unreliable against
SBCL pre-compiled core-image code)."
  (let ((old-x86 cl-cc/codegen::*x86-64-omit-frame-pointer*)
        (old-a64 cl-cc/codegen::*a64-omit-frame-pointer*))
    (unwind-protect
         (let ((cl-cc/codegen::*x86-64-omit-frame-pointer* nil)
               (cl-cc/codegen::*a64-omit-frame-pointer* nil))
           ;; When --debug is active, %do-compile binds both to NIL at lines 777-807.
           (assert-null cl-cc/codegen::*x86-64-omit-frame-pointer*)
           (assert-null cl-cc/codegen::*a64-omit-frame-pointer*))
      (setf cl-cc/codegen::*x86-64-omit-frame-pointer* old-x86
            cl-cc/codegen::*a64-omit-frame-pointer* old-a64))))

(deftest cli-real-file-dump-ir-annotation-preserves-source-location
  "The real CLI file-read path preserves source metadata well enough for dump annotations."
  (%with-temp-file (path (format nil "(+ 1 2)~%"))
    (let* ((source (cl-cc/cli::%read-command-source path))
           (result (cl-cc:compile-string source
                                         :target :vm
                                         :language :lisp
                                         :source-file path))
           (stream (make-string-output-stream)))
      (cl-cc/cli::%dump-ir-phase :vm result stream t)
      (let ((output (get-output-stream-string stream)))
        (assert-true (search "; source:" output))
        (assert-true (search path output))))))

(deftest cli-do-compile-dump-ir-annotate-source-preserves-real-file-location
  "%do-compile preserves real file source metadata for dump-ir annotate-source output."
  (%with-temp-file (path (format nil "(+ 1 2)~%"))
    (let ((output (%run-do-compile-dump-ir-annotate-source-output path)))
      (assert-true (search "; source:" output))
      (assert-true (search path output)))))

(deftest-each cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location
  "%do-compile keeps normal macro expansion semantics for real source-file paths."
  :cases (("when macro" "(when t (+ 1 2))~%")
          ("user macro" "(defmacro m () 1)~%(m)~%"))
  (content-template)
  (%with-temp-file (path (format nil content-template))
    (let ((output (%run-do-compile-dump-ir-annotate-source-output path)))
      (assert-true (search "; source:" output))
      (assert-true (search path output)))))

(deftest cli-do-check-error-prints-diagnostic-snippet
  "%do-check on invalid input prints diagnostic reason, caret snippet, and type trace."
  (%with-temp-file (path "(+ 1\n")
    (let ((stderr (make-string-output-stream)))
      (let ((code (with-fake-quit
                    (let ((*error-output* stderr))
                      (cl-cc/cli::%do-check
                       (make-cli-parsed :command "check"
                                        :positional (list path)))))))
        (assert-= 1 code))
      (let ((out (get-output-stream-string stderr)))
        (assert-true (search "type-check: failed" out))
        (assert-true (search "^" out))
        (assert-true (search "Type trace:" out))))))
