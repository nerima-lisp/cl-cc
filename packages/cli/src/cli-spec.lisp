;;;; cli/src/cli-spec.lisp — cl-cli application spec + parsing engine
;;;;
;;;; This module makes the external cl-cli library the CLI's argument parser.
;;;; A single declarative application spec is *generated* from the CLI's own
;;;; sources of truth — `*flag-spec*` (args.lisp) and `*cli-command-dispatch*`
;;;; / `*cli-help-strings*` (main.lisp) — so the parser, shell completion, and
;;;; generated reference docs can never drift from the accepted flag set.
;;;;
;;;;   parse-args            cl-cli:parse-argv → the legacy parsed-args struct
;;;;   cl-cc completion <sh> shell completion scripts (bash/zsh/fish/…)
;;;;   cl-cc docs [format]   man / markdown / JSON reference docs
;;;;   cl-cc version         version string from the app spec
;;;;
;;;; Loads last in the system so every table it reads is already defined.

(in-package :cl-cc/cli)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Flag-spec → cl-cli option translation
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *cli-long->short*
  '(("--output"    . #\o)
    ("--opt-level" . #\O)
    ("--help"      . #\h))
  "Long flags that also carry a short form.  In the flat *flag-spec* the short
and long variants are separate entries; cl-cli models them as one option with a
:short character, so the long flag owns the short here and the bare short
entries are skipped during generation.")

(defparameter *cli-skip-flags*
  '("-o" "-O" "-h")
  "Bare short-form *flag-spec* keys folded into their long partner via
*CLI-LONG->SHORT*, so they are not emitted as standalone cl-cli options.")

(defparameter *cli-option-key-overrides*
  '(("--help" . :cli-help))
  "Explicit cl-cli option keys for flags whose derived key would collide with a
reserved cl-cli key (:help / :version).")

(defun %legacy-kind->cli-kind (kind)
  "Map an args.lisp flag type to the corresponding cl-cli option KIND."
  (ecase kind
    (:bool            :flag)
    (:string          :value)
    (:optional-string :optional-value)))

(defun %long-flag->option-name (long)
  "Strip the leading dashes from a long flag string: \"--opt-level\" → \"opt-level\"."
  (string-left-trim "-" long))

(defparameter *cli-flag-descriptions*
  '(("--output"    . "Output file")
    ("--arch"      . "Target architecture: x86-64 | arm64 | wasm32")
    ("--lang"      . "Source language: lisp | elisp | php | js | javascript")
    ("--dump-ir"   . "Dump IR for a phase: ast, cps, ssa, vm, opt, asm")
    ("--stdlib"    . "Eagerly prepend the standard library")
    ("--no-stdlib" . "Disable lazy stdlib auto-require")
    ("--verbose"   . "Show compilation details on stderr")
    ("--strict"    . "Treat type warnings as errors (check only)")
    ("--tier"      . "Compilation tier: 0 fast, 1 optimized")
    ("--timeout"   . "Maximum execution time in seconds")
    ("--no-timeout" . "Disable the CLI timeout")
    ("--profile"   . "Enable VM profile collection/reporting")
    ("--opt-level" . "Optimization level (-O0 … -O3)")
    ("--core"      . "Load a CL-CC core image before running")
    ("--executable" . "Prepend a loader stub when saving a core")
    ("--Werror"    . "Treat compiler warnings as errors"))
  "Curated help text for the most user-facing flags; the rest render without a
description in generated docs/completions.")

(defun %build-global-options ()
  "Build the cl-cli global-option list from the authoritative *flag-spec*, and
alongside it a map from each option's cl-cli key to the legacy flag string(s)
the rest of the CLI reads (\"--output\" plus \"-o\").  Returns (values options
key->legacy-hash)."
  (let ((options '())
        (key->legacy (make-hash-table)))
    (loop for (long . kind) in *flag-spec*
          unless (member long *cli-skip-flags* :test #'string=)
            do (let* ((short (cdr (assoc long *cli-long->short* :test #'string=)))
                      (key-override (cdr (assoc long *cli-option-key-overrides*
                                                :test #'string=)))
                      (opt (cl-cli:make-option
                            :name (%long-flag->option-name long)
                            :key key-override
                            :kind (%legacy-kind->cli-kind kind)
                            :short short
                            :description (cdr (assoc long *cli-flag-descriptions*
                                                     :test #'string=))))
                      (key (cl-cli:option-key opt)))
                 (push opt options)
                 (setf (gethash key key->legacy)
                       (cons long (when short
                                    (list (format nil "-~A" short)))))))
    (values (nreverse options) key->legacy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Dispatch table → cl-cli commands
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *cli-command-descriptions*
  '(("run"       . "Compile and run a source file")
    ("compile"   . "Compile to a native binary or AOT .wasm")
    ("save-core" . "Save a CL-CC native core image")
    ("eval"      . "Evaluate an expression inline")
    ("repl"      . "Start the interactive REPL")
    ("check"     . "Type-check a source file without executing it")
    ("selfhost"  . "Run the self-hosting profile workload")
    ("symbols"   . "Index workspace symbols; use --fuzzy <query>")
    ("profile"   . "Generate profiling artifacts such as flame graphs")
    ("compile-commands" . "Generate compile_commands.json")
    ("install"   . "Register/compile a local ASDF system")
    ("uninstall" . "Remove a registered local system")
    ("completion" . "Print a shell completion script (bash/zsh/fish/…)")
    ("docs"      . "Render reference docs (markdown | man | json)")
    ("version"   . "Print the cl-cc version")
    ("help"      . "Show help for cl-cc or a specific command"))
  "Short, help/manpage-friendly descriptions per subcommand; commands absent
here fall back to a generated one-liner.")

(defparameter *cli-file-commands*
  '("run" "compile" "check" "doc" "doctest" "show-types" "objdump"
    "macrostep" "reduce" "disasm" "inspect" "abi-dump")
  "Commands whose positional argument is a file path — flagged with a :file
completion hint so shells offer path completion.")

(defun %cli-command-names ()
  "All subcommand names cl-cli should know about: the dispatch table plus the
specially-handled install/uninstall/help commands."
  (remove-duplicates
   (append (mapcar #'car *cli-command-dispatch*)
           '("install" "uninstall" "help"))
   :test #'string= :from-end t))

(defun %cli-commands ()
  "Build the cl-cli command list mirroring the CLI's subcommand tree.  Every
command carries a rest positional so cl-cli collects the file/expression
arguments the flat model used to leave in `parsed-args-positional`."
  (loop for name in (%cli-command-names)
        collect (cl-cli:make-command
                 :name name
                 :description (or (cdr (assoc name *cli-command-descriptions*
                                              :test #'string=))
                                  (format nil "cl-cc ~A" name))
                 :positionals
                 (list (cl-cli:make-positional
                        :key :args
                        :name "ARGS"
                        :rest-p t
                        :value-hint (when (member name *cli-file-commands*
                                                  :test #'string=)
                                      :file))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; The application spec
;;; ─────────────────────────────────────────────────────────────────────────

(defvar *cli-key->legacy* nil
  "Hash-table mapping each generated cl-cli option key to the legacy flag
string(s) the CLI reads, used to rebuild the parsed-args flag table.")

(defvar *cl-cc-cli-app* nil
  "The declarative cl-cli specification for the cl-cc command tree, generated
from *FLAG-SPEC* and *CLI-COMMAND-DISPATCH*.  It is both the live parser (see
`parse-args`) and the source for completion/docs/version rendering.

:auto-help is NIL — --help / -h are ordinary flags so cl-cc keeps its own
hand-written multi-line help renderer (%print-help) instead of cl-cli's.")

(let ((options nil))
  (multiple-value-setq (options *cli-key->legacy*) (%build-global-options))
  (setf *cl-cc-cli-app*
        (cl-cli:make-app
         :name "cl-cc"
         :version *version*
         :summary "Self-hosting Common Lisp compiler and multi-language toolchain"
         :description
         "cl-cc compiles ANSI Common Lisp (and PHP / JavaScript frontends) to a
register-based bytecode VM and on to native x86-64, AArch64, and WebAssembly."
         :global-options options
         :commands (%cli-commands)
         :auto-help nil)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Parsing engine: cl-cli:parse-argv → parsed-args
;;; ─────────────────────────────────────────────────────────────────────────

(defun %invocation->parsed-args (inv)
  "Translate a cl-cli invocation into the legacy PARSED-ARGS struct the CLI
handlers consume: leaf subcommand name, rest positionals, and a flag hash-table
keyed by the canonical flag strings (only options actually supplied are set, so
`flag` still returns NIL for absent flags)."
  (let* ((result (make-parsed-args))
         (ht (parsed-args-flags result))
         (command (cl-cli:invocation-command inv)))
    (when command
      (setf (parsed-args-command result) (cl-cli:command-name command))
      (setf (parsed-args-positional result)
            (copy-list (ignore-errors (cl-cli:positional-value inv :args)))))
    (maphash (lambda (key legacy-keys)
               (let ((source (cl-cli:option-value-source inv key)))
                 (when (and source (not (eq source :default)))
                   (let ((value (cl-cli:option-value inv key)))
                     (dolist (lk legacy-keys)
                       (setf (gethash lk ht) value))))))
             *cli-key->legacy*)
    result))

(defun %split-eq (token)
  "Split TOKEN at the first '='.  Returns (values name inline-present-p)."
  (let ((pos (position #\= token)))
    (if pos
        (values (subseq token 0 pos) t)
        (values token nil))))

(defun %flag-value-p (name)
  "True when the flag NAME takes a mandatory value (:string in *flag-spec*),
so the following token is its value.  :optional-string flags take a value only
via the inline =form, matching cl-cli's :optional-value semantics."
  (eq (cdr (assoc name *flag-spec* :test #'string=)) :string))

(defun %option-token-p (token)
  "True when TOKEN looks like an option (leading dash, not a bare \"-\")."
  (and (> (length token) 1)
       (char= (char token 0) #\-)
       (not (string= token "-"))))

(defun %partition-argv (argv)
  "Group ARGV into (values option-tokens positional-tokens) so cl-cli — whose
rest positional consumes greedily — still parses options that appear after a
positional (the common `compile file --arch x` form).  Value-bearing flags
carry their value token along; unknown option-looking tokens are routed to the
option list so cl-cli reports them as unknown.  A literal `--` ends option
scanning, matching cl-cli's own separator semantics."
  (let ((opts '())
        (positionals '())
        (rest (copy-list argv))
        (literal nil))
    (loop while rest do
      (let ((token (pop rest)))
        (cond
          (literal (push token positionals))
          ((string= token "--") (setf literal t))
          ((%option-token-p token)
           (push token opts)
           (multiple-value-bind (name inline-p) (%split-eq token)
             (when (and (not inline-p) (%flag-value-p name) rest)
               (push (pop rest) opts))))
          (t (push token positionals)))))
    (values (nreverse opts) (nreverse positionals))))

(defun %script-mode-argv-p (argv)
  "True when ARGV begins the FR-809 --script fast path."
  (and argv (string= (first argv) "--script")))

(defun %parse-script-args (argv)
  "Preserve the FR-809 --script fast path outside cl-cli: the token after
--script is the script file (stored as the command) and everything else becomes
the script argv (dropping a single explicit -- positional separator)."
  (let ((result (make-parsed-args))
        (rest (rest argv)))                 ; drop "--script"
    (setf (gethash "--script" (parsed-args-flags result)) t)
    (unless rest
      (error 'arg-parse-error :message "Flag --script requires a script file"))
    (setf (parsed-args-command result) (first rest)
          (parsed-args-positional result)
          (loop for token in (rest rest)
                unless (string= token "--") collect token))
    result))

(defun parse-args (argv)
  "Parse ARGV (a list of strings, without the program name) into a PARSED-ARGS
struct.  cl-cli:parse-argv is the parsing engine; the FR-809 --script fast path
is handled directly to preserve its script-argv semantics.  cl-cli usage errors
are re-signalled as ARG-PARSE-ERROR so existing callers keep their contract."
  (if (%script-mode-argv-p argv)
      (%parse-script-args argv)
      (handler-case
          (multiple-value-bind (opts positionals) (%partition-argv argv)
            (%invocation->parsed-args
             ;; parse-argv treats element 0 as the program name and parses the
             ;; rest, so prepend a synthetic argv0.  Options precede positionals
             ;; so cl-cli's greedy rest positional does not swallow them.
             (cl-cli:parse-argv *cl-cc-cli-app*
                                (list* "cl-cc" (append opts positionals)))))
        (cl-cli:cli-usage-error (e)
          (error 'arg-parse-error :message (cl-cli:cli-error-message e))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Handlers backed by cl-cli
;;; ─────────────────────────────────────────────────────────────────────────

(defun %do-completion (parsed)
  "cl-cc completion <shell> — emit a shell completion script via cl-cli."
  (let ((shell (or (first (parsed-args-positional parsed)) "bash")))
    (handler-case
        (progn
          (cl-cli:render-completion *cl-cc-cli-app* shell *standard-output*)
          (terpri *standard-output*))
      (cl-cli:cli-usage-error (e)
        (format *error-output* "~A~%" (cl-cli:cli-error-message e))
        (format *error-output*
                "Supported shells: bash, zsh, fish, powershell, nushell, elvish~%")
        (%cli-exit 2)))))

(defun %do-docs (parsed)
  "cl-cc docs [markdown|man|json] — render reference documentation via cl-cli."
  (let ((format (or (first (parsed-args-positional parsed)) "markdown")))
    (handler-case
        (cl-cli:render-docs *cl-cc-cli-app* format *standard-output*)
      (cl-cli:cli-usage-error (e)
        (format *error-output* "~A~%" (cl-cli:cli-error-message e))
        (format *error-output* "Supported formats: markdown, man, json~%")
        (%cli-exit 2)))))

(defun %do-version (parsed)
  "cl-cc version — print the version carried by the cl-cli app spec."
  (declare (ignore parsed))
  (format t "cl-cc ~A~%" *version*))
