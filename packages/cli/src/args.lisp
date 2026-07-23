;;;; cli/src/args.lisp — Parsed-args data model (parsing engine: cl-cli)
;;;;
;;;; Historically this file carried a hand-written flat argument parser.  It has
;;;; been migrated onto the external cl-cli library: the actual parse now runs
;;;; through `cl-cli:parse-argv` against the declarative application spec built
;;;; in cli-spec.lisp.  This file keeps only the stable data model the rest of
;;;; the CLI is written against —
;;;;
;;;;   parsed-args   struct: command + positionals + flag hash-table
;;;;   flag / flag-or accessors keyed by canonical flag string ("--output", "-o")
;;;;   *flag-spec*   the single source of truth for the flag set, from which
;;;;                 cli-spec.lisp generates the cl-cli option objects
;;;;
;;;; `parse-args` itself lives in cli-spec.lisp (it needs the app spec, which in
;;;; turn needs the command-dispatch table defined in main.lisp).

(in-package :cl-cc/cli)

;;; --- Condition ---

(define-condition arg-parse-error (error)
  ((message :initarg :message :reader arg-parse-error-message))
  (:report (lambda (c s)
             (format s "Argument error: ~A" (arg-parse-error-message c)))))

;;; --- Flag specification table ---
;;; Associates flag name (string) → type (:bool, :string, or :optional-string).
;;; cli-spec.lisp turns each entry into a cl-cli option (:bool → :flag,
;;; :string → :value, :optional-string → :optional-value), so this table stays
;;; the authoritative registry of every flag the CLI accepts.

(defvar *flag-spec*
  '(("--output"  . :string)
    ("-o"        . :string)
    ("--arch"    . :string)
    ("--target"  . :string)    ; FR-219: --target wasm32 | wasm64 | wasm32-wasi
    ("--lang"    . :string)
    ("--dump-ir" . :string)
    ("--stdlib"  . :bool)
    ("--no-stdlib" . :bool)
     ("--aot"     . :bool)      ; FR-219: enable AOT static .wasm compilation
     ("--streaming" . :bool)   ; FR-232: generate instantiateStreaming JS glue
     ("--validate" . :bool)    ; FR-305: validate output Wasm
     ("--sri" . :bool)         ; FR-307: generate SRI metadata
     ("--wat" . :bool)         ; FR-322: disassemble wasm to WAT
     ("--decompile" . :bool)   ; FR-322: use wasm-decompile pseudo-code
    ("--memory64" . :bool)     ; FR-213: enable 64-bit linear memory
    ("--bigint"  . :bool)      ; FR-236: enable JS BigInt i64 conversion
    ("--source-map" . :bool)   ; FR-223: generate .wasm.map
    ("--emit-names" . :bool)   ; FR-242: emit Wasm name section metadata
    ("--emit-debug-info" . :bool) ; FR-222 alias for --debug-info
    ("--type-reflection" . :bool) ; FR-263 devtools JS type metadata
    ("--stack-inspection" . :bool) ; FR-269 stack inspection helpers
    ("--memory-profiler" . :bool) ; FR-318 heap profiler helpers
    ("--hot-reload" . :bool) ; FR-317 table.set reload helpers
    ("--watch" . :bool) ; FR-808 re-run FILE on every save (run/repl watch mode)
    ("--incremental-repl" . :bool) ; FR-288 browser Wasm REPL helpers
    ("--system" . :string)
    ("--annotate-source" . :bool)
    ("--debug"  . :bool)
     ("--opt-remarks" . :string)
     ("--optimization-report" . :bool)
     ("--verbose" . :bool)
    ("--strict"  . :bool)
     ("--strict-no-alloc" . :bool)
     ("--pass-pipeline" . :string)
      ("--opt-bisect-limit" . :string)
       ("--debug-info" . :bool)
      ("--sanitize" . :string)
      ("--lto" . :string)
      ("--eh-model" . :string)
      ("--incremental" . :bool)
       ("--perf-map" . :bool)
       ("--bolt" . :bool)
       ("--bolt-profile" . :string) ; FR: BOLT profile data path
        ("--verify-transforms" . :bool)
        ("--parallel" . :string)
      ("--tier" . :string)
     ("--block-compile" . :bool)
     ("--print-pass-timings" . :bool)
    ("--time-passes" . :bool)
    ("--trace-json" . :string)
    ("--coverage" . :optional-string)
    ("--pgo-generate" . :string)
    ("--pgo-use" . :string)
    ("--spectre-mitigations" . :bool)
    ("--jit-cache-stats" . :bool)
    ("--profile" . :bool)
     ("--flamegraph" . :string)
     ("--fuzzy" . :string)
     ("--stats" . :bool)
    ("--trace-emit" . :bool)
    ("--retpoline" . :bool)
    ("--stack-protector" . :bool)
     ("--shadow-stack" . :bool)
     ("--compress" . :bool)
      ("--no-compress" . :bool)
      ("--deterministic" . :bool)
      ("--reproducible" . :bool)
      ("--script" . :bool)
      ("--build-id" . :string)
      ("--asan" . :bool)
    ("--msan" . :bool)
    ("--tsan" . :bool)
    ("--ubsan" . :bool)
    ("--hwasan" . :bool)
       ("--timeout" . :string)
       ("--no-timeout" . :bool)
      ("--gc-min-heap" . :string)
      ("--gc-max-heap" . :string)
      ;; save-core / run: core image controls (previously read by handlers but
      ;; not registered — folded in here so cl-cli accepts them uniformly).
      ("--core" . :string)
      ("--executable" . :bool)
      ("--toplevel" . :string)
      ("--compression" . :string)
      ;; fuzz: deterministic seed
      ("--seed" . :string)
       ;; FR-276: optimization level
      ("-O" . :string)
      ("--opt-level" . :string)
      ;; FR-241: macro expansion tracing
      ("--trace-macros" . :bool)
      ;; FR-153: macro expansion memoization
      ("--memoize-macros" . :bool)
      ("--dump-image" . :string)
      ("--Werror" . :bool)
      ("--Werror-category" . :string)
      ("--help"    . :bool)
    ("-h"        . :bool))
  "Alist of (flag-string . type) where type is :bool, :string, or
:optional-string.  cli-spec.lisp derives the cl-cli option objects from this
table, so it is the single source of truth for the accepted flag set.")

;;; The cl-cli application spec is defined later (cli-spec.lisp); parse-args and
;;; the invocation→parsed-args bridge reference it as a special variable.
(declaim (special *cl-cc-cli-app*))

;;; --- Struct ---

(defstruct parsed-args
  "Result of parsing a command-line argument list."
  (command    nil                              :type (or string null))
  (positional '()                              :type list)
  (flags      (make-hash-table :test #'equal) :type hash-table))

;;; --- Flag accessor helpers ---

(defun flag (parsed key)
  "Return the value of flag KEY (long form, e.g. \"--output\") from PARSED,
or NIL when the flag was not supplied."
  (gethash key (parsed-args-flags parsed)))

(defun flag-or (parsed long short)
  "Return the value of flag LONG; if absent, try SHORT.
Useful for flags that have both a long and short form (e.g. --output / -o)."
  (or (flag parsed long)
      (when short (flag parsed short))))
