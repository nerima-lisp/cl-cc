;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection — umbrella ASDF system.
;;;;
;;;; All subsystems live in packages/**/. This file wires them together:
;;;;   eval-when: pre-load the per-package .asd files
;;;;   "cl-cc":           umbrella package (src/package.lisp) + compile pipeline
;;;;   "cl-cc/test":      the canonical unit test suite
;;;;   "cl-cc/test/e2e":  self-hosting end-to-end regression tests
;;;;
;;;; The test systems live in THIS file rather than a separate cl-cc-test.asd.
;;;; That is not merely the org convention (PACKAGE_STANDARD.md, "exactly one
;;;; <pkg>.asd at the root"): ASDF resolves a slash-qualified secondary system
;;;; such as "cl-cc/test" by loading the .asd named after its PRIMARY system,
;;;; i.e. cl-cc.asd. Defining it anywhere else would make
;;;; (asdf:find-system "cl-cc/test") fail unless the other file happened to
;;;; have been loaded first by hand.
;;;;
;;;; System name designators are strings, not keywords or uninterned symbols,
;;;; so that reading this file does not depend on the reader's package state.

;;;; This form comes FIRST, before any other form. ASDF binds *package* to
;;;; ASDF-USER only for a file it loads itself; read any other way -- a REPL
;;;; `load`, an editor evaluating the buffer, flake.nix parsing :version -- the
;;;; file is read in whatever package happens to be current, and an unqualified
;;;; `defsystem` then fails to read at all. Saying it makes the file
;;;; self-contained. See PACKAGE_STANDARD.md "asd の書き方".
(in-package #:asdf-user)

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (flet ((ensure-system-asd (system-name relative-asd here)
           (unless (asdf:find-system system-name nil)
             (load (merge-pathnames relative-asd here)))))
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      ;; cl-cc-bootstrap, cl-cc-vm, cl-cc-ast, cl-cc-type, cl-cc-binary,
      ;; cl-cc-runtime, cl-cc-mir and cl-cc-target
      ;; are deliberately absent. They live in the standalone repositories of the same name under
      ;; nerima-lisp and reach this build as flake.nix inputs.
      ;;
      ;; They used to be defined in packages/{ast,type} as well. Because
      ;; ENSURE-SYSTEM-ASD only loads a file when FIND-SYSTEM misses, and the
      ;; cl-cc source tree is scanned ahead of the injected derivations, the
      ;; in-tree copies won in *both* Nix and a bare checkout -- so the
      ;; repositories were never compiled, and the two definitions had drifted
      ;; apart in the meantime. Removing the in-tree copies is what makes the
      ;; resolution unambiguous.
      (ensure-system-asd :cl-cc-compile "packages/compile/cl-cc-compile.asd" here)
       (ensure-system-asd :cl-cc-stdlib "packages/stdlib/cl-cc-stdlib.asd" here)
       (ensure-system-asd :cl-cc-pipeline "packages/pipeline/cl-cc-pipeline.asd" here)
       (ensure-system-asd :cl-cc-selfhost "packages/selfhost/cl-cc-selfhost.asd" here)
        (ensure-system-asd :cl-cc-repl "packages/repl/cl-cc-repl.asd" here))))

(asdf:defsystem "cl-cc"
  :description "Self-hosting Common Lisp compiler: reader and macroexpander through CPS, typed IR, and optimiser to bytecode, native x86-64/ARM64/WebAssembly, PHP, and JavaScript."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :bug-tracker "https://github.com/nerima-lisp/cl-cc/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-cc.git")
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-parse :cl-cc-binary
                :cl-cc-runtime :cl-cc-mir :cl-cc-target
                :cl-cc-type :cl-cc-optimize :cl-cc-regalloc :cl-cc-emit :cl-cc-expand
                :cl-cc-compile :cl-cc-cps :cl-cc-codegen :cl-cc-vm :cl-cc-stdlib
                 :cl-cc-pipeline :cl-cc-selfhost :cl-cc-repl :cl-cc-php :cl-cc-javascript)
  :components
  ((:module "src" :pathname "src" :serial t :components ((:file "package"))) (:module "ffi-dynlib" :pathname "src/ffi/" :components ((:file "dynlib"))) (:module "fpga-hls" :pathname "packages/emit/src" :serial t :components ((:file "fpga"))))
  ;; So `asdf:test-system "cl-cc"` runs the suite. Only the canonical unit
  ;; suite is chained: "cl-cc/test/e2e" is the self-hosting regression run and
  ;; is invoked deliberately, not as a side effect of testing the umbrella.
  :in-order-to ((test-op (test-op "cl-cc/test"))))

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (flet ((maybe-load-asd (system-name relative-asd here)
           (let ((asd-path (merge-pathnames relative-asd here)))
             (when (and (probe-file asd-path)
                        (not (asdf:find-system system-name nil)))
               (load asd-path)))))
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      ;; Development/test systems that depend on :cl-cc are loaded explicitly
      ;; so slash-qualified test systems resolve from the root ASD alone.
      (maybe-load-asd :cl-cc-cli "packages/cli/cl-cc-cli.asd" here)
      (maybe-load-asd :cl-cc-testing-framework "packages/testing-framework/cl-cc-testing-framework.asd" here)
      (maybe-load-asd :cl-cc-docgen "packages/docgen/cl-cc-docgen.asd" here)
      (maybe-load-asd :cl-cc-tools "packages/tools/cl-cc-tools.asd" here)
      (maybe-load-asd :cl-cc-prolog-tools "packages/prolog-tools/cl-cc-prolog-tools.asd" here)
      (maybe-load-asd :cl-cc-formatter "packages/formatter/cl-cc-formatter.asd" here)
      (maybe-load-asd :cl-cc-jit "src/jit/cl-cc-jit.asd" here))))

;; :cl-cc-cli is defined in packages/cli/cl-cc-cli.asd.
;; :cl-cc-testing-framework is defined in packages/testing-framework/cl-cc-testing-framework.asd.
;; Both are loaded by the eval-when block above, which is why the test systems
;; below can name them in :depends-on.

(asdf:defsystem "cl-cc/test"
  :description "Canonical unit test suite for cl-cc: per-subsystem specs plus the ANSI conformance and coverage-evidence suites."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :bug-tracker "https://github.com/nerima-lisp/cl-cc/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-cc.git")
  :depends-on (:cl-cc :cl-cc-jit :cl-cc-cli :cl-cc-testing-framework :cl-cc-php :cl-cc-javascript :cl-cc-tools :cl-cc-formatter)
  :serial t
  :components
  (;; Unit tests — each module now lives in its workspace's tests/ dir
   ;;
   ;; testing-framework-tests (framework-runner-tests, framework-parallel-tests,
   ;; persistent-tests, timing-tests, entrypoint-contract-tests, ...) tested the
   ;; removed homegrown registry/TAP/parallel-worker/coverage runner's own
   ;; internals directly; there is no equivalent to port them to now that
   ;; cl-weave is the engine (cl-weave has its own 615-test self-suite covering
   ;; the same ground for cl-weave's own internals).
    (:module "cli-tests" :pathname "packages/cli/tests" :serial t :components ((:file "test-support") (:file "args-tests") (:file "cli-tests") (:file "flamegraph-tests") (:file "main-tests") (:file "main-dump-tests") (:file "main-utils-tests") (:file "plugin-tests")))
    (:module "vm-tests"
    :pathname "packages/vm/tests"
    :serial t
    :components
    ((:file "vm-instructions-tests")
     (:file "list-tests")
     (:file "list-alist-tests")
     (:file "list-coerce-tests")
     (:file "array-tests")
     (:file "vm-execute-tests")
     (:file "vm-execute-tests-2")
      (:file "vm-call-tests")
      (:file "primitives-tests")
      (:file "primitives-typep-tests")
      (:file "vm-bignum-tests")
      (:file "vm-transcendental-tests")
     (:file "vm-numeric-tests")
     (:file "vm-extensions-tests")
     (:file "vm-bitwise-tests")
     (:file "vm-clos-tests")
     (:file "vm-clos-execute-tests")
     (:file "vm-run-tests")
     (:file "vm-run-vm2-tests")
     (:file "vm-run-fusion-tests")
     (:file "vm-dispatch-tests")
     (:file "vm-dispatch-gf-tests")
     (:file "vm-dispatch-gf-multi-tests")
     (:file "vm-bridge-tests")
     (:file "vm-opcodes-tests")
     (:file "vm-opcodes-defs-tests")
     (:file "vm-tests")
     (:file "vm-public-api-lint-tests")
     (:file "package-tests")
     (:file "vm-environment-tests")
     (:file "conditions-tests")
     (:file "hash-tests")
     (:file "symbols-tests")
     (:file "strings-tests")
     (:file "vm-sequence-tests")
     (:file "format-tests")
      (:file "io-tests")
      (:file "io-runners-tests")
      (:file "string-builder-tests")
      (:file "persistent-tests")
      (:file "json-tests")
      (:file "regex-tests")
       (:file "runtime-stdlib-2-vm-tests")
       (:file "runtime-stdlib-2-completion-tests")
       (:file "runtime-stdlib-3-tests")
       (:file "runtime-stdlib-3-numeric-tests")
       (:file "runtime-stdlib-3-debug-tests")
       (:file "runtime-stdlib-3-io-tests")
       (:file "crash-report-tests")
       (:file "vm-mop-tests")
 (:file "vm-mop-remaining-features-tests")))
    (:module "parse-tests"
    :pathname "packages/parse/tests"
    :serial t
    :components
    ((:file "cl-parser-tests")
     (:file "cl-parser-error-tests")
     (:file "cl-parser-ast-tests")
     (:file "cl-parser-new-tests")
     (:file "cl-parser-new-extended-tests")
     (:module "cl"
      :serial t
      :components
      ((:file "lower-tests")
       (:file "lower-arithmetic-tests")
       (:file "parser-roundtrip-tests")
       (:file "grammar-tokenstream-tests")
       (:file "grammar-parser-tests")))
     (:file "cst-tests")
     (:file "lexer-tests")
     (:file "lexer-dispatch-tests")
     (:file "grammar-tests")
     (:file "grammar-special-form-tests")
     (:file "pratt-tests")
     (:file "pratt-pipeline-tests")
     (:file "combinator-tests")
     (:file "combinator-tests-2")
     (:file "parser-combinator-tests")
     (:file "incremental-tests")
     (:file "cst-to-ast-tests")
     (:file "diagnostics-tests")))
   ;; php85-tests-registration.lisp dropped: it meta-tested the removed
      ;; homegrown *test-registry*/persist-* registration mechanism's own
      ;; internals directly, which has no cl-weave equivalent to preserve.
   (:module "compile-tests"
    :pathname "packages/compile/tests"
    :serial t
    :components
    ((:file "cps-tests")
     (:file "cps-ast-tests")
     (:file "cps-ast-semantic-tests")
     (:file "cps-ast-transform-tests")
     (:file "cps-ast-extended-tests")
      (:file "cps-ast-functional-tests")
      (:module "cps-package-tests"
       :pathname "../../cps/tests"
       :serial t
       :components
       ((:file "cps-coverage-matrix-tests")
        (:file "cps-trmc-tests")))
        (:file "builtin-registry-tests")
       (:file "builtin-registry-data-tests")
       (:file "fr-586-set-tests")
       (:file "fr-316-benchmark-tests")
       (:file "fr-318-compiler-warning-tests")
      (:file "builtin-registry-data-ext-tests")
     (:file "builtin-registry-stream-tests")
     (:file "closure-tests")
     (:file "closure-escape-tests")
     (:file "context-tests")
     (:file "codegen-tests")
     (:file "codegen-fold-tests")
     (:file "codegen-fold-eval-tests")
     (:file "codegen-phase2-helpers")
     (:file "codegen-core-tests")
     (:file "codegen-core-helper-tests")
     (:file "codegen-core-array-sink-tests")
     (:file "codegen-type-predicate-tests")
     (:file "codegen-core-let-tests")
     (:file "codegen-functions-tests")
     (:file "codegen-functions-callsite-tests")
     (:file "codegen-functions-params-tests")
     (:file "codegen-clos-tests")
     (:file "codegen-clos-slot-tests")
     (:file "codegen-control-tests")
      (:file "codegen-calls-tests")
      (:file "fr-009-pic-tests")
      (:file "codegen-toplevel-cps-tests")
     (:file "codegen-locals-tests")
     (:file "codegen-numeric-hints-tests")
     (:file "codegen-io-tests")
     (:file "codegen-io-read-tests")
     (:file "codegen-hash-table-tests")
     (:file "codegen-slot-predicates-tests")
     (:file "codegen-string-kwargs-tests")
     (:file "phase2-handler-tests")
     (:file "codegen-phase2-tests")
     (:file "codegen-phase2-io-tests")
     (:file "stdlib-source-tests")
     (:file "pipeline-native-tests")
     (:file "pipeline-native-io-tests")
     (:file "pipeline-native-routing-tests")
     (:file "compiler-selfhost-fixtures")
     (:file "compiler-selfhost-fixtures-ext")
     (:file "compiler-tests")
     (:file "compiler-tests-pbt-tests")
     (:file "compiler-tests-call-forms")
     (:file "compiler-tests-stdlib")
     (:file "compiler-tests-stdlib-io")
     (:file "compiler-tests-extended")
     (:file "compiler-tests-extended-stdlib")
     (:file "compiler-tests-runtime")
     (:file "compiler-tests-runtime-string-tests")
     (:file "compiler-tests-runtime-hof-tests")
     (:file "compiler-tests-runtime-selfhost")
     (:file "compiler-tests-selfhost")
     (:file "compiler-tests-selfhost-types")
     (:file "call-conv-tests")
     (:file "control-flow-tests")
     (:file "clos-tests")
     (:file "clos-compile-tests")
     (:file "clos-dispatch-tests")
     (:file "stream-tests")
     (:file "predicate-tests")
     (:file "codegen-runtime-tests")
     (:file "pipeline-tests")
     (:file "pipeline-eval-tests")
     (:file "pipeline-repl-tests")
     (:file "standalone-load-tests")
     (:file "closure-pipeline-tests")
     (:file "nan-boxing-tests")
     (:module "pbt"
      :serial t
      :components
      ((:file "package")
       (:file "suite")
       (:file "generators-cl-weave")
       (:file "generators-types")
       (:file "generators-typed-ast")
       (:file "generators-macho")
       (:file "generator-pbt-tests")
       (:file "vm-pbt-tests")
       (:file "cps-pbt-tests")
       (:file "ast-pbt-tests")
       (:file "ast-pbt-extended-tests")
       (:file "macro-pbt-tests")
       (:file "macro-pbt-props-tests")
       (:file "macro-pbt-mv-tests")
       (:file "macro-pbt-hygiene-tests")
       (:file "macro-pbt-binding-tests")
       (:file "macro-pbt-advanced-tests")
       (:file "prolog-pbt-tests")
       (:file "vm-heap-pbt-tests")))))
   (:module "optimize-tests"
    :pathname "packages/optimize/tests"
    :serial t
    :components
    ((:file "optimizer-tables-tests")
     (:file "optimizer-strength-tests")
     (:file "optimizer-licm-tests")
     (:file "optimizer-copyprop-tests")
     (:file "optimizer-strength-ext-tests")
     (:file "optimizer-cse-gvn-tests")
      (:file "optimizer-dataflow-tests")
      (:file "optimizer-memory-tests")
      (:file "optimizer-memory-pass-tests")
      (:file "optimizer-fr-tests")
      (:file "optimizer-flow-tests")
     (:file "optimizer-flow-block-tests")
     (:file "optimizer-pipeline-tests")
     (:file "optimizer-security-tests")
     (:file "optimizer-concurrency-tests")
     (:file "optimizer-roadmap-tests")
     (:file "optimizer-roadmap-backend-tests")
     (:file "optimizer-inline-tests")
     (:file "optimizer-purity-tests")
     (:file "optimizer-inline-pass-tests")
     (:file "optimizer-inline-pass-tests-2")
      (:file "optimizer-inline-pass-ext-tests")
      (:file "optimizer-closure-tests")
     (:file "effects-tests")
     (:file "cfg-tests")
     (:file "ssa-tests")
     (:file "egraph-tests")
     (:file "egraph-extraction-tests")
     (:file "egraph-rules-tests")
     (:file "egraph-negation-tests")
     (:file "egraph-rules-bitwise-tests")
     (:file "optimizer-prolog-peephole-tests")
     (:file "optimizer-tests")
     (:file "optimizer-e2e-tests")
     (:file "optimizer-tests-lowlevel2")
     (:file "optimizer-cfg-inline-tests")
     (:file "optimizer-inlining-tests")
     (:file "optimizer-lowlevel-tests")
     (:file "optimizer-lowlevel-dce-tests")
     (:file "optimizer-dataflow-passes-tests")
      (:file "optimizer-store-analysis-tests")
      (:file "native-advanced-optimizer-tests")
      (:file "optimizer-strength-inline-tests")
      (:file "optimizer-jump-threading-tests")
      (:file "optimizer-loop-peel-tests")
      (:file "optimizer-loop-rotate-tests")
      (:file "optimizer-loop-unroll-tests")
      (:file "optimizer-loop-unswitch-tests")
      (:file "optimizer-dead-loop-tests")
      (:file "optimizer-dae-tests")
      (:file "optimizer-div-const-tests")
      (:file "optimizer-strength-reduce-tests")
      (:file "optimizer-idiom-tests")))
   (:module "emit-tests"
    :pathname "packages/emit/tests"
    :serial t
    :components
    ((:file "mir-tests")
     (:file "mir-target-tests")
     (:file "mir-isel-tests")
     (:file "regalloc-tests")
     (:file "regalloc-lsa-tests")
     (:file "regalloc-color-tests")
     (:file "regalloc-spill-tests")
     (:file "wasm-tests") (:file "wasm-binary-tests")
      (:file "wasm-ir-tests")
      (:file "wasm-types-tests")
      (:file "wasm-opcodes-tests")
      (:file "wasm-features-tests")
      (:file "aarch64-codegen-tests")
     (:file "aarch64-emit-tests")
     (:file "aarch64-encoding-tests")
     (:file "target-tests")
     (:file "wasm-extract-tests")
     (:file "wasm-trampoline-tests")
     (:file "wasm-trampoline-build-tests")
     (:file "x86-64-regs-tests")
     (:file "x86-64-sequences-tests")
     (:file "x86-64-codegen-tests")
     (:file "x86-64-codegen-emitter-tests")
      (:file "x86-64-codegen-insn-tests")
      (:file "x86-64-encoding-tests")
      (:file "x86-64-vm-emitter-tests")
      (:file "x86-64-encoding-size-tests")
       (:file "x86-64-emit-tests")
       (:file "x86-64-emit-logical-tests")
       (:file "x86-64-emit-ops-tests")
         (:file "llvm-ir-tests")
         (:file "mlir-tests")
         (:file "native-advanced-bridge-tests")
       (:file "security-hardening-tests")
        (:file "ppc64-codegen-tests")
       (:file "ebpf-tests")
       (:file "riscv64-tests")
      (:file "elf-tests")
      (:file "elf-extended-tests")
      (:file "macho-tests")
      (:file "macho-builder-tests") (:file "fpga-tests")))
     (:module "binary-tests"
       :pathname "packages/binary/tests"
       :serial t
       :components
        ((:file "binary-buffer-tests")
         (:file "macho-fat-tests")
         (:file "binary-fr-tests")
         (:file "binary-wxorx-tests")
         (:file "binary-constpool-tests")
         (:file "got-plt-tests")
         (:file "patchable-entry-tests")))
     (:module "pipeline-tests"
       :pathname "packages/pipeline/tests"
       :serial t
       :components
       ((:file "perfmap-tests")
        (:file "pipeline-incremental-hot-parallel-tests")))
     (:module "tools-tests"
      :pathname "packages/tools/tests"
      :serial t
      :components
      ((:file "lsp-server-tests")
       (:file "dap-server-tests")))
     (:module "formatter-tests"
      :pathname "packages/formatter/tests"
      :serial t
      :components
      ((:file "formatter-tests")))
     (:module "docgen-tests"
      :pathname "packages/docgen/tests"
      :serial t
      :components
      ((:file "docgen-suite-tests")))
     (:module "runtime-tests"
      :pathname "packages/runtime/tests"
      :serial t
    :components
    ((:file "runtime-tests")
     (:file "runtime-tests-2")
     (:file "runtime-strings-chars-tests")
     (:file "runtime-clos-tests")
     (:file "runtime-advanced-tests")
      (:file "runtime-io-tests")
      (:file "runtime-serialize-tests")
      (:file "gc-tests")
      (:file "gc-fr-tests")
      (:file "gc-stats-tests")
     (:file "gc-sweep-major-tests")
     (:file "heap-tests")
      (:file "heap-sanitizer-tests")
     (:file "heap-gc-tests")
     (:file "heap-trace-tests")
      (:file "gc-write-barrier-tests")
         (:file "runtime-subsystem-fr-tests")
         (:file "runtime-stdlib-2-runtime-tests")
         (:file "runtime-stdlib-3-os-tests")
         (:file "runtime-stdlib-3-image-tests")
        (:file "continuous-profile-tests")
        (:file "deadlock-tests")
       (:file "value-tests")
       (:file "dynlib-tests")
       (:file "frame-tests")))
   (:module "migration-safety"
     :pathname "packages/umbrella-tests"
     :components
     ((:file "migration-safety-tests")))
    (:module "coverage-tests"
     :pathname "t"
     :components
     ((:file "runtime-stdlib-2-coverage-tests")))
     (:module "native-advanced-evidence"
      :pathname "t"
      :serial t
      :components
      ((:file "native-advanced-evidence-tests")
       (:file "tooling-advanced-1-evidence-tests")
       (:file "tooling-advanced-2-evidence-tests")))
    ;; --- ANSI Conformance Tests (expected-fail for known gaps) ---
    (:module "conformance"
     :pathname "t/conformance"
     :serial t
     :components
     (      (:file "package-conformance-tests")
      (:file "package-smoke-tests")
      (:file "format-conformance-tests")
      (:file "number-conformance-tests")
      (:file "native-io-conformance-tests"))))
   :perform (asdf:test-op (op c)
               (declare (ignore op c))
               (uiop:symbol-call :cl-cc/test 'run-tests)))

(asdf:defsystem "cl-cc/test/e2e"
  :description "End-to-end regression tests that compile cl-cc with itself and check the result against the host-compiled build."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :bug-tracker "https://github.com/nerima-lisp/cl-cc/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-cc.git")
  :depends-on ("cl-cc/test")
  :serial t
  :components
  ((:module "e2e"
     :pathname "t/e2e"
     :serial t
     :components
     ((:file "selfhost-test-support")
      (:file "selfhost-tests")
      (:file "selfhost-meta-tests"))))
  :perform (asdf:test-op (op c)
              (declare (ignore op c))
              (uiop:symbol-call :cl-cc/test 'run-suite
                                (uiop:find-symbol* '#:selfhost-suite :cl-cc/test)
                                :parallel nil
                                :random nil)))
