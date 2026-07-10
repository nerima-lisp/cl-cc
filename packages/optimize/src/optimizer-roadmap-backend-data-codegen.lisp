;;;; optimizer-roadmap-backend-data-codegen.lisp -- Code generation, CFG, stack, and tooling evidence
;;;; Split from optimizer-roadmap-backend-data.lisp.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-codegen-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Aggregated in original lookup order by optimizer-roadmap-backend-data.lisp.
  '(
    (306 386
         ("packages/pipeline/src/pipeline.lisp"
          "packages/compile/src/codegen.lisp"
          "packages/vm/src/vm-dispatch.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp")
         (opt-ic-transition opt-record-speculation-failure
          opt-materialize-deopt-state opt-shape-slot-offset
          opt-jit-cache-select-eviction)
         (optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (403 403
         ("packages/optimize/src/ssa-construction.lisp"
          "packages/optimize/src/ssa-phi-elim.lisp"
          "packages/optimize/src/cfg-analysis.lisp"
          "packages/optimize/tests/ssa-tests.lisp")
         (ssa-destroy ssa-sequentialize-copies cfg-split-critical-edges)
         (ssa-destroy-places-phi-copies-before-terminator
          ssa-destroy-keeps-conditional-edge-phi-copy-on-target-edge
          ssa-round-trip-cases))
    (404 404
         ("packages/optimize/src/ssa-construction.lisp"
          "packages/optimize/tests/ssa-tests.lisp")
         (ssa-sequentialize-copies)
         (ssa-seq-copies-behavior
          ssa-round-trip-cases))
    (405 405
           ("packages/optimize/src/cfg-analysis.lisp"
            "packages/optimize/src/cfg.lisp"
          "packages/optimize/src/package.lisp"
          "packages/optimize/src/optimizer-pipeline-roadmap.lisp"
          "packages/optimize/tests/cfg-tests.lisp"
           "packages/optimize/tests/optimizer-roadmap-tests.lisp")
          (cfg-split-critical-edges)
          (cfg-critical-edge-splitting-inserts-landing-pad
           optimize-roadmap-pipeline-includes-modern-optimization-passes))
    (388 388
         ("packages/codegen/src/x86-64-regs.lisp"
          "packages/codegen/src/x86-64-codegen-core.lisp"
          "packages/codegen/src/x86-64-codegen-dispatch.lisp"
          "packages/codegen/src/x86-64-encoding.lisp"
          "packages/codegen/src/aarch64-codegen.lisp"
          "packages/codegen/src/aarch64-program.lisp"
          "packages/codegen/src/aarch64-emitters.lisp"
          "packages/cli/src/args.lisp"
          "packages/cli/src/handlers.lisp"
          "packages/emit/tests/x86-64-codegen-tests.lisp"
          "packages/emit/tests/x86-64-encoding-size-tests.lisp"
          "packages/emit/tests/x86-64-encoding-tests.lisp"
          "packages/emit/tests/aarch64-codegen-tests.lisp"
          "packages/cli/tests/args-tests.lisp"
          "packages/cli/tests/cli-tests.lisp")
         (("CL-CC/CODEGEN" . "*X86-64-OMIT-FRAME-POINTER*")
          ("CL-CC/CODEGEN" . "X86-64-CODEGEN-TARGET")
          ("CL-CC/CODEGEN" . "X86-64-MEMORY-MOD")
          ("CL-CC/CODEGEN" . "*A64-OMIT-FRAME-POINTER*")
          ("CL-CC/CODEGEN" . "A64-CODEGEN-TARGET"))
         (x86-64-fpe-codegen-target-frees-rbp
          x86-64-empty-program-minimal-return-byte
          x86-64-leaf-and-nonleaf-without-spills-share-fpe-layout
          x86-vm-program-default-fpe-allocates-rsp-spill-frame
          x86-vm-program-debug-opt-out-keeps-rbp-spills
          x86-mov-memory-displacement-widths
          aarch64-fpe-codegen-target-frees-x29
          aarch64-leaf-and-nonleaf-without-spills-share-fpe-layout
          aarch64-default-fpe-uses-sp-relative-spill-frame
          aarch64-debug-opt-out-keeps-fp-lr-pair-and-fp-spills
          |ARGS-TESTS/CLI-ARGS-BOOL-FLAGS [debug]|
          cli-do-compile-debug-binds-backend-frame-pointer-switches))
    (389 389
         ("packages/codegen/src/x86-64-regs.lisp"
          "packages/compile/src/codegen-locals.lisp"
          "packages/emit/src/package.lisp"
          "packages/emit/tests/x86-64-regs-tests.lisp"
          "packages/emit/tests/x86-64-encoding-size-tests.lisp"
          "packages/emit/tests/x86-64-emit-tests.lisp")
         (("CL-CC/CODEGEN" . "X86-64-RED-ZONE-SPILL-P"))
         (x86-64-regs-red-zone-spill-leaf-within-limit-returns-true
          x86-vm-program-leaf-red-zone-spills-skip-rbp-frame
          x86-emit-spill-operations-rsp-red-zone))
    (391 391
         ("packages/codegen/src/x86-64-codegen-core.lisp"
          "packages/codegen/src/aarch64-program.lisp"
          "packages/emit/tests/x86-64-encoding-size-tests.lisp"
          "packages/emit/tests/aarch64-codegen-tests.lisp")
         (("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-PROBES")
          ("CL-CC/CODEGEN" . "EMIT-A64-STACK-PROBES"))
         (x86-stack-probe-count-thresholds
          x86-stack-probe-emits-non-mutating-rsp-page-touch
          x86-large-spill-frame-inserts-stack-probe-before-rsp-allocation
           aarch64-stack-probe-emits-page-touch-sequence
           aarch64-large-spill-frame-inserts-stack-probe-before-prologue-when-fpe-disabled))
    (400 400
         ("packages/expand/src/macros-control-flow-case.lisp"
          "packages/expand/src/macros-stdlib.lisp"
          "packages/expand/tests/macros-control-flow-loop-tests.lisp"
          "packages/expand/tests/macro-ecase-tests.lisp"
          "packages/expand/tests/macro-etypecase-tests.lisp")
         (("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES")
          ("CL-CC/EXPAND" . "%TYPECASE-BUILD-TYPEP-CHAIN"))
         (case-expands-dense-integer-keys-into-table-dispatch
          ecase-expands-to-let-with-case
          etypecase-expands-to-let-with-typecase))
    (401 401
         ("packages/expand/src/macros-control-flow-case.lisp"
          "packages/expand/src/macros-stdlib.lisp"
          "packages/expand/tests/macros-control-flow-loop-tests.lisp"
          "packages/expand/tests/macro-ecase-tests.lisp"
          "packages/expand/tests/macro-etypecase-tests.lisp")
         (("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TABLE")
          ("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES"))
         (case-expands-dense-integer-keys-into-table-dispatch
          ecase-expands-to-let-with-case
          etypecase-expands-to-let-with-typecase))
    (462 462
         ("packages/cli/src/main-utils.lisp"
          "packages/cli/src/main-dump.lisp"
          "packages/cli/src/handlers.lisp"
          "packages/cli/tests/main-utils-tests.lisp"
          "packages/cli/tests/main-dump-tests.lisp")
         (("CL-CC/CLI" . "%WRITE-FLAMEGRAPH-SVG")
          ("CL-CC/CLI" . "%PARSE-COMPILE-OPTS")
          ("CL-CC/CLI" . "COMPILE-OPTS-FLAMEGRAPH-PATH"))
         (cli-write-flamegraph-svg-emits-svg-document
          cli-parse-compile-opts-reads-shared-flags))
    (463 463
         ("packages/cli/src/main-dump.lisp"
          "packages/cli/src/main-utils.lisp"
          "packages/cli/src/args.lisp"
          "packages/cli/src/handlers.lisp"
          "packages/pipeline/src/pipeline.lisp"
          "packages/cli/tests/main-dump-tests.lisp"
          "packages/cli/tests/cli-tests.lisp")
         (("CL-CC/CLI" . "%DUMP-IR-PHASE")
          ("CL-CC/CLI" . "%DUMP-AST-PHASE")
          ("CL-CC/CLI" . "%DUMP-CPS-PHASE")
          ("CL-CC/CLI" . "%DUMP-SSA-PHASE")
          ("CL-CC/CLI" . "%DUMP-VM-PHASE")
          ("CL-CC/CLI" . "%DUMP-OPT-PHASE")
          ("CL-CC/CLI" . "%DUMP-ASM-PHASE")
          ("CL-CC/CLI" . "*IR-PHASE-DUMP-FNS*")
          ("CL-CC/CLI" . "*IR-PHASES*"))
         (cli-dump-ir-phase-dispatches-all-phases
          cli-dump-ir-phase-annotate-source-writes-comment-for-ast
          cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt
          cli-dump-ir-phase-asm-output-is-ansi-colored
          cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location
          cli-real-file-dump-ir-annotation-preserves-source-location
          cli-do-compile-dump-ir-annotate-source-preserves-real-file-location
          cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location
          cli-dump-ir-phase-phase-table-covers-all-recognized-phases
          cli-dump-ir-phase-invalid-signals-error))
    (465 465
          ("packages/pipeline/src/pipeline-native.lisp"
           "packages/compile/tests/pipeline-native-tests.lisp"
           "packages/compile/tests/pipeline-native-io-tests.lisp")
          (("CL-CC/PIPELINE" . "%COMPILE-CACHE-KEY")
           ("CL-CC/PIPELINE" . "%COMPILE-CACHE-PATH")
           ("CL-CC/PIPELINE" . "COMPILE-FILE-TO-NATIVE"))
          (pipeline-native-compile-file-cache-hit-copies-artifact
           pipeline-native-compile-file-cache-hit-skips-native-compilation
           pipeline-native-cache-key-differs-by-dimension
           pipeline-native-cache-key-ignores-observability-options
           pipeline-native-compile-file-cache-key-receives-option-plist))
    (387 502
          ("packages/codegen/src/x86-64-codegen-core.lisp"
           "packages/codegen/src/aarch64-codegen.lisp"
           "packages/regalloc/src/regalloc.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp")
         (opt-sea-node-schedulable-p opt-merge-module-summaries
          opt-stack-map-live-root-p opt-shape-slot-offset
          opt-adaptive-compilation-threshold)
         (optimize-roadmap-support-helpers-have-conservative-behavior))
    )
  "Roadmap evidence profile ranges for the codegen backend data slice.")
