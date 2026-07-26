;;;; optimizer-roadmap-backend-data-foundation.lisp -- Foundational and promoted backend feature evidence
;;;; Split from optimizer-roadmap-backend-data.lisp.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-foundation-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Aggregated in original lookup order by optimizer-roadmap-backend-data.lisp.
  '(
    (16 16
         ("packages/optimize/src/optimizer-memory-alias.lisp"
          "packages/optimize/src/optimizer-memory-forward.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-memory-pass-tests.lisp"
          "packages/optimize/tests/optimizer-store-analysis-tests.lisp")
         (opt-compute-heap-aliases opt-pass-dead-store-elim)
         (dead-store-elim-overwrite-without-read-removes-earlier-store
          dead-store-elim-read-between-stores-preserves-both
          dead-store-elim-store-reaching-ret-is-preserved))
    (9 9
         ("packages/optimize/src/optimizer-roadmap-backend.lisp"
           "packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp"
           "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (make-opt-ic-site opt-ic-site-state opt-ic-transition opt-ic-resolve-target)
         (optimize-ic-resolve-target-prefers-site-local-entry
          optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (8 8
         ("packages/regalloc/src/regalloc.lisp"
          "packages/emit/tests/regalloc-tests.lisp")
         (("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS")
          ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
         (regalloc-float-vregs-allocated-to-distinct-xmm-registers))
    (19 19
         ("packages/optimize/src/optimizer-roadmap-backend.lisp"
           "packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp"
           "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (make-opt-ic-site opt-ic-site-megamorphic-fallback opt-ic-transition
          make-opt-megamorphic-cache opt-mega-cache-put opt-mega-cache-get opt-ic-resolve-target)
         (optimize-ic-resolve-target-uses-shared-megamorphic-cache
          optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (17 17
         ("packages/optimize/src/optimizer-memory-alias.lisp"
          "packages/optimize/src/optimizer-memory-alias-basic.lisp"
          "packages/optimize/tests/optimizer-lowlevel-tests.lisp"
          "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
         (opt-compute-heap-aliases opt-compute-heap-kinds opt-may-alias-by-type-p)
         (heap-kind-helper-distinguishes-object-classes
          heap-root-kind-table-integrity-and-lookup))
    (14 14
          ("packages/compile/src/codegen-core.lisp"
           "packages/compile/src/codegen.lisp"
           "packages/optimize/src/optimizer-memory-dse.lisp"
           "packages/optimize/src/optimizer-pipeline.lisp"
           "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
          (opt-pass-cons-slot-forward)
          (cons-slot-forward-replaces-car-with-original-car-register
           cons-slot-forward-replaces-cdr-through-move-alias
           cons-slot-forward-source-overwrite-kills-fact
           cons-slot-forward-rplaca-kills-fact
           cons-slot-forward-cons-overwriting-source-is-conservative))
    (15 15
         ("packages/optimize/src/optimizer-flow-loop.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-flow-tests.lisp")
          (opt-pass-code-sinking)
          (code-sinking-moves-const-into-target-block
           code-sinking-noop-when-value-is-read-multiple-times
           code-sinking-moves-cons-into-target-block
           code-sinking-noop-for-cons-read-multiple-times))
    (216 216
          ("packages/optimize/src/optimizer-memory-alias.lisp"
           "packages/optimize/src/optimizer-memory-forward.lisp"
           "packages/optimize/src/optimizer-memory-alias-basic.lisp"
           "packages/optimize/src/optimizer-pipeline.lisp"
           "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
          (opt-pass-store-to-load-forward opt-compute-heap-aliases opt-must-alias-p)
          (store-to-load-forward-prior-store-replaces-get-global
           store-to-load-forward-cross-block-dominating-store-replaces-get-global
           store-to-load-forward-prior-slot-write-replaces-slot-read
           store-to-load-forward-no-prior-store-preserves-get-global
           store-to-load-forward-join-disagree-preserves-get-global))
    (217 217
          ("packages/optimize/src/optimizer-memory-alias.lisp"
           "packages/optimize/tests/optimizer-memory-tests.lisp")
          (opt-compute-memory-ssa-snapshot opt-memory-ssa-version-at)
          (memory-ssa-snapshot-assigns-monotonic-versions-for-def-use-chain
           memory-ssa-snapshot-slot-location-uses-alias-root))
    (218 218
          ("packages/vm/src/vm-numeric.lisp"
           "packages/vm/tests/vm-numeric-tests.lisp")
          (("CL-CC/VM" . "VM-BIGNUM-DIGIT-VECTOR")
           ("CL-CC/VM" . "VM-BIGNUM-SCHOOLBOOK-MULTIPLY-DIGITS")
           ("CL-CC/VM" . "VM-BIGNUM-MULTIPLICATION-STRATEGY")
           ("CL-CC/VM" . "VM-BIGNUM-MULTIPLY-PLAN"))
          (vm-bignum-digit-vector-splits-little-endian-digits
           vm-bignum-schoolbook-multiply-digits-computes-product-digits
           vm-bignum-multiplication-strategy-selects-thresholded-plan
           vm-bignum-multiply-plan-records-digits-sign-and-strategy))
    (219 219
          ("packages/vm/src/vm-numeric.lisp"
           "packages/vm/tests/vm-numeric-tests.lisp")
          (("CL-CC/VM" . "VM-COMPLEX-UNBOX-PLAN")
           ("CL-CC/VM" . "VM-COMPLEX-UNBOXED-ADD-PLAN"))
          (vm-complex-unbox-plan-splits-local-complex
           vm-complex-unbox-plan-keeps-escaping-complex-boxed
           vm-complex-unboxed-add-plan-adds-components))
    (251 251
          ("packages/optimize/src/optimizer-dataflow.lisp"
           "packages/optimize/tests/optimizer-dataflow-tests.lisp")
          (make-opt-abstract-domain opt-run-abstract-interpretation)
          (abstract-domain-struct-retains-operators
           abstract-interpretation-runs-over-cfg-and-produces-result))
    (252 252
          ("packages/regalloc/src/regalloc.lisp"
           "packages/regalloc/src/regalloc-allocate.lisp"
           "packages/emit/tests/regalloc-tests.lisp")
          (("CL-CC/REGALLOC" . "REGALLOC-BUILD-DIRECT-CALL-GRAPH")
           ("CL-CC/REGALLOC" . "REGALLOC-COMPUTE-INTERPROCEDURAL-HINTS")
           ("CL-CC/REGALLOC" . "REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS")
           ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
          (regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain
           regalloc-interprocedural-policy-hook-derives-preferences
           regalloc-interprocedural-policy-caller-saved-respects-call-crossing-safety
           regalloc-interprocedural-policy-end-to-end-keeps-call-crossing-safe
           regalloc-interprocedural-policy-prefers-callee-saved-on-call-crossing))
    (253 253
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp")
         (make-opt-cow-object opt-cow-copy opt-cow-write)
          (optimize-cow-copy-is-constant-time-share
           optimize-cow-write-detaches-when-shared))
    (254 254
           ("packages/optimize/src/optimizer-speculative-peval.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (make-opt-bump-region opt-bump-allocate opt-bump-mark opt-bump-reset
            make-opt-slab-pool opt-slab-allocate opt-slab-free)
           (optimize-bump-region-mark-reset-restores-cursor
            optimize-slab-pool-reuses-freed-object))
    (297 297
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (opt-pgo-best-successor opt-pgo-build-hot-chain opt-pgo-rotate-loop)
           (optimize-pgo-build-hot-chain-prefers-hottest-successors
            optimize-pgo-rotate-loop-places-preferred-exit-at-bottom))
    (295 295
           ("packages/optimize/src/optimizer-speculative-peval.lisp"
            "packages/pipeline/src/pipeline.lisp"
            "packages/compile/src/codegen.lisp"
            "packages/cli/src/main-utils.lisp"
            "packages/cli/src/handlers.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp"
            "packages/compile/tests/pipeline-tests.lisp"
            "packages/cli/tests/cli-tests.lisp")
           (opt-pgo-build-counter-plan
            opt-pgo-make-profile-template
            ("CL-CC/COMPILE" . "COMPILATION-RESULT-PGO-COUNTER-PLAN"))
           (optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids
            optimize-pgo-make-profile-template-zero-initializes-counts
            pipeline-compile-string-emits-pgo-counter-plan
            cli-maybe-make-profiled-vm-state-enabled-for-pgo-generate
            cli-write-pgo-profile-emits-file))
    (301 301
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (make-opt-module-summary opt-merge-module-summaries opt-thinlto-should-import-p)
           (optimize-merge-module-summaries-aggregates-exports-and-counts
            optimize-thinlto-import-decision-respects-budget-linkage-and-cycles))
    (310 310
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (opt-adaptive-compilation-threshold opt-tier-transition)
           (optimize-adaptive-compilation-threshold-reacts-to-warmup-pressure-and-failures
            optimize-tier-transition-promotes-through-runtime-tiers))
    (311 311
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-osr-point opt-osr-trigger-p opt-osr-materialize-entry)
          (optimize-osr-trigger-p-uses-hotness-threshold
           optimize-osr-materialize-entry-maps-machine-to-vm-registers))
    (312 312
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-deopt-frame opt-materialize-deopt-state)
          (optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers))
    (444 444
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-shape-descriptor-for-slots opt-shape-slot-offset)
          (optimize-shape-descriptor-slots-map-to-stable-offsets))
    (445 445
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-shape-transition-cache opt-shape-transition-put opt-shape-transition-get)
          (optimize-shape-transition-cache-stores-forward-transitions))
    (446 446
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-ic-patch-plan opt-ic-make-patch-plan)
          (optimize-ic-make-patch-plan-classifies-state-transitions))
    (447 447
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (opt-build-inline-polymorphic-dispatch)
          (optimize-build-inline-polymorphic-dispatch-builds-guard-chain))
    (449 449
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-async-state-machine opt-build-async-state-machine)
          (optimize-build-async-state-machine-builds-linear-transitions))
    (450 450
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (opt-choose-coroutine-lowering-strategy)
          (optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed))
    (451 451
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-channel-site opt-channel-select-path opt-channel-should-jump-table-select-p)
          (optimize-channel-select-path-classifies-buffered-sync-and-contended-cases
           optimize-channel-jump-table-select-threshold))
    (452 452
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-stm-plan opt-stm-build-plan opt-stm-needs-log-p)
          (optimize-stm-plan-skips-log-for-pure-block
           optimize-stm-plan-enables-log-for-impure-read-write))
    (453 453
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-lockfree-plan opt-lockfree-select-reclamation opt-lockfree-build-plan)
          (lockfree-reclamation-cases))
    (315 315
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/codegen/src/x86-64-codegen-core.lisp"
            "packages/codegen/src/aarch64-codegen.lisp"
            "packages/emit/tests/x86-64-codegen-tests.lisp"
            "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
            "packages/emit/tests/aarch64-codegen-tests.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (make-opt-cfi-plan opt-build-cfi-plan opt-cfi-entry-opcode
            ("CL-CC/CODEGEN" . "X86-64-CFI-PLAN")
            ("CL-CC/CODEGEN" . "EMIT-X86-64-CFI-ENTRY")
            ("CL-CC/CODEGEN" . "AARCH64-CFI-PLAN")
            ("CL-CC/CODEGEN" . "EMIT-AARCH64-CFI-ENTRY"))
           (optimize-build-cfi-plan-selects-target-specific-guards
            x86-64-cfi-entry-emits-endbr64-bytes
            x86-64-program-with-indirect-call-starts-with-endbr64
            x86-64-call-cfi-guard-avoids-clobbering-rax-target
            aarch64-cfi-entry-emits-bti-c-bytes
            aarch64-program-with-indirect-call-starts-with-bti-c))
    (316 316
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/codegen/src/x86-64-codegen-dispatch.lisp"
            "packages/codegen/src/x86-64-regs.lisp"
            "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
            "packages/cli/src/args.lisp"
            "packages/cli/src/main-dump.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (opt-should-use-retpoline-p opt-retpoline-thunk-name
            ("CL-CC/CODEGEN" . "EMIT-VM-CALL-LIKE-INST")
            ("CL-CC/CODEGEN" . "EMIT-VM-TAIL-CALL-INST")
            ("CL-CC/CODEGEN" . "*X86-64-USE-RETPOLINE*"))
           (optimize-retpoline-thunk-name-is-target-register-specific
            x86-64-call-encoding-retpoline
            x86-64-tail-call-encoding-retpoline))
    (317 317
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/codegen/src/x86-64-codegen-core.lisp"
            "packages/emit/tests/x86-64-codegen-tests.lisp"
            "packages/cli/src/args.lisp"
            "packages/cli/src/main-dump.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (opt-needs-stack-canary-p opt-stack-canary-emit-plan
            opt-stack-canary-prologue-seq opt-stack-canary-epilogue-seq
            ("CL-CC/CODEGEN" . "X86-64-STACK-CANARY-PLAN")
            ("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-CANARY-PROLOGUE")
            ("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-CANARY-EPILOGUE"))
           (optimize-needs-stack-canary-p-follows-stack-buffer-presence
            optimize-stack-canary-emit-plan-describes-prologue-and-epilogue
            optimize-stack-canary-sequences-describe-prologue-and-epilogue-ops
            optimize-stack-canary-sequences-are-empty-when-disabled
            x86-64-stack-canary-plan-materializes-prologue-and-epilogue
            x86-64-stack-protector-emitter-signature-bytes))
    (318 318
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/codegen/src/x86-64-codegen-core.lisp"
            "packages/codegen/src/x86-64-codegen-dispatch.lisp"
            "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp")
           (make-opt-shadow-stack-plan opt-build-shadow-stack-plan
            ("CL-CC/CODEGEN" . "EMIT-VM-SHADOW-STACK-CONTROL-INST")
            ("CL-CC/CODEGEN" . "*X86-64-SHADOW-STACK-ENABLED*"))
           (optimize-build-shadow-stack-plan-enables-only-for-x86-64-with-cet
            optimize-shadow-stack-plan-requires-save-restore-for-nonlocal-control
            x86-64-shadow-stack-control-inst-emits-saveprevssp-when-enabled
            x86-64-shadow-stack-control-inst-uses-distinct-restore-marker
            x86-64-shadow-stack-control-inst-uses-incsspq-for-adjust-paths
            x86-64-shadow-stack-control-inst-covers-vm-establish-catch
            x86-64-shadow-stack-control-inst-covers-vm-throw))
    (320 320
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/codegen/src/wasm-emit-instrs.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp"
           "packages/emit/tests/wasm-tests.lisp")
          (opt-wasm-select-tailcall-opcode
           opt-wasm-select-direct-tailcall-opcode
           make-opt-wasm-tailcall-plan
           opt-build-wasm-tailcall-plan)
          (wasm-tail-call-dispatch-uses-return-call-indirect
           wasm-tail-call-direct-path-uses-return-call-when-callee-known))
    (321 321
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-wasm-gc-layout
           opt-build-wasm-gc-layout
           opt-wasm-gc-layout-valid-p
           opt-wasm-gc-runtime-host-compatible-p
           opt-build-wasm-gc-optimization-plan)
          (optimize-build-wasm-gc-layout-preserves-kind-and-fields
           optimize-wasm-gc-layout-validates-struct-and-array-shapes
           optimize-wasm-gc-runtime-host-compatibility-requires-feature-and-valid-layout
           optimize-wasm-gc-optimization-plan-reflects-layout-kind))
    (330 330
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-debug-loc opt-build-dwarf-line-row)
          (optimize-build-dwarf-line-row-preserves-location-fields))
    (331 331
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (make-opt-debug-loc opt-build-wasm-source-map-entry)
          (optimize-build-wasm-source-map-entry-preserves-offset-and-source))
    (333 333
          ("packages/optimize/src/optimizer-speculative-ic.lisp"
           "packages/optimize/tests/optimizer-pipeline-tests.lisp")
          (opt-format-diagnostic-reason)
          (optimize-format-diagnostic-reason-renders-rpass-like-message))
    (335 335
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp"
            "packages/codegen/src/x86-64-codegen-core.lisp"
            "packages/codegen/src/aarch64-codegen.lisp")
           (make-opt-tls-plan opt-build-tls-plan)
           (optimize-build-tls-plan-selects-architecture-specific-base-register))
    (336 336
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp"
            "packages/codegen/src/x86-64-codegen-core.lisp"
            "packages/codegen/src/aarch64-codegen.lisp")
           (make-opt-atomic-plan opt-select-atomic-opcode opt-build-atomic-plan)
           (optimize-select-atomic-opcode-reflects-target-and-operation))
    (337 337
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp"
            "packages/vm/src/hash.lisp"
            "packages/vm/src/hash-execute.lisp"
            "packages/vm/tests/hash-tests.lisp")
           (make-opt-htm-plan opt-build-htm-plan)
           (optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention
            hash-lock-elision-wrapper-falls-back-after-abort
            hash-lock-elision-disabled-after-abort-threshold))
    (338 338
           ("packages/optimize/src/optimizer-speculative-ic.lisp"
            "packages/optimize/tests/optimizer-pipeline-tests.lisp"
             "packages/runtime/src/gc-major-sweep.lisp"
            "packages/runtime/src/gc-write-barrier.lisp"
            "packages/runtime/tests/gc-sweep-major-tests.lisp"
            "packages/runtime/tests/gc-write-barrier-tests.lisp")
           (make-opt-concurrent-gc-plan opt-build-concurrent-gc-plan)
           (optimize-build-concurrent-gc-plan-selects-satb-and-short-stw-for-latency-sensitive-mode
            gc-configure-concurrent-mode-updates-runtime-flags
            gc-major-collect-enters-concurrent-state-when-enabled
            gc-concurrent-assist-marks-satb-old-pointers-with-budget
            gc-write-barrier-satb-snapshot-major-gc-concurrent-black-object))
    (209 209
           ("packages/optimize/src/optimizer-inline-cost.lisp"
             "packages/optimize/src/optimizer.lisp"
             "packages/optimize/src/optimizer-speculative-peval.lisp"
             "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp")
          (opt-pass-inline opt-pass-fold opt-specialize-constant-args)
          (opt-pass-inline-propagates-constant-argument-into-inlined-body
           optimize-specialize-constant-args-builds-residual-body))
    (210 210
           ("packages/optimize/src/optimizer-inline-cost.lisp"
            "packages/optimize/src/optimizer-dataflow-sccp.lisp"
            "packages/optimize/src/optimizer-speculative-peval.lisp"
            "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp")
            (opt-pass-inline opt-pass-sccp opt-sccp-analyze-binding-times)
            (opt-pass-inline-propagates-constant-argument-into-inlined-body
             optimize-sccp-analyze-binding-times-classifies-lattice-values))
    (211 211
           ("packages/optimize/src/optimizer-inline.lisp"
            "packages/optimize/src/optimizer-inline-cost.lisp"
             "packages/optimize/src/optimizer-speculative-peval.lisp"
             "packages/optimize/tests/optimizer-inline-tests.lisp"
             "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp")
            (opt-pass-devirtualize opt-pass-inline opt-build-specialization-plan)
            (opt-pass-devirtualize-is-idempotent-for-already-direct-call
             opt-pass-inline-inlines-eligible-call
             optimize-build-specialization-plan-reuses-cache-for-constant-signature))
    )
  "Roadmap evidence profile ranges for the foundation backend data slice.")
