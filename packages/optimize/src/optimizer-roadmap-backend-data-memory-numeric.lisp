;;;; optimizer-roadmap-backend-data-memory-numeric.lisp -- Memory, numeric, and allocation backend evidence
;;;; Split from optimizer-roadmap-backend-data.lisp.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-memory-numeric-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Aggregated in original lookup order by optimizer-roadmap-backend-data.lisp.
  '(
    (236 236
         ("packages/expand/src/macros-control-flow-case.lisp"
          "packages/expand/tests/macros-control-flow-loop-tests.lisp")
         (("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TREE")
          ("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TABLE")
          ("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES")
          ("CL-CC/EXPAND" . "%TYPECASE-BUILD-TYPEP-CHAIN"))
         (case-expands-sparse-integer-keys-into-binary-search
          case-expands-dense-integer-keys-into-table-dispatch
          typecase-prunes-subsumed-later-clause
           case-collect-integer-pairs-extracts-default
          typecase-build-typep-chain-single))
    (255 255
         ("packages/vm/src/list.lisp"
          "packages/vm/src/list-execute.lisp"
          "packages/vm/src/exports-runtime.lisp"
          "packages/vm/src/exports-instructions-constructors-core.lisp"
          "packages/compile/src/builtin-registry-data-ext.lisp"
          "packages/expand/src/expander-data.lisp"
          "packages/vm/tests/list-tests.lisp"
          "packages/compile/tests/builtin-registry-data-ext-tests.lisp"
          "packages/compile/tests/pipeline-eval-tests.lisp")
         (vm-hash-cons vm-clear-hash-cons-table make-vm-hash-cons)
         (vm-hash-cons-behavior
          vm-hash-cons-instruction-reuses-identical-flat-pairs
          builtin-binary-custom-representative-entries
          pipeline-run-string-hash-cons-reuses-flat-pairs))
    (256 256
         ("packages/optimize/src/optimizer-inline-pass.lisp"
          "packages/optimize/src/optimizer-purity.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-inline-pass-tests.lisp"
          "packages/optimize/tests/optimizer-purity-tests.lisp")
         (opt-make-pure-function-memo-table opt-pure-function-memo-get
          opt-pure-function-memo-put opt-pass-pure-call-optimization)
         (opt-memo-roundtrip
          opt-memo-put-ignores-impure-label
          opt-pass-pure-call-reuses-repeated-known-direct-call
          opt-pass-pure-call-removes-dead-known-direct-call
          optimize-instructions-pass-pipeline-runs-pure-call-optimization))
    (185 256
         ("packages/optimize/src/optimizer-inline.lisp"
          "packages/optimize/src/optimizer-memory-alias.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/vm/src/list.lisp"
          "packages/vm/tests/vm-tests.lisp")
         (opt-profile-record-call-chain opt-profile-record-allocation
          opt-bump-allocate opt-slab-allocate opt-merge-module-summaries)
         (optimize-backend-roadmap-support-evidence-has-behavior))
    (282 282
          ("packages/optimize/src/optimizer-strength.lisp"
           "packages/optimize/src/optimizer-pipeline.lisp"
           "packages/optimize/tests/optimizer-strength-tests.lisp")
         (opt-pass-strength-reduce opt-power-of-2-p
          %opt-find-verified-reciprocal-div-params
          %opt-div-by-verified-reciprocal-seq)
         (fr-282-div-by-2-emits-ash-neg-1
          fr-282-div-by-256-emits-ash-neg-8
           fr-282-div-by-3-bounded-nonnegative-dividend-emits-reciprocal-seq
           fr-282-div-by-7-bounded-nonnegative-dividend-emits-reciprocal-seq
           fr-282-div-by-3-unknown-dividend-not-transformed
           fr-282-div-by-7-unknown-dividend-not-transformed
           fr-282-div-by-3-negative-dividend-transformed-when-bounded
           fr-282-div-by-3-bounded-negative-dividend-emits-reciprocal-seq
           fr-282-div-by-7-bounded-mixed-sign-dividend-emits-reciprocal-seq
           fr-282-div-by-0-not-transformed
           fr-282-div-by-negative-not-transformed
           fr-282-div-non-constant-rhs-not-transformed))
    (283 283
          ("packages/vm/src/vm-instructions.lisp"
           "packages/vm/src/vm-bitwise.lisp"
           "packages/codegen/src/x86-64-encoding-instrs.lisp"
           "packages/codegen/src/x86-64-sequences.lisp"
           "packages/codegen/src/x86-64-emit-ops.lisp"
           "packages/codegen/src/x86-64-codegen-dispatch.lisp"
           "packages/codegen/src/x86-64-codegen-core.lisp"
           "packages/codegen/src/aarch64-codegen.lisp"
           "packages/codegen/src/aarch64-emitters.lisp"
           "packages/codegen/src/aarch64-program.lisp"
           "packages/codegen/src/aarch64-codegen-labels.lisp"
           "packages/vm/tests/vm-bitwise-tests.lisp"
           "packages/emit/tests/x86-64-encoding-tests.lisp"
           "packages/emit/tests/x86-64-sequences-tests.lisp"
           "packages/emit/tests/x86-64-emit-ops-tests.lisp"
           "packages/emit/tests/aarch64-encoding-tests.lisp"
           "packages/emit/tests/aarch64-codegen-tests.lisp"
           "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
          (("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-U")
           ("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-S")
           ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-U")
           ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-S")
           ("CL-CC/CODEGEN" . "EMIT-MUL-RM64")
           ("CL-CC/CODEGEN" . "EMIT-IMUL-RM64")
           ("CL-CC/CODEGEN" . "EMIT-MUL-HIGH-SEQUENCE")
           ("CL-CC/CODEGEN" . "EMIT-VM-INTEGER-MUL-HIGH-U")
           ("CL-CC/CODEGEN" . "EMIT-VM-INTEGER-MUL-HIGH-S")
           ("CL-CC/CODEGEN" . "ENCODE-UMULH")
           ("CL-CC/CODEGEN" . "ENCODE-SMULH")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-INTEGER-MUL-HIGH-U")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-INTEGER-MUL-HIGH-S"))
          (vm-mul-high-64-semantics
           x86-mul-rm64-high-encodings
           x86-seq-mul-high-sequence-encodings
           x86-emit-mul-high-emits-19-bytes
           x86-mul-high-size-and-dispatch-registered
           a64-mul-high-encoders
           aarch64-mul-high-emitter-encodings
           aarch64-mul-high-size-and-dispatch-registered
           optimize-backend-roadmap-fr-283-has-specific-evidence))
    (284 284
          ("packages/optimize/src/optimizer-recognition.lisp"
           "packages/optimize/src/optimizer-pipeline.lisp"
           "packages/codegen/src/aarch64-emitters.lisp"
          "packages/codegen/src/aarch64-codegen-labels.lisp"
          "packages/codegen/src/aarch64-program.lisp"
          "packages/emit/tests/x86-64-emit-ops-tests.lisp"
          "packages/emit/tests/aarch64-codegen-tests.lisp"
          "packages/optimize/tests/optimizer-store-analysis-tests.lisp"
          "packages/optimize/tests/optimizer-strength-tests.lisp")
         (opt-pass-rotate-recognition opt-rotate-recognition-match-at)
         (aarch64-rotate-emitter-encoding
          rotate-recognition-collapses-shift-or-tree
          rotate-recognition-collapses-rotate-idiom))
    (285 285
         ("packages/optimize/src/optimizer-recognition.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/codegen/src/aarch64-emitters.lisp"
          "packages/codegen/src/aarch64-codegen-labels.lisp"
          "packages/codegen/src/aarch64-program.lisp"
          "packages/emit/tests/aarch64-codegen-tests.lisp"
          "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
          "packages/optimize/tests/optimizer-store-analysis-tests.lisp")
         (opt-pass-bswap-recognition opt-bswap-recognition-match-at)
         (aarch64-bswap-emitter-encoding
          bswap-recognition-collapses-byte-swap-tree))
    (286 286
         ("packages/vm/src/vm-transcendental.lisp"
           "packages/codegen/src/x86-64-encoding-instrs.lisp"
           "packages/codegen/src/x86-64-emit-ops.lisp"
           "packages/codegen/src/x86-64-regs.lisp"
           "packages/codegen/src/x86-64-codegen-dispatch.lisp"
           "packages/codegen/src/x86-64-codegen-core.lisp"
          "packages/codegen/src/aarch64-codegen.lisp"
          "packages/codegen/src/aarch64-emitters.lisp"
          "packages/codegen/src/aarch64-program.lisp"
          "packages/codegen/src/aarch64-codegen-labels.lisp"
          "packages/emit/tests/x86-64-encoding-tests.lisp"
          "packages/emit/tests/x86-64-emit-ops-tests.lisp"
          "packages/emit/tests/x86-64-codegen-tests.lisp"
          "packages/emit/tests/aarch64-encoding-tests.lisp"
          "packages/emit/tests/aarch64-codegen-tests.lisp")
          (("CL-CC" . "MAKE-VM-SQRT")
           ("CL-CC/CODEGEN" . "EMIT-SQRTSD-XX")
           ("CL-CC/CODEGEN" . "EMIT-VM-SQRT")
           ("CL-CC/CODEGEN" . "EMIT-VM-SIN")
           ("CL-CC/CODEGEN" . "EMIT-VM-COS")
           ("CL-CC/CODEGEN" . "EMIT-VM-EXP")
           ("CL-CC/CODEGEN" . "EMIT-VM-LOG")
           ("CL-CC/CODEGEN" . "EMIT-VM-TAN")
           ("CL-CC/CODEGEN" . "EMIT-VM-ASIN")
           ("CL-CC/CODEGEN" . "EMIT-VM-ACOS")
           ("CL-CC/CODEGEN" . "EMIT-VM-ATAN")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-SIN")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-COS")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-EXP")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-LOG")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-TAN")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-ASIN")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-ACOS")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-ATAN")
           ("CL-CC/CODEGEN" . "ENCODE-FSQRT")
           ("CL-CC/CODEGEN" . "EMIT-A64-VM-SQRT"))
          (x86-xmm-instruction-encoding
           x86-emit-sqrt-emits-sqrtsd-sequence
           x86-emit-libm-unary-emits-21-bytes
           x86-64-emitter-table-spot-checks
           a64-fsqrt-encoder
           aarch64-libm-unary-emitter-size
           aarch64-sqrt-emitter-encoding
           aarch64-sqrt-size-and-dispatch-registered))
    (302 302
         ("packages/optimize/src/optimizer-strength.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-strength-tests.lisp"
          "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
         (opt-pass-strength-reduce)
         (strength-reduce-mod-by-power-of-2-emits-logand))
    (305 305
         ("packages/optimize/src/optimizer-strength.lisp"
          "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
         (opt-pass-strength-reduce %opt-mul-by-const-seq)
         (strength-reduce-mul-by-const-decomposes
          mul-by-const-seq-cases mul-by-const-seq-correctness))
    (290 290
         ("packages/regalloc/src/regalloc.lisp"
          "packages/regalloc/src/regalloc-allocate.lisp"
          "packages/emit/tests/regalloc-tests.lisp")
         (("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS")
          ("CL-CC/REGALLOC" . "LINEAR-SCAN-ALLOCATE")
          ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
         (regalloc-liveness-three-overlapping-intervals
          regalloc-liveness-forward-branch-extends-interval
          regalloc-allocate-fits-in-physical-regs-with-distinct-assignments
          regalloc-spill-pressure-exceeds-pool-causes-spills))
    (292 292
         ("packages/regalloc/src/regalloc.lisp"
          "packages/regalloc/src/regalloc-allocate.lisp"
          "packages/emit/tests/regalloc-tests.lisp")
         (("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS")
          ("CL-CC/REGALLOC" . "%LSA-TRY-COALESCE")
          ("CL-CC/REGALLOC" . "LINEAR-SCAN-ALLOCATE"))
         (regalloc-allocate-coalesces-move-to-same-physical-reg))
    (293 293
         ("packages/regalloc/src/regalloc-allocate.lisp"
          "packages/emit/tests/regalloc-tests.lisp")
         (("CL-CC/REGALLOC" . "INSERT-SPILL-CODE")
          ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
         (regalloc-spill-pressure-exceeds-pool-causes-spills
          regalloc-spill-rewrite-two-spilled-srcs-use-distinct-scratch-regs
           regalloc-spill-rewrite-spilled-src-and-dst-use-separate-scratch
           regalloc-integration-rematerializes-spilled-constant-as-vm-const))
    (303 303
         ("packages/vm/src/vm-instructions.lisp"
          "packages/vm/src/vm-execute.lisp"
          "packages/codegen/src/x86-64-emit-ops.lisp"
          "packages/codegen/src/x86-64-codegen-dispatch.lisp"
          "packages/codegen/src/x86-64-codegen-core.lisp"
          "packages/codegen/src/aarch64-emitters.lisp"
          "packages/codegen/src/aarch64-program.lisp"
          "packages/codegen/src/aarch64-codegen-labels.lisp"
          "packages/emit/tests/x86-64-emit-ops-tests.lisp"
          "packages/emit/tests/aarch64-emit-tests.lisp")
         (("CL-CC" . "MAKE-VM-ADD-CHECKED")
          ("CL-CC" . "MAKE-VM-SUB-CHECKED")
          ("CL-CC" . "MAKE-VM-MUL-CHECKED")
          ("CL-CC/CODEGEN" . "EMIT-VM-ADD-CHECKED")
          ("CL-CC/CODEGEN" . "EMIT-VM-SUB-CHECKED")
          ("CL-CC/CODEGEN" . "EMIT-VM-MUL-CHECKED")
          ("CL-CC/CODEGEN" . "EMIT-A64-VM-ADD-CHECKED")
          ("CL-CC/CODEGEN" . "EMIT-A64-VM-SUB-CHECKED")
          ("CL-CC/CODEGEN" . "EMIT-A64-VM-MUL-CHECKED"))
         (x86-emit-add-checked-emits-14-bytes
          x86-emit-sub-checked-emits-14-bytes
          x86-emit-mul-checked-emits-15-bytes
          aarch64-emit-add-checked-emits-12-bytes
          aarch64-emit-sub-checked-emits-12-bytes
          aarch64-emit-mul-checked-emits-24-bytes))
    )
  "Roadmap evidence profile ranges for the memory-numeric backend data slice.")
