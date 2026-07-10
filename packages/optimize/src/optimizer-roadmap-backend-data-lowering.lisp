;;;; optimizer-roadmap-backend-data-lowering.lisp -- Lowering, egraph, overflow, and runtime evidence
;;;; Split from optimizer-roadmap-backend-data.lisp.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-lowering-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Aggregated in original lookup order by optimizer-roadmap-backend-data.lisp.
  '(
    (257 305
         ("packages/optimize/src/optimizer.lisp"
          "packages/optimize/src/optimizer-memory-alias.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-strength-tests.lisp"
          "packages/optimize/tests/optimizer-memory-tests.lisp")
         (opt-compute-cfg-value-ranges opt-compute-value-ranges
          opt-guard-record opt-weaken-guard opt-adaptive-compilation-threshold)
         (optimize-backend-roadmap-support-evidence-has-behavior))
    (350 350
         ("packages/optimize/src/egraph.lisp"
           "packages/optimize/src/egraph-match.lisp"
           "packages/optimize/src/egraph-saturation.lisp"
           "packages/optimize/src/egraph-rules.lisp"
           "packages/optimize/tests/egraph-extraction-tests.lisp")
         (egraph-add egraph-merge egraph-saturate egraph-extract egraph-default-cost)
         (egraph-saturate-empty-graph-terminates-at-iter-0
          egraph-saturate-with-empty-rules-terminates
          egraph-extract-nullary-node-returns-non-nil
          egraph-extract-binary-add-returns-compound))
    (351 351
         ("packages/optimize/src/egraph-rules.lisp"
          "packages/optimize/src/egraph-rules-advanced.lisp"
          "packages/optimize/src/optimizer-algebraic.lisp"
          "packages/optimize/src/optimizer.lisp"
          "packages/optimize/tests/egraph-rules-tests.lisp"
          "packages/optimize/tests/egraph-rules-bitwise-tests.lisp"
          "packages/optimize/tests/egraph-negation-tests.lisp"
          "packages/optimize/tests/optimizer-tests.lisp"
          "packages/optimize/tests/optimizer-tables-tests.lisp")
         (egraph-rule-register egraph-builtin-rules %opt-apply-algebraic-action)
         (optimizer-algebraic-identity
          egraph-rule-const-producing-rules-fire
          egraph-rule-registry-complete
          egraph-rule-double-neg-then-identity
          opt-apply-algebraic-action-move-lhs))
    (352 352
          ("packages/optimize/src/optimizer-memory-alias.lisp"
           "packages/optimize/src/optimizer-memory-ranges.lisp"
           "packages/optimize/src/optimizer.lisp"
           "packages/optimize/tests/optimizer-memory-tests.lisp"
           "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
          (opt-interval-logand opt-interval-bit-width
           opt-pass-elide-proven-overflow-checks
           %opt-rewrite-logand-low-bit-test opt-pass-fold)
          (value-ranges-logand-mask-with-unknown-input-narrows-to-8-bit
           value-ranges-add-of-masked-8-bit-values-is-9-bit-wide
           overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add
           optimize-instructions-rewrites-logand-one-eq-zero-to-evenp))
    (355 355
          ("packages/expand/src/expander-basic.lisp"
           "packages/expand/tests/expander-basic-tests.lisp"
           "packages/compile/src/codegen-io-ext.lisp")
         (("CL-CC/EXPAND" . "%FORMAT-DIRECTIVE-EXPANSION")
          ("CL-CC/EXPAND" . "%PARSE-FORMAT-LITERAL")
          ("CL-CC/EXPAND" . "%FORMAT-LITERAL-EXPANSION"))
         (expander-format-literal-single-aesthetic-directive
          expander-format-literal-supported-directives
          expander-format-literal-unsupported-directive-falls-back
          expander-format-literal-extra-args-fall-back))
    (380 380
          ("packages/compile/src/codegen-values.lisp"
           "packages/compile/src/codegen-values-helpers.lisp"
           "packages/compile/tests/codegen-functions-callsite-tests.lisp"
          "packages/compile/tests/compiler-tests-selfhost.lisp")
         (("CL-CC/COMPILE" . "%COMPILE-APPLY-LITERAL-SPREAD")
          ("CL-CC/COMPILE" . "%APPLY-ARGUMENT-PLAN")
          ("CL-CC/COMPILE" . "%LITERAL-APPLY-SPREAD-VALUES")
          ("CL-CC/COMPILE" . "%PROPER-LIST-P"))
         (codegen-apply-compilation
          codegen-apply-quoted-nil-compilation
          codegen-apply-improper-quoted-list-falls-back-to-vm-apply
          apply-spread-args-numeric
          apply-spread-quoted-nil-preserves-evaluation-order
          apply-improper-quoted-list-signals-error))
    (367 367
         ("packages/compile/src/codegen-core-let.lisp"
          "packages/compile/src/codegen-core-let-emit-pass.lisp"
          "packages/compile/tests/codegen-core-let-tests.lisp"
          "packages/compile/tests/codegen-core-tests.lisp")
         (("CL-CC/COMPILE" . "%AST-LET-BINDING-IGNORED-P"))
         (ast-let-binding-ignored-p
          codegen-let-binding-declaration-controls-own-move
          codegen-let-ignore-binding-enables-dce-of-unused-initializer))
    (363 363
         ("packages/expand/src/macros-runtime-support.lisp"
          "packages/expand/src/expander-data.lisp"
          "packages/compile/src/context.lisp"
          "packages/compile/src/codegen.lisp"
          "packages/compile/src/codegen-core-let-emit-pass.lisp"
          "packages/expand/tests/macros-runtime-support-tests.lisp"
          "packages/compile/tests/compiler-tests-extended-stdlib.lisp"
          "packages/compile/tests/codegen-control-tests.lisp")
         (("CL-CC/EXPAND" . "DECLARATION-OPTIMIZE-QUALITY")
          ("CL-CC/COMPILE" . "%GLOBAL-OPTIMIZE-QUALITY")
          ("CL-CC/COMPILE" . "%LOCAL-OPTIMIZE-QUALITY"))
         (declaim-optimize-updates-registry
          compile-declaim-optimize-form-records-global-policy
          compile-declaim-safety-zero-suppresses-later-defun-type-assertion
          compile-declaim-safety-zero-suppresses-top-level-the-type-assertion
          codegen-the-with-local-let-safety-zero-skips-typep))
    (364 364
         ("packages/expand/src/expander-data.lisp"
          "packages/expand/src/macro.lisp"
          "packages/expand/src/expander.lisp"
          "packages/expand/src/expander-basic.lisp"
          "packages/expand/src/macros-stdlib-ansi.lisp"
          "packages/compile/src/codegen.lisp"
          "packages/pipeline/src/pipeline.lisp"
          "packages/repl/src/pipeline-repl-load.lisp"
          "packages/expand/tests/macro-definition-tests.lisp")
         (("CL-CC/EXPAND" . "REGISTER-COMPILER-MACRO")
          ("CL-CC/EXPAND" . "COMPILER-MACRO-FUNCTION")
          ("CL-CC/EXPAND" . "COMPILER-MACROEXPAND-ALL"))
         (define-compiler-macro-expands-call
          compiler-macro-function-accesses-registered-expander
          define-compiler-macro-expands-funcall-function-designator
          define-compiler-macro-can-decline-with-whole-form
          define-compiler-macro-binds-environment
          compiler-macro-lambda-list-whole-and-environment))
    (360 360
         ("packages/compile/src/codegen-core.lisp"
          "packages/compile/src/codegen-core-control.lisp"
          "packages/type/src/inference-handlers.lisp"
          "packages/compile/tests/codegen-control-tests.lisp"
          "packages/type/tests/inference-forms-tests.lisp"
          "packages/compile/tests/compiler-tests-extended-stdlib.lisp")
         (("CL-CC/COMPILE" . "%COMPILE-IF-BRANCH")
          ("CL-CC/TYPE" . "INFER-THE")
          ("CL-CC/TYPE" . "INFER-SETQ"))
         (codegen-the-with-declared-integer-type-emits-typep
          infer-the-matching-type-is-fixnum
          infer-setq-returns-fixnum
          compile-declaim-safety-zero-suppresses-top-level-the-type-assertion))
    (366 366
         ("packages/expand/src/macros-runtime-support.lisp"
          "packages/cps/src/cps.lisp"
          "packages/expand/tests/macros-stdlib-io-tests.lisp")
         (("CL-CC/EXPAND" . "*LOAD-TIME-VALUE-CACHE*")
          ("CL-CC/CPS" . "%MAKE-CPS-SEXP-DISPATCH-TABLE"))
         (load-time-value-expands-to-quote
          load-time-value-is-memoized-during-expansion))
    (370 370
         ("packages/compile/src/context.lisp"
          "packages/compile/src/package.lisp"
          "packages/compile/tests/context-tests.lisp")
         (("CL-CC/COMPILE" . "*BUILTIN-SPECIAL-VARIABLES*")
          ("CL-CC/COMPILE" . "%RESOLVE-PACKAGE-SYMBOL-SPECS"))
         (ctx-initialization
          ctx-repl-state-persistence))
    (374 374
         ("packages/vm/src/vm-dispatch-gf.lisp"
          "packages/vm/src/vm-dispatch-gf-multi.lisp"
          "packages/vm/src/vm-clos-execute.lisp"
          "packages/vm/tests/vm-clos-tests.lisp"
          "packages/vm/tests/vm-dispatch-gf-multi-tests.lisp"
          "packages/runtime/tests/runtime-clos-tests.lisp"
          "packages/compile/tests/clos-dispatch-tests.lisp")
         (("CL-CC/VM" . "%VM-EXTRACT-EQL-SPECIALIZER-KEYS")
          ("CL-CC/VM" . "%VM-GF-EQL-METHODS"))
         (rt-call-generic-eql-dispatch
          rt-call-generic-eql-index-precedes-class-fallback
          eql-specializer-dispatch-index
          gf-multi-single-dispatch-eql-index-hit-precedes-class
          gf-multi-single-dispatch-eql-index-avoids-linear-scan
          clos-eql-specializer))
    (376 376
         ("packages/stdlib/src/stdlib-source-clos.lisp"
          "packages/expand/src/macros-stdlib-ansi.lisp"
          "packages/expand/src/expander-control.lisp"
          "packages/compile/src/codegen-control.lisp"
          "packages/expand/tests/macros-stdlib-ansi-tests.lisp"
          "packages/expand/tests/expander-control-helpers-tests.lisp"
          "packages/compile/tests/codegen-runtime-tests.lisp")
         (("CL-CC/EXPAND" . "%EXPAND-HANDLER-CASE-FORM")
          ("CL-CC/COMPILE" . "COMPILE-AST"))
         (define-condition-basic-structure
          handler-case-no-error-wraps-in-block
          codegen-handler-case-run-cases))
    (377 377
         ("packages/ast/src/ast.lisp"
          "packages/compile/src/codegen-control.lisp"
          "packages/compile/tests/codegen-runtime-tests.lisp"
          "packages/compile/tests/compiler-tests-runtime-hof-tests.lisp")
         (("CL-CC/AST" . "MAKE-AST-UNWIND-PROTECT")
          ("CL-CC/COMPILE" . "COMPILE-AST"))
         (codegen-unwind-protect-run-cases
          unwind-protect-cleanup-visible))
    (378 378
         ("packages/compile/src/codegen-values.lisp"
          "packages/compile/src/codegen-values-helpers.lisp"
          "packages/vm/src/vm-execute-mv.lisp"
          "packages/vm/src/vm-instructions.lisp"
          "packages/optimize/src/effects.lisp"
          "packages/optimize/src/optimizer-tables.lisp"
          "packages/compile/tests/codegen-runtime-tests.lisp"
          "packages/vm/tests/vm-execute-tests-2.lisp"
          "packages/vm/tests/primitives-tests.lisp")
         (("CL-CC/COMPILE" . "%COMPILE-MVB-VALUE-REGISTERS")
          ("CL-CC/COMPILE" . "%COMPILE-VALUES-LIST-REGISTERS")
          ("CL-CC/VM" . "VM-VALUES-TYPEP-CHECK"))
         (codegen-values-compilation
          codegen-mvb-compilation-cases
          codegen-mv-call-direct-path
          vm-execute-vm-values-stores-all
          vm-execute-mv-bind-distributes
          vm-execute-vm-values-buffer-management))
    (379 379
         ("packages/vm/src/vm-dsl.lisp"
          "packages/vm/src/io.lisp"
          "packages/vm/src/vm-extensions.lisp"
          "packages/vm/src/vm-dsl.lisp"
          "packages/vm/tests/vm-extensions-tests.lisp"
          "packages/runtime/tests/runtime-advanced-tests.lisp"
          "packages/expand/tests/expander-setf-places-tests.lisp")
         (("CL-CC" . "MAKE-VM-SYMBOL-GET")
          ("CL-CC" . "MAKE-VM-SET-SYMBOL-PLIST")
          ("CL-CC/VM" . "VM-SYMBOL-PLIST-READ-SNAPSHOT")
          ("CL-CC/VM" . "VM-SYSTEM-PROPERTY-SET")
          ("CL-CC/VM" . "VM-SYSTEM-PROPERTY-GET"))
         (vm-symbol-set-and-get-roundtrip-with-host-sync
          vm-set-symbol-plist-overwrites-and-promotes-long-plist
          vm-system-property-storage-is-separate
          vm-symbol-plist-lock-and-read-barrier-are-usable
          rt-symbol-plist-roundtrip
          expander-setf-get-place))
    )
  "Roadmap evidence profile ranges for the lowering backend data slice.")
