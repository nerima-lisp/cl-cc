;;;; optimizer-roadmap-backend-data-speculative.lisp -- Speculative, polyhedral, and learned backend evidence
;;;; Split from optimizer-roadmap-backend-data.lisp.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-speculative-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Aggregated in original lookup order by optimizer-roadmap-backend-data.lisp.
  '(
    (516 516
         ("packages/optimize/src/optimizer-strength-ext.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-strength-ext-tests.lisp"
          "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
         (opt-pass-reassociate opt-reassociate-commutative-p opt-copy-commutative-binop)
         (reassociate-commutative-p-true-for-commutative-ops
          reassociate-moves-constant-inward
          commutative-binop-table-coverage))
    (517 517
         ("packages/optimize/src/optimizer-dataflow.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-dataflow-tests.lisp")
         (opt-run-dataflow opt-compute-available-expressions opt-compute-reaching-definitions)
         (dataflow-worklist-forward-branch-join-converges
          available-expressions-join-intersects-predecessors
          reaching-definitions-join-unions-predecessors))
    (518 518
         ("packages/optimize/src/optimizer-dataflow.lisp"
          "packages/optimize/src/optimizer-cse-gvn.lisp"
          "packages/optimize/tests/optimizer-dataflow-tests.lisp"
          "packages/optimize/tests/optimizer-cse-gvn-tests.lisp")
         (opt-compute-available-expressions opt-compute-reaching-definitions opt-pass-gvn)
         (available-expressions-join-intersects-predecessors
          reaching-definitions-join-unions-predecessors
          gvn-uses-available-expressions-at-join))
    (519 519
         ("packages/optimize/src/egraph-saturation.lisp"
          "packages/optimize/src/egraph.lisp"
          "packages/optimize/tests/egraph-extraction-tests.lisp")
         (egraph-extract egraph-default-cost)
         (egraph-extract-nullary-node-returns-non-nil
          egraph-extract-binary-add-returns-compound))
    (520 520
         ("packages/optimize/src/optimizer-copyprop.lisp"
          "packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/tests/optimizer-copyprop-tests.lisp")
         (opt-pass-copy-prop %opt-copy-prop-merge %opt-copy-prop-rewrite-block)
         (copyprop-merge-disagreement-cases
          copyprop-pass-basic-rewrite
          copyprop-pass-chain-rewrite
          copy-prop-rewrite-block-rewrites-instructions))
    (521 521
         ("packages/optimize/src/optimizer-cse-gvn.lisp"
          "packages/optimize/src/cfg.lisp"
          "packages/optimize/tests/optimizer-cse-gvn-tests.lisp"
          "packages/optimize/tests/optimizer-tests-lowlevel2.lisp")
         (opt-pass-gvn cfg-compute-dominators)
         (gvn-uses-available-expressions-at-join
          gvn-redundant-overwrite-does-not-poison-same-syntax-expression
          optimizer-gvn-dominates-branch))
    (522 522
         ("packages/optimize/src/optimizer-purity.lisp"
          "packages/optimize/src/optimizer-inline-cost.lisp"
          "packages/optimize/src/optimizer-inline-pass.lisp"
          "packages/optimize/tests/optimizer-inline-tests.lisp")
          (opt-build-call-graph opt-call-graph-recursive-labels
           opt-pass-inline opt-pass-global-dce)
          (opt-build-call-graph-no-calls
           opt-call-graph-recursive-labels-no-recursion
           opt-call-graph-recursive-labels-direct-recursion
           opt-call-graph-recursive-labels-mutual-recursion
           opt-pass-inline-skips-recursive-callee))
    (523 523
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (opt-build-affine-loop-summary
          opt-pass-affine-loop-analysis)
         (optimize-affine-loop-summary-builds-descriptor
          optimize-pass-affine-loop-analysis-captures-real-loop-summary
          optimize-backend-roadmap-analysis-evidence-is-loaded
          optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (524 524
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (opt-loop-interchange-plan
          opt-pass-loop-interchange)
         (optimize-loop-interchange-plan-requires-safety
          optimize-pass-loop-interchange-handles-nested-canonical-loop
          optimize-pass-loop-interchange-skips-side-effecting-loop
          optimize-backend-roadmap-analysis-evidence-is-loaded
          optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (525 525
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (opt-polyhedral-schedule-plan
          opt-pass-polyhedral-schedule)
         (optimize-polyhedral-schedule-plan-preserves-objective
          optimize-pass-polyhedral-schedule-reorders-loop-body
          optimize-backend-roadmap-analysis-evidence-is-loaded
          optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (526 526
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (opt-loop-fusion-fission-plan
          opt-pass-loop-fusion-fission)
         (optimize-loop-fusion-fission-plan-selects-strategy
          optimize-pass-loop-fusion-fission-fuses-adjacent-loops
          optimize-pass-loop-fusion-fission-skips-unsafe-fusion
          optimize-pass-loop-fusion-fission-splits-oversized-loop
          optimize-backend-roadmap-analysis-evidence-is-loaded
          optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (527 527
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (opt-ml-inline-score-plan)
         (optimize-ml-inline-score-plan-is-deterministic
          optimize-backend-roadmap-analysis-evidence-is-loaded
          optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (528 528
         ("packages/optimize/src/optimizer-speculative-peval.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
         (opt-learned-codegen-cost-plan)
         (optimize-learned-codegen-cost-plan-is-target-aware
          optimize-backend-roadmap-analysis-evidence-is-loaded
          optimize-backend-roadmap-evidence-covers-doc-fr-list))
    )
  "Roadmap evidence profile ranges for the speculative backend data slice.")
