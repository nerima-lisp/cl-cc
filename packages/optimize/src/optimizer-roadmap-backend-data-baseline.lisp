;;;; optimizer-roadmap-backend-data-baseline.lisp -- Broad baseline backend support evidence
;;;; Split from optimizer-roadmap-backend-data.lisp.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-baseline-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Aggregated in original lookup order by optimizer-roadmap-backend-data.lisp.
  '(
    (4 56
          ("packages/compile/src/codegen.lisp"
           "packages/compile/src/codegen-control.lisp"
           "packages/compile/src/codegen-core.lisp"
          "packages/optimize/src/optimizer-memory-alias.lisp"
          "packages/optimize/src/optimizer-inline.lisp"
          "packages/optimize/tests/optimizer-memory-tests.lisp"
          "packages/optimize/tests/optimizer-inline-tests.lisp")
         (opt-compute-points-to opt-array-bounds-check-eliminable-p
          opt-known-callee-labels opt-function-summary-safe-to-inline-p)
         (optimize-backend-roadmap-analysis-evidence-is-loaded))
    (57 184
         ("packages/optimize/src/optimizer-pipeline.lisp"
          "packages/optimize/src/optimizer-dataflow.lisp"
          "packages/optimize/src/optimizer-purity.lisp"
          "packages/optimize/tests/optimizer-pipeline-tests.lisp"
          "packages/optimize/tests/optimizer-dataflow-tests.lisp")
          (opt-lattice-meet opt-run-dataflow opt-profile-record-edge
           opt-profile-record-value opt-stack-map-live-root-p)
          (optimize-backend-roadmap-support-evidence-has-behavior))
    )
  "Roadmap evidence profile ranges for the baseline backend data slice.")
