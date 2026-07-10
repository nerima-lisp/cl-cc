(in-package :cl-cc/optimize)
;;; -----------------------------------------------------------------------------
;;; Optimizer Pass Registry and Convergence Pipeline
;;; -----------------------------------------------------------------------------

;;; Single source of truth: ordered keyword → function pairs.
;;; *opt-convergence-passes* and *opt-pass-registry* are both derived from this.
(defparameter *opt-pass-table*
  `((:prolog-rewrite            . ,#'%maybe-apply-prolog-rewrite)
    (:superopt                  . ,#'%maybe-run-superopt)
    (:sequence-fusion           . ,#'opt-pass-sequence-fusion)
    (:egraph                    . ,#'optimize-with-egraph)
    (:call-site-splitting       . ,#'opt-pass-call-site-splitting)
     (:demand-analysis           . ,#'opt-pass-demand-analysis)
     (:devirtualize              . ,#'opt-pass-devirtualize)
     (:speculative-inline        . ,#'%maybe-run-speculative-inline)
     (:if-conversion             . ,#'opt-pass-if-conversion)
    (:closure-capture-dedup     . ,#'opt-pass-closure-capture-dedup)
    (:closure-thunk-sharing     . ,#'opt-pass-closure-thunk-sharing)
      (:inline                    . ,#'opt-pass-inline-iterative)
      (:fold                      . ,#'opt-pass-fold)
       (:overflow-check-elim       . ,#'%maybe-run-overflow-check-elimination)
       (:bounds-check-elim         . ,#'%maybe-run-bounds-check-elimination)
       (:value-range-propagation   . ,#'%maybe-run-value-range-propagation)
       (:sccp                      . ,#'opt-pass-sccp)
     (:cons-slot-forward         . ,#'opt-pass-cons-slot-forward)
      (:strength-reduce           . ,#'opt-pass-strength-reduce)
      (:iv-strength-reduce        . ,#'%maybe-run-iv-strength-reduce)
      (:div-by-const              . ,#'%maybe-run-div-by-const)
      (:bitwidth-reduction        . ,#'%maybe-run-bitwidth-reduction)
      (:idiom-recognition         . ,#'%maybe-run-idiom-recognition)
      (:fma-recognition           . ,#'opt-pass-fma-recognition)
      (:bswap-recognition         . ,#'opt-pass-bswap-recognition)
      (:rotate-recognition        . ,#'opt-pass-rotate-recognition)
      (:fill-recognition          . ,#'opt-pass-fill-recognition)
       (:copy-recognition          . ,#'opt-pass-copy-recognition)
       (:trmc                      . ,#'%maybe-run-trmc)
       (:auto-vectorization        . ,#'opt-pass-auto-vectorization)
       (:slp-vectorize             . ,#'opt-pass-slp-vectorize)
       (:function-outlining        . ,#'opt-pass-function-outlining)
      (:safepoint-polling         . ,#'opt-pass-safepoint-polling)
      (:software-pipelining       . ,#'opt-pass-software-pipelining)
      (:affine-loop-analysis      . ,#'%maybe-run-fr523-affine-loop-analysis)
     (:loop-interchange          . ,#'%maybe-run-fr524-loop-interchange)
     (:polyhedral-schedule       . ,#'%maybe-run-fr525-polyhedral-schedule)
      (:loop-fusion-fission       . ,#'%maybe-run-fr526-loop-fusion-fission)
        (:loop-fusion               . ,#'%maybe-run-loop-fusion)
        (:loop-fission              . ,#'%maybe-run-loop-fission)
        (:loop-tile                 . ,#'%maybe-run-loop-tile)
        (:autotune-simd             . ,#'%maybe-run-autotune-simd)
         (:polyhedral                . ,#'%maybe-run-polyhedral)
        (:mlgo-inline               . ,#'%maybe-run-mlgo-inline)
        (:ml-regalloc               . ,#'%maybe-run-ml-regalloc)
        (:loop-unswitch             . ,#'%maybe-run-loop-unswitch)
      (:reassociate               . ,#'opt-pass-reassociate)
     (:copy-prop                 . ,#'opt-pass-copy-prop)
     (:pure-call-optimization    . ,#'%maybe-run-pure-call-optimization)
     (:gvn                       . ,#'opt-pass-gvn)
     (:batch-concatenate         . ,#'opt-pass-batch-concatenate)
    (:cse                       . ,#'opt-pass-cse)
    (:jump                      . ,(symbol-function 'opt-pass-jump))
    (:loop-unrolling            . ,#'opt-pass-loop-unrolling-adaptive)
    (:loop-unroll               . ,#'%maybe-run-loop-unroll)
    (:loop-rotation             . ,#'opt-pass-loop-rotation)
    (:loop-rotate               . ,#'%maybe-run-loop-rotate)
      (:loop-peeling              . ,#'opt-pass-loop-peel)
      (:loop-peel                 . ,#'%maybe-run-loop-peel)
      (:prefetch-insertion        . ,#'opt-pass-prefetch-insertion)
      (:allocation-sinking
       . ,#'(lambda (instructions)
              (let ((cfg (cfg-build instructions))
                    (alias-facts (opt-compute-heap-aliases instructions)))
                (opt-sink-allocations instructions cfg alias-facts))))
     (:code-sinking              . ,#'opt-pass-code-sinking)
    (:unreachable               . ,#'opt-pass-unreachable)
    (:dead-basic-blocks         . ,#'opt-pass-dead-basic-blocks)
    (:store-to-load-forward     . ,#'opt-pass-store-to-load-forward)
    (:dead-store-elim           . ,#'opt-pass-dead-store-elim)
    (:load-store-coalescing     . ,#'%maybe-run-load-store-coalescing)
    (:nil-check-elim            . ,#'opt-pass-dominated-type-check-elim)
    (:dominated-type-check-elim . ,#'opt-pass-dominated-type-check-elim)
    (:branch-correlation        . ,#'opt-pass-branch-correlation)
    (:tail-duplication          . ,#'%maybe-run-tail-duplication)
    (:block-merge               . ,#'opt-pass-block-merge)
    (:tail-merge                . ,#'opt-pass-tail-merge)
    (:pre                       . ,#'opt-pass-pre)
    (:constant-hoist            . ,#'opt-pass-licm)
    (:global-dce                . ,#'opt-pass-global-dce)
     (:dead-labels               . ,#'opt-pass-dead-labels)
      (:hot-cold-layout           . ,#'opt-pass-hot-cold-layout)
      (:dead-loop-elimination     . ,#'%maybe-run-dead-loop-elimination)
      (:dead-argument-elimination . ,#'%maybe-run-dead-argument-elimination)
      (:cps-reduce                . ,#'%maybe-run-cps-reduce)
      (:defunctionalize           . ,#'%maybe-run-defunctionalize)
      (:delimited-continuations   . ,#'%maybe-run-delimited-continuations)
      (:escape-analysis           . ,#'%maybe-run-escape-analysis)
        (:branch-weights            . ,#'opt-analyze-branch-weights)
        (:path-profiling            . ,#'%maybe-run-path-profiling)
        (:dce                       . ,#'opt-pass-dce)
       (:optimization-remarks      . ,#'%maybe-run-optimization-remarks)
       (:abstract-interpretation   . ,#'%maybe-run-abstract-interpretation)
       (:translation-validation    . ,#'%maybe-run-translation-validation)
       (:schedule-local            . ,#'opt-pass-schedule-local))
  "Ordered (keyword . function) pairs — single source for pipeline and registry.")

(defparameter *opt-pass-registry*
  (loop with ht = (make-hash-table :test #'eq)
        for (k . v) in *opt-pass-table*
        do (setf (gethash k ht) v)
        finally (return ht))
  "Keyword → pass function mapping derived from *opt-pass-table*.")

(defparameter *opt-default-convergence-pass-keys*
  '(:prolog-rewrite
     :call-site-splitting
       :devirtualize
       :if-conversion
       :closure-capture-dedup
      :closure-thunk-sharing
       :inline
         :overflow-check-elim
         :sccp
       :cons-slot-forward
         :value-range-propagation
         :bounds-check-elim
       :sequence-fusion
     :demand-analysis
        :fma-recognition
        :iv-strength-reduce
        :div-by-const
        :bitwidth-reduction
        :idiom-recognition
        :bswap-recognition
     :rotate-recognition
      :fill-recognition
       :copy-recognition
       :trmc
       :auto-vectorization
      :slp-vectorize
       :function-outlining
       :safepoint-polling
       :software-pipelining
       :affine-loop-analysis
     :loop-interchange
     :polyhedral-schedule
      :loop-fusion-fission
       :loop-fusion
       :loop-fission
       :loop-tile
       :autotune-simd
       :loop-unswitch
      :reassociate
     :copy-prop
     :pure-call-optimization
     :gvn
     :batch-concatenate
    :cse
    :jump
     :loop-unrolling
     :loop-unroll
     :loop-rotation
     :loop-rotate
     :loop-peeling
     :loop-peel
     :prefetch-insertion
      :allocation-sinking
     :code-sinking
    :unreachable
    :dead-basic-blocks
    :store-to-load-forward
     :dead-store-elim
     :load-store-coalescing
     :nil-check-elim
    :dominated-type-check-elim
    :branch-correlation
    :tail-duplication
    :block-merge
    :tail-merge
    :pre
     :constant-hoist
     :global-dce
     :dead-labels
     :hot-cold-layout
     :dead-loop-elimination
     :dead-argument-elimination
     :cps-reduce
     :defunctionalize
     :escape-analysis
        :branch-weights
        :path-profiling
        :dce
       :fma-recognition
       :optimization-remarks
       :abstract-interpretation
       :translation-validation
       :schedule-local)
  "Default convergence pipeline keys.
`:egraph` remains available as an explicit pass, but the default rewrite stage is
`:prolog-rewrite`, which already composes both the Prolog peephole backend and
the e-graph engine.")

(defparameter *opt-convergence-passes*
  (mapcar (lambda (k) (gethash k *opt-pass-registry*))
          *opt-default-convergence-pass-keys*)
  "Ordered default pass functions derived from `*opt-default-convergence-pass-keys*`.")
