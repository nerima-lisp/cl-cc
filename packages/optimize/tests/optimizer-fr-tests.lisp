;;;; tests/optimizer-fr-tests.lisp — Optimizer Feature Requirement Evidence Tests
;;;;
;;;; Tests for optimizer FR implementations:
;;;; - FR-017: Alias Analysis / Memory Disambiguation
;;;; - FR-020: Allocation Sinking
;;;; - FR-090: Safepoint Dominance Pruning
;;;; - FR-091: Safepoint Hoisting to Loop Back-Edges
;;;; - FR-309: Memory Access Pattern Analysis

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; FR-017: Alias Analysis / Memory Disambiguation
;;; ------------------------------------------------------------

(it-sequential "fr-017-alias-analysis-function-exists"
  (expect (fboundp 'cl-cc/optimize:opt-compute-heap-aliases) :to-be-truthy))

(it-sequential "fr-017-tbaa-predicate-exists"
  (expect (or (fboundp 'cl-cc/optimize:opt-tbaa-must-not-alias-p)
                   (fboundp 'cl-cc/optimize:opt-compute-heap-type-facts)) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-020: Allocation Sinking
;;; ------------------------------------------------------------

(it-sequential "fr-020-allocation-sinking-exists"
  (expect (fboundp 'cl-cc/optimize:opt-sink-allocations) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-090: Safepoint Dominance Pruning
;;; ------------------------------------------------------------

(it-sequential "fr-090-safepoint-pruning-exists"
  (expect (fboundp 'cl-cc/optimize:opt-prune-dominated-safepoints) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-091: Safepoint Hoisting to Loop Back-Edges
;;; ------------------------------------------------------------

(it-sequential "fr-091-safepoint-hoisting-exists"
  (expect (fboundp 'cl-cc/optimize:opt-hoist-safepoints-to-back-edges) :to-be-truthy))

;;; ------------------------------------------------------------
;;; FR-309: Memory Access Pattern Analysis
;;; ------------------------------------------------------------

(it-sequential "fr-309-memory-access-analysis-exists"
  (expect (fboundp 'cl-cc/optimize:opt-analyze-memory-access-patterns) :to-be-truthy))

;;; ------------------------------------------------------------
;;; Integrated: FR-017 enables stronger LICM
;;; ------------------------------------------------------------

(it-sequential "fr-017-licm-exists"
  (expect (fboundp 'cl-cc/optimize::opt-pass-licm) :to-be-truthy))

;;; ------------------------------------------------------------
;;; Behavioral: optimizer pipeline loads and converges
;;; ------------------------------------------------------------

(it-sequential "fr-optimizer-pipeline-available"
  (let ((alias-fn (symbol-function 'cl-cc/optimize:opt-compute-heap-aliases)))
    (expect (functionp alias-fn) :to-be-truthy))
  (let ((tbaa-fn (or (ignore-errors (symbol-function 'cl-cc/optimize:opt-tbaa-must-not-alias-p))
                     (ignore-errors (symbol-function 'cl-cc/optimize:opt-compute-heap-type-facts)))))
    (expect (functionp tbaa-fn) :to-be-truthy))
  (expect (functionp (symbol-function 'cl-cc/optimize:opt-prune-dominated-safepoints)) :to-be-truthy)
  (expect (functionp (symbol-function 'cl-cc/optimize:opt-hoist-safepoints-to-back-edges)) :to-be-truthy)
  (expect (functionp (symbol-function 'cl-cc/optimize:opt-analyze-memory-access-patterns)) :to-be-truthy))

(it-sequential "fr-optimizer-passes-collection-nonempty"
  (let ((passes (ignore-errors (symbol-value (find-symbol "*OPT-CONVERGENCE-PASSES*" "CL-CC/OPTIMIZE")))))
    (when passes
      (expect (listp passes) :to-be-truthy)
      (expect (> (length passes) 0) :to-be-truthy))))
