;;;; tests/unit/optimize/optimizer-concurrency-tests.lisp
;;;; Unit tests for concurrency/async helpers in the optimizer:
;;;; async state machine, coroutine strategy, channel select, STM, lock-free reclamation.

(in-package :cl-cc/test)

;;; ─── Async state machine ─────────────────────────────────────────────────────

(it-sequential "optimize-build-async-state-machine-builds-linear-transitions"
  (let* ((sm (cl-cc/optimize::opt-build-async-state-machine '(:await-1 :await-2)))
         (trans (cl-cc/optimize::opt-async-sm-transitions sm)))
    (expect (= 3 (length (cl-cc/optimize::opt-async-sm-states sm))) :to-be-truthy)
    (expect (= 2 (length trans)) :to-be-truthy)
    (expect (getf (first trans) :await) :to-be :await-1)
    (expect (= 2 (getf (second trans) :to)) :to-be-truthy)))

;;; ─── Coroutine strategy ──────────────────────────────────────────────────────

(it-sequential "optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed"
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p nil) :to-be :stackless)
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc t :deep-yield-p nil) :to-be :stackful)
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p t) :to-be :stackful))

;;; ─── Channel select ──────────────────────────────────────────────────────────

(it-sequential "optimize-channel-select-path-classifies-buffered-sync-and-contended-cases"
  (let ((buffered (cl-cc/optimize::make-opt-channel-site
                   :buffer-size 8 :queue-depth 2 :contention 0 :select-arity 2))
        (sync (cl-cc/optimize::make-opt-channel-site
               :buffer-size 0 :queue-depth 0 :contention 0 :select-arity 2))
        (contended (cl-cc/optimize::make-opt-channel-site
                    :buffer-size 8 :queue-depth 2 :contention 9 :select-arity 2)))
    (expect (cl-cc/optimize::opt-channel-select-path buffered) :to-be :fast-buffered)
    (expect (cl-cc/optimize::opt-channel-select-path sync) :to-be :synchronous-rendezvous)
    (expect (cl-cc/optimize::opt-channel-select-path contended) :to-be :contended-fallback)))

(it-sequential "optimize-channel-jump-table-select-threshold"
  (let ((small (cl-cc/optimize::make-opt-channel-site
                :buffer-size 1 :queue-depth 0 :contention 0 :select-arity 2))
        (large (cl-cc/optimize::make-opt-channel-site
                :buffer-size 1 :queue-depth 0 :contention 0 :select-arity 6)))
    (expect (cl-cc/optimize::opt-channel-should-jump-table-select-p small :threshold 4) :to-be-falsy)
    (expect (cl-cc/optimize::opt-channel-should-jump-table-select-p large :threshold 4) :to-be-truthy)))

;;; ─── STM plan ────────────────────────────────────────────────────────────────

(it-sequential "optimize-stm-plan-skips-log-for-pure-block"
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads '(:x) :writes nil :pure-p t)))
    (expect (cl-cc/optimize::opt-stm-needs-log-p plan) :to-be-falsy)
    (expect (cl-cc/optimize::opt-stm-plan-inline-log-p plan) :to-be-falsy)))

(it-sequential "optimize-stm-plan-enables-log-for-impure-read-write"
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads '(:x) :writes '(:y) :pure-p nil)))
    (expect (cl-cc/optimize::opt-stm-needs-log-p plan) :to-be-truthy)
    (expect (cl-cc/optimize::opt-stm-plan-inline-log-p plan) :to-be-truthy)))

;;; ─── Lock-free reclamation ───────────────────────────────────────────────────

(it-sequential "lockfree-reclamation-cases no-aba"
  (destructuring-bind (aba-p contention expected) (list nil 10 :none)
    (expect (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-p :contention contention) :to-be expected)))

(it-sequential "lockfree-reclamation-cases aba-low"
  (destructuring-bind (aba-p contention expected) (list t 1 :hazard-pointer)
    (expect (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-p :contention contention) :to-be expected)))

progn
(it-sequential "lockfree-reclamation-cases aba-high"
  (destructuring-bind (aba-p contention expected) (list t 6 :epoch)
    (expect (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-p :contention contention) :to-be expected)))

(it-sequential "fr-450 coroutine lowering keeps call/cc stackful with deep yield"
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
            :supports-call/cc t :deep-yield-p t) :to-be :stackful))

(it-sequential "fr-451 synchronous rendezvous wins for a wide select"
  (let ((site (cl-cc/optimize::make-opt-channel-site
               :buffer-size 0 :queue-depth 0 :contention 0 :select-arity 8)))
    (expect (cl-cc/optimize::opt-channel-select-path site)
            :to-be :synchronous-rendezvous)
    (expect (cl-cc/optimize::opt-channel-should-jump-table-select-p site :threshold 4)
            :to-be-truthy)))

(it-sequential "fr-452 pure stm lowering omits logs even with declared writes"
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads (quote (:hot-cell)) :writes (quote (:result)) :pure-p t)))
    (expect (cl-cc/optimize::opt-stm-needs-log-p plan) :to-be-falsy)
    (expect (cl-cc/optimize::opt-stm-plan-inline-log-p plan) :to-be-falsy)))
