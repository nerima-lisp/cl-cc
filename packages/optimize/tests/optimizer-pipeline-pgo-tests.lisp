;;;; tests/unit/optimize/optimizer-pipeline-pgo-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — PGO, ThinLTO, tiered JIT, deopt/OSR
;;;;
;;;; Covers: PGO counter plans, profile templates, ThinLTO import, tiered JIT
;;;;   compilation thresholds, deoptimization materialization, and OSR triggers.

(in-package :cl-cc/test)

;;; ─── PGO: hot chain / loop rotation ─────────────────────────────────────────

(it-sequential "optimize-pgo-build-hot-chain-prefers-hottest-successors"
  (let ((edges (make-hash-table :test #'equal)))
    (setf (gethash (cons :entry :cold) edges) 1
          (gethash (cons :entry :hot) edges) 10
          (gethash (cons :hot :exit) edges) 5
          (gethash (cons :cold :exit) edges) 1)
    (expect (cl-cc/optimize:opt-pgo-build-hot-chain
                   :entry
                   '((:entry . (:cold :hot))
                     (:hot . (:exit))
                     (:cold . (:exit)))
                   edges) :to-equal '(:entry :hot :exit))))

(it-sequential "optimize-pgo-rotate-loop-places-preferred-exit-at-bottom"
  (expect (cl-cc/optimize:opt-pgo-rotate-loop
                 '(:header :exit :latch)
                 :exit) :to-equal '(:latch :header :exit)))

;;; ─── ThinLTO module summaries ────────────────────────────────────────────────

(it-sequential "optimize-merge-module-summaries-aggregates-exports-and-counts"
  (let* ((a (cl-cc/optimize::make-opt-module-summary
             :module :a :exports '(fa fb) :function-count 2 :type-summaries nil))
         (b (cl-cc/optimize::make-opt-module-summary
             :module :b :exports '(fb fc) :function-count 3 :type-summaries nil))
         (merged (cl-cc/optimize::opt-merge-module-summaries (list a b))))
    (expect (getf merged :modules) :to-equal '(:a :b))
    (expect (member 'fa (getf merged :exports)) :to-be-truthy)
    (expect (member 'fb (getf merged :exports)) :to-be-truthy)
    (expect (member 'fc (getf merged :exports)) :to-be-truthy)
    (expect (= 5 (getf merged :function-count)) :to-be-truthy)))

(it-sequential "optimize-thinlto-import-decision-respects-budget-linkage-and-cycles"
  (let ((candidate (cl-cc/optimize:make-opt-function-summary
                    :name 'callee :inst-count 12 :exported-p nil :importable-p t))
        (exported (cl-cc/optimize:make-opt-function-summary
                   :name 'public :inst-count 12 :exported-p t :importable-p t))
        (too-large (cl-cc/optimize:make-opt-function-summary
                    :name 'big :inst-count 200 :exported-p nil :importable-p t)))
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                  candidate '(other) :budget 20) :to-be-truthy)
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                   exported '(other) :budget 20) :to-be-falsy)
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                   too-large '(other) :budget 20) :to-be-falsy)
    (expect (cl-cc/optimize:opt-thinlto-should-import-p
                   candidate '(callee) :budget 20) :to-be-falsy)))

;;; ─── Tiered JIT compilation thresholds ──────────────────────────────────────

(it-sequential "optimize-adaptive-compilation-threshold-reacts-to-warmup-pressure-and-failures"
  (expect (= 300 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :warmup-p t)) :to-be-truthy)
  (expect (= 1800 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :cache-pressure 0.8)) :to-be-truthy)
  (expect (= 2700 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :failures 2)) :to-be-truthy)
  (expect (= 1800 (cl-cc/optimize::opt-adaptive-compilation-threshold
                  :base 900 :warmup-p t :cache-pressure 0.8 :failures 2)) :to-be-truthy))

(it-sequential "optimize-tier-transition-cases interpreter-below-threshold"
  (destructuring-bind (result expected) (list (cl-cc/optimize:opt-tier-transition :interpreter 99  :baseline-threshold 100) :interpreter)
    (expect result :to-be expected)))

(it-sequential "optimize-tier-transition-cases interpreter-at-threshold"
  (destructuring-bind (result expected) (list (cl-cc/optimize:opt-tier-transition :interpreter 100 :baseline-threshold 100) :baseline)
    (expect result :to-be expected)))

(it-sequential "optimize-tier-transition-cases baseline-below-threshold"
  (destructuring-bind (result expected) (list (cl-cc/optimize:opt-tier-transition :baseline 999   :optimized-threshold 1000) :baseline)
    (expect result :to-be expected)))

(it-sequential "optimize-tier-transition-cases baseline-at-threshold"
  (destructuring-bind (result expected) (list (cl-cc/optimize:opt-tier-transition :baseline 1000  :optimized-threshold 1000) :optimized)
    (expect result :to-be expected)))

;;; ─── Deoptimization ──────────────────────────────────────────────────────────

(it-sequential "optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers"
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 42
                 :register-map '((:rax . :r0) (:rbx . :r1))
                 :inlined-frames nil))
         (machine '((:rax . 11) (:rbx . 22) (:rcx . 33)))
         (state (cl-cc/optimize::opt-materialize-deopt-state frame machine)))
    (expect state :to-equal '((:r0 . 11) (:r1 . 22)))))

;;; ─── On-Stack Replacement (OSR) ──────────────────────────────────────────────

(it-sequential "optimize-osr-trigger-p-threshold-cases fires-at-lower-threshold"
  (destructuring-bind (threshold expected) (list 1000 t)
    (let ((osr (cl-cc/optimize::make-opt-osr-point
              :loop-id :L1 :vm-pc 77 :live-registers nil :hotness 1200)))
    (expect (not (null (cl-cc/optimize::opt-osr-trigger-p osr :threshold threshold))) :to-equal expected))))

(it-sequential "optimize-osr-trigger-p-threshold-cases silent-at-higher-threshold"
  (destructuring-bind (threshold expected) (list 2000 nil)
    (let ((osr (cl-cc/optimize::make-opt-osr-point
              :loop-id :L1 :vm-pc 77 :live-registers nil :hotness 1200)))
    (expect (not (null (cl-cc/optimize::opt-osr-trigger-p osr :threshold threshold))) :to-equal expected))))

(it-sequential "optimize-osr-materialize-entry-maps-machine-to-vm-registers"
  (let* ((osr (cl-cc/optimize::make-opt-osr-point
               :loop-id :L2
               :vm-pc 88
               :live-registers '((:rax . :r0) (:r10 . :r7))
               :hotness 1500))
         (machine '((:rax . 3) (:r10 . 9) (:rbx . 99)))
         (state (cl-cc/optimize::opt-osr-materialize-entry osr machine)))
    (expect state :to-equal '((:r0 . 3) (:r7 . 9)))))

;;; ─── PGO counter plan and profile template (FR-295) ─────────────────────────

(it-sequential "optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids"
  (let* ((plan (cl-cc/optimize:opt-pgo-build-counter-plan
                :entry
                '((:entry :left :right)
                  (:left :exit)
                  (:right :exit)
                  (:exit))))
         (bb (getf plan :bb-counters))
         (edge (getf plan :edge-counters)))
    (expect bb :to-equal '((:entry . 0) (:left . 1) (:exit . 2) (:right . 3)))
    (expect edge :to-equal '(((:entry . :left) . 0)
                    ((:entry . :right) . 1)
                    ((:left . :exit) . 2)
                    ((:right . :exit) . 3)))
    (expect (= 4 (getf plan :total-bb)) :to-be-truthy)
    (expect (= 4 (getf plan :total-edge)) :to-be-truthy)))

(it-sequential "optimize-pgo-make-profile-template-zero-initializes-counts"
  (let* ((plan (cl-cc/optimize:opt-pgo-build-counter-plan
                :entry
                '((:entry :left :right)
                  (:left :exit)
                  (:right :exit)
                  (:exit))))
         (profile (cl-cc/optimize:opt-pgo-make-profile-template plan)))
    (expect (getf profile :magic) :to-be :cl-cc-pgo-v1)
    (expect (getf profile :bb-counts) :to-equal '((:entry . 0) (:left . 0) (:exit . 0) (:right . 0)))
    (expect (getf profile :branch-counts) :to-equal '(((:entry . :left) . 0)
                    ((:entry . :right) . 0)
                    ((:left . :exit) . 0)
                    ((:right . :exit) . 0)))
    (expect (= (getf plan :total-bb) (getf profile :total-bb)) :to-be-truthy)
    (expect (= (getf plan :total-edge) (getf profile :total-edge)) :to-be-truthy)))
