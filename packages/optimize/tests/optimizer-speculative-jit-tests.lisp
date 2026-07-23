;;;; tests/unit/optimize/optimizer-speculative-jit-tests.lisp
;;;; Unit tests for speculative JIT helpers — ThinLTO, PGO, tiered JIT, deopt,
;;;; OSR, shape transitions, IC patch, async state machines, channels, STM,
;;;; lock-free, CFI, retpoline, stack canary, shadow stack, Wasm GC,
;;;; DWARF/source-map debug, TLS, atomics, HTM, concurrent GC.

(in-package :cl-cc/test)

;;; ─── ThinLTO / Tiered JIT / Deopt helpers (FR-301/310/312 partial) ───────

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

(it-sequential "optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers"
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 42
                 :register-map '((:rax . :r0) (:rbx . :r1))
                 :inlined-frames nil))
         (machine '((:rax . 11) (:rbx . 22) (:rcx . 33)))
         (state (cl-cc/optimize::opt-materialize-deopt-state frame machine)))
    (expect state :to-equal '((:r0 . 11) (:r1 . 22)))))

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

(it-sequential "optimize-shape-descriptor-slot-offsets name-at-0"
  (destructuring-bind (slot-name expected-offset) (list 'name 0)
    (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 7 '(name age active))))
    (expect (= expected-offset (cl-cc/optimize::opt-shape-slot-offset shape slot-name)) :to-be-truthy))))

(it-sequential "optimize-shape-descriptor-slot-offsets age-at-1"
  (destructuring-bind (slot-name expected-offset) (list 'age 1)
    (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 7 '(name age active))))
    (expect (= expected-offset (cl-cc/optimize::opt-shape-slot-offset shape slot-name)) :to-be-truthy))))

(it-sequential "optimize-shape-descriptor-slot-offsets active-at-2"
  (destructuring-bind (slot-name expected-offset) (list 'active 2)
    (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 7 '(name age active))))
    (expect (= expected-offset (cl-cc/optimize::opt-shape-slot-offset shape slot-name)) :to-be-truthy))))

(it-sequential "optimize-shape-transition-cache-stores-forward-transitions"
  (let ((cache (cl-cc/optimize::make-opt-shape-transition-cache :max-size 2)))
    (cl-cc/optimize::opt-shape-transition-put cache 1 'slot-a 2)
    (cl-cc/optimize::opt-shape-transition-put cache 2 'slot-b 3)
    (multiple-value-bind (child found)
        (cl-cc/optimize::opt-shape-transition-get cache 1 'slot-a)
      (expect found :to-be-truthy)
      (expect (= 2 child) :to-be-truthy))))

(it-sequential "optimize-ic-patch-plan-state-transitions uninit-to-mono"
  (destructuring-bind (from to expected-kind) (list :uninitialized :monomorphic :install-monomorphic)
    (expect (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site from to :target)) :to-be expected-kind)))

(it-sequential "optimize-ic-patch-plan-state-transitions mono-to-poly"
  (destructuring-bind (from to expected-kind) (list :monomorphic :polymorphic :promote-polymorphic)
    (expect (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site from to :target)) :to-be expected-kind)))

(it-sequential "optimize-ic-patch-plan-state-transitions poly-to-mega"
  (destructuring-bind (from to expected-kind) (list :polymorphic :megamorphic :promote-megamorphic)
    (expect (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site from to :target)) :to-be expected-kind)))

(it-sequential "optimize-build-inline-polymorphic-dispatch-builds-guard-chain"
  (let* ((entries '((:shape-a . :method-a) (:shape-b . :method-b)))
         (chain (cl-cc/optimize::opt-build-inline-polymorphic-dispatch entries :obj)))
    (expect (= 2 (length chain)) :to-be-truthy)
    (expect (getf (first chain) :shape) :to-be :shape-a)
    (expect (getf (first chain) :receiver) :to-be :obj)
    (expect (getf (second chain) :target) :to-be :method-b)))

(it-sequential "optimize-build-async-state-machine-builds-linear-transitions"
  (let* ((sm (cl-cc/optimize::opt-build-async-state-machine '(:await-1 :await-2)))
         (trans (cl-cc/optimize::opt-async-sm-transitions sm)))
    (expect (= 3 (length (cl-cc/optimize::opt-async-sm-states sm))) :to-be-truthy)
    (expect (= 2 (length trans)) :to-be-truthy)
    (expect (getf (first trans) :await) :to-be :await-1)
    (expect (= 2 (getf (second trans) :to)) :to-be-truthy)))

(it-sequential "optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed"
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p nil) :to-be :stackless)
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc t :deep-yield-p nil) :to-be :stackful)
  (expect (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p t) :to-be :stackful))

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

(it-sequential "optimize-stm-plan-pure-vs-impure pure-no-log"
  (destructuring-bind (reads writes pure-p expected-needs-log expected-inline-log) (list '(:x) nil t nil nil)
    (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads reads :writes writes :pure-p pure-p)))
    (expect (not (null (cl-cc/optimize::opt-stm-needs-log-p plan))) :to-equal expected-needs-log)
    (expect (not (null (cl-cc/optimize::opt-stm-plan-inline-log-p plan))) :to-equal expected-inline-log))))

(it-sequential "optimize-stm-plan-pure-vs-impure impure-with-log"
  (destructuring-bind (reads writes pure-p expected-needs-log expected-inline-log) (list '(:x) '(:y) nil t t)
    (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads reads :writes writes :pure-p pure-p)))
    (expect (not (null (cl-cc/optimize::opt-stm-needs-log-p plan))) :to-equal expected-needs-log)
    (expect (not (null (cl-cc/optimize::opt-stm-plan-inline-log-p plan))) :to-equal expected-inline-log))))

(it-sequential "optimize-lockfree-reclamation-policy-selection no-risk-none"
  (destructuring-bind (aba-risk-p contention expected) (list nil 10 :none)
    (expect (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-risk-p :contention contention) :to-be expected)))

(it-sequential "optimize-lockfree-reclamation-policy-selection aba-low-hazard"
  (destructuring-bind (aba-risk-p contention expected) (list t 1 :hazard-pointer)
    (expect (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-risk-p :contention contention) :to-be expected)))

(it-sequential "optimize-lockfree-reclamation-policy-selection aba-high-epoch"
  (destructuring-bind (aba-risk-p contention expected) (list t 6 :epoch)
    (expect (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-risk-p :contention contention) :to-be expected)))

(it-sequential "optimize-build-cfi-plan-selects-target-specific-guards"
  (let ((x86 (cl-cc/optimize::opt-build-cfi-plan :target :x86-64 :has-indirect-calls-p t))
        (arm (cl-cc/optimize::opt-build-cfi-plan :target :aarch64 :has-indirect-calls-p t)))
    (expect (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p x86) :to-be-truthy)
    (expect (cl-cc/optimize::opt-cfi-plan-insert-bti-p x86) :to-be-falsy)
    (expect (cl-cc/optimize::opt-cfi-plan-insert-bti-p arm) :to-be-truthy)
    (expect (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p arm) :to-be-falsy)))

(it-sequential "optimize-cfi-entry-opcode-by-target-and-calls x86-with-calls"
  (destructuring-bind (target has-indirect-calls-p expected) (list :x86-64 t :endbr64)
    (expect (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p has-indirect-calls-p)) :to-be expected)))

(it-sequential "optimize-cfi-entry-opcode-by-target-and-calls arm-with-calls"
  (destructuring-bind (target has-indirect-calls-p expected) (list :aarch64 t :bti-c)
    (expect (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p has-indirect-calls-p)) :to-be expected)))

(it-sequential "optimize-cfi-entry-opcode-by-target-and-calls x86-no-calls"
  (destructuring-bind (target has-indirect-calls-p expected) (list :x86-64 nil :none)
    (expect (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p has-indirect-calls-p)) :to-be expected)))

(it-sequential "optimize-should-use-retpoline-p-cases x86-no-ibrs"
  (destructuring-bind (target supports-ibrs-p expected) (list :x86-64 nil t)
    (expect (cl-cc/optimize::opt-should-use-retpoline-p
                 :target target :has-indirect-branch-p t :supports-ibrs-p supports-ibrs-p) :to-equal expected)))

(it-sequential "optimize-should-use-retpoline-p-cases x86-with-ibrs"
  (destructuring-bind (target supports-ibrs-p expected) (list :x86-64 t nil)
    (expect (cl-cc/optimize::opt-should-use-retpoline-p
                 :target target :has-indirect-branch-p t :supports-ibrs-p supports-ibrs-p) :to-equal expected)))

(it-sequential "optimize-should-use-retpoline-p-cases aarch64"
  (destructuring-bind (target supports-ibrs-p expected) (list :aarch64 nil nil)
    (expect (cl-cc/optimize::opt-should-use-retpoline-p
                 :target target :has-indirect-branch-p t :supports-ibrs-p supports-ibrs-p) :to-equal expected)))

(it-sequential "optimize-retpoline-thunk-names-are-register-specific r11"
  (destructuring-bind (register expected) (list :r11 "__clcc_retpoline_r11")
    (expect (cl-cc/optimize:opt-retpoline-thunk-name register) :to-equal expected)))

(it-sequential "optimize-retpoline-thunk-names-are-register-specific rax"
  (destructuring-bind (register expected) (list :rax "__clcc_retpoline_rax")
    (expect (cl-cc/optimize:opt-retpoline-thunk-name register) :to-equal expected)))

(it-sequential "optimize-needs-stack-canary-p-cases with-buffer"
  (destructuring-bind (has-stack-buffer-p expected) (list t t)
    (expect (cl-cc/optimize::opt-needs-stack-canary-p
                 :has-stack-buffer-p has-stack-buffer-p) :to-equal expected)))

(it-sequential "optimize-needs-stack-canary-p-cases without-buffer"
  (destructuring-bind (has-stack-buffer-p expected) (list nil nil)
    (expect (cl-cc/optimize::opt-needs-stack-canary-p
                 :has-stack-buffer-p has-stack-buffer-p) :to-equal expected)))

(it-sequential "optimize-stack-canary-emit-plan-describes-prologue-and-epilogue"
  (let ((enabled (cl-cc/optimize:opt-stack-canary-emit-plan
                  :has-stack-buffer-p t
                  :guard-slot -8
                  :failure-target 'panic))
        (disabled (cl-cc/optimize:opt-stack-canary-emit-plan
                   :has-stack-buffer-p nil)))
    (expect (getf enabled :enabled-p) :to-be-truthy)
    (expect (= -8 (getf enabled :guard-slot)) :to-be-truthy)
    (expect (getf enabled :load-source) :to-be :tls-canary)
    (expect (getf enabled :failure-target) :to-be 'panic)
    (expect (getf disabled :enabled-p) :to-be-falsy)
    (expect (getf disabled :guard-slot) :to-be-null)))

(it-sequential "optimize-stack-canary-sequences-describe-prologue-and-epilogue-ops"
  (let ((plan (cl-cc/optimize:opt-stack-canary-emit-plan
               :has-stack-buffer-p t
               :guard-slot -8
               :failure-target 'panic)))
    (expect (cl-cc/optimize:opt-stack-canary-prologue-seq
                   plan :temp-reg :tmp) :to-equal '((:op :load-canary :source :tls-canary :dst :tmp)
                    (:op :store-canary :src :tmp :slot -8)))
    (expect (cl-cc/optimize:opt-stack-canary-epilogue-seq
                   plan :temp-reg :tmp) :to-equal '((:op :load-canary :source -8 :dst :tmp)
                    (:op :compare-canary :left :tmp :right :tls-canary)
                    (:op :branch-if-canary-mismatch :target panic)))))

(it-sequential "optimize-stack-canary-sequences-are-empty-when-disabled"
  (let ((plan (cl-cc/optimize:opt-stack-canary-emit-plan
               :has-stack-buffer-p nil)))
    (expect (cl-cc/optimize:opt-stack-canary-prologue-seq plan) :to-be-null)
    (expect (cl-cc/optimize:opt-stack-canary-epilogue-seq plan) :to-be-null)))

(it-sequential "optimize-build-shadow-stack-plan-enables-only-for-x86-64-with-cet"
  (let ((enabled (cl-cc/optimize:opt-build-shadow-stack-plan
                  :target :x86-64
                  :supports-cet-ss-p t))
        (no-cet (cl-cc/optimize:opt-build-shadow-stack-plan
                 :target :x86-64
                 :supports-cet-ss-p nil))
        (wrong-arch (cl-cc/optimize:opt-build-shadow-stack-plan
                     :target :aarch64
                     :supports-cet-ss-p t)))
    (expect (cl-cc/optimize:opt-shadow-stack-plan-enabled-p enabled) :to-be-truthy)
    (expect (cl-cc/optimize:opt-shadow-stack-plan-target enabled) :to-be :x86-64)
    (expect (cl-cc/optimize:opt-shadow-stack-plan-needs-incsssp-p enabled) :to-be-truthy)
    (expect (cl-cc/optimize:opt-shadow-stack-plan-needs-save-restore-p enabled) :to-be-falsy)
    (expect (cl-cc/optimize:opt-shadow-stack-plan-enabled-p no-cet) :to-be-falsy)
    (expect (cl-cc/optimize:opt-shadow-stack-plan-enabled-p wrong-arch) :to-be-falsy)))

(it-sequential "optimize-shadow-stack-plan-requires-save-restore-for-nonlocal-control"
  (let ((plan (cl-cc/optimize:opt-build-shadow-stack-plan
               :target :x86-64
               :supports-cet-ss-p t
               :has-nonlocal-control-p t)))
    (expect (cl-cc/optimize:opt-shadow-stack-plan-needs-save-restore-p plan) :to-be-truthy)))

(it-sequential "optimize-wasm-tailcall-opcode-selection tail-direct-enabled"
  (destructuring-bind (tail-position-p indirect-p enabled-p expected) (list t nil t :return-call)
    (expect (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-position-p
              :indirect-p      indirect-p
              :enabled-p       enabled-p) :to-be expected)))

(it-sequential "optimize-wasm-tailcall-opcode-selection tail-indirect-enabled"
  (destructuring-bind (tail-position-p indirect-p enabled-p expected) (list t t t :return-call-indirect)
    (expect (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-position-p
              :indirect-p      indirect-p
              :enabled-p       enabled-p) :to-be expected)))

(it-sequential "optimize-wasm-tailcall-opcode-selection non-tail-disabled"
  (destructuring-bind (tail-position-p indirect-p enabled-p expected) (list nil nil nil :call)
    (expect (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-position-p
              :indirect-p      indirect-p
              :enabled-p       enabled-p) :to-be expected)))

(it-sequential "optimize-wasm-tailcall-disabled-tail-signals"
  (signals error (cl-cc/optimize::opt-wasm-select-tailcall-opcode
     :tail-position-p t
     :indirect-p nil
     :enabled-p nil)))

(it-sequential "optimize-build-wasm-gc-layout-preserves-kind-and-fields"
  (let ((layout (cl-cc/optimize::opt-build-wasm-gc-layout
                 :kind :struct
                 :fields '((slot-a . i32) (slot-b . externref))
                 :nullable-p t)))
    (expect (cl-cc/optimize::opt-wasm-gc-kind layout) :to-be :struct)
    (expect (cl-cc/optimize::opt-wasm-gc-fields layout) :to-equal '((slot-a . i32) (slot-b . externref)))
    (expect (cl-cc/optimize::opt-wasm-gc-nullable-p layout) :to-be-truthy)))

(it-sequential "optimize-wasm-gc-layout-validates-struct-and-array-shapes"
  (let ((struct-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                        :kind :struct
                        :fields '((slot-a . i32) (slot-b . eqref))
                        :nullable-p t))
        (array-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                       :kind :array
                       :fields '(eqref)
                       :nullable-p nil))
        (bad-array-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                           :kind :array
                           :fields '(eqref i32)
                           :nullable-p nil)))
    (expect (cl-cc/optimize:opt-wasm-gc-layout-valid-p struct-layout) :to-be-truthy)
    (expect (cl-cc/optimize:opt-wasm-gc-layout-valid-p array-layout) :to-be-truthy)
    (expect (cl-cc/optimize:opt-wasm-gc-layout-valid-p bad-array-layout) :to-be-falsy)))

(it-sequential "optimize-wasm-gc-runtime-host-compatibility-requires-feature-and-valid-layout"
  (let ((layout (cl-cc/optimize:opt-build-wasm-gc-layout
                 :kind :struct
                 :fields '((slot-a . i32))
                 :nullable-p t))
        (bad-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                     :kind :array
                     :fields '(eqref i32)
                     :nullable-p nil)))
    (expect (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      layout
      :host-supports-wasm-gc-p t) :to-be-truthy)
    (expect (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      layout
      :host-supports-wasm-gc-p nil) :to-be-falsy)
    (expect (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      bad-layout
      :host-supports-wasm-gc-p t) :to-be-falsy)))

(it-sequential "optimize-wasm-gc-optimization-plan-reflects-layout-kind"
  (let* ((struct-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                         :kind :struct
                         :fields '((slot-a . i32))
                         :nullable-p t))
         (array-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                        :kind :array
                        :fields '(eqref)
                        :nullable-p nil))
         (struct-plan (cl-cc/optimize:opt-build-wasm-gc-optimization-plan struct-layout))
         (array-plan (cl-cc/optimize:opt-build-wasm-gc-optimization-plan array-layout)))
    (expect (getf struct-plan :layout-valid-p) :to-be-truthy)
    (expect (getf struct-plan :inline-field-access-p) :to-be-truthy)
    (expect (getf struct-plan :bounds-check-elision-p) :to-be-falsy)
    (expect (getf array-plan :layout-valid-p) :to-be-truthy)
    (expect (getf array-plan :inline-field-access-p) :to-be-falsy)
    (expect (getf array-plan :bounds-check-elision-p) :to-be-truthy)))

(it-sequential "optimize-build-dwarf-line-row-preserves-location-fields"
  (let* ((loc (cl-cc/optimize::make-opt-debug-loc
               :file "src/foo.lisp" :line 42 :column 7 :symbol 'foo))
         (row (cl-cc/optimize::opt-build-dwarf-line-row #x1000 loc)))
    (expect (getf row :address) :to-be #x1000)
    (expect (getf row :file) :to-equal "src/foo.lisp")
    (expect (= 42 (getf row :line)) :to-be-truthy)
    (expect (= 7 (getf row :column)) :to-be-truthy)))

(it-sequential "optimize-build-wasm-source-map-entry-preserves-offset-and-source"
  (let* ((loc (cl-cc/optimize::make-opt-debug-loc
               :file "src/bar.lisp" :line 10 :column 3 :symbol 'bar))
         (entry (cl-cc/optimize::opt-build-wasm-source-map-entry 128 loc)))
    (expect (= 128 (getf entry :offset)) :to-be-truthy)
    (expect (getf entry :source) :to-equal "src/bar.lisp")
    (expect (= 10 (getf entry :line)) :to-be-truthy)
    (expect (= 3 (getf entry :column)) :to-be-truthy)))

(it-sequential "optimize-format-diagnostic-reason-renders-rpass-like-message"
  (expect (cl-cc/optimize::opt-format-diagnostic-reason
                 "inline" "skipped" "callee too large") :to-equal "inline: skipped (callee too large)"))

(it-sequential "optimize-build-tls-plan-selects-architecture-specific-base-register"
  (let ((x86 (cl-cc/optimize::opt-build-tls-plan :target :x86-64 :hot-access-p t))
        (arm (cl-cc/optimize::opt-build-tls-plan :target :aarch64 :hot-access-p t)))
    (expect (cl-cc/optimize::opt-tls-plan-uses-inline-tls-p x86) :to-be-truthy)
    (expect (cl-cc/optimize::opt-tls-plan-base-register x86) :to-be :fs)
    (expect (cl-cc/optimize::opt-tls-plan-base-register arm) :to-be :tpidr_el0)))

(it-sequential "optimize-select-atomic-opcode-target-operation-matrix x86-incf-acq-rel"
  (destructuring-bind (target operation memory-order expected) (list :x86-64 :incf :acq-rel :lock-xadd)
    (expect (cl-cc/optimize::opt-select-atomic-opcode
              :target target :operation operation :memory-order memory-order) :to-be expected)))

(it-sequential "optimize-select-atomic-opcode-target-operation-matrix x86-cas-seq-cst"
  (destructuring-bind (target operation memory-order expected) (list :x86-64 :cas :seq-cst :lock-cmpxchg)
    (expect (cl-cc/optimize::opt-select-atomic-opcode
              :target target :operation operation :memory-order memory-order) :to-be expected)))

(it-sequential "optimize-select-atomic-opcode-target-operation-matrix arm-incf-acq-rel"
  (destructuring-bind (target operation memory-order expected) (list :aarch64 :incf :acq-rel :ldadd)
    (expect (cl-cc/optimize::opt-select-atomic-opcode
              :target target :operation operation :memory-order memory-order) :to-be expected)))

(it-sequential "optimize-select-atomic-opcode-target-operation-matrix arm-cas-seq-cst"
  (destructuring-bind (target operation memory-order expected) (list :aarch64 :cas :seq-cst :ldxr-stxr)
    (expect (cl-cc/optimize::opt-select-atomic-opcode
              :target target :operation operation :memory-order memory-order) :to-be expected)))

(it-sequential "optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention"
  (let ((enabled (cl-cc/optimize::opt-build-htm-plan
                  :target :x86-64
                  :supports-htm-p t
                  :low-contention-p t))
        (disabled (cl-cc/optimize::opt-build-htm-plan
                   :target :x86-64
                   :supports-htm-p t
                   :low-contention-p nil)))
    (expect (cl-cc/optimize::opt-htm-plan-uses-htm-p enabled) :to-be-truthy)
    (expect (cl-cc/optimize::opt-htm-plan-begin-opcode enabled) :to-be :xbegin)
    (expect (cl-cc/optimize::opt-htm-plan-end-opcode enabled) :to-be :xend)
    (expect (cl-cc/optimize::opt-htm-plan-abort-opcode enabled) :to-be :xabort)
    (expect (cl-cc/optimize::opt-htm-plan-fallback-lock-p enabled) :to-be-truthy)
    (expect (cl-cc/optimize::opt-htm-plan-uses-htm-p disabled) :to-be-falsy)))

(it-sequential "optimize-build-concurrent-gc-plan-selects-satb-and-short-stw-for-latency-sensitive-mode"
  (let ((concurrent (cl-cc/optimize::opt-build-concurrent-gc-plan
                     :latency-sensitive-p t
                     :heap-size (* 128 1024 1024)))
        (stw (cl-cc/optimize::opt-build-concurrent-gc-plan
              :latency-sensitive-p nil
              :heap-size (* 128 1024 1024))))
    (expect (cl-cc/optimize::opt-conc-gc-plan-concurrent-mark-p concurrent) :to-be-truthy)
    (expect (cl-cc/optimize::opt-conc-gc-plan-write-barrier concurrent) :to-be :satb)
    (expect (cl-cc/optimize::opt-conc-gc-plan-mutator-assist-p concurrent) :to-be-truthy)
    (expect (cl-cc/optimize::opt-conc-gc-plan-stw-phases concurrent) :to-equal '(:initial-mark :final-remark))
    (expect (cl-cc/optimize::opt-conc-gc-plan-concurrent-mark-p stw) :to-be-falsy)
    (expect (cl-cc/optimize::opt-conc-gc-plan-write-barrier stw) :to-be :incremental-update)
    (expect (cl-cc/optimize::opt-conc-gc-plan-stw-phases stw) :to-equal '(:full-mark-sweep))))
