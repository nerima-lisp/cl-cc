;;;; tests/unit/optimize/optimizer-roadmap-backend-tests.lisp
;;;; Unit tests for optimizer-pipeline-roadmap.lisp — runtime/support helpers
;;;; and docs/notes/optimize-backend.md evidence
;;;;
;;;; Covers: IC, lattice, profile, deopt, shape, and tiering helpers;
;;;;   optimize-backend-roadmap doc parsing and evidence coverage.

(in-package :cl-cc/test)

(it-sequential "optimize-formal-tooling-superopt-abstract-and-translation-validation"
  (let* ((window (list (cl-cc:make-vm-move :dst :r1 :src :r0)
                       (cl-cc:make-vm-move :dst :r1 :src :r0)))
         (optimized (cl-cc/optimize:opt-pass-superopt window)))
    (expect (= 1 (length optimized)) :to-be-truthy)
    (expect (typep (first optimized) 'cl-cc/vm:vm-move) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst (first optimized)) :to-be :r1)
    (expect (cl-cc/vm::vm-src (first optimized)) :to-be :r0))
  (let* ((program (list (cl-cc:make-vm-const :dst :r0 :value 2)
                        (cl-cc:make-vm-const :dst :r1 :value 5)
                        (cl-cc:make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
         (state (cl-cc/optimize:ai-compute-fixed-point program))
         (facts (cl-cc/optimize::ai-state-facts state))
         (r2 (gethash :r2 facts)))
    (expect (getf r2 :interval) :to-equal '(7 . 7))
    (expect (getf r2 :nullness) :to-be :non-null))
  (let ((before (list (cl-cc:make-vm-const :dst :r0 :value 1)
                      (cl-cc:make-vm-halt :reg :r0)))
        (after (list (cl-cc:make-vm-const :dst :r0 :value 2)
                     (cl-cc:make-vm-halt :reg :r0))))
    (expect (cl-cc/optimize::translation-validation-equivalent-p before before) :to-be-truthy)
    (expect (cl-cc/optimize::translation-validation-equivalent-p before after) :to-be-falsy)
    (let ((cl-cc/optimize:*translation-validation-enabled* t))
      (expect (handler-bind ((warning #'muffle-warning))
                         (cl-cc/optimize:validate-optimizer-translation :unit before after)) :to-be after))))

(it-sequential "optimize-differential-fuzzing-deterministic-smoke"
  (let* ((result (cl-cc/optimize:opt-run-compiler-fuzz
                  :trials 8
                  :seed 753
                  :max-program-length 8
                  :optimizer #'identity)))
    (expect (getf result :ok) :to-be-truthy)
    (expect (= 8 (getf result :trials)) :to-be-truthy)
    (expect (= 753 (getf result :seed)) :to-be-truthy))
  (let* ((program (cl-cc/optimize::opt-generate-random-ir-program
                   :state (cl-cc/optimize::%opt-fuzz-random-state 9)
                   :max-program-length 6))
         (expected (multiple-value-list (cl-cc/optimize::opt-fuzz-interpret-ir program)))
         (actual (multiple-value-list (cl-cc/optimize::opt-fuzz-interpret-ir (copy-list program)))))
    (expect actual :to-equal expected)))

(it-sequential "optimize-roadmap-runtime-helpers-have-concrete-behavior"
  (let ((site (cl-cc/optimize::make-opt-ic-site :max-polymorphic-entries 2)))
    (expect (cl-cc/optimize::opt-ic-site-state site) :to-be :uninitialized)
    (cl-cc/optimize::opt-ic-transition site :shape-a :target-a)
    (expect (cl-cc/optimize::opt-ic-site-state site) :to-be :monomorphic)
    (cl-cc/optimize::opt-ic-transition site :shape-b :target-b)
    (expect (cl-cc/optimize::opt-ic-site-state site) :to-be :polymorphic)
    (cl-cc/optimize::opt-ic-transition site :shape-c :target-c)
    (expect (cl-cc/optimize::opt-ic-site-state site) :to-be :megamorphic)
    (expect (= 3 (cl-cc/optimize::opt-ic-site-misses site)) :to-be-truthy)
    (expect (= 3 (length (cl-cc/optimize::opt-ic-site-megamorphic-fallback site))) :to-be-truthy))
  (let ((spec-log (cl-cc/optimize::make-opt-speculation-log :threshold 2)))
    (expect (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard) :to-be-falsy)
    (expect (= 1 (cl-cc/optimize::opt-record-speculation-failure spec-log :site :guard)) :to-be-truthy)
    (expect (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard) :to-be-falsy)
    (expect (= 2 (cl-cc/optimize::opt-record-speculation-failure spec-log :site :guard)) :to-be-truthy)
    (expect (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard) :to-be-truthy))
  (let ((profile (cl-cc/optimize::make-opt-profile-data)))
    (expect (= 3 (cl-cc/optimize::opt-profile-record-edge profile :entry :exit 3)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize::opt-profile-record-value profile :ic-site 42 2)) :to-be-truthy)
    (expect (= 4 (cl-cc/optimize::opt-profile-record-call-chain profile '(:main :callee) 4)) :to-be-truthy)
    (expect (cl-cc/optimize::opt-profile-record-allocation profile :alloc-site 64 2) :to-equal (cons 2 64)))
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 7
                 :register-map '((:rax . :r0) (:rbx . :r1))))
         (state (cl-cc/optimize::opt-materialize-deopt-state
                 frame
                 '((:rax . 10) (:rbx . 20)))))
    (expect state :to-equal '((:r0 . 10) (:r1 . 20))))
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots
                9
                '(:class :slots))))
    (expect (= 0 (cl-cc/optimize::opt-shape-slot-offset shape :class)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize::opt-shape-slot-offset shape :slots)) :to-be-truthy)
    (expect (cl-cc/optimize::opt-shape-slot-offset shape :missing) :to-be-null))
  (expect (= 120 (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base 90
             :warmup-p t
             :cache-pressure 0.7
             :failures 1)) :to-be-truthy))

(it-sequential "optimize-roadmap-support-helpers-have-conservative-behavior"
  (let* ((bottom (cl-cc/optimize::opt-lattice-bottom))
         (seven-a (cl-cc/optimize::opt-lattice-constant 7))
         (seven-b (cl-cc/optimize::opt-lattice-constant 7))
         (eight (cl-cc/optimize::opt-lattice-constant 8))
         (overdefined (cl-cc/optimize::opt-lattice-overdefined)))
    (expect (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet bottom seven-a)) :to-be :constant)
    (expect (= 7 (cl-cc/optimize::opt-lattice-value-value
               (cl-cc/optimize::opt-lattice-meet seven-a seven-b))) :to-be-truthy)
    (expect (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet seven-a eight)) :to-be :overdefined)
    (expect (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet seven-a overdefined)) :to-be :overdefined))
  (let ((pure-summary (cl-cc/optimize::make-opt-function-summary
                       :name 'pure-helper
                       :pure-p t
                       :effects nil))
        (effectful-summary (cl-cc/optimize::make-opt-function-summary
                            :name 'effectful-helper
                            :pure-p t
                            :effects '(:heap-write))))
    (expect (cl-cc/optimize::opt-function-summary-safe-to-inline-p pure-summary) :to-be-truthy)
    (expect (cl-cc/optimize::opt-function-summary-safe-to-inline-p effectful-summary) :to-be-falsy))
  (let* ((pool (cl-cc/optimize::make-opt-slab-pool :object-size 2))
         (first-object (cl-cc/optimize::opt-slab-allocate pool)))
    (expect first-object :to-equal '(:slab-object 2 1))
    (cl-cc/optimize::opt-slab-free pool first-object)
    (expect (cl-cc/optimize::opt-slab-allocate pool) :to-be first-object))
  (let ((region (cl-cc/optimize::make-opt-bump-region :limit 8)))
    (expect (= 0 (cl-cc/optimize::opt-bump-allocate region 3)) :to-be-truthy)
    (expect (= 3 (cl-cc/optimize::opt-bump-mark region)) :to-be-truthy)
    (expect (= 4 (cl-cc/optimize::opt-bump-allocate region 2 :alignment 4)) :to-be-truthy)
    (expect (cl-cc/optimize::opt-bump-allocate region 4) :to-be-null)
    (cl-cc/optimize::opt-bump-reset region)
    (expect (= 3 (cl-cc/optimize::opt-bump-region-cursor region)) :to-be-truthy))
  (let ((stack-map (cl-cc/optimize::make-opt-stack-map :pc 42 :roots '(:r0 :r2))))
    (expect (cl-cc/optimize::opt-stack-map-live-root-p stack-map :r0) :to-be-truthy)
    (expect (cl-cc/optimize::opt-stack-map-live-root-p stack-map :r1) :to-be-falsy))
  (let ((guard (cl-cc/optimize::make-opt-guard-state :executions 9)))
    (expect (cl-cc/optimize::opt-guard-record guard t) :to-be :tag-bit-test)
    (expect (cl-cc/optimize::opt-guard-record guard nil) :to-be :full-type-check))
  (let* ((cold (cl-cc/optimize::make-opt-jit-cache-entry :id :cold :size 8 :warmth 1))
         (warm (cl-cc/optimize::make-opt-jit-cache-entry :id :warm :size 8 :warmth 9))
         (evicted (cl-cc/optimize::opt-jit-cache-select-eviction
                   (list warm cold)
                   :current-size 90
                   :max-size 100)))
    (expect evicted :to-be cold))
  (let ((summary (cl-cc/optimize::opt-merge-module-summaries
                  (list (cl-cc/optimize::make-opt-module-summary
                         :module :a
                         :exports '(foo)
                         :function-count 2)
                        (cl-cc/optimize::make-opt-module-summary
                         :module :b
                         :exports '(foo bar)
                         :function-count 3)))))
    (expect (getf summary :modules) :to-equal '(:a :b))
    (expect (= 5 (getf summary :function-count)) :to-be-truthy)
    (expect (member 'foo (getf summary :exports)) :to-be-truthy)
    (expect (member 'bar (getf summary :exports)) :to-be-truthy))
  (expect (cl-cc/optimize::opt-sea-node-schedulable-p
    (cl-cc/optimize::make-opt-sea-node :id :n1 :op :add :controls '(:entry))) :to-be-truthy)
  (expect (cl-cc/optimize::opt-sea-node-schedulable-p
      (cl-cc/optimize::make-opt-sea-node :id :n2 :controls '(:entry))) :to-be-falsy))

(it-sequential "optimizer-roadmap-value-profiling-top-k-and-range-behavior"
  (let ((profile (cl-cc/optimize:make-opt-profile-data :value-limit 2)))
    (expect (= 2 (cl-cc/optimize:opt-profile-record-value profile :site 10 2)) :to-be-truthy)
    (expect (= 3 (cl-cc/optimize:opt-profile-record-value profile :site 20 3)) :to-be-truthy)
    (expect (= 1 (cl-cc/optimize:opt-profile-record-value profile :site 30 1)) :to-be-truthy)
    (expect (cl-cc/optimize:opt-profile-top-values profile :site) :to-equal '((20 . 3) (10 . 2)))
    (expect (cl-cc/optimize:opt-profile-top-values profile :site 1) :to-equal '((20 . 3)))
    (expect (cl-cc/optimize:opt-profile-value-range profile :site) :to-equal '(10 . 30))))

(it-sequential "optimizer-roadmap-speculation-log-gating-and-persistence-behavior"
  (let* ((cl-cc/optimize:*opt-speculation-log*
           (cl-cc/optimize:make-opt-speculation-log :threshold 2))
         (temp-path (merge-pathnames
                     (make-pathname :name (format nil "opt-spec-log-~D-~D"
                                                  (get-universal-time)
                                                  (random 1000000))
                                    :type "prof")
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (expect (cl-cc/optimize:opt-speculation-allowed-p :site :guard) :to-be-truthy)
           (expect (= 1 (cl-cc/optimize:opt-record-speculation-failure
                      cl-cc/optimize:*opt-speculation-log*
                      :site
                      :guard)) :to-be-truthy)
           (expect (cl-cc/optimize:opt-speculation-allowed-p :site :guard) :to-be-truthy)
           (expect (= 2 (cl-cc/optimize:opt-record-speculation-failure
                      cl-cc/optimize:*opt-speculation-log*
                      :site
                      :guard)) :to-be-truthy)
           (expect (cl-cc/optimize:opt-speculation-allowed-p :site :guard) :to-be-falsy)
           (cl-cc/optimize:opt-save-speculation-log temp-path)
           (expect (cl-cc/optimize:opt-clear-speculation-log) :to-be cl-cc/optimize:*opt-speculation-log*)
           (expect (cl-cc/optimize:opt-speculation-allowed-p :site :guard) :to-be-truthy)
           (setf (cl-cc/optimize::opt-spec-log-threshold cl-cc/optimize:*opt-speculation-log*) 99)
           (cl-cc/optimize:opt-load-speculation-log temp-path)
           (expect (= 2 (cl-cc/optimize::opt-spec-log-threshold
                      cl-cc/optimize:*opt-speculation-log*)) :to-be-truthy)
           (expect (cl-cc/optimize:opt-speculation-failed-p
             cl-cc/optimize:*opt-speculation-log*
             :site
             :guard) :to-be-truthy)
           (expect (cl-cc/optimize:opt-speculation-allowed-p :site :guard) :to-be-falsy))
      (when (probe-file temp-path)
        (delete-file temp-path)))))


(defun %optimize-backend-doc-content ()
  "Return the current optimize-backend roadmap document text."
  (%doc-content #P"docs/notes/optimize-backend.md"))

(defun %optimize-backend-doc-completed-heading-contradictions ()
  "Return ✅ optimize-backend FR headings whose own section says implementation is absent."
  (%doc-completed-heading-contradictions
   #P"docs/notes/optimize-backend.md"
   (list "未実装" "未着手" "未対応" "未完"
         "未統合" "未接続" "欠落" "未定義" "不可能"
         "回転命令なし" "検出なし" "エミッションなし" "分岐なし"
         "分解なし" "ガードなし" "ABI 固定" "全 caller-saved")
   :reset-on-section-boundary t))

(defun %optimize-backend-evidence-status-for-feature (feature)
  "Return expected evidence status for optimize-backend FEATURE."
  (let ((status (cl-cc/optimize::opt-roadmap-feature-status feature)))
    (case status
      (:unknown :planned)
      (otherwise status))))

(defun %optimize-backend-test-anchor-name= (expected actual)
  "Return T when EXPECTED and ACTUAL name the same test anchor."
  (and (symbolp expected)
       (symbolp actual)
       (string= (symbol-name expected)
                (symbol-name actual))))

(defun %optimize-backend-assert-member (expected values test)
  "Assert that EXPECTED is present in VALUES according to TEST."
  (expect (member expected values :test test) :to-be-truthy))

(defun %optimize-backend-assert-evidence-contains
    (evidence modules api-symbols test-anchors)
  "Assert that EVIDENCE contains the supplied module, API, and test anchors."
  (dolist (module modules)
    (%optimize-backend-assert-member
     module
     (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
     #'string=))
  (dolist (api-symbol api-symbols)
    (%optimize-backend-assert-member
     api-symbol
     (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
     #'equal))
  (dolist (test-anchor test-anchors)
    (%optimize-backend-assert-member
     test-anchor
     (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
     #'%optimize-backend-test-anchor-name=)))

(defun %optimize-backend-assert-evidence-case
    (feature-id status modules api-symbols test-anchors)
  "Assert the roadmap evidence contract for FEATURE-ID."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
    (expect evidence :to-be-truthy)
    (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be status)
    (%optimize-backend-assert-evidence-contains
     evidence
     modules
     api-symbols
     test-anchors)))

(it-sequential "optimize-backend-roadmap-completed-headings-avoid-incomplete-language"
  (expect (%optimize-backend-doc-completed-heading-contradictions) :to-be-null))

(it-sequential "optimize-backend-roadmap-evidence-covers-doc-fr-list"
  (let* ((features (cl-cc/optimize:optimize-backend-roadmap-doc-features))
         (ids (mapcar #'cl-cc/optimize::opt-roadmap-feature-id features))
         (table (cl-cc/optimize:optimize-backend-roadmap-register-doc-evidence))
         (implemented 0)
         (not-implemented 0)
         (profiles nil))
    (expect (= 232 (length ids)) :to-be-truthy)
    (expect (= (length ids) (hash-table-count table)) :to-be-truthy)
    (dolist (feature features)
      (let* ((feature-id (cl-cc/optimize::opt-roadmap-feature-id feature))
             (doc-status (cl-cc/optimize::opt-roadmap-feature-status feature))
             (evidence-status (%optimize-backend-evidence-status-for-feature feature))
             (evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (expect (search ":" feature-id) :to-be-falsy)
        (expect (member doc-status '(:implemented :partial :planned :unknown)) :to-be-truthy)
        (expect evidence :to-be-truthy)
        (expect (cl-cc/optimize::opt-roadmap-evidence-feature-id evidence) :to-equal feature-id)
        (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be evidence-status)
        (expect (member "docs/notes/optimize-backend.md"
                 (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                 :test #'string=) :to-be-truthy)
         (when (eq evidence-status :implemented)
           (expect (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence) :to-be-truthy))
        (push (cl-cc/optimize::opt-roadmap-evidence-modules evidence) profiles)
        (if (eq evidence-status :implemented)
            (progn
              (incf implemented)
              (expect (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence) :to-be-truthy))
            (progn
              (incf not-implemented)
              (expect (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence) :to-be-falsy)))))
    (expect (> implemented 0) :to-be-truthy)
    (expect (>= not-implemented 0) :to-be-truthy)
    (expect (<= implemented (length ids)) :to-be-truthy)
    (expect (> (length (remove-duplicates profiles :test #'equal)) 5) :to-be-truthy)))

(it-sequential "optimize-backend-roadmap-status-summary-counts-headings"
  (let* ((summary (cl-cc/optimize:optimize-backend-roadmap-status-summary))
         (total (getf summary :total 0))
         (implemented (getf summary :implemented 0))
         (partial (getf summary :partial 0))
         (planned (getf summary :planned 0))
         (unknown (getf summary :unknown 0)))
    (expect (= 232 total) :to-be-truthy)
    (expect (= 232 implemented) :to-be-truthy)
    (expect (= total (+ implemented partial planned unknown)) :to-be-truthy)
    (expect (= partial 0) :to-be-truthy)
    (expect (= planned 0) :to-be-truthy)
    (expect (= unknown 0) :to-be-truthy)))

(it-sequential "optimize-backend-roadmap-all-fr-complete-gate-is-strict"
  (expect (cl-cc/optimize:optimize-backend-roadmap-all-fr-complete-p) :to-be-truthy))

(it-sequential "optimize-backend-roadmap-fr-ids-by-status-partitions-document"
  (let* ((implemented (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :implemented))
         (partial (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :partial))
         (planned (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :planned))
         (unknown (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :unknown))
         (all (append implemented partial planned unknown))
         (all-doc (cl-cc/optimize:optimize-backend-roadmap-doc-fr-ids)))
    (expect (= (length all-doc) (length all)) :to-be-truthy)
    (expect (= (length all-doc) (length (remove-duplicates all :test #'string=))) :to-be-truthy)
    (expect partial :to-be-null)
    (expect planned :to-be-null)
    (expect unknown :to-be-null)
    (expect (every (lambda (id) (member id all-doc :test #'string=)) all) :to-be-truthy)))

(it-sequential "optimize-backend-roadmap-analysis-evidence-is-loaded"
  (let ((complete 0)
        (open 0))
    (dolist (feature-id '("FR-007" "FR-116" "FR-209" "FR-282" "FR-370" "FR-502"))
      (let* ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id))
             (status (and evidence
                          (cl-cc/optimize::opt-roadmap-evidence-status evidence))))
        (expect evidence :to-be-truthy)
        (expect (member status '(:implemented :partial :planned)) :to-be-truthy)
        (expect (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence) :to-be-truthy)
        (if (eq status :implemented)
            (incf complete)
            (progn
              (incf open)
              (expect (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence) :to-be-falsy)))))
     (expect (= 6 (+ complete open)) :to-be-truthy)))

(it-sequential "optimize-backend-roadmap-promoted-existing-frs-have-specific-evidence"
  (dolist (case '(("FR-014" :implemented
                    "packages/optimize/src/optimizer-memory-dse.lisp"
                   cl-cc/optimize::opt-pass-cons-slot-forward
                   cl-cc/optimize::cons-slot-forward-replaces-car-with-original-car-register)
                  ("FR-015" :implemented
                   "packages/optimize/src/optimizer-flow-loop.lisp"
                   cl-cc/optimize::opt-pass-code-sinking
                   cl-cc/optimize::code-sinking-moves-const-into-target-block)
                  ("FR-351" :implemented
                   "packages/optimize/src/egraph-rules.lisp"
                   cl-cc/optimize::egraph-rule-register
                   cl-cc/optimize::egraph-rule-registry-complete)
                  ("FR-403" :implemented
                   "packages/optimize/src/ssa-construction.lisp"
                   cl-cc/optimize::ssa-destroy
                   cl-cc/optimize::ssa-destroy-places-phi-copies-before-terminator)
                  ("FR-404" :implemented
                   "packages/optimize/src/ssa-construction.lisp"
                   cl-cc/optimize::ssa-sequentialize-copies
                   cl-cc/optimize::ssa-seq-copies-behavior)))
    (destructuring-bind (feature-id status module api-symbol test-anchor) case
      (%optimize-backend-assert-evidence-case
       feature-id
       status
       (list module)
       (list api-symbol)
       (list test-anchor)))))

(it-sequential "optimize-backend-roadmap-phase40-frs-have-specific-evidence"
  (dolist (case '(("FR-209" :implemented
                   "packages/optimize/src/optimizer-speculative-peval.lisp"
                   cl-cc/optimize::opt-specialize-constant-args
                   optimize-specialize-constant-args-builds-residual-body)
                  ("FR-210" :implemented
                   "packages/optimize/src/optimizer-speculative-peval.lisp"
                   cl-cc/optimize::opt-sccp-analyze-binding-times
                   optimize-sccp-analyze-binding-times-classifies-lattice-values)
                  ("FR-211" :implemented
                   "packages/optimize/src/optimizer-speculative-peval.lisp"
                   cl-cc/optimize::opt-build-specialization-plan
                   optimize-build-specialization-plan-reuses-cache-for-constant-signature)))
    (destructuring-bind (feature-id status module api-symbol test-anchor) case
      (%optimize-backend-assert-evidence-case
       feature-id
       status
       (list module)
       (list api-symbol)
       (list test-anchor)))))

(it-sequential "optimize-backend-roadmap-fr-217-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-217"
   :implemented
   '("packages/optimize/src/optimizer-memory-alias.lisp"
     "packages/optimize/tests/optimizer-memory-tests.lisp")
   '(cl-cc/optimize::opt-compute-memory-ssa-snapshot
     cl-cc/optimize::opt-memory-ssa-version-at)
   '(memory-ssa-snapshot-assigns-monotonic-versions-for-def-use-chain
     memory-ssa-snapshot-slot-location-uses-alias-root)))

(it-sequential "optimize-backend-roadmap-fr-251-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-251"
   :implemented
   '("packages/optimize/src/optimizer-dataflow.lisp"
     "packages/optimize/tests/optimizer-dataflow-tests.lisp")
   '(cl-cc/optimize::make-opt-abstract-domain
     cl-cc/optimize::opt-run-abstract-interpretation)
   '(abstract-domain-struct-retains-operators
     abstract-interpretation-runs-over-cfg-and-produces-result)))

(it-sequential "optimize-backend-roadmap-fr-252-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-252"
   :implemented
   '("packages/regalloc/src/regalloc.lisp"
     "packages/regalloc/src/regalloc-allocate.lisp"
     "packages/emit/tests/regalloc-tests.lisp")
   '(("CL-CC/REGALLOC" . "REGALLOC-BUILD-DIRECT-CALL-GRAPH")
     ("CL-CC/REGALLOC" . "REGALLOC-COMPUTE-INTERPROCEDURAL-HINTS")
     ("CL-CC/REGALLOC" . "REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS")
     ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
   '(regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain
     regalloc-interprocedural-policy-hook-derives-preferences
     regalloc-interprocedural-policy-caller-saved-respects-call-crossing-safety
     regalloc-interprocedural-policy-end-to-end-keeps-call-crossing-safe
     regalloc-interprocedural-policy-prefers-callee-saved-on-call-crossing)))

(it-sequential "optimize-backend-roadmap-fr-253-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-253"
   :implemented
   '("packages/optimize/src/optimizer-speculative-peval.lisp"
     "packages/optimize/tests/optimizer-pipeline-tests.lisp")
   '(cl-cc/optimize::make-opt-cow-object
     cl-cc/optimize::opt-cow-copy
     cl-cc/optimize::opt-cow-write)
   '(optimize-cow-copy-is-constant-time-share
     optimize-cow-write-detaches-when-shared)))

(it-sequential "optimize-backend-roadmap-fr-254-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-254"
   :implemented
   '("packages/optimize/src/optimizer-speculative-peval.lisp"
     "packages/optimize/tests/optimizer-pipeline-tests.lisp")
   '(cl-cc/optimize::make-opt-bump-region
     cl-cc/optimize::opt-bump-allocate
     cl-cc/optimize::opt-bump-mark
     cl-cc/optimize::opt-bump-reset
     cl-cc/optimize::make-opt-slab-pool
     cl-cc/optimize::opt-slab-allocate
     cl-cc/optimize::opt-slab-free)
   '(optimize-bump-region-mark-reset-restores-cursor
     optimize-slab-pool-reuses-freed-object)))

(it-sequential "optimize-backend-roadmap-fr-008-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-008"
   :implemented
   '("packages/regalloc/src/regalloc.lisp"
     "packages/emit/tests/regalloc-tests.lisp")
   nil
   '(regalloc-float-vregs-allocated-to-distinct-xmm-registers)))

(it-sequential "optimize-backend-roadmap-fr-283-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-283"
   :implemented
   '("packages/vm/src/vm-bitwise.lisp"
     "packages/codegen/src/x86-64-sequences.lisp"
     "packages/codegen/src/aarch64-codegen.lisp"
     "packages/emit/tests/x86-64-encoding-tests.lisp"
     "packages/emit/tests/aarch64-codegen-tests.lisp")
   '(("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-U")
     ("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-S")
     ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-U")
     ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-S")
     ("CL-CC/CODEGEN" . "EMIT-MUL-HIGH-SEQUENCE")
     ("CL-CC/CODEGEN" . "ENCODE-UMULH")
     ("CL-CC/CODEGEN" . "ENCODE-SMULH"))
   '(vm-mul-high-64-semantics
     x86-mul-rm64-high-encodings
     x86-seq-mul-high-sequence-encodings
     x86-mul-high-size-and-dispatch-registered
     a64-mul-high-encoders
     aarch64-mul-high-emitter-encodings
     aarch64-mul-high-size-and-dispatch-registered)))

(it-sequential "optimize-backend-roadmap-fr-303-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-303"
   :implemented
   '("packages/vm/src/vm-instructions.lisp"
     "packages/codegen/src/x86-64-emit-ops.lisp"
     "packages/codegen/src/aarch64-emitters.lisp"
     "packages/emit/tests/x86-64-emit-ops-tests.lisp"
     "packages/emit/tests/aarch64-emit-tests.lisp")
   '(("CL-CC" . "MAKE-VM-ADD-CHECKED")
     ("CL-CC" . "MAKE-VM-SUB-CHECKED")
     ("CL-CC" . "MAKE-VM-MUL-CHECKED")
     ("CL-CC/CODEGEN" . "EMIT-VM-ADD-CHECKED")
     ("CL-CC/CODEGEN" . "EMIT-A64-VM-ADD-CHECKED"))
   '(x86-emit-add-checked-emits-14-bytes
     x86-emit-sub-checked-emits-14-bytes
     x86-emit-mul-checked-emits-15-bytes
     aarch64-emit-add-checked-emits-12-bytes
     aarch64-emit-sub-checked-emits-12-bytes
     aarch64-emit-mul-checked-emits-24-bytes)))

(it-sequential "optimize-backend-roadmap-fr-295-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-295"
   :implemented
   '("packages/optimize/src/optimizer-speculative-peval.lisp"
     "packages/pipeline/src/pipeline.lisp"
     "packages/compile/src/codegen.lisp"
     "packages/cli/src/main-utils.lisp"
     "packages/cli/src/handlers.lisp"
     "packages/optimize/tests/optimizer-pipeline-tests.lisp"
     "packages/compile/tests/pipeline-tests.lisp"
     "packages/cli/tests/cli-tests.lisp")
   '(cl-cc/optimize::opt-pgo-build-counter-plan
     cl-cc/optimize::opt-pgo-make-profile-template
     ("CL-CC/COMPILE" . "COMPILATION-RESULT-PGO-COUNTER-PLAN"))
   '(optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids
     optimize-pgo-make-profile-template-zero-initializes-counts
     pipeline-compile-string-emits-pgo-counter-plan
     cli-maybe-make-profiled-vm-state-enabled-for-pgo-generate
     cli-write-pgo-profile-emits-file)))

(it-sequential "optimize-backend-roadmap-fr-352-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-352"
   :implemented
   '("packages/optimize/src/optimizer-memory-alias.lisp"
     "packages/optimize/src/optimizer-memory-interval.lisp"
     "packages/optimize/src/optimizer.lisp"
     "packages/optimize/tests/optimizer-memory-tests.lisp"
     "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
   '(cl-cc/optimize::opt-interval-logand
     cl-cc/optimize::opt-interval-bit-width
     cl-cc/optimize::opt-pass-elide-proven-overflow-checks
     cl-cc/optimize::%opt-rewrite-logand-low-bit-test
     cl-cc/optimize::opt-pass-fold)
   '(value-ranges-logand-mask-with-unknown-input-narrows-to-8-bit
     value-ranges-add-of-masked-8-bit-values-is-9-bit-wide
     overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add
     optimize-instructions-rewrites-logand-one-eq-zero-to-evenp)))

(it-sequential "optimize-backend-roadmap-fr-523-to-fr-528-have-fr-specific-evidence"
  (dolist (case '(("FR-523" cl-cc/optimize::opt-pass-affine-loop-analysis
                    optimize-affine-loop-summary-builds-descriptor)
                  ("FR-524" cl-cc/optimize::opt-pass-loop-interchange
                   optimize-pass-loop-interchange-handles-nested-canonical-loop)
                  ("FR-525" cl-cc/optimize::opt-pass-polyhedral-schedule
                   optimize-pass-polyhedral-schedule-reorders-loop-body)
                  ("FR-526" cl-cc/optimize::opt-pass-loop-fusion-fission
                   optimize-pass-loop-fusion-fission-splits-oversized-loop)
                  ("FR-527" cl-cc/optimize::opt-ml-inline-score-plan
                   optimize-ml-inline-score-plan-is-deterministic)
                  ("FR-528" cl-cc/optimize::opt-learned-codegen-cost-plan
                   optimize-learned-codegen-cost-plan-is-target-aware)))
    (destructuring-bind (feature-id api-symbol test-anchor) case
      (%optimize-backend-assert-evidence-case
       feature-id
       :implemented
       '("packages/optimize/src/optimizer-speculative-peval.lisp"
         "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (list api-symbol)
       (list test-anchor)))))

(it-sequential "optimize-backend-roadmap-audited-fr-statuses-match-doc"
  (let ((table (make-hash-table :test #'equal)))
    (dolist (feature (cl-cc/optimize:optimize-backend-roadmap-doc-features))
      (setf (gethash (cl-cc/optimize::opt-roadmap-feature-id feature) table)
            (cl-cc/optimize::opt-roadmap-feature-status feature)))
    (dolist (case '(("FR-303" :implemented)
                    ("FR-352" :implemented)
                    ("FR-360" :implemented)
                    ("FR-366" :implemented)
                    ("FR-370" :implemented)
                    ("FR-374" :implemented)
                    ("FR-376" :implemented)
                    ("FR-377" :implemented)
                     ("FR-379" :implemented)
                     ("FR-389" :implemented)
                     ("FR-391" :implemented)
                     ("FR-400" :implemented)
                     ("FR-401" :implemented)
                     ("FR-462" :implemented)
                     ("FR-463" :implemented)))
      (destructuring-bind (feature-id expected-status) case
        (expect (gethash feature-id table) :to-be expected-status)))))

(it-sequential "optimize-backend-roadmap-audited-frs-have-specific-evidence"
  (dolist (case '(("FR-303" :implemented
                   "packages/codegen/src/x86-64-emit-ops.lisp"
                   ("CL-CC/CODEGEN" . "EMIT-VM-ADD-CHECKED")
                   x86-emit-add-checked-emits-14-bytes)
                  ("FR-352" :implemented
                   "packages/optimize/src/optimizer-memory-alias.lisp"
                   cl-cc/optimize::opt-interval-logand
                   overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add)
                  ("FR-360" :implemented
                    "packages/compile/src/codegen-core-control.lisp"
                    ("CL-CC/COMPILE" . "%COMPILE-IF-BRANCH")
                    codegen-the-with-declared-integer-type-emits-typep)
                   ("FR-366" :implemented "packages/expand/src/macros-runtime-support.lisp" ("CL-CC/EXPAND" . "*LOAD-TIME-VALUE-CACHE*") load-time-value-expansion-preserves-form-without-evaluation)
                  ("FR-370" :implemented
                   "packages/compile/src/context.lisp"
                   ("CL-CC/COMPILE" . "*BUILTIN-SPECIAL-VARIABLES*")
                   ctx-initialization)
                   ("FR-374" :implemented
                    "packages/vm/src/vm-dispatch-gf.lisp"
                    ("CL-CC/VM" . "%VM-GF-EQL-METHODS")
                    gf-multi-single-dispatch-eql-index-hit-precedes-class)
                  ("FR-376" :implemented
                   "packages/expand/src/expander-control.lisp"
                   ("CL-CC/EXPAND" . "%EXPAND-HANDLER-CASE-FORM")
                   codegen-handler-case-run-cases)
                  ("FR-377" :implemented
                   "packages/compile/src/codegen-control.lisp"
                   ("CL-CC/COMPILE" . "COMPILE-AST")
                   codegen-unwind-protect-run-cases)
                  ("FR-379" :implemented
                   "packages/vm/src/vm-extensions.lisp"
                   ("CL-CC/VM" . "VM-SYMBOL-PLIST-READ-SNAPSHOT")
                   vm-set-symbol-plist-overwrites-and-promotes-long-plist)
                    ("FR-388" :implemented
                     "packages/codegen/src/x86-64-regs.lisp"
                     ("CL-CC/CODEGEN" . "*X86-64-OMIT-FRAME-POINTER*")
                     x86-vm-program-default-fpe-allocates-rsp-spill-frame)
                    ("FR-389" :implemented
                      "packages/codegen/src/x86-64-regs.lisp"
                      ("CL-CC/CODEGEN" . "X86-64-RED-ZONE-SPILL-P")
                      x86-vm-program-leaf-red-zone-spills-skip-rbp-frame)
                   ("FR-391" :implemented
                    "packages/codegen/src/x86-64-codegen-core.lisp"
                    ("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-PROBES")
                    x86-stack-probe-count-thresholds)
                   ("FR-400" :implemented
                    "packages/expand/src/macros-control-flow-case.lisp"
                   ("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES")
                   ecase-expands-to-let-with-case)
                  ("FR-401" :implemented
                   "packages/expand/src/macros-control-flow-case.lisp"
                   ("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TABLE")
                   case-expands-dense-integer-keys-into-table-dispatch)
                   ("FR-462" :implemented
                    "packages/cli/src/main-utils.lisp"
                    ("CL-CC/CLI" . "%WRITE-FLAMEGRAPH-SVG")
                    cli-write-flamegraph-svg-emits-svg-document)
                   ("FR-463" :implemented
                    "packages/cli/src/main-dump.lisp"
                    ("CL-CC/CLI" . "%DUMP-IR-PHASE")
                    cli-dump-ir-phase-dispatches-all-phases)))
    (destructuring-bind (feature-id status module api-symbol test-anchor) case
      (%optimize-backend-assert-evidence-case
       feature-id
       status
       (list module)
       (list api-symbol)
       (list test-anchor)))))

(it-sequential "optimize-backend-roadmap-support-evidence-has-behavior"
  (let* ((bottom (cl-cc/optimize::opt-lattice-bottom))
         (const-a (cl-cc/optimize::opt-lattice-constant :a))
         (const-b (cl-cc/optimize::opt-lattice-constant :b)))
    (expect (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet bottom const-a)) :to-be :constant)
    (expect (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet const-a const-b)) :to-be :overdefined))
  (let ((profile (cl-cc/optimize::make-opt-profile-data)))
    (expect (= 5 (cl-cc/optimize::opt-profile-record-edge profile :a :b 5)) :to-be-truthy)
    (expect (= 2 (cl-cc/optimize::opt-profile-record-call-chain profile '(:a :b) 2)) :to-be-truthy)
    (expect (cl-cc/optimize::opt-profile-record-allocation profile :site 32 3) :to-equal (cons 3 32)))
  (let ((region (cl-cc/optimize::make-opt-bump-region :limit 16)))
    (expect (= 0 (cl-cc/optimize::opt-bump-allocate region 4)) :to-be-truthy)
    (expect (= 8 (cl-cc/optimize::opt-bump-allocate region 4 :alignment 8)) :to-be-truthy))
  (let ((guard (cl-cc/optimize::make-opt-guard-state :executions 10)))
    (expect (cl-cc/optimize::opt-guard-record guard t) :to-be :tag-bit-test))
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 1 '(:x :y :z))))
    (expect (= 2 (cl-cc/optimize::opt-shape-slot-offset shape :z)) :to-be-truthy)))

(it-sequential "optimize-backend-roadmap-fr-463-has-specific-evidence"
  (%optimize-backend-assert-evidence-case
   "FR-463"
   :implemented
   '("packages/cli/src/main-dump.lisp"
     "packages/cli/src/main-utils.lisp"
     "packages/cli/src/args.lisp"
     "packages/cli/src/handlers.lisp"
     "packages/pipeline/src/pipeline.lisp"
     "packages/cli/tests/main-dump-tests.lisp"
     "packages/cli/tests/cli-tests.lisp")
   '(("CL-CC/CLI" . "%DUMP-IR-PHASE")
     ("CL-CC/CLI" . "%DUMP-AST-PHASE")
     ("CL-CC/CLI" . "%DUMP-CPS-PHASE")
     ("CL-CC/CLI" . "%DUMP-SSA-PHASE")
     ("CL-CC/CLI" . "%DUMP-VM-PHASE")
     ("CL-CC/CLI" . "%DUMP-OPT-PHASE")
     ("CL-CC/CLI" . "%DUMP-ASM-PHASE")
     ("CL-CC/CLI" . "*IR-PHASE-DUMP-FNS*")
     ("CL-CC/CLI" . "*IR-PHASES*"))
   '(cli-dump-ir-phase-dispatches-all-phases
     cli-dump-ir-phase-annotate-source-writes-comment-for-ast
     cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt
     cli-dump-ir-phase-asm-output-is-ansi-colored
     cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location
     cli-real-file-dump-ir-annotation-preserves-source-location
     cli-do-compile-dump-ir-annotate-source-preserves-real-file-location
     cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location
     cli-dump-ir-phase-phase-table-covers-all-recognized-phases
     cli-dump-ir-phase-invalid-signals-error)))

(progn
  (it-sequential
    "optimize-backend-roadmap-fr-345-fr-366-fr-367-integrated-evidence-resolves"
    (let ((table (make-hash-table :test #'equal)))
      (dolist (feature (cl-cc/optimize:optimize-backend-roadmap-doc-features))
        (setf (gethash (cl-cc/optimize::opt-roadmap-feature-id feature) table) (cl-cc/optimize::opt-roadmap-feature-status feature)))
      (dolist (feature-id '("FR-345" "FR-366" "FR-367"))
        (expect (gethash feature-id table) :to-be :implemented)))
    (dolist (module
        '("packages/optimize/src/optimizer-trans-validate.lisp"
          "packages/optimize/t/optimize-boundary-test.lisp"
          "packages/vm/src/vm-dsl.lisp"
          "packages/vm/src/vm-serialize.lisp"
          "packages/vm/t/vm-boundary-test.lisp"
          "packages/compile/src/codegen-core-let.lisp"
          "packages/compile/src/codegen-core-let-emit-pass.lisp"))
      (expect (cl-cc/optimize::%opt-roadmap-module-present-p module) :to-be-truthy))
    (dolist (api-symbol
        '(("CL-CC/OPTIMIZE" . "TRANSLATION-VALIDATION-EQUIVALENT-P")
          ("CL-CC/OPTIMIZE" . "VALIDATE-OPTIMIZER-TRANSLATION")
          ("CL-CC/VM" . "VM-WRITE-TO-FASL")
          ("CL-CC/VM" . "VM-READ-FROM-FASL")
          ("CL-CC/COMPILE" . "%AST-LET-BINDING-IGNORED-P")))
      (expect
        (cl-cc/optimize::%opt-roadmap-api-entry-fbound-p api-symbol)
        :to-be-truthy))
    (dolist (test-anchor
        '(optimize-differential-fuzzing-deterministic-smoke
          compile-run-matches-reference-arith
          tier-0-and-tier-1-match-reference-arith
          load-time-value-expansion-preserves-form-without-evaluation
          ast-let-binding-ignored-p
          codegen-let-ignore-binding-enables-dce-of-unused-initializer))
      (expect
        (cl-cc/optimize::%opt-roadmap-test-anchor-registered-p test-anchor)
        :to-be-truthy)))
  (it-sequential
    "optimize-backend-roadmap-fr-442-has-specific-evidence"
    (%optimize-backend-assert-evidence-case
      "FR-442"
      :implemented
      (quote ("packages/emit/src/fpga.lisp" "packages/emit/tests/fpga-tests.lisp"))
      (quote (("CL-CC/EMIT" . "LOWER-FPGA-HLS") ("CL-CC/EMIT" . "EMIT-FPGA-VERILOG")))
      (quote
        (fpga-hls-builds-a-timed-resource-shared-pipeline
          fpga-hls-lowers-case-to-fsm-ir
          fpga-hls-rejects-impure-and-unsupported-forms)))))
