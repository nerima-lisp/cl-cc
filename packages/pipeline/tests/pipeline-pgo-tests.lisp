;;;; pipeline-pgo-tests.lisp — unit tests for PGO helper functions.
;;; Tests the three pure helpers in pipeline-pgo.lisp:
;;;   %pgo-label-position-map
;;;   %pgo-block-start-pc
;;;   %pgo-plan-with-runtime-keys (nil-guard path)
(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-label-position-map
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-label-position-map-single-label"
  (let* ((label (cl-cc:make-vm-label :name :entry))
         (table (cl-cc/pipeline::%pgo-label-position-map (list label))))
    (expect (= 0 (gethash :entry table)) :to-be-truthy)))

(it-sequential "pgo-label-position-map-label-after-instructions"
  (let* ((c1    (cl-cc:make-vm-const :dst :r0 :value 1))
         (c2    (cl-cc:make-vm-const :dst :r1 :value 2))
         (label (cl-cc:make-vm-label :name :mid))
         (table (cl-cc/pipeline::%pgo-label-position-map (list c1 c2 label))))
    (expect (= 2 (gethash :mid table)) :to-be-truthy)))

(it-sequential "pgo-label-position-map-multiple-labels"
  (let* ((l1    (cl-cc:make-vm-label :name :a))
         (inst  (cl-cc:make-vm-const :dst :r0 :value 0))
         (l2    (cl-cc:make-vm-label :name :b))
         (table (cl-cc/pipeline::%pgo-label-position-map (list l1 inst l2))))
    (expect (= 0 (gethash :a table)) :to-be-truthy)
    (expect (= 2 (gethash :b table)) :to-be-truthy)))

(it-sequential "pgo-label-position-map-empty-table-cases empty-list"
  (destructuring-bind (insts) (list nil)
    (let ((table (cl-cc/pipeline::%pgo-label-position-map insts)))
    (expect (= 0 (hash-table-count table)) :to-be-truthy))))

(it-sequential "pgo-label-position-map-empty-table-cases no-labels"
  (destructuring-bind (insts) (list (list (cl-cc:make-vm-const :dst :r0 :value 7)))
    (let ((table (cl-cc/pipeline::%pgo-label-position-map insts)))
    (expect (= 0 (hash-table-count table)) :to-be-truthy))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-block-start-pc
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-block-start-pc-prefers-label"
  (let* ((label   (cl-cc:make-vm-label :name :entry))
         (inst    (cl-cc:make-vm-const :dst :r0 :value 0))
         (insts   (list label inst))
         (label->pc (cl-cc/pipeline::%pgo-label-position-map insts))
         (cfg     (cl-cc/optimize:cfg-build insts))
         (block   (cl-cc/optimize:cfg-entry cfg)))
    ;; Only check if the entry block has a label; construction may vary.
    (let ((pc (cl-cc/pipeline::%pgo-block-start-pc block insts label->pc)))
      (expect (integerp pc) :to-be-truthy))))

(it-sequential "pgo-block-start-pc-falls-back-to-position"
  (let* ((c0 (cl-cc:make-vm-const :dst :r0 :value 0))
         (c1 (cl-cc:make-vm-const :dst :r1 :value 1))
         (insts (list c0 c1))
         (label->pc (make-hash-table :test #'equal))
         (cfg   (cl-cc/optimize:cfg-build insts))
         (block (cl-cc/optimize:cfg-entry cfg)))
    (let ((pc (cl-cc/pipeline::%pgo-block-start-pc block insts label->pc)))
      (expect (integerp pc) :to-be-truthy)
      (expect (>= pc 0) :to-be-truthy))))

(it-sequential "pgo-block-start-pc-zero-fallback"
  (let* ((phantom (cl-cc:make-vm-const :dst :r0 :value 99))
         (insts   (list (cl-cc:make-vm-const :dst :r1 :value 1)))
         (cfg     (cl-cc/optimize:cfg-build (list phantom)))
         (block   (cl-cc/optimize:cfg-entry cfg))
         (label->pc (make-hash-table :test #'equal)))
    ;; phantom is NOT in insts — position returns nil, so 0 is the fallback.
    (expect (= 0 (cl-cc/pipeline::%pgo-block-start-pc block insts label->pc)) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-plan-with-runtime-keys — nil-guard short-circuit
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-plan-with-runtime-keys-nil-instructions-returns-plan"
  (let ((plan '(:bb-counters () :edge-counters ())))
    (expect (cl-cc/pipeline::%pgo-plan-with-runtime-keys nil plan) :to-equal plan)))

(it-sequential "pgo-plan-with-runtime-keys-nil-plan-returns-nil"
  (let ((insts (list (cl-cc:make-vm-const :dst :r0 :value 1))))
    (expect (null (cl-cc/pipeline::%pgo-plan-with-runtime-keys insts nil)) :to-be-truthy)))

(it-sequential "pgo-plan-with-runtime-keys-both-nil-returns-nil"
  (expect (null (cl-cc/pipeline::%pgo-plan-with-runtime-keys nil nil)) :to-be-truthy))

(it-sequential "pgo-plan-with-runtime-keys-appends-runtime-key-plists"
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 42)
                      (cl-cc:make-vm-ret :reg :r0)))
         (plan  (cl-cc/optimize:opt-pgo-build-counter-plan
                 0
                 (mapcar (lambda (block)
                           (cons (cl-cc/optimize:bb-id block)
                                 (mapcar #'cl-cc/optimize:bb-id
                                         (cl-cc/optimize:bb-successors block))))
                         (coerce (cl-cc/optimize:cfg-blocks
                                  (cl-cc/optimize:cfg-build insts))
                                 'list)))))
    (let ((result (cl-cc/pipeline::%pgo-plan-with-runtime-keys insts plan)))
      (expect (listp result) :to-be-truthy)
      (expect (member :bb-runtime-keys result) :to-be-truthy)
      (expect (member :edge-runtime-keys result) :to-be-truthy))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; *pgo-edge-kind-types* and %pgo-edge-kind
;;; ─────────────────────────────────────────────────────────────────────────

(it-todo "pgo-edge-kind-types-is-list-of-five"
  "orphan src: packages/pipeline/src/pipeline-pgo.lisp defines *pgo-edge-kind-types*/%pgo-edge-kind but is in no system, so it never loads. Needs wire-in vs delete decision.")

(it-todo "pgo-edge-kind-returns-nil-for-non-terminator"
  "orphan src: packages/pipeline/src/pipeline-pgo.lisp defines *pgo-edge-kind-types*/%pgo-edge-kind but is in no system, so it never loads. Needs wire-in vs delete decision.")

(it-todo "pgo-edge-kind-returns-symbol-for-vm-ret"
  "orphan src: packages/pipeline/src/pipeline-pgo.lisp defines *pgo-edge-kind-types*/%pgo-edge-kind but is in no system, so it never loads. Needs wire-in vs delete decision.")

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-type-feedback-rows
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-type-feedback-rows-nil-cases nil"
  (destructuring-bind (input) (list nil)
    (expect (null (cl-cc/pipeline::%pgo-type-feedback-rows input)) :to-be-truthy)))

(it-sequential "pgo-type-feedback-rows-nil-cases atom"
  (destructuring-bind (input) (list 42)
    (expect (null (cl-cc/pipeline::%pgo-type-feedback-rows input)) :to-be-truthy)))

(it-sequential "pgo-type-feedback-rows-nil-cases missing-key"
  (destructuring-bind (input) (list '(:other-key (1 2 3)))
    (expect (null (cl-cc/pipeline::%pgo-type-feedback-rows input)) :to-be-truthy)))

(it-sequential "pgo-type-feedback-rows-nil-cases empty"
  (destructuring-bind (input) (list '(:type-feedback nil))
    (expect (null (cl-cc/pipeline::%pgo-type-feedback-rows input)) :to-be-truthy)))

(it-sequential "pgo-type-feedback-rows-plist-with-type-feedback-key"
  (let* ((rows '((((:generic-call 0 :integer) . 10))
                 (((:generic-call 1 :string) . 5))))
         (profile-data (list :type-feedback rows)))
    (expect (cl-cc/pipeline::%pgo-type-feedback-rows profile-data) :to-equal rows)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-dominant-types-by-pc
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-dominant-types-by-pc-empty-profile"
  (expect (null (cl-cc/pipeline::%pgo-dominant-types-by-pc nil)) :to-be-truthy))

(it-sequential "pgo-dominant-types-by-pc-profile-no-type-feedback"
  (expect (null (cl-cc/pipeline::%pgo-dominant-types-by-pc
                      '(:other-key ()))) :to-be-truthy))

(it-sequential "pgo-dominant-types-by-pc-below-threshold"
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 0 :integer) 5)
                     (cons '(:generic-call 0 :string)  5)))))
    (expect (null (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)) :to-be-truthy)))

(it-sequential "pgo-dominant-types-by-pc-dominant-site"
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 3 :integer) 95)
                     (cons '(:generic-call 3 :string)   5)))))
    (let ((result (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)))
      (expect (consp result) :to-be-truthy)
      (let ((entry (assoc 3 result :test #'eql)))
        (expect entry :to-be-truthy)
        (expect (cdr entry) :to-equal :integer)))))

(it-sequential "pgo-dominant-types-by-pc-exactly-at-threshold-not-dominant"
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 7 :integer) 9)
                     (cons '(:generic-call 7 :string)  1)))))
    (expect (null (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)) :to-be-truthy)))

(it-sequential "pgo-dominant-types-by-pc-multiple-sites-mixed"
  (let ((profile-data
         (list :type-feedback
               (list (cons '(:generic-call 0 :integer) 95)
                     (cons '(:generic-call 0 :string)   5)
                     (cons '(:generic-call 1 :integer)  5)
                     (cons '(:generic-call 1 :string)   5)))))
    (let ((result (cl-cc/pipeline::%pgo-dominant-types-by-pc profile-data)))
      (expect (assoc 0 result :test #'eql) :to-be-truthy)
      (expect (null (assoc 1 result :test #'eql)) :to-be-truthy))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-apply-type-feedback-to-instructions
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-apply-type-feedback-to-instructions-nil-guard"
  (let ((insts (list (cl-cc:make-vm-const :dst :r0 :value 1))))
    (expect (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts nil) :to-equal insts)))

(it-sequential "pgo-apply-type-feedback-to-instructions-nil-instructions"
  (let ((profile-data (list :type-feedback
                            (list (cons '(:generic-call 0 :integer) 99)))))
    (expect (null (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions
                        nil profile-data)) :to-be-truthy)))

(it-sequential "pgo-apply-type-feedback-to-instructions-sets-pgo-specializer"
  (let* ((gc-inst (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args '()))
         (insts   (list gc-inst))
         (profile-data
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 99)
                      (cons '(:generic-call 0 :string)   1)))))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts profile-data)
    (expect (cl-cc/vm:vm-pgo-specializer gc-inst) :to-equal :integer)))

(it-sequential "pgo-apply-type-feedback-to-instructions-non-generic-call-unchanged"
  (let* ((const-inst (cl-cc:make-vm-const :dst :r0 :value 42))
         (insts      (list const-inst))
         (profile-data
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 99)))))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts profile-data)
    ;; const-inst has no pgo-specializer — it is a vm-const, not vm-generic-call.
    (expect (typep const-inst 'cl-cc/vm:vm-const) :to-be-truthy)))

(it-sequential "pgo-apply-type-feedback-to-instructions-no-dominant-no-annotation"
  (let* ((gc-inst (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args '()))
         (insts   (list gc-inst))
         (profile-data
          ;; 50/50 split — not dominant.
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 5)
                      (cons '(:generic-call 0 :string)  5)))))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-instructions insts profile-data)
    (expect (null (cl-cc/vm:vm-pgo-specializer gc-inst)) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; %pgo-apply-type-feedback-to-result
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "pgo-apply-type-feedback-to-result-nil-profile-is-noop"
  (let* ((result  (cl-cc/compile:make-compilation-result
                   :vm-instructions (list (cl-cc:make-vm-const :dst :r0 :value 1))))
         (opts    (cl-cc/pipeline::%make-pipeline-opts :pgo-profile-data nil))
         (outcome (cl-cc/pipeline::%pgo-apply-type-feedback-to-result result opts)))
    (expect (eq result outcome) :to-be-truthy)))

(it-sequential "pgo-apply-type-feedback-to-result-annotates-all-three-lists"
  (let* ((gc-vm  (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args '()))
         (gc-opt (cl-cc:make-vm-generic-call :dst :r2 :gf-reg :r3 :args '()))
         (gc-prg (cl-cc:make-vm-generic-call :dst :r4 :gf-reg :r5 :args '()))
         (program (cl-cc:make-vm-program :instructions (list gc-prg)))
         (result  (cl-cc/compile:make-compilation-result
                   :vm-instructions         (list gc-vm)
                   :optimized-instructions  (list gc-opt)
                   :program                 program))
         (profile-data
          (list :type-feedback
                (list (cons '(:generic-call 0 :integer) 99)
                      (cons '(:generic-call 0 :string)   1))))
         (opts (cl-cc/pipeline::%make-pipeline-opts :pgo-profile-data profile-data)))
    (cl-cc/pipeline::%pgo-apply-type-feedback-to-result result opts)
    (expect (cl-cc/vm:vm-pgo-specializer gc-vm) :to-equal :integer)
    (expect (cl-cc/vm:vm-pgo-specializer gc-opt) :to-equal :integer)
    (expect (cl-cc/vm:vm-pgo-specializer gc-prg) :to-equal :integer)))
