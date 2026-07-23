;;;; tests/unit/optimize/optimizer-pipeline-loop-transform-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — polyhedral and loop transform passes
;;;;
;;;; Covers: FR-523 affine loop analysis, FR-524 loop interchange,
;;;;   FR-525 polyhedral scheduling, FR-526 loop fusion/fission,
;;;;   FR-527 ML inline scoring, FR-528 learned codegen costs.

(in-package :cl-cc/test)

;;; ─── FR-523 Affine loop analysis ────────────────────────────────────────────

(it-sequential "optimize-affine-loop-summary-builds-descriptor"
  (let ((summary (cl-cc/optimize::opt-build-affine-loop-summary
                  :induction-vars '(:i)
                  :bounds '((:i 0 100))
                  :accesses '((:a :i)))))
    (expect (getf summary :kind) :to-be :affine-loop-summary)
    (expect (getf summary :induction-vars) :to-equal '(:i))
    (expect (getf summary :bounds) :to-equal '((:i 0 100)))))

(it-sequential "optimize-pass-affine-loop-analysis-captures-real-loop-summary"
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-get-global :dst :r2 :name 'g)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (_ (cl-cc/optimize::opt-pass-affine-loop-analysis program))
         (summaries cl-cc/optimize::*opt-last-affine-loop-summaries*)
         (summary (first summaries)))
    (expect (listp summaries) :to-be-truthy)
    (expect summary :to-be-truthy)
    (expect (getf summary :kind) :to-be :affine-loop-summary)
    (expect (getf summary :induction-vars) :to-equal '(:ri))
    (expect (some (lambda (access)
                         (eq (getf access :kind) :read-global))
                       (getf summary :accesses)) :to-be-truthy)))

;;; ─── FR-524 Loop interchange ─────────────────────────────────────────────────

(it-sequential "optimize-loop-interchange-plan-safety-gate safe-applies"
  (destructuring-bind (dependence-safe-p expected-applied) (list t t)
    (let ((plan (cl-cc/optimize::opt-loop-interchange-plan
               :loops '(:i :j)
               :cache-locality-score 3
               :dependence-safe-p dependence-safe-p)))
    (expect (not (null (getf plan :applied-p))) :to-equal expected-applied))))

(it-sequential "optimize-loop-interchange-plan-safety-gate unsafe-blocked"
  (destructuring-bind (dependence-safe-p expected-applied) (list nil nil)
    (let ((plan (cl-cc/optimize::opt-loop-interchange-plan
               :loops '(:i :j)
               :cache-locality-score 3
               :dependence-safe-p dependence-safe-p)))
    (expect (not (null (getf plan :applied-p))) :to-equal expected-applied))))

(it-sequential "optimize-pass-loop-interchange-handles-nested-canonical-loop"
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-mul :dst :r7 :lhs :r3 :rhs :r4)
                        (make-vm-move :dst :r8 :src :r5)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (optimized (cl-cc/optimize::opt-pass-loop-interchange program)))
    (expect (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)) :to-be-falsy)
    (expect (typep (nth 6 optimized) 'vm-move) :to-be-truthy)
    (expect (typep (nth 7 optimized) 'vm-mul) :to-be-truthy)))

(it-sequential "optimize-pass-loop-interchange-skips-side-effecting-loop"
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-set-global :src :r2 :name 'g)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (optimized (cl-cc/optimize::opt-pass-loop-interchange program)))
    (expect (mapcar #'instruction->sexp program) :to-equal (mapcar #'instruction->sexp optimized))))

;;; ─── FR-525 Polyhedral scheduling ───────────────────────────────────────────

(it-sequential "optimize-polyhedral-schedule-plan-preserves-objective"
  (let ((plan (cl-cc/optimize::opt-polyhedral-schedule-plan
               :statements '(:s0 :s1)
               :constraints '((:s0-before :s1))
               :objective :throughput-max)))
    (expect (getf plan :kind) :to-be :polyhedral-schedule)
    (expect (getf plan :objective) :to-be :throughput-max)
    (expect (getf plan :statements) :to-equal '(:s0 :s1))))

(it-sequential "optimize-pass-polyhedral-schedule-reorders-loop-body"
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-mul :dst :r7 :lhs :r3 :rhs :r4)
                        (make-vm-move :dst :r8 :src :r5)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (optimized (cl-cc/optimize::opt-pass-polyhedral-schedule program)))
    (expect (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)) :to-be-falsy)
    (expect (typep (nth 6 optimized) 'vm-move) :to-be-truthy)
    (expect (typep (nth 7 optimized) 'vm-mul) :to-be-truthy)))

;;; ─── FR-526 Loop fusion / fission ───────────────────────────────────────────

(it-sequential "optimize-loop-fusion-fission-strategy-selection fusion-low-pressure"
  (destructuring-bind (register-pressure expected-strategy) (list 16 :fusion)
    (let ((plan (cl-cc/optimize::opt-loop-fusion-fission-plan
               :loops '(:l0 :l1)
               :register-pressure register-pressure
               :instruction-budget 20)))
    (expect (getf plan :strategy) :to-be expected-strategy))))

(it-sequential "optimize-loop-fusion-fission-strategy-selection fission-high-pressure"
  (destructuring-bind (register-pressure expected-strategy) (list 64 :fission)
    (let ((plan (cl-cc/optimize::opt-loop-fusion-fission-plan
               :loops '(:l0 :l1)
               :register-pressure register-pressure
               :instruction-budget 20)))
    (expect (getf plan :strategy) :to-be expected-strategy))))

(it-sequential "optimize-pass-loop-fusion-fission-fuses-adjacent-loops"
  (let* ((program
           (list (make-vm-const :dst :ri :value 0)
                 (make-vm-const :dst :rj :value 0)
                 (make-vm-const :dst :rlim :value 4)
                 (make-vm-const :dst :rstep :value 1)
                  ;; loop A
                  (make-vm-label :name :la)
                  (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                  (make-vm-jump-zero :reg :rc :label :lax)
                  (make-vm-move :dst :r10 :src :r11)
                  (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                  (make-vm-jump :label :la)
                  (make-vm-label :name :lax)
                  ;; loop B
                  (make-vm-label :name :lb)
                  (make-vm-lt :dst :rc :lhs :rj :rhs :rlim)
                  (make-vm-jump-zero :reg :rc :label :lbx)
                  (make-vm-move :dst :r12 :src :r13)
                  (make-vm-add :dst :rj :lhs :rj :rhs :rstep)
                  (make-vm-jump :label :lb)
                  (make-vm-label :name :lbx)))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (expect (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)) :to-be-falsy)
    (expect (= 1 (count-if (lambda (inst)
                            (and (typep inst 'vm-label)
                                 (eq (cl-cc/vm::vm-name inst) :la)))
                          optimized)) :to-be-truthy)
    (expect (= 0 (count-if (lambda (inst)
                            (and (typep inst 'vm-label)
                                 (eq (cl-cc/vm::vm-name inst) :lb)))
                          optimized)) :to-be-truthy)))

(it-sequential "optimize-pass-loop-fusion-fission-skips-unsafe-fusion"
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rj :value 1) ;; different init
                        (make-vm-const :dst :rlim :value 4)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :la)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :lax)
                        (make-vm-move :dst :r10 :src :r11)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :la)
                        (make-vm-label :name :lax)
                        (make-vm-label :name :lb)
                        (make-vm-lt :dst :rc2 :lhs :rj :rhs :rlim)
                        (make-vm-jump-zero :reg :rc2 :label :lbx)
                        (make-vm-move :dst :r12 :src :r13)
                        (make-vm-add :dst :rj :lhs :rj :rhs :rstep)
                        (make-vm-jump :label :lb)
                        (make-vm-label :name :lbx)))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (expect (mapcar #'instruction->sexp program) :to-equal (mapcar #'instruction->sexp optimized))))

(it-sequential "optimize-pass-loop-fusion-fission-splits-oversized-loop"
  (let* ((core (loop for idx from 0 below 36
                     collect (make-vm-move :dst :r8 :src :r5)))
         (program (append (list (make-vm-const :dst :ri :value 0)
                                (make-vm-const :dst :rlim :value 8)
                                (make-vm-const :dst :rstep :value 1)
                                (make-vm-label :name :l0)
                                (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                                (make-vm-jump-zero :reg :rc :label :l1))
                          core
                          (list (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                                (make-vm-jump :label :l0)
                                (make-vm-label :name :l1))))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (expect (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)) :to-be-falsy)
    (expect (some (lambda (inst)
                         (and (typep inst 'vm-label)
                              (search "__SPLIT" (string (cl-cc/vm::vm-name inst)))))
                       optimized) :to-be-truthy)))

;;; ─── FR-527 ML inline scoring ────────────────────────────────────────────────

(it-sequential "optimize-ml-inline-score-plan-is-deterministic"
  (let ((a (cl-cc/optimize::opt-ml-inline-score-plan
            :features '(:hot-loop :small-body)
            :model-version "mlgo-v2"))
        (b (cl-cc/optimize::opt-ml-inline-score-plan
            :features '(:hot-loop :small-body)
            :model-version "mlgo-v2")))
    (expect (getf a :kind) :to-be :ml-inline-score)
    (expect (= (getf a :score) (getf b :score)) :to-be-truthy)
    (expect (= 2 (getf a :feature-count)) :to-be-truthy)))

;;; ─── FR-528 Learned codegen cost ─────────────────────────────────────────────

(it-sequential "optimize-learned-codegen-cost-plan-is-target-aware"
  (let ((x86 (cl-cc/optimize::opt-learned-codegen-cost-plan
              :opcode-features '(:mul :add)
              :target :x86-64))
        (arm (cl-cc/optimize::opt-learned-codegen-cost-plan
              :opcode-features '(:mul :add)
              :target :aarch64)))
    (expect (getf x86 :kind) :to-be :learned-codegen-cost)
    (expect (getf x86 :target) :to-be :x86-64)
    (expect (getf arm :target) :to-be :aarch64)
    (expect (/= (getf x86 :predicted-cost)
                     (getf arm :predicted-cost)) :to-be-truthy)))
