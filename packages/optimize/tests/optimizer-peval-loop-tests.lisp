;;;; tests/unit/optimize/optimizer-peval-loop-tests.lisp
;;;; Unit tests for partial evaluation and loop-level optimization helpers.
;;;;
;;;; Covers: FR-209 constant specialization, FR-210 SCCP binding-times,
;;;;   FR-211 specialization plan cache, FR-295 PGO counter plans,
;;;;   program-level partial evaluation, offline BTA, specialize-known-args pass,
;;;;   FR-523 affine loop analysis, FR-524 loop interchange, FR-525 polyhedral
;;;;   scheduling, FR-526 loop fusion/fission, FR-527 MLGO scoring,
;;;;   FR-528 learned codegen cost.

(in-package :cl-cc/test)

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(it-sequential "optimize-specialize-constant-args-builds-residual-body"
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((if (= x 0) 0 (* x y)))
           '((x . 0)))))
    (expect (cl-cc/optimize:opt-partial-spec-original-name specialization) :to-equal 'f)
    (expect (cl-cc/optimize:opt-partial-spec-signature specialization) :to-equal '((x . 0)))
    (expect (cl-cc/optimize:opt-partial-spec-static-args specialization) :to-equal '((x . 0)))
    (expect (cl-cc/optimize:opt-partial-spec-dynamic-args specialization) :to-equal '(y))
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal '((if (= 0 0) 0 (* 0 y))))))

(it-sequential "optimize-specialize-constant-args-respects-lexical-binders-and-quoted-data"
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((quote x)
             (let ((x y) (z x)) (+ x z y))
             (let* ((z x) (x y)) (+ x z y))
             (lambda (x) (+ x y))
             (setq x y)
             (x y))
           '((x . 0) (y . 7)))))
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal '((quote x)
                    (let ((x 7) (z 0)) (+ x z 7))
                    (let* ((z 0) (x 7)) (+ x z 7))
                    (lambda (x) (+ x 7))
                    (setq x 7)
                    (x 7)))))

(it-sequential "optimize-specialize-constant-args-kills-signature-after-setq"
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((setq x y)
             (+ x y)
             (setq x (+ x 1))
             (+ x y)
             (setq x y y x)
             (+ x y))
           '((x . 0) (y . 7)))))
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal '((setq x 7)
                    (+ x 7)
                    (setq x (+ x 1))
                    (+ x 7)
                    (setq x 7 y x)
                    (+ x y)))))

(it-sequential "optimize-sccp-analyze-binding-times-classifies-lattice-values"
  (let* ((analysis
           (cl-cc/optimize:opt-sccp-analyze-binding-times
            '(x y z)
            `((x . ,(cl-cc/optimize:opt-lattice-constant 42))
              (y . ,(cl-cc/optimize:opt-lattice-overdefined))))))
    (expect (cl-cc/optimize:opt-binding-time-kind (first analysis)) :to-be :static)
    (expect (cl-cc/optimize:opt-binding-time-value (first analysis)) :to-equal 42)
    (expect (cl-cc/optimize:opt-binding-time-kind (second analysis)) :to-be :dynamic)
    (expect (cl-cc/optimize:opt-binding-time-kind (third analysis)) :to-be :dynamic)
    (expect (cl-cc/optimize:opt-binding-time-lattice (third analysis)) :to-be-null)))

(it-sequential "optimize-build-specialization-plan-reuses-cache-for-constant-signature"
  (let* ((cache (make-hash-table :test #'equal))
         (first-plan
           (cl-cc/optimize:opt-build-specialization-plan
            'f '(x y) '((x . 3)) :cache cache))
         (second-plan
           (cl-cc/optimize:opt-build-specialization-plan
            'f '(x y) '((x . 3)) :cache cache)))
    (expect first-plan :to-be-truthy)
    (expect second-plan :to-be-truthy)
    (expect (cl-cc/optimize:opt-specialization-plan-clone-needed-p first-plan) :to-be-truthy)
    (expect (cl-cc/optimize:opt-specialization-plan-cache-hit-p first-plan) :to-be-falsy)
    (expect (cl-cc/optimize:opt-specialization-plan-clone-needed-p second-plan) :to-be-falsy)
    (expect (cl-cc/optimize:opt-specialization-plan-cache-hit-p second-plan) :to-be-truthy)
    (expect (cl-cc/optimize:opt-specialization-plan-specialized-name second-plan) :to-equal (cl-cc/optimize:opt-specialization-plan-specialized-name first-plan))
    (expect (cl-cc/optimize:opt-specialization-plan-signature first-plan) :to-equal '((x . 3)))
    (expect (cl-cc/optimize:opt-specialization-plan-dynamic-args first-plan) :to-equal '(y))))

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

(it-sequential "optimize-partial-evaluate-program-propagates-constants-through-call-graph"
  (let* ((defs
           '((callee :params (:x :y)
              :body ((+ x y)))
             (caller :params (:a)
              :body ((callee a 7)))))
         (result (cl-cc/optimize:opt-partial-evaluate-program
                  defs
                  :constant-bindings-by-function '((caller . ((:a . 3))))))
         (reports (cl-cc/optimize::opt-partial-program-function-results result))
         (callee-report (cdr (assoc 'callee reports :test #'equal))))
    (expect callee-report :to-be-truthy)
    (expect (assoc :y (cl-cc/optimize:opt-partial-eval-signature callee-report) :test #'equal) :to-be-truthy)
    (expect (cdr (assoc :y (cl-cc/optimize:opt-partial-eval-signature callee-report) :test #'equal)) :to-equal 7)))

(it-sequential "optimize-partial-evaluate-program-uses-offline-bta-to-prune-static-forms"
  (let* ((report (cl-cc/optimize:opt-partial-evaluate-function
                  'f
                  '(x)
                  '((+ x 1) x)
                  :constant-bindings '((x . 9))))
         (kinds (cl-cc/optimize:opt-partial-eval-form-kinds report))
         (dynamic-body (cl-cc/optimize:opt-partial-eval-dynamic-body report)))
    (expect (listp kinds) :to-be-truthy)
    (expect (member :static kinds) :to-be-truthy)
    (expect (listp dynamic-body) :to-be-truthy)))

(it-sequential "opt-pass-specialize-known-args-emits-specialized-clone-and-redirects-call"
  (let* ((func-label "add2")
         (closure (make-vm-closure :dst :r1 :label func-label :params '(:x :y) :captured nil))
         (body (list (make-vm-label :name func-label)
                     (make-vm-add :dst :r9 :lhs :x :rhs :y)
                     (make-vm-ret :reg :r9)))
         (caller (list (make-vm-const :dst :r2 :value 5)
                       (make-vm-call :dst :r3 :func :r1 :args '(:r2 :r4))))
         (optimized (cl-cc/optimize:opt-pass-specialize-known-args
                     (append (list closure) body caller)))
         (has-specialized-label
           (some (lambda (inst)
                   (and (typep inst 'cl-cc/vm::vm-label)
                        (search "__spec__" (cl-cc/vm::vm-name inst))))
                 optimized))
         (rewritten-call
           (find-if (lambda (inst)
                      (and (typep inst 'cl-cc/vm::vm-call)
                           (equal (cl-cc/vm::vm-args inst) '(:r4))))
                    optimized)))
    (expect has-specialized-label :to-be-truthy)
    (expect rewritten-call :to-be-truthy)))

;;; ─── FR-523/524/525/526/527/528 Affine/Polyhedral/Loop/ML passes ──────────

(it-sequential "optimize-affine-loop-summary-builds-descriptor"
  (let ((summary (cl-cc/optimize::opt-build-affine-loop-summary
                  :induction-vars '(:i)
                  :bounds '((:i 0 100))
                  :accesses '((:a :i)))))
    (expect (getf summary :kind) :to-be :affine-loop-summary)
    (expect (getf summary :induction-vars) :to-equal '(:i))
    (expect (getf summary :bounds) :to-equal '((:i 0 100)))))

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

(it-sequential "optimize-polyhedral-schedule-plan-preserves-objective"
  (let ((plan (cl-cc/optimize::opt-polyhedral-schedule-plan
               :statements '(:s0 :s1)
               :constraints '((:s0-before :s1))
               :objective :throughput-max)))
    (expect (getf plan :kind) :to-be :polyhedral-schedule)
    (expect (getf plan :objective) :to-be :throughput-max)
    (expect (getf plan :statements) :to-equal '(:s0 :s1))))

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
