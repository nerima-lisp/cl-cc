;;;; tests/unit/optimize/optimizer-pipeline-peval-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — partial evaluation helpers
;;;;
;;;; Covers: FR-209 opt-specialize-constant-args, FR-210 SCCP binding-time
;;;;   analysis, FR-211 specialization plan caching, program-level partial
;;;;   evaluation, and opt-pass-specialize-known-args.

(in-package :cl-cc/test)

;;; ─── FR-209 opt-specialize-constant-args ────────────────────────────────────

(it-sequential "optimize-specialize-constant-args-cases basic-substitution"
  (destructuring-bind (label params body static-bindings &key check-residual check-signature check-dynamic) (list 'f '(x y) '((if (= x 0) 0 (* x y))) '((x . 0)) :check-residual '((if (= 0 0) 0 (* 0 y))) :check-signature '((x . 0)) :check-dynamic '(y))
    (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           label params body static-bindings)))
    (expect (cl-cc/optimize:opt-partial-spec-original-name specialization) :to-equal label)
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal check-residual)
    (when check-signature
      (expect (cl-cc/optimize:opt-partial-spec-static-args specialization) :to-equal check-signature))
    (when check-dynamic
      (expect (cl-cc/optimize:opt-partial-spec-dynamic-args specialization) :to-equal check-dynamic)))))

(it-sequential "optimize-specialize-constant-args-cases lexical-binders-and-quoted"
  (destructuring-bind (label params body static-bindings &key check-residual check-signature check-dynamic) (list 'f '(x y) '((quote x)
      (let ((x y) (z x)) (+ x z y))
      (let* ((z x) (x y)) (+ x z y))
      (lambda (x) (+ x y))
      (setq x y)
      (x y)) '((x . 0) (y . 7)) :check-residual '((quote x)
                      (let ((x 7) (z 0)) (+ x z 7))
                      (let* ((z 0) (x 7)) (+ x z 7))
                      (lambda (x) (+ x 7))
                      (setq x 7)
                      (x 7)) :check-signature nil :check-dynamic nil)
    (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           label params body static-bindings)))
    (expect (cl-cc/optimize:opt-partial-spec-original-name specialization) :to-equal label)
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal check-residual)
    (when check-signature
      (expect (cl-cc/optimize:opt-partial-spec-static-args specialization) :to-equal check-signature))
    (when check-dynamic
      (expect (cl-cc/optimize:opt-partial-spec-dynamic-args specialization) :to-equal check-dynamic)))))

(it-sequential "optimize-specialize-constant-args-cases kills-signature-after-setq"
  (destructuring-bind (label params body static-bindings &key check-residual check-signature check-dynamic) (list 'f '(x y) '((setq x y)
      (+ x y)
      (setq x (+ x 1))
      (+ x y)
      (setq x y y x)
      (+ x y)) '((x . 0) (y . 7)) :check-residual '((setq x 7)
                      (+ x 7)
                      (setq x (+ x 1))
                      (+ x 7)
                      (setq x 7 y x)
                      (+ x y)) :check-signature nil :check-dynamic nil)
    (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           label params body static-bindings)))
    (expect (cl-cc/optimize:opt-partial-spec-original-name specialization) :to-equal label)
    (expect (cl-cc/optimize:opt-partial-spec-residual-body specialization) :to-equal check-residual)
    (when check-signature
      (expect (cl-cc/optimize:opt-partial-spec-static-args specialization) :to-equal check-signature))
    (when check-dynamic
      (expect (cl-cc/optimize:opt-partial-spec-dynamic-args specialization) :to-equal check-dynamic)))))

;;; ─── FR-210 SCCP binding-time analysis ──────────────────────────────────────

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

;;; ─── FR-211 specialization plan caching ──────────────────────────────────────

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

;;; ─── Program-level partial evaluation ───────────────────────────────────────

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

;;; ─── opt-pass-specialize-known-args ─────────────────────────────────────────

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
