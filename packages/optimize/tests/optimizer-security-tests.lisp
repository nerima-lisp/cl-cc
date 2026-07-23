;;;; tests/unit/optimize/optimizer-security-tests.lisp
;;;; Unit tests for security hardening helpers in the optimizer:
;;;; CFI (control-flow integrity), retpoline, stack canary, shadow stack.

(in-package :cl-cc/test)

;;; ─── CFI ─────────────────────────────────────────────────────────────────────

(it-sequential "optimize-build-cfi-plan-selects-target-specific-guards"
  (let ((x86 (cl-cc/optimize::opt-build-cfi-plan :target :x86-64 :has-indirect-calls-p t))
        (arm (cl-cc/optimize::opt-build-cfi-plan :target :aarch64 :has-indirect-calls-p t)))
    (expect (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p x86) :to-be-truthy)
    (expect (cl-cc/optimize::opt-cfi-plan-insert-bti-p x86) :to-be-falsy)
    (expect (cl-cc/optimize::opt-cfi-plan-insert-bti-p arm) :to-be-truthy)
    (expect (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p arm) :to-be-falsy)))

(it-sequential "cfi-entry-opcode-cases x86-64-indirect"
  (destructuring-bind (target indirect-p expected) (list :x86-64 t :endbr64)
    (expect (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p indirect-p)) :to-be expected)))

(it-sequential "cfi-entry-opcode-cases aarch64-indirect"
  (destructuring-bind (target indirect-p expected) (list :aarch64 t :bti-c)
    (expect (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p indirect-p)) :to-be expected)))

(it-sequential "cfi-entry-opcode-cases x86-64-no-indirect"
  (destructuring-bind (target indirect-p expected) (list :x86-64 nil :none)
    (expect (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p indirect-p)) :to-be expected)))

;;; ─── Retpoline ───────────────────────────────────────────────────────────────

(it-sequential "retpoline-cases x86-indirect-no-ibrs"
  (destructuring-bind (target indirect-p ibrs-p expected) (list :x86-64 t nil t)
    (if expected
      (expect (cl-cc/optimize::opt-should-use-retpoline-p
                    :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p) :to-be-truthy)
      (expect (cl-cc/optimize::opt-should-use-retpoline-p
                     :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p) :to-be-falsy))))

(it-sequential "retpoline-cases x86-indirect-ibrs"
  (destructuring-bind (target indirect-p ibrs-p expected) (list :x86-64 t t nil)
    (if expected
      (expect (cl-cc/optimize::opt-should-use-retpoline-p
                    :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p) :to-be-truthy)
      (expect (cl-cc/optimize::opt-should-use-retpoline-p
                     :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p) :to-be-falsy))))

(it-sequential "retpoline-cases aarch64"
  (destructuring-bind (target indirect-p ibrs-p expected) (list :aarch64 t nil nil)
    (if expected
      (expect (cl-cc/optimize::opt-should-use-retpoline-p
                    :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p) :to-be-truthy)
      (expect (cl-cc/optimize::opt-should-use-retpoline-p
                     :target target :has-indirect-branch-p indirect-p :supports-ibrs-p ibrs-p) :to-be-falsy))))

(it-sequential "optimize-retpoline-thunk-name-is-target-register-specific"
  (expect (cl-cc/optimize:opt-retpoline-thunk-name :r11) :to-equal "__clcc_retpoline_r11")
  (expect (cl-cc/optimize:opt-retpoline-thunk-name :rax) :to-equal "__clcc_retpoline_rax"))

;;; ─── Stack canary ────────────────────────────────────────────────────────────

(it-sequential "optimize-needs-stack-canary-p-follows-stack-buffer-presence"
  (expect (cl-cc/optimize::opt-needs-stack-canary-p :has-stack-buffer-p t) :to-be-truthy)
  (expect (cl-cc/optimize::opt-needs-stack-canary-p :has-stack-buffer-p nil) :to-be-falsy))

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

;;; ─── Shadow stack ────────────────────────────────────────────────────────────

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
