;;;; tests/unit/optimize/optimizer-inline-pass-ext-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp — inlining policy and full passes.
;;;;
;;;; Covers:
;;;;   Inlining policy — opt-inline-inst-cost, opt-inline-body-cost,
;;;;                     opt-adaptive-inline-threshold, opt-inline-eligible-p
;;;;   Full passes     — opt-pass-global-dce, opt-pass-inline
;;;;
;;;; Depends on helpers defined in optimizer-inline-pass-tests.lisp
;;;; (loaded before this file via :serial t ASDF).

(in-package :cl-cc/test)

;;; ─── opt-inline-inst-cost ───────────────────────────────────────────────────

(it-sequential "opt-inline-inst-cost-returns-number-ext"
  (dolist (inst (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r1 :src :r0)
                      (make-vm-ret   :reg :r0)))
    (expect (>= (cl-cc/optimize::opt-inline-inst-cost inst) 0) :to-be-truthy)))

;;; ─── opt-inline-body-cost ───────────────────────────────────────────────────

(it-sequential "opt-inline-body-cost-cases excludes-ret"
  (destructuring-bind (body expected-cost) (list (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)) (cl-cc/optimize::opt-inline-inst-cost (make-vm-const :dst :r0 :value 1)))
    (expect (= expected-cost (cl-cc/optimize::opt-inline-body-cost body)) :to-be-truthy)))

(it-sequential "opt-inline-body-cost-cases only-ret"
  (destructuring-bind (body expected-cost) (list (list (make-vm-ret :reg :r0)) 0)
    (expect (= expected-cost (cl-cc/optimize::opt-inline-body-cost body)) :to-be-truthy)))

;;; ─── opt-adaptive-inline-threshold ──────────────────────────────────────────

(it-sequential "opt-adaptive-threshold-cheap-body-ext"
  (let* ((ci   (make-vm-closure :dst :r9 :label "cheap" :params '(:r0) :captured nil))
         ;; vm-move has cost ≤ 1 — saturate cheap-ratio to 1.0
         (body (loop repeat 5
                     collect (make-vm-move :dst :r1 :src :r0)))
         (def  (list :closure ci :params '(:r0) :body (append body (list (make-vm-ret :reg :r1)))))
         (threshold (cl-cc/optimize::opt-adaptive-inline-threshold def)))
    ;; Should be > base (15) because cheap-ratio >= 0.75
    (expect (>= threshold 15) :to-be-truthy)))

(it-sequential "opt-adaptive-threshold-has-floor-ext"
  (let* ((ci  (make-vm-closure :dst :r9 :label "empty" :params '(:r0) :captured nil))
         (def (list :closure ci :params '(:r0) :body (list (make-vm-ret :reg :r0))))
         (threshold (cl-cc/optimize::opt-adaptive-inline-threshold def)))
    (expect (>= threshold 8) :to-be-truthy)))

;;; ─── opt-inline-eligible-p ──────────────────────────────────────────────────

(defun %make-eligible-def (label params body-insts)
  "Build a plist-style func def suitable for opt-inline-eligible-p."
  (let ((ci (make-vm-closure :dst :r9 :label label :params params :captured nil)))
    (list :closure ci
          :params params
          :body (append body-insts (list (make-vm-ret :reg (first params)))))))

(it-sequential "opt-inline-eligible-simple-function-ext"
  (let* ((def (%make-eligible-def "add1" '(:r0)
                 (list (make-vm-const :dst :r1 :value 1)))))
    (expect (cl-cc/optimize::opt-inline-eligible-p def 50) :to-be-truthy)))

(it-sequential "opt-inline-eligible-rejects-captured-vars-ext"
  (let* ((ci  (make-vm-closure :dst :r9 :label "cap"
                                :params '(:r0)
                                :captured '((:r5 . 42))))
         (def (list :closure ci :params '(:r0)
                    :body (list (make-vm-ret :reg :r0)))))
    (expect (cl-cc/optimize::opt-inline-eligible-p def 50) :to-be-null)))

(it-sequential "opt-inline-eligible-rejects-over-threshold-ext"
  (let* ((big-body (loop repeat 20
                         collect (make-vm-move :dst :r1 :src :r0)))
         (def (%make-eligible-def "big" '(:r0) big-body)))
    ;; threshold=0 → any non-empty body exceeds it
    (expect (cl-cc/optimize::opt-inline-eligible-p def 0) :to-be-null)))

;;; ─── opt-pass-global-dce ────────────────────────────────────────────────────

(it-sequential "opt-pass-global-dce-empty-program-ext"
  (expect (cl-cc/optimize::opt-pass-global-dce nil) :to-be-null))

(it-sequential "opt-pass-global-dce-preserves-reachable-function-ext"
  (let* (;; Function "fn" takes :r1 as param (non-nil so it gets collected)
         (cl   (make-vm-closure :dst :r0 :label "fn" :params '(:r1) :captured nil))
         (lbl  (make-vm-label :name "fn"))
         (body (make-vm-const :dst :r2 :value 1))
         (ret  (make-vm-ret   :reg :r2))
         ;; Top-level: call the closure held in :r0
         (call (make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize::opt-pass-global-dce insts)))
    ;; All instructions should be preserved since "fn" is reachable
    (expect (= (length insts) (length result)) :to-be-truthy)))

(it-sequential "opt-pass-global-dce-removes-dead-function-ext"
  (let* (;; Dead function: must have non-nil params so opt-collect-function-defs
         ;; picks it up. Without params, it's invisible to the DCE analysis.
         (dead-cl  (make-vm-closure :dst :r9 :label "dead"
                                    :params '(:r0) :captured nil))
         (dead-lbl (make-vm-label :name "dead"))
         (dead-body (make-vm-const :dst :r1 :value 99))
         (dead-ret  (make-vm-ret   :reg :r0))
         ;; Top-level: just a constant, never loads or calls "dead"
         (top-const (make-vm-const :dst :r0 :value 0))
         (insts (list dead-cl dead-lbl dead-body dead-ret top-const))
         (result (cl-cc/optimize::opt-pass-global-dce insts)))
    ;; Only top-const should survive — 4 dead-function instructions removed
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (cl-cc:vm-const-p (first result)) :to-be-truthy)))

;;; ─── opt-pass-inline ────────────────────────────────────────────────────────

(it-sequential "opt-pass-inline-empty-program-ext"
  (expect (cl-cc/optimize::opt-pass-inline nil) :to-be-null))

(it-sequential "opt-pass-inline-inlines-eligible-call-ext"
  (let* (;; Define a trivial function: (lambda (:r1) (const :r2 42) (ret :r1))
         (cl   (make-vm-closure :dst :r0 :label "const42" :params '(:r1) :captured nil))
         (lbl  (make-vm-label :name "const42"))
         (body (make-vm-const :dst :r2 :value 42))
         (ret  (make-vm-ret   :reg :r2))
         ;; Call it: (const42 :r5) → :r6
         (call (make-vm-call :dst :r6 :func :r0 :args (list :r5)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize::opt-pass-inline insts :threshold 50)))
    ;; vm-call should be gone; result should contain vm-move for arg passing
    (expect (find-if #'cl-cc:vm-call-p result) :to-be-null)
    ;; A vm-move or vm-const should appear for argument/return
    (expect (some (lambda (i) (or (cl-cc:vm-move-p i) (cl-cc:vm-const-p i))) result) :to-be-truthy)))

(it-sequential "opt-pass-inline-preserves-non-eligible-call-ext"
  (let* ((cl   (make-vm-closure :dst :r0 :label "fn" :params '(:r1)
                                 :captured '((:r5 . 42))))
         (lbl  (make-vm-label :name "fn"))
         (body (make-vm-const :dst :r2 :value 1))
         (ret  (make-vm-ret   :reg :r2))
         (call (make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize::opt-pass-inline insts :threshold 50)))
    ;; Captured vars → not eligible → vm-call must remain in output
    (expect (find-if #'cl-cc:vm-call-p result) :to-be-truthy)))
