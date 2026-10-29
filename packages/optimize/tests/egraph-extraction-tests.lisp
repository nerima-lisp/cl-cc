;;;; tests/unit/optimize/egraph-extraction-tests.lisp — E-Graph Extraction/Saturation Tests
;;;
;;; Tests for saturation, extraction, cost model, enode internals,
;;; optimize-with-egraph, and the iteration-budget thresholds data table.
;;;
;;; Helper make-test-egraph and egraph-find-dst-inst are defined in
;;; egraph-tests.lisp (loaded first).

(in-package :cl-cc/test)

(defmacro assert-egraph-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── Saturation ──────────────────────────────────────────────────────────

;;; ─── Extraction ──────────────────────────────────────────────────────────

;;; ─── Optimize-With-Egraph ────────────────────────────────────────────────

;;; ─── Cost Model ──────────────────────────────────────────────────────────

;;; ─── enode-memo-key ──────────────────────────────────────────────────────

;;; ─── egraph-canonical-enode ──────────────────────────────────────────────

;;; ─── egraph-build-rhs ────────────────────────────────────────────────────

;;; ─── egraph-class-has-op-p / egraph-class-const-value ───────────────────

;;; ─── vm-inst-to-enode-op ─────────────────────────────────────────────────

(it-sequential "vm-inst-to-enode-op-strips-vm-prefix const"
  (destructuring-bind (expected-op-prefix inst) (list 'vm-const (cl-cc:make-vm-const :dst :r0 :value 1))
    (let ((op (cl-cc/optimize/rewriting:vm-inst-to-enode-op inst)))
    (expect (symbolp op) :to-be-truthy)
    ;; The result op should NOT start with VM-
    (expect (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))) :to-be-falsy))))

(it-sequential "vm-inst-to-enode-op-strips-vm-prefix move"
  (destructuring-bind (expected-op-prefix inst) (list 'vm-move (cl-cc:make-vm-move  :dst :r0 :src :r1))
    (let ((op (cl-cc/optimize/rewriting:vm-inst-to-enode-op inst)))
    (expect (symbolp op) :to-be-truthy)
    ;; The result op should NOT start with VM-
    (expect (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))) :to-be-falsy))))

(it-sequential "vm-inst-to-enode-op-strips-vm-prefix add"
  (destructuring-bind (expected-op-prefix inst) (list 'vm-add (cl-cc:make-vm-add   :dst :r0 :lhs :r1 :rhs :r2))
    (let ((op (cl-cc/optimize/rewriting:vm-inst-to-enode-op inst)))
    (expect (symbolp op) :to-be-truthy)
    ;; The result op should NOT start with VM-
    (expect (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))) :to-be-falsy))))

;;; ─── egraph-add-instructions ─────────────────────────────────────────────

;;; ─── Compound pattern matching ───────────────────────────────────────────

;;; ─── *opt-iteration-budget-thresholds* data coverage ────────────────────

;;; ─── %egraph-rewrite-inst ────────────────────────────────────────────────

