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

(it-sequential "egraph-saturate-empty-graph-terminates-at-iter-0"
  (let* ((eg (make-test-egraph))
         (rules (cl-cc/optimize:egraph-builtin-rules)))
    (multiple-value-bind (saturated iter _fuel)
        (cl-cc/optimize:egraph-saturate eg rules :limit 10 :fuel 1000)
      (expect saturated :to-be-truthy)
      (expect (= 0 iter) :to-be-truthy))))

(it-sequential "egraph-saturate-with-empty-rules-terminates"
  (let* ((eg (make-test-egraph)))
    (cl-cc/optimize:egraph-add eg 'const)
    (multiple-value-bind (saturated _iter _fuel)
        (cl-cc/optimize:egraph-saturate eg nil :limit 5 :fuel 1000)
      (expect saturated :to-be-truthy))))

;;; ─── Extraction ──────────────────────────────────────────────────────────

(it-sequential "egraph-extract-nullary-node-returns-non-nil"
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize:egraph-add eg 'const)))
    (let ((cls (gethash (cl-cc/optimize:egraph-find eg id) (cl-cc:eg-classes eg))))
      (when cls (setf (cl-cc:ec-data cls) 42)))
    (expect (cl-cc/optimize:egraph-extract eg id) :to-be-truthy)))

(it-sequential "egraph-extract-binary-add-returns-compound"
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc/optimize:egraph-add eg 'const))
         (c2  (cl-cc/optimize:egraph-add eg 'const))
         (add (cl-cc/optimize:egraph-add eg 'add c1 c2)))
    (expect (cl-cc/optimize:egraph-extract eg add) :to-be-truthy)))

;;; ─── Optimize-With-Egraph ────────────────────────────────────────────────

(it-sequential "optimize-with-egraph-simple-sequence-returns-non-empty"
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize:optimize-with-egraph insts)))
    (expect (>= (length result) 1) :to-be-truthy)))

(it-sequential "optimize-with-egraph-empty-input-returns-nil"
  (expect (cl-cc/optimize:optimize-with-egraph nil) :to-be-null))

(it-sequential "optimize-with-egraph-lowers-const"
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-sub   :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc:make-vm-ret   :reg :r1)))
         (out (cl-cc/optimize:optimize-with-egraph insts))
         (r1  (egraph-find-dst-inst out :r1)))
    (expect (cl-cc:vm-const-p r1) :to-be-truthy)
    (expect (cl-cc/vm::vm-value r1) :to-equal 0)))

(it-sequential "optimize-with-egraph-lowers-alias"
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 0)
                      (cl-cc:make-vm-const :dst :r1 :value 5)
                      (cl-cc:make-vm-cons  :dst :r2 :car-src :r1 :cdr-src :r0)
                      (cl-cc:make-vm-cons  :dst :r3 :car-src :r1 :cdr-src :r0)
                      (cl-cc:make-vm-ret   :reg :r3)))
         (out (cl-cc/optimize:optimize-with-egraph insts))
         (r3  (egraph-find-dst-inst out :r3)))
    (expect (cl-cc:vm-move-p r3) :to-be-truthy)
    (expect (cl-cc/vm::vm-src r3) :to-equal :r2)))

;;; ─── Cost Model ──────────────────────────────────────────────────────────

(it-sequential "egraph-cost-model-cases const"
  (destructuring-bind (op children expected) (list 'const nil 0)
    (expect (= expected (cl-cc/optimize:egraph-default-cost op children)) :to-be-truthy)))

(it-sequential "egraph-cost-model-cases add"
  (destructuring-bind (op children expected) (list 'add '(1 1) 3)
    (expect (= expected (cl-cc/optimize:egraph-default-cost op children)) :to-be-truthy)))

(it-sequential "egraph-cost-model-cases call"
  (destructuring-bind (op children expected) (list 'call nil 10)
    (expect (= expected (cl-cc/optimize:egraph-default-cost op children)) :to-be-truthy)))

;;; ─── enode-memo-key ──────────────────────────────────────────────────────

(it-sequential "enode-memo-key nullary"
  (destructuring-bind (op children expected) (list 'const nil '(const))
    (let ((node (cl-cc/optimize:make-e-node :op op :children children)))
    (expect (cl-cc/optimize::enode-memo-key node) :to-equal expected))))

(it-sequential "enode-memo-key binary"
  (destructuring-bind (op children expected) (list 'add '(0 1) '(add 0 1))
    (let ((node (cl-cc/optimize:make-e-node :op op :children children)))
    (expect (cl-cc/optimize::enode-memo-key node) :to-equal expected))))

;;; ─── egraph-canonical-enode ──────────────────────────────────────────────

(it-sequential "egraph-canonical-enode-updates-children"
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize:egraph-add eg 'const))
         (id1 (cl-cc/optimize:egraph-add eg 'var))
         ;; Force id1 -> id0 in union-find
         (canon (cl-cc/optimize:egraph-merge eg id0 id1))
         (node  (cl-cc/optimize:make-e-node :op 'add :children (list id1))))
    (let ((cnode (cl-cc/optimize::egraph-canonical-enode eg node)))
      (expect (cl-cc:en-op cnode) :to-be 'add)
      (expect (= canon (first (cl-cc:en-children cnode))) :to-be-truthy))))

;;; ─── egraph-build-rhs ────────────────────────────────────────────────────

(it-sequential "egraph-build-rhs-pattern-var-looks-up-binding"
  (let* ((eg      (make-test-egraph))
         (id      (cl-cc/optimize:egraph-add eg 'const))
         (bindings (list (cons '?x id))))
    (expect (= id (cl-cc/optimize::egraph-build-rhs eg '?x bindings)) :to-be-truthy)))

(it-sequential "egraph-build-rhs-symbol-adds-nullary-node"
  (let* ((eg (make-test-egraph)))
    (expect (integerp (cl-cc/optimize::egraph-build-rhs eg 'zero nil)) :to-be-truthy)))

(it-sequential "egraph-build-rhs-compound-builds-tree-with-op"
  (let* ((eg      (make-test-egraph))
         (id0     (cl-cc/optimize:egraph-add eg 'const))
         (bindings (list (cons '?x id0)))
         (result  (cl-cc/optimize::egraph-build-rhs eg '(neg ?x) bindings)))
    (expect (integerp result) :to-be-truthy)
    (expect (cl-cc/optimize::egraph-class-has-op-p eg result 'neg) :to-be-truthy)))

(it-sequential "egraph-build-rhs-numeric-sets-ec-data"
  (let* ((eg     (make-test-egraph))
         (result (cl-cc/optimize::egraph-build-rhs eg 42 nil)))
    (expect (integerp result) :to-be-truthy)
    (expect (= 42 (cl-cc/optimize::egraph-class-const-value eg result)) :to-be-truthy)))

;;; ─── egraph-class-has-op-p / egraph-class-const-value ───────────────────

(it-sequential "egraph-class-has-op-p matching-op"
  (destructuring-bind (query-op expected) (list 'const t)
    (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize:egraph-add eg 'const)))
    (assert-egraph-boolean-case expected
      (expect (cl-cc/optimize::egraph-class-has-op-p eg id query-op) :to-be-truthy)
      (expect (cl-cc/optimize::egraph-class-has-op-p eg id query-op) :to-be-falsy)))))

(it-sequential "egraph-class-has-op-p different-op"
  (destructuring-bind (query-op expected) (list 'add nil)
    (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize:egraph-add eg 'const)))
    (assert-egraph-boolean-case expected
      (expect (cl-cc/optimize::egraph-class-has-op-p eg id query-op) :to-be-truthy)
      (expect (cl-cc/optimize::egraph-class-has-op-p eg id query-op) :to-be-falsy)))))

(it-sequential "egraph-class-const-value-cases const-returns-data"
  (destructuring-bind (op set-data-p expected) (list 'const t 99)
    (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize:egraph-add eg op)))
    (when set-data-p
      (let ((cls (gethash (cl-cc/optimize:egraph-find eg id) (cl-cc:eg-classes eg))))
        (setf (cl-cc:ec-data cls) 99)))
    (assert-egraph-boolean-case expected
      (expect (= expected (cl-cc/optimize::egraph-class-const-value eg id)) :to-be-truthy)
      (expect (cl-cc/optimize::egraph-class-const-value eg id) :to-be-null)))))

(it-sequential "egraph-class-const-value-cases non-const-nil"
  (destructuring-bind (op set-data-p expected) (list 'add nil nil)
    (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize:egraph-add eg op)))
    (when set-data-p
      (let ((cls (gethash (cl-cc/optimize:egraph-find eg id) (cl-cc:eg-classes eg))))
        (setf (cl-cc:ec-data cls) 99)))
    (assert-egraph-boolean-case expected
      (expect (= expected (cl-cc/optimize::egraph-class-const-value eg id)) :to-be-truthy)
      (expect (cl-cc/optimize::egraph-class-const-value eg id) :to-be-null)))))

;;; ─── vm-inst-to-enode-op ─────────────────────────────────────────────────

(it-sequential "vm-inst-to-enode-op-strips-vm-prefix const"
  (destructuring-bind (expected-op-prefix inst) (list 'vm-const (cl-cc:make-vm-const :dst :r0 :value 1))
    (let ((op (cl-cc/optimize::vm-inst-to-enode-op inst)))
    (expect (symbolp op) :to-be-truthy)
    ;; The result op should NOT start with VM-
    (expect (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))) :to-be-falsy))))

(it-sequential "vm-inst-to-enode-op-strips-vm-prefix move"
  (destructuring-bind (expected-op-prefix inst) (list 'vm-move (cl-cc:make-vm-move  :dst :r0 :src :r1))
    (let ((op (cl-cc/optimize::vm-inst-to-enode-op inst)))
    (expect (symbolp op) :to-be-truthy)
    ;; The result op should NOT start with VM-
    (expect (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))) :to-be-falsy))))

(it-sequential "vm-inst-to-enode-op-strips-vm-prefix add"
  (destructuring-bind (expected-op-prefix inst) (list 'vm-add (cl-cc:make-vm-add   :dst :r0 :lhs :r1 :rhs :r2))
    (let ((op (cl-cc/optimize::vm-inst-to-enode-op inst)))
    (expect (symbolp op) :to-be-truthy)
    ;; The result op should NOT start with VM-
    (expect (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))) :to-be-falsy))))

;;; ─── egraph-add-instructions ─────────────────────────────────────────────

(it-sequential "egraph-add-instructions-maps-dst-registers-to-distinct-classes"
  (let* ((eg    (make-test-egraph))
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)))
         (reg->class (cl-cc/optimize::egraph-add-instructions eg insts)))
    (expect (integerp (gethash :r0 reg->class)) :to-be-truthy)
    (expect (integerp (gethash :r1 reg->class)) :to-be-truthy)
    (expect (/= (gethash :r0 reg->class) (gethash :r1 reg->class)) :to-be-truthy)))

(it-sequential "egraph-add-instructions-const-stores-ec-data"
  (let* ((eg    (make-test-egraph))
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 42)))
         (reg->class (cl-cc/optimize::egraph-add-instructions eg insts)))
    (expect (= 42 (cl-cc/optimize::egraph-class-const-value eg (gethash :r0 reg->class))) :to-be-truthy)))

;;; ─── Compound pattern matching ───────────────────────────────────────────

(it-sequential "egraph-match-pattern-compound-binds-pattern-vars"
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc/optimize:egraph-add eg 'const))
         (c2  (cl-cc/optimize:egraph-add eg 'var))
         (add (cl-cc/optimize:egraph-add eg 'add c1 c2))
         (matches (cl-cc/optimize:egraph-match-pattern eg '(add ?x ?y) add)))
    (expect (>= (length matches) 1) :to-be-truthy)
    (let ((b (car matches)))
      (expect (assoc '?x b) :to-be-truthy)
      (expect (assoc '?y b) :to-be-truthy))))

(it-sequential "egraph-match-pattern-literal-symbol-matches-nullary-node"
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize:egraph-add eg 'zero))
         (matches (cl-cc/optimize:egraph-match-pattern eg 'zero id)))
    (expect (= 1 (length matches)) :to-be-truthy)))

;;; ─── *opt-iteration-budget-thresholds* data coverage ────────────────────

(it-sequential "opt-iteration-budget-thresholds-content tiny-threshold"
  (destructuring-bind (threshold delta) (list 50 -12)
    (let ((entry (assoc threshold cl-cc/optimize::*opt-iteration-budget-thresholds*)))
    (expect entry :to-be-truthy)
    (expect (= delta (cdr entry)) :to-be-truthy))))

(it-sequential "opt-iteration-budget-thresholds-content small-threshold"
  (destructuring-bind (threshold delta) (list 150 -6)
    (let ((entry (assoc threshold cl-cc/optimize::*opt-iteration-budget-thresholds*)))
    (expect entry :to-be-truthy)
    (expect (= delta (cdr entry)) :to-be-truthy))))

(it-sequential "opt-iteration-budget-thresholds-content medium-threshold"
  (destructuring-bind (threshold delta) (list 400 0)
    (let ((entry (assoc threshold cl-cc/optimize::*opt-iteration-budget-thresholds*)))
    (expect entry :to-be-truthy)
    (expect (= delta (cdr entry)) :to-be-truthy))))

(it-sequential "opt-iteration-budget-thresholds-content large-threshold"
  (destructuring-bind (threshold delta) (list 800 8)
    (let ((entry (assoc threshold cl-cc/optimize::*opt-iteration-budget-thresholds*)))
    (expect entry :to-be-truthy)
    (expect (= delta (cdr entry)) :to-be-truthy))))

;;; ─── %egraph-rewrite-inst ────────────────────────────────────────────────

(it-sequential "egraph-rewrite-inst-no-dst-returns-unchanged"
  (let* ((eg         (make-test-egraph))
         (reg-map    (make-hash-table :test #'eq))
         (class->regs (make-hash-table :test #'equal))
         (ret        (make-vm-ret :reg :r0)))
    (let ((result (cl-cc/optimize::%egraph-rewrite-inst ret eg reg-map class->regs)))
      (expect result :to-be ret))))

(it-sequential "egraph-rewrite-inst-dst-in-const-class-becomes-vm-const"
  (let* ((eg         (make-test-egraph))
         (class-id   (cl-cc/optimize:egraph-add eg 'const))
         (class-ht   (gethash (cl-cc/optimize:egraph-find eg class-id) (cl-cc:eg-classes eg)))
         (reg-map    (make-hash-table :test #'eq))
         (class->regs (make-hash-table :test #'equal))
         (move       (make-vm-move :dst :r1 :src :r0)))
    (setf (cl-cc:ec-data class-ht) 42)
    (setf (gethash :r1 reg-map) class-id)
    (let ((result (cl-cc/optimize::%egraph-rewrite-inst move eg reg-map class->regs)))
      (expect (cl-cc:vm-const-p result) :to-be-truthy)
      (expect (cl-cc:vm-dst result) :to-be :r1)
      (expect (cl-cc/vm::vm-value result) :to-equal 42))))

(it-sequential "egraph-rewrite-inst-dst-with-alias-becomes-vm-move"
  (let* ((eg         (make-test-egraph))
         (class-id   (cl-cc/optimize:egraph-add eg 'add
                       (cl-cc/optimize:egraph-add eg 'const)
                       (cl-cc/optimize:egraph-add eg 'const)))
         (reg-map    (make-hash-table :test #'eq))
         (class->regs (make-hash-table :test #'equal))
         (canon      (cl-cc/optimize:egraph-find eg class-id))
         (add-inst   (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (setf (gethash :r2 reg-map) class-id)
    (setf (gethash canon class->regs) (list :r2 :r3))
    (let ((result (cl-cc/optimize::%egraph-rewrite-inst add-inst eg reg-map class->regs)))
      (expect (cl-cc:vm-move-p result) :to-be-truthy)
      (expect (cl-cc:vm-dst result) :to-be :r2)
      (expect (cl-cc/vm::vm-src result) :to-be :r3))))
