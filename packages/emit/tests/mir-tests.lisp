;;;; tests/unit/emit/mir-tests.lisp — Unit tests for MIR (Phase 1)
;;;
;;; Covers: mir-value, mir-const, mir-inst, mir-block, mir-function,
;;;         mir-module, builder API, SSA variable tracking, CFG utilities,
;;;         target-desc and predefined targets.

(in-package :cl-cc/test)


;;;; ─── mir-value ────────────────────────────────────────────────────────

(it-sequential "mir-value-creation"
  (let* ((fn (mir-make-function :test-fn))
         (v0 (mir-new-value fn))
         (v1 (mir-new-value fn))
         (v2 (mir-new-value fn :name :x :type :integer))
         (c  (make-mir-const :value 42)))
    (expect (= 0 (mirv-id v0)) :to-be-truthy)
    (expect (= 1 (mirv-id v1)) :to-be-truthy)
    (expect (= 2 (mirv-id v2)) :to-be-truthy)
    (expect (mirv-name v2) :to-be :x)
    (expect (mirv-type v2) :to-be :integer)
    (expect (= 3 (mirf-value-counter fn)) :to-be-truthy)
    (expect (mirv-type v0) :to-be :any)
    (expect (mir-value-p v0) :to-be-truthy)
    (expect (mir-value-p c) :to-be-falsy)
    (expect (mir-value-p 42) :to-be-falsy)))

;;;; ─── mir-const ────────────────────────────────────────────────────────

(it-sequential "mir-const-types integer"
  (destructuring-bind (val type verify) (list 42 :integer (lambda (c)
             (assert-equal 42 (mirc-value c))))
    (let ((c (make-mir-const :value val :type type)))
    (expect (mir-const-p c) :to-be-truthy)
    (expect (mirc-type c) :to-be type)
    (funcall verify c))))

(it-sequential "mir-const-types nil"
  (destructuring-bind (val type verify) (list nil :pointer (lambda (c)
             (assert-null (mirc-value c))))
    (let ((c (make-mir-const :value val :type type)))
    (expect (mir-const-p c) :to-be-truthy)
    (expect (mirc-type c) :to-be type)
    (funcall verify c))))

(it-sequential "mir-const-types string"
  (destructuring-bind (val type verify) (list "hello" :any (lambda (c)
             (assert-equal "hello" (mirc-value c))))
    (let ((c (make-mir-const :value val :type type)))
    (expect (mir-const-p c) :to-be-truthy)
    (expect (mirc-type c) :to-be type)
    (funcall verify c))))

;;;; ─── mir-block ────────────────────────────────────────────────────────

(it-sequential "mir-block-creation"
  (let* ((fn (mir-make-function :test-fn))
         (b1 (mir-new-block fn))
         (b2 (mir-new-block fn :label :then)))
    (expect (null (mirf-entry fn)) :to-be-falsy)
    (expect (mirb-label (mirf-entry fn)) :to-be :entry)
    (expect (mirb-label b2) :to-be :then)
    (expect (= (mirb-id b1) (mirb-id b2)) :to-be-falsy)
    (expect (mirb-insts b1) :to-be-null)
    (expect (mirb-preds b1) :to-be-null)
    (expect (mirb-succs b1) :to-be-null)
    (expect (mirb-phis b1) :to-be-null)
    (expect (mirb-sealed-p b1) :to-be-falsy)))

(it-sequential "mir-block-pred-succ-linking"
  (let* ((fn   (mir-make-function :f))
         (b1   (mirf-entry fn))
         (b2   (mir-new-block fn :label :exit)))
    (mir-add-succ b1 b2)
    (expect (member b2 (mirb-succs b1) :test #'eq) :to-be-truthy)
    (expect (member b1 (mirb-preds b2) :test #'eq) :to-be-truthy)
    ;; idempotent
    (mir-add-succ b1 b2)
    (expect (= 1 (length (mirb-succs b1))) :to-be-truthy)))

;;;; ─── mir-function ──────────────────────────────────────────────────────

(it-sequential "mir-make-function-structure"
  (let ((fn (mir-make-function :fib)))
    (expect (mirf-name fn) :to-be :fib)
    (expect (null (mirf-entry fn)) :to-be-falsy)
    (expect (mirb-label (mirf-entry fn)) :to-be :entry)
    (expect (= 0 (mirf-value-counter fn)) :to-be-truthy)
    (expect (= 1 (mirf-block-counter fn)) :to-be-truthy))
  (let* ((fn (mir-make-function :add))
         (p0 (mir-new-value fn :name :a :type :integer))
         (p1 (mir-new-value fn :name :b :type :integer)))
    (setf (mirf-params fn) (list p0 p1))
    (expect (= 2 (length (mirf-params fn))) :to-be-truthy)
    (expect (mirv-name (first (mirf-params fn))) :to-be :a)))

;;;; ─── mir-emit ──────────────────────────────────────────────────────────

(it-sequential "mir-emit-wires-op-dst-srcs-and-block"
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn :name :result :type :integer))
         (a    (make-mir-const :value 1 :type :integer))
         (b    (make-mir-const :value 2 :type :integer))
         (inst (mir-emit blk :add :dst dst :srcs (list a b))))
    (expect (mir-inst-p inst) :to-be-truthy)
    (expect (miri-op inst) :to-be :add)
    (expect (miri-dst inst) :to-be dst)
    (expect (= 2 (length (miri-srcs inst))) :to-be-truthy)
    (expect (= 1 (length (mirb-insts blk))) :to-be-truthy)
    (expect (miri-block inst) :to-be blk)))

(it-sequential "mir-emit-sets-def-inst-pointer"
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn))
         (inst (mir-emit blk :const :dst dst :srcs (list (make-mir-const :value 0)))))
    (expect (mirv-def-inst dst) :to-be inst)))

(it-sequential "mir-emit-ret-has-nil-dst"
  (let* ((fn  (mir-make-function :f))
         (blk (mirf-entry fn))
         (v   (mir-new-value fn)))
    (expect (miri-dst (mir-emit blk :ret :srcs (list v))) :to-be-null)))

(it-sequential "mir-emit-phi-goes-to-phis-list-not-insts"
  (let* ((fn  (mir-make-function :f))
         (blk (mir-new-block fn :label :loop))
         (dst (mir-new-value fn :name :x))
         (phi (mir-emit blk :phi :dst dst)))
    (expect (member phi (mirb-phis blk) :test #'eq) :to-be-truthy)
    (expect (mirb-insts blk) :to-be-null)))

(it-sequential "mir-emit-preserves-instruction-ordering"
  (let* ((fn  (mir-make-function :f))
         (blk (mirf-entry fn))
         (d0  (mir-new-value fn))
         (d1  (mir-new-value fn))
         (d2  (mir-new-value fn))
         (i0  (mir-emit blk :const :dst d0 :srcs (list (make-mir-const :value 1))))
         (i1  (mir-emit blk :const :dst d1 :srcs (list (make-mir-const :value 2))))
         (i2  (mir-emit blk :add   :dst d2 :srcs (list d0 d1))))
    (expect (= 3 (length (mirb-insts blk))) :to-be-truthy)
    (expect (first  (mirb-insts blk)) :to-be i0)
    (expect (second (mirb-insts blk)) :to-be i1)
    (expect (third  (mirb-insts blk)) :to-be i2)))

;;;; ─── SSA variable tracking ────────────────────────────────────────────

(it-sequential "mir-ssa-variable-read-write"
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (v    (mir-new-value fn :name :x)))
    (mir-write-var fn :x blk v)
    (expect (mir-read-var fn :x blk) :to-be v))
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (b1    (mir-new-block fn :label :b1))
         (v     (mir-new-value fn :name :y)))
    (mir-seal-block fn entry)
    (mir-seal-block fn b1)
    (mir-add-succ entry b1)
    (mir-write-var fn :y entry v)
    (expect (mir-read-var fn :y b1) :to-be v)
    (expect (mirb-phis b1) :to-be-null)))

(it-sequential "mir-seal-block-resolves-incomplete-phi"
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (b1    (mir-new-block fn :label :b1))
         (b2    (mir-new-block fn :label :b2))
         (merge (mir-new-block fn :label :merge))
         (v1    (mir-new-value fn :name :val1))
         (v2    (mir-new-value fn :name :val2)))
    ;; CFG: entry → b1, entry → b2, b1 → merge, b2 → merge
    (mir-add-succ entry b1)
    (mir-add-succ entry b2)
    (mir-add-succ b1 merge)
    (mir-add-succ b2 merge)
    ;; Seal all blocks except merge (its preds must be known first)
    (mir-seal-block fn entry)
    (mir-seal-block fn b1)
    (mir-seal-block fn b2)
    ;; Write different definitions in b1 and b2
    (mir-write-var fn :z b1 v1)
    (mir-write-var fn :z b2 v2)
    ;; Now read :z in merge (not sealed yet) — should create incomplete phi
    (let ((phi-val (mir-read-var fn :z merge)))
      (expect (null phi-val) :to-be-falsy)
      ;; There should be one phi in merge's phis or incomplete-phis
      (expect (= 0 (hash-table-count (mirb-incomplete-phis merge))) :to-be-falsy)
      ;; Seal merge — should resolve the phi
      (mir-seal-block fn merge)
      (expect (= 0 (hash-table-count (mirb-incomplete-phis merge))) :to-be-truthy)
      ;; After sealing, the phi should have 2 src operands
      (when (mirb-phis merge)
        (let ((phi-inst (first (mirb-phis merge))))
          (expect (= 2 (length (miri-srcs phi-inst))) :to-be-truthy))))))

;;;; ─── CFG utilities ────────────────────────────────────────────────────

(it-sequential "mir-rpo-single-block"
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (rpo   (mir-rpo fn)))
    (expect (= 1 (length rpo)) :to-be-truthy)
    (expect (first rpo) :to-be entry)))

(it-sequential "mir-linear-chain-rpo-and-dominators"
  (let* ((fn  (mir-make-function :f))
         (b0  (mirf-entry fn))
         (b1  (mir-new-block fn :label :b1))
         (b2  (mir-new-block fn :label :b2)))
    (mir-add-succ b0 b1)
    (mir-add-succ b1 b2)
    (let ((rpo (mir-rpo fn)))
      (expect (= 3 (length rpo)) :to-be-truthy)
      (let ((pos (lambda (b) (position b rpo :test #'eq))))
        (expect (< (funcall pos b0) (funcall pos b1)) :to-be-truthy)
        (expect (< (funcall pos b1) (funcall pos b2)) :to-be-truthy)))
    (let ((idom (mir-dominators fn)))
      (expect (gethash (mirb-id b0) idom) :to-be b0)
      (expect (gethash (mirb-id b1) idom) :to-be b0)
      (expect (gethash (mirb-id b2) idom) :to-be b1))))

(it-sequential "mir-diamond-cfg-rpo-and-dominators"
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (then  (mir-new-block fn :label :then))
         (else  (mir-new-block fn :label :else))
         (merge (mir-new-block fn :label :merge)))
    (mir-add-succ entry then)
    (mir-add-succ entry else)
    (mir-add-succ then merge)
    (mir-add-succ else merge)
    (let ((rpo (mir-rpo fn)))
      (expect (= 4 (length rpo)) :to-be-truthy)
      (expect (first rpo) :to-be entry))
    (let ((idom (mir-dominators fn)))
      (expect (gethash (mirb-id merge) idom) :to-be entry)
      (expect (gethash (mirb-id then) idom) :to-be entry)
      (expect (gethash (mirb-id else) idom) :to-be entry))))

;;;; ─── mir-module ────────────────────────────────────────────────────────

(it-sequential "mir-module-has-empty-functions-and-globals"
  (let ((m (make-mir-module)))
    (expect (mirm-functions m) :to-be-null)
    (expect (mirm-globals m) :to-be-null)
    (expect (null (mirm-string-table m)) :to-be-falsy)))

(it-sequential "mir-generic-ops-contains-all-core-ops"
  (dolist (op '(:add :sub :mul :div :mod :neg
                :band :bor :bxor :bnot
                :lt :le :gt :ge :eq :ne
                :load :store :alloca
                :call :tail-call :ret :jump :branch
                :phi :values :mv-bind :safepoint :nop))
    (expect (member op *mir-generic-ops*) :to-be-truthy)))

(it-sequential "mir-op-effect-kind-classifies-core-ops pure arithmetic"
  (destructuring-bind (op expected) (list :add :pure)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops division may raise"
  (destructuring-bind (op expected) (list :div :control)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops modulo may raise"
  (destructuring-bind (op expected) (list :mod :control)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops memory read"
  (destructuring-bind (op expected) (list :load :read-only)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops memory write"
  (destructuring-bind (op expected) (list :store :write-global)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops stack allocation"
  (destructuring-bind (op expected) (list :alloca :alloc)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops control transfer"
  (destructuring-bind (op expected) (list :branch :control)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops unknown call"
  (destructuring-bind (op expected) (list :call :unknown)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-op-effect-kind-classifies-core-ops unknown op"
  (destructuring-bind (op expected) (list :not-a-mir-op :unknown)
    (expect (cl-cc/mir:mir-op-effect-kind op) :to-be expected)))

(it-sequential "mir-inst-effect-kind-allows-meta-override"
  (let* ((fn (mir-make-function :effects))
         (blk (mirf-entry fn))
         (dst (mir-new-value fn))
         (call (mir-emit blk :call :dst dst :meta '(:effect-kind :read-only))))
    (expect (cl-cc/mir:mir-op-effect-kind :call) :to-be :unknown)
    (expect (cl-cc/mir:mir-inst-effect-kind call) :to-be :read-only)
    (expect (cl-cc/mir:mir-inst-pure-p call) :to-be-falsy)
    (expect (cl-cc/mir:mir-inst-dce-eligible-p call) :to-be-falsy)))

(it-sequential "mir-inst-effect-kind-rejects-malformed-meta"
  (let* ((fn (mir-make-function :effects))
         (blk (mirf-entry fn))
         (dst (mir-new-value fn))
         (call (mir-emit blk :call :dst dst :meta (cons :effect-kind :read-only)))
         (div (mir-emit blk :div :dst (mir-new-value fn)
                        :srcs (list (mir-new-value fn :type :integer)
                                    (mir-new-value fn :type :integer)))))
    (expect (cl-cc/mir:mir-inst-effect-kind call) :to-be :unknown)
    (expect (cl-cc/mir:mir-inst-effect-kind div) :to-be :control)
    (expect (cl-cc/mir:mir-inst-dce-eligible-p div) :to-be-falsy)))

(it-sequential "mir-propagate-types-updates-instructions-and-values"
  (let* ((fn (mir-make-function :typed))
         (blk (mirf-entry fn))
         (a (mir-new-value fn))
         (b (mir-new-value fn))
         (sum (mir-new-value fn))
         (cmp (mir-new-value fn)))
    (mir-emit blk :const :dst a :srcs (list (make-mir-const :value 1 :type :integer)))
    (mir-emit blk :const :dst b :srcs (list (make-mir-const :value 2 :type :integer)))
    (let ((add-inst (mir-emit blk :add :dst sum :srcs (list a b)))
          (cmp-inst (mir-emit blk :lt :dst cmp :srcs (list sum b))))
      (cl-cc/mir:mir-propagate-types fn)
      (expect (miri-type add-inst) :to-be :integer)
      (expect (mirv-type sum) :to-be :integer)
      (expect (miri-type cmp-inst) :to-be :boolean)
      (expect (mirv-type cmp) :to-be :boolean))))

(it-sequential "mir-propagate-types-joins-phi-input-types-conservatively"
  (let* ((fn (mir-make-function :phi-types))
         (entry (mirf-entry fn))
         (then (mir-new-block fn :label :then))
         (else (mir-new-block fn :label :else))
         (merge (mir-new-block fn :label :merge))
         (int-a (mir-new-value fn :type :integer))
         (int-b (mir-new-value fn :type :integer))
         (bool-c (mir-new-value fn :type :boolean))
         (phi-same-dst (mir-new-value fn))
         (phi-mixed-dst (mir-new-value fn))
         (phi-same (mir-emit merge :phi :dst phi-same-dst
                             :srcs (list (cons then int-a) (cons else int-b))))
         (phi-mixed (mir-emit merge :phi :dst phi-mixed-dst
                              :srcs (list (cons then int-a) (cons else bool-c)))))
    (mir-add-succ entry then)
    (mir-add-succ entry else)
    (mir-add-succ then merge)
    (mir-add-succ else merge)
    (cl-cc/mir:mir-propagate-types fn)
    (expect (miri-type phi-same) :to-be :integer)
    (expect (mirv-type phi-same-dst) :to-be :integer)
    (expect (miri-type phi-mixed) :to-be :any)
    (expect (mirv-type phi-mixed-dst) :to-be :any)))

(it-sequential "mir-propagate-types-reaches-fixed-point-for-late-producers"
  (let* ((fn (mir-make-function :fixed-point-types))
         (entry (mirf-entry fn))
         (merge (mir-new-block fn :label :merge))
         (late (mir-new-block fn :label :late))
         (a (mir-new-value fn))
         (b (mir-new-value fn))
         (late-sum (mir-new-value fn))
         (phi-dst (mir-new-value fn))
         (phi (mir-emit merge :phi :dst phi-dst
                        :srcs (list (cons entry a) (cons late late-sum)))))
    (mir-add-succ entry merge)
    (mir-add-succ merge late)
    (mir-emit entry :const :dst a :srcs (list (make-mir-const :value 1 :type :integer)))
    (mir-emit late :const :dst b :srcs (list (make-mir-const :value 2 :type :integer)))
    (mir-emit late :add :dst late-sum :srcs (list a b))
    (cl-cc/mir:mir-propagate-types fn)
    (expect (miri-type phi) :to-be :integer)
    (expect (mirv-type phi-dst) :to-be :integer)))


;;; ─── %mir-rpo-dfs (extracted helper) ────────────────────────────────────────

(it-sequential "mir-rpo-dfs-single-block"
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (visited (make-hash-table))
         (cell    (list nil)))
    (cl-cc/mir::%mir-rpo-dfs entry visited cell)
    (expect (car cell) :to-equal (list entry))
    (expect (gethash (mirb-id entry) visited) :to-be-truthy)))

(it-sequential "mir-rpo-dfs-no-revisit"
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (visited (make-hash-table))
         (cell    (list nil)))
    (cl-cc/mir::%mir-rpo-dfs entry visited cell)
    (cl-cc/mir::%mir-rpo-dfs entry visited cell)
    (expect (= 1 (length (car cell))) :to-be-truthy)))

(it-sequential "mir-rpo-dfs-chain-post-order"
  (let* ((fn (mir-make-function :f))
         (a  (mirf-entry fn))
         (b  (mir-new-block fn :label :b))
         (c  (mir-new-block fn :label :c)))
    (mir-add-succ a b)
    (mir-add-succ b c)
    (let ((visited (make-hash-table))
          (cell    (list nil)))
      (cl-cc/mir::%mir-rpo-dfs a visited cell)
      (expect (car cell) :to-equal (list a b c)))))
