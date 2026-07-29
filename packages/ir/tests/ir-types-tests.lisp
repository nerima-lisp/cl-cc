;;;; tests/unit/compile/ir/ir-types-tests.lisp — Compile IR Foundation Tests
;;;;
;;;; Tests for src/compile/ir/{types,block,ssa}.lisp
;;;; Covers: ir-value allocation, ir-block construction, CFG edge management,
;;;;         ir-rpo, ir-dominators (linear + diamond), SSA write/read/seal,
;;;;         ir-verify-ssa.
;;;; Printer tests → ir-printer-tests.lisp.

(in-package :cl-cc/test)


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; ir-value allocation (types.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ir-value-creation"
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (v0 (cl-cc/ir:ir-new-value fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn :type :integer)))
    ;; monotonically increasing IDs
    (expect (cl-cc/ir:irv-id v0) :to-equal 0)
    (expect (cl-cc/ir:irv-id v1) :to-equal 1)
    (expect (cl-cc/ir:irv-id v2) :to-equal 2)
    ;; type annotation
    (expect (cl-cc/ir:irv-type v2) :to-be :integer)
    (expect (cl-cc/ir:irv-type v0) :to-be-null)
    ;; predicate
    (expect (cl-cc/ir:ir-value-p v0) :to-be-truthy)
    (expect (cl-cc/ir:ir-value-p 42) :to-be-falsy)
    (expect (cl-cc/ir:ir-value-p nil) :to-be-falsy)
    (expect (cl-cc/ir:ir-value-p "string") :to-be-falsy)
    ;; def slot initially nil
    (expect (cl-cc/ir:irv-def v0) :to-be-null)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; ir-block allocation and ir-make-function (types.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ir-block-creation"
  (let* ((fn  (cl-cc/ir:ir-make-function 'my-fn :return-type :integer))
         (b1  (cl-cc/ir:ir-new-block fn))
         (b2  (cl-cc/ir:ir-new-block fn :then))
         (fn2 (cl-cc/ir:ir-make-function 'test))
         (ba  (cl-cc/ir:ir-new-block fn2 :b1))
         (bb  (cl-cc/ir:ir-new-block fn2 :b2)))
    ;; ir-make-function creates entry block
    (expect (cl-cc/ir:ir-function-p fn) :to-be-truthy)
    (expect (cl-cc/ir:ir-block-p    (cl-cc/ir:irf-entry fn)) :to-be-truthy)
    (expect (cl-cc/ir:irb-label (cl-cc/ir:irf-entry fn)) :to-be :entry)
    (expect (cl-cc/ir:irf-return-type fn) :to-equal :integer)
    (expect (cl-cc/ir:irf-name fn) :to-be 'my-fn)
    ;; auto-generated labels and IDs (entry=0, b1=1, b2=2)
    (expect (cl-cc/ir:irb-id b1) :to-equal 1)
    (expect (cl-cc/ir:irb-id b2) :to-equal 2)
    (expect (cl-cc/ir:irb-label b1) :to-be :block1)
    (expect (cl-cc/ir:irb-label b2) :to-be :then)
    ;; new block starts empty
    (expect (cl-cc/ir:irb-insts        b1) :to-be-null)
    (expect (cl-cc/ir:irb-params       b1) :to-be-null)
    (expect (cl-cc/ir:irb-predecessors b1) :to-be-null)
    (expect (cl-cc/ir:irb-successors   b1) :to-be-null)
    (expect (cl-cc/ir:irb-terminator   b1) :to-be-null)
    ;; blocks appended in creation order
    (let ((blocks (cl-cc/ir:irf-blocks fn2)))
      (expect (length blocks) :to-equal 3)
      (expect (first blocks) :to-be (cl-cc/ir:irf-entry fn2))
      (expect (second blocks) :to-be ba)
      (expect (third  blocks) :to-be bb))))


(it-sequential "ir-emit-appends-in-order-with-back-pointer"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (i1    (cl-cc/ir:make-ir-inst))
         (i2    (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-emit entry i1)
    (cl-cc/ir:ir-emit entry i2)
    (expect (length (cl-cc/ir:irb-insts entry)) :to-equal 2)
    (expect (first  (cl-cc/ir:irb-insts entry)) :to-be i1)
    (expect (second (cl-cc/ir:irb-insts entry)) :to-be i2)
    (expect (cl-cc/ir:iri-block i1) :to-be entry)
    (expect (cl-cc/ir:iri-block i2) :to-be entry)))

(it-sequential "ir-set-terminator-installs-terminator-and-back-pointer"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (term  (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-set-terminator entry term)
    (expect (cl-cc/ir:irb-terminator entry) :to-be term)
    (expect (cl-cc/ir:iri-block term) :to-be entry)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; RPO traversal (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────


(it-sequential "ir-rpo-linear-chain-in-order"
  (let* ((fn  (cl-cc/ir:ir-make-function 'test))
         (a   (cl-cc/ir:irf-entry fn))
         (b   (cl-cc/ir:ir-new-block fn :mid))
         (c   (cl-cc/ir:ir-new-block fn :exit)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (expect (length rpo) :to-equal 3)
      (expect (first  rpo) :to-be a)
      (expect (second rpo) :to-be b)
      (expect (third  rpo) :to-be c))))

(it-sequential "ir-rpo-excludes-unreachable-blocks"
  (let* ((fn          (cl-cc/ir:ir-make-function 'test))
         (entry        (cl-cc/ir:irf-entry fn))
         (reachable    (cl-cc/ir:ir-new-block fn :reachable))
         (_unreachable (cl-cc/ir:ir-new-block fn :unreachable)))
    (declare (ignore _unreachable))
    (cl-cc/ir:ir-add-edge entry reachable)
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (expect (length rpo) :to-equal 2)
      (expect (member entry     rpo :test #'eq) :to-be-truthy)
      (expect (member reachable rpo :test #'eq) :to-be-truthy))))

(it-sequential "ir-rpo-diamond-includes-all-four-blocks"
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge a c)
    (cl-cc/ir:ir-add-edge b d)
    (cl-cc/ir:ir-add-edge c d)
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (expect (length rpo) :to-equal 4)
      (expect (first rpo) :to-be a)
      (expect (member b rpo :test #'eq) :to-be-truthy)
      (expect (member c rpo :test #'eq) :to-be-truthy)
      (expect (member d rpo :test #'eq) :to-be-truthy))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Dominator tree (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ir-dominators-entry-self-dominates"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (idom  (cl-cc/ir:ir-dominators fn)))
    (expect (gethash entry idom) :to-be entry)))

(it-sequential "ir-dominators-linear-chain"
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (expect (gethash a idom) :to-be a)
      (expect (gethash b idom) :to-be a)
      (expect (gethash c idom) :to-be b))))

(it-sequential "ir-dominators-branch-from-single-node"
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (cl-cc/ir:ir-add-edge b d)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (expect (gethash a idom) :to-be a)
      (expect (gethash b idom) :to-be a)
      (expect (gethash c idom) :to-be b)
      (expect (gethash d idom) :to-be b))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA variable tracking (ssa.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ir-ssa-write-read-var"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (val   (cl-cc/ir:ir-new-value fn :type :integer))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (v1    (cl-cc/ir:ir-new-value fn))
         (v2    (cl-cc/ir:ir-new-value fn)))
    ;; same-block read
    (cl-cc/ir:ir-write-var fn 'x entry val)
    (expect (cl-cc/ir:ir-read-var fn 'x entry) :to-be val)
    ;; propagation through sealed predecessor
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-seal-block fn next)
    (expect (cl-cc/ir:ir-read-var fn 'x next) :to-be val)
    ;; overwrite in same block keeps last value
    (cl-cc/ir:ir-write-var fn 'y entry v1)
    (cl-cc/ir:ir-write-var fn 'y entry v2)
    (expect (cl-cc/ir:ir-read-var fn 'y entry) :to-be v2)))

(it-sequential "ir-read-var-undefined-returns-nil"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn)))
    (cl-cc/ir:ir-seal-block fn entry)
    (expect (cl-cc/ir:ir-read-var fn 'undefined-var entry) :to-be-null)))

(it-sequential "ir-seal-block-marks-sealed"
  (let* ((fn  (cl-cc/ir:ir-make-function 'test))
         (blk (cl-cc/ir:ir-new-block fn :b)))
    (expect (cl-cc/ir:irb-sealed-p blk) :to-be-falsy)
    (cl-cc/ir:ir-seal-block fn blk)
    (expect (cl-cc/ir:irb-sealed-p blk) :to-be-truthy)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA verifier (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ir-verify-ssa-valid-cases empty-function"
  (destructuring-bind (has-insts) (list nil)
    (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn)))
    (when has-insts
      (let ((v0 (cl-cc/ir:ir-new-value fn))
            (v1 (cl-cc/ir:ir-new-value fn)))
        (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v0))
        (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v1))))
    (expect (cl-cc/ir:ir-verify-ssa fn) :to-be-truthy))))

(it-sequential "ir-verify-ssa-valid-cases unique-results"
  (destructuring-bind (has-insts) (list t)
    (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn)))
    (when has-insts
      (let ((v0 (cl-cc/ir:ir-new-value fn))
            (v1 (cl-cc/ir:ir-new-value fn)))
        (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v0))
        (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v1))))
    (expect (cl-cc/ir:ir-verify-ssa fn) :to-be-truthy))))
