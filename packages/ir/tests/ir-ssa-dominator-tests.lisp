;;;; packages/ir/tests/ir-ssa-dominator-tests.lisp — IR Dominator & Collect-Uses Tests
;;;
;;; Continuation of ir-ssa-advanced-tests.lisp.
;;; Covers: ir-dominators (deeper chains), ir-collect-uses (with operands),
;;; ir-verify-ssa (cross-block), and independent variable tracking.
;;;
;;; Note: test-use-inst struct/method is defined in ir-ssa-advanced-tests.lisp
;;; and available here via shared :cl-cc/test package.

(in-package :cl-cc/test)

;;; ─── ir-dominators: deeper chains and loops ─────────────────────────────────

(it-sequential "ir-dominators-deep-chain"
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (cl-cc/ir:ir-add-edge c d)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (expect (gethash a idom) :to-be a)
      (expect (gethash b idom) :to-be a)
      (expect (gethash c idom) :to-be b)
      (expect (gethash d idom) :to-be c))))

(it-sequential "ir-dominators-two-branches-then-merge"
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d))
         (e  (cl-cc/ir:ir-new-block fn :e)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge a c)
    (cl-cc/ir:ir-add-edge b d)
    (cl-cc/ir:ir-add-edge c d)
    (cl-cc/ir:ir-add-edge d e)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (expect (gethash a idom) :to-be a)
      (expect (gethash d idom) :to-be a)
      (expect (gethash e idom) :to-be d))))

(it-sequential "ir-dominators-unreachable-absent"
  (let* ((fn      (cl-cc/ir:ir-make-function 'test-fn))
         (entry   (cl-cc/ir:irf-entry fn))
         (reached (cl-cc/ir:ir-new-block fn :reached))
         (orphan  (cl-cc/ir:ir-new-block fn :orphan)))
    (cl-cc/ir:ir-add-edge entry reached)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (expect (gethash entry   idom) :to-be-truthy)
      (expect (gethash reached idom) :to-be-truthy)
      (expect (gethash orphan  idom) :to-be-falsy))))

;;; ─── ir-collect-uses with custom operands ────────────────────────────────────

(it-sequential "ir-collect-uses-single-operand"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn))
         ;; i0 produces v0; i1 uses v0 and produces v1
         (i0    (cl-cc/ir:make-ir-inst :result v0))
         (i1    (make-test-use-inst :result v1 :operands (list v0))))
    (cl-cc/ir:ir-emit entry i0)
    (cl-cc/ir:ir-emit entry i1)
    (let ((uses (cl-cc/ir:ir-collect-uses fn)))
      ;; v0 is used by i1
      (expect (member i1 (gethash v0 uses) :test #'eq) :to-be-truthy)
      ;; v1 is not used by anyone
      (expect (gethash v1 uses) :to-be-null))))

(it-sequential "ir-collect-uses-multiple-operands"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn))
         (v2    (cl-cc/ir:ir-new-value fn))
         ;; i1 uses v0; i2 also uses v0
         (i1    (make-test-use-inst :result v1 :operands (list v0)))
         (i2    (make-test-use-inst :result v2 :operands (list v0))))
    (cl-cc/ir:ir-emit entry i1)
    (cl-cc/ir:ir-emit entry i2)
    (let ((uses (cl-cc/ir:ir-collect-uses fn)))
      (let ((users (gethash v0 uses)))
        (expect (length users) :to-equal 2)
        (expect (member i1 users :test #'eq) :to-be-truthy)
        (expect (member i2 users :test #'eq) :to-be-truthy)))))

(it-sequential "ir-collect-uses-across-blocks"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn))
         ;; define v0 in entry; use it in next
         (i0    (cl-cc/ir:make-ir-inst :result v0))
         (i1    (make-test-use-inst :result v1 :operands (list v0))))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-emit entry i0)
    (cl-cc/ir:ir-emit next  i1)
    (let ((uses (cl-cc/ir:ir-collect-uses fn)))
      (expect (member i1 (gethash v0 uses) :test #'eq) :to-be-truthy))))

;;; ─── ir-verify-ssa: cross-block checks ──────────────────────────────────────

(it-sequential "ir-verify-ssa-cross-block-valid"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v0))
    (cl-cc/ir:ir-emit next  (cl-cc/ir:make-ir-inst :result v1))
    (expect (cl-cc/ir:ir-verify-ssa fn) :to-be-truthy))
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (i0    (cl-cc/ir:make-ir-inst))
         (i1    (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-emit entry i0)
    (cl-cc/ir:ir-emit entry i1)
    (expect (cl-cc/ir:ir-verify-ssa fn) :to-be-truthy)))

;;; ─── ir-write-var / ir-read-var: multiple independent variables ──────────────

(it-sequential "ir-ssa-independent-vars"
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (va    (cl-cc/ir:ir-new-value fn))
         (vb    (cl-cc/ir:ir-new-value fn))
         (vc    (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'a entry va)
    (cl-cc/ir:ir-write-var fn 'b entry vb)
    (cl-cc/ir:ir-write-var fn 'c entry vc)
    (expect (cl-cc/ir:ir-read-var fn 'a entry) :to-be va)
    (expect (cl-cc/ir:ir-read-var fn 'b entry) :to-be vb)
    (expect (cl-cc/ir:ir-read-var fn 'c entry) :to-be vc))
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (va    (cl-cc/ir:ir-new-value fn))
         (vb    (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn next)
    (cl-cc/ir:ir-write-var fn 'a entry va)
    (cl-cc/ir:ir-write-var fn 'b entry vb)
    (expect (cl-cc/ir:ir-read-var fn 'a next) :to-be va)
    (expect (cl-cc/ir:ir-read-var fn 'b next) :to-be vb)))
