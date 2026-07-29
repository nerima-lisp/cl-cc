;;;; tests/unit/compile/ir/ir-block-ssa-tests.lisp — IR SSA Tests
;;;
;;; Tests for ir-collect-uses, ir-verify-ssa, Braun SSA
;;; (ir-write-var/ir-read-var), ir-seal-block, ir-new-value, ir-new-block.
;;; Depends on make-test-fn defined in ir-block-tests.lisp (same package, loads first).

(in-package :cl-cc/test)

;;; ─── ir-collect-uses ────────────────────────────────────────────────────────

(it-sequential "ir-collect-uses-empty"
  (let* ((fn (make-test-fn))
         (uses (cl-cc/ir:ir-collect-uses fn)))
    (expect (hash-table-count uses) :to-equal 0)))

;;; ─── ir-verify-ssa ──────────────────────────────────────────────────────────

(it-sequential "ir-verify-ssa-behavior"
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v2))
    (expect (cl-cc/ir:ir-verify-ssa fn) :to-be-truthy))
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (expect (handler-case (progn (cl-cc/ir:ir-verify-ssa fn) nil)
       (error () t)) :to-be-truthy)))

;;; ─── Braun SSA: ir-write-var / ir-read-var ──────────────────────────────────

(it-sequential "ir-ssa-write-read-same-block"
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (val (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'x blk val)
    (expect (cl-cc/ir:ir-read-var fn 'x blk) :to-be val))
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'x blk v1)
    (cl-cc/ir:ir-write-var fn 'x blk v2)
    (expect (cl-cc/ir:ir-read-var fn 'x blk) :to-be v2)))

(it-sequential "ir-ssa-read-propagates-from-predecessor"
  (let* ((fn (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next (cl-cc/ir:ir-new-block fn :next))
         (val (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn next)
    (cl-cc/ir:ir-write-var fn 'x entry val)
    (expect (cl-cc/ir:ir-read-var fn 'x next) :to-be val)))

(it-sequential "ir-ssa-join-creates-block-arg"
  (let* ((fn (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (left (cl-cc/ir:ir-new-block fn :left))
         (right (cl-cc/ir:ir-new-block fn :right))
         (join (cl-cc/ir:ir-new-block fn :join))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry left)
    (cl-cc/ir:ir-add-edge entry right)
    (cl-cc/ir:ir-add-edge left join)
    (cl-cc/ir:ir-add-edge right join)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn left)
    (cl-cc/ir:ir-seal-block fn right)
    (cl-cc/ir:ir-seal-block fn join)
    (cl-cc/ir:ir-write-var fn 'x left v1)
    (cl-cc/ir:ir-write-var fn 'x right v2)
    (let ((result (cl-cc/ir:ir-read-var fn 'x join)))
      (expect (cl-cc/ir:ir-value-p result) :to-be-truthy)
      (expect (> (length (cl-cc/ir:irb-params join)) 0) :to-be-truthy))))

(it-sequential "ir-ssa-sealed-block-behavior marks-sealed"
  (destructuring-bind (scenario) (list :sealed)
    (let* ((fn  (make-test-fn))
         (blk (cl-cc/ir:ir-new-block fn :orphan)))
    (cl-cc/ir:ir-seal-block fn blk)
    (ecase scenario
      (:sealed   (expect (cl-cc/ir:irb-sealed-p blk) :to-be-truthy))
      (:read-nil (expect (cl-cc/ir:ir-read-var fn 'x blk) :to-be-falsy))))))

(it-sequential "ir-ssa-sealed-block-behavior no-def-returns-nil"
  (destructuring-bind (scenario) (list :read-nil)
    (let* ((fn  (make-test-fn))
         (blk (cl-cc/ir:ir-new-block fn :orphan)))
    (cl-cc/ir:ir-seal-block fn blk)
    (ecase scenario
      (:sealed   (expect (cl-cc/ir:irb-sealed-p blk) :to-be-truthy))
      (:read-nil (expect (cl-cc/ir:ir-read-var fn 'x blk) :to-be-falsy))))))

;;; ─── ir-new-value / ir-new-block ────────────────────────────────────────────

(it-sequential "ir-new-value-increments-id"
  (let* ((fn (make-test-fn))
         (v0 (cl-cc/ir:ir-new-value fn))
         (v1 (cl-cc/ir:ir-new-value fn)))
    (expect (cl-cc/ir:irv-id v0) :to-equal 0)
    (expect (cl-cc/ir:irv-id v1) :to-equal 1)))

(it-sequential "ir-block-and-function-construction"
  (let* ((fn (make-test-fn))
         (b1 (cl-cc/ir:ir-new-block fn :test)))
    (expect (cl-cc/ir:irb-id (cl-cc/ir:irf-entry fn)) :to-equal 0)
    (expect (cl-cc/ir:irb-id b1) :to-equal 1)
    (expect (length (cl-cc/ir:irf-blocks fn)) :to-equal 2)
    (expect (cl-cc/ir:ir-block-p (cl-cc/ir:irf-entry fn)) :to-be-truthy)
    (expect (cl-cc/ir:irb-label (cl-cc/ir:irf-entry fn)) :to-be :entry)))
