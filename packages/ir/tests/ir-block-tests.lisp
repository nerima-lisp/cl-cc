;;;; tests/unit/compile/ir/ir-block-tests.lisp — CFG Block Structure Tests
;;;
;;; Tests for ir-add-edge, ir-emit, ir-set-terminator, ir-rpo, ir-dominators.
;;; SSA construction/verification → ir-block-ssa-tests.lisp.

(in-package :cl-cc/test)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-test-fn (&optional (name 'test-fn))
  "Create a fresh IR function with entry block."
  (cl-cc/ir:ir-make-function name))

(defun make-test-inst (fn &key result)
  "Create a simple IR instruction, optionally with a result value."
  (let ((inst (cl-cc/ir:make-ir-inst :result result)))
    inst))

;;; ─── ir-add-edge ────────────────────────────────────────────────────────────

(it-sequential "ir-add-edge-behavior"
  (let* ((fn   (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next)))
    ;; Before edge: no successors, no predecessors
    (expect (cl-cc/ir:irb-successors entry) :to-be-null)
    (expect (cl-cc/ir:irb-predecessors next) :to-be-null)
    (cl-cc/ir:ir-add-edge entry next)
    ;; After edge: entry has next as successor, next has entry as predecessor
    (expect (cl-cc/ir:irb-successors entry) :to-equal (list next))
    (expect (cl-cc/ir:irb-predecessors next) :to-equal (list entry)))
  (let* ((fn    (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (b1    (cl-cc/ir:ir-new-block fn :b1))
         (b2    (cl-cc/ir:ir-new-block fn :b2)))
    (cl-cc/ir:ir-add-edge entry b1)
    (cl-cc/ir:ir-add-edge entry b2)
    (expect (length (cl-cc/ir:irb-successors entry)) :to-equal 2))
  (let* ((fn    (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (b1    (cl-cc/ir:ir-new-block fn :b1)))
    (cl-cc/ir:ir-add-edge entry b1)
    (cl-cc/ir:ir-add-edge entry b1)
    (expect (length (cl-cc/ir:irb-successors entry)) :to-equal 1)))

;;; ─── ir-emit / ir-set-terminator ────────────────────────────────────────────

(it-sequential "ir-emit-behavior"
  (let* ((fn   (make-test-fn))
         (blk  (cl-cc/ir:irf-entry fn))
         (i1   (cl-cc/ir:make-ir-inst))
         (i2   (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-emit blk i1)
    (cl-cc/ir:ir-emit blk i2)
    (let ((body (cl-cc/ir:irb-insts blk)))
      (expect (length body) :to-equal 2)
      (expect (first body) :to-be i1)
      (expect (second body) :to-be i2))))

(it-sequential "ir-set-terminator-stores"
  (let* ((fn   (make-test-fn))
         (blk  (cl-cc/ir:irf-entry fn))
         (term (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-set-terminator blk term)
    (expect (cl-cc/ir:irb-terminator blk) :to-be term)))

;;; ─── ir-rpo ─────────────────────────────────────────────────────────────────

(it-sequential "ir-rpo-cases single-block"
  (destructuring-bind (setup verify) (list (lambda ()
      (let ((fn (make-test-fn)))
        (cl-cc/ir:ir-seal-block fn (cl-cc/ir:irf-entry fn))
        (values fn (list (cl-cc/ir:irf-entry fn))))) (lambda (fn expected-order)
      (assert-equal expected-order (cl-cc/ir:ir-rpo fn))))
    (multiple-value-bind (fn expected-order) (funcall setup)
    (funcall verify fn expected-order))))

(it-sequential "ir-rpo-cases linear-chain"
  (destructuring-bind (setup verify) (list (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (b1    (cl-cc/ir:ir-new-block fn :b1))
             (b2    (cl-cc/ir:ir-new-block fn :b2)))
        (cl-cc/ir:ir-add-edge entry b1)
        (cl-cc/ir:ir-add-edge b1 b2)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn b1)
        (cl-cc/ir:ir-seal-block fn b2)
        (values fn (list entry b1 b2)))) (lambda (fn expected-order)
      (assert-equal expected-order (cl-cc/ir:ir-rpo fn))))
    (multiple-value-bind (fn expected-order) (funcall setup)
    (funcall verify fn expected-order))))

(it-sequential "ir-rpo-cases diamond"
  (destructuring-bind (setup verify) (list (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (left  (cl-cc/ir:ir-new-block fn :left))
             (right (cl-cc/ir:ir-new-block fn :right))
             (join  (cl-cc/ir:ir-new-block fn :join)))
        (cl-cc/ir:ir-add-edge entry left)
        (cl-cc/ir:ir-add-edge entry right)
        (cl-cc/ir:ir-add-edge left join)
        (cl-cc/ir:ir-add-edge right join)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn left)
        (cl-cc/ir:ir-seal-block fn right)
        (cl-cc/ir:ir-seal-block fn join)
        (values fn (list entry left right join)))) (lambda (fn expected-order)
      (let ((rpo (cl-cc/ir:ir-rpo fn)))
        (assert-= 4 (length rpo))
        (assert-eq (first expected-order) (first rpo))
        (assert-eq (car (last expected-order)) (car (last rpo))))))
    (multiple-value-bind (fn expected-order) (funcall setup)
    (funcall verify fn expected-order))))

(it-sequential "ir-rpo-cases back-edge-loop"
  (destructuring-bind (setup verify) (list (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (body  (cl-cc/ir:ir-new-block fn :body))
             (exit  (cl-cc/ir:ir-new-block fn :exit)))
        (cl-cc/ir:ir-add-edge entry body)
        (cl-cc/ir:ir-add-edge body exit)
        (cl-cc/ir:ir-add-edge body body)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn body)
        (cl-cc/ir:ir-seal-block fn exit)
        (values fn nil))) (lambda (fn _)
      (let ((rpo (cl-cc/ir:ir-rpo fn)))
        (assert-= 3 (length rpo))
        (assert-true (member (cl-cc/ir:irf-entry fn) rpo)))))
    (multiple-value-bind (fn expected-order) (funcall setup)
    (funcall verify fn expected-order))))

;;; ─── ir-dominators ──────────────────────────────────────────────────────────

(it-sequential "ir-dominators-cases entry-dominates-itself"
  (destructuring-bind (setup verify) (list (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn)))
        (cl-cc/ir:ir-seal-block fn entry)
        (values fn entry (list (cons entry entry))))) (lambda (fn entry checks)
      (declare (ignore entry))
      (let ((idom (cl-cc/ir:ir-dominators fn)))
        (dolist (pair checks)
          (assert-eq (cdr pair) (gethash (car pair) idom))))))
    (multiple-value-bind (fn entry checks) (funcall setup)
    (funcall verify fn entry checks))))

(it-sequential "ir-dominators-cases linear-chain-dominators"
  (destructuring-bind (setup verify) (list (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (b1    (cl-cc/ir:ir-new-block fn :b1))
             (b2    (cl-cc/ir:ir-new-block fn :b2)))
        (cl-cc/ir:ir-add-edge entry b1)
        (cl-cc/ir:ir-add-edge b1 b2)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn b1)
        (cl-cc/ir:ir-seal-block fn b2)
        (values fn entry (list (cons b1 entry) (cons b2 b1))))) (lambda (fn entry checks)
      (declare (ignore entry))
      (let ((idom (cl-cc/ir:ir-dominators fn)))
        (dolist (pair checks)
          (assert-eq (cdr pair) (gethash (car pair) idom))))))
    (multiple-value-bind (fn entry checks) (funcall setup)
    (funcall verify fn entry checks))))

(it-sequential "ir-dominators-cases diamond-join-dominated-by-entry"
  (destructuring-bind (setup verify) (list (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (left  (cl-cc/ir:ir-new-block fn :left))
             (right (cl-cc/ir:ir-new-block fn :right))
             (join  (cl-cc/ir:ir-new-block fn :join)))
        (cl-cc/ir:ir-add-edge entry left)
        (cl-cc/ir:ir-add-edge entry right)
        (cl-cc/ir:ir-add-edge left join)
        (cl-cc/ir:ir-add-edge right join)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn left)
        (cl-cc/ir:ir-seal-block fn right)
        (cl-cc/ir:ir-seal-block fn join)
        (values fn entry (list (cons left entry) (cons right entry) (cons join entry))))) (lambda (fn entry checks)
      (declare (ignore entry))
      (let ((idom (cl-cc/ir:ir-dominators fn)))
        (dolist (pair checks)
          (assert-eq (cdr pair) (gethash (car pair) idom))))))
    (multiple-value-bind (fn entry checks) (funcall setup)
    (funcall verify fn entry checks))))

;;; ─── %ir-rpo-dfs (extracted helper) ──────────────────────────────────────────

(it-sequential "ir-rpo-dfs-visits-single-block"
  (let* ((fn    (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (visited (make-hash-table :test #'eq))
         (cell    (list nil)))
    (cl-cc/ir::%ir-rpo-dfs entry visited cell)
    (expect (car cell) :to-equal (list entry))
    (expect (gethash entry visited) :to-be-truthy)))

(it-sequential "ir-rpo-dfs-does-not-revisit"
  (let* ((fn    (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (visited (make-hash-table :test #'eq))
         (cell    (list nil)))
    (cl-cc/ir::%ir-rpo-dfs entry visited cell)
    (cl-cc/ir::%ir-rpo-dfs entry visited cell)
    (expect (length (car cell)) :to-equal 1)))

(it-sequential "ir-rpo-dfs-traverses-chain"
  (let* ((fn  (make-test-fn))
         (a   (cl-cc/ir:irf-entry fn))
         (b   (cl-cc/ir:ir-new-block fn :b))
         (c   (cl-cc/ir:ir-new-block fn :c)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (let ((visited (make-hash-table :test #'eq))
          (cell    (list nil)))
      (cl-cc/ir::%ir-rpo-dfs a visited cell)
      (expect (car cell) :to-equal (list a b c)))))
