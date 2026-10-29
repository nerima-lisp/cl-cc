;;;; tests/unit/optimize/optimizer-memory-pass-tests.lisp
;;;; Unit tests for optimizer-memory-alias.lisp — alias queries, store/load passes
;;;;
;;;; Covers: opt-must-alias-p, opt-may-alias-p, opt-slot-alias-key,
;;;;   opt-rewrite-inst-regs, opt-pass-dead-store-elim,
;;;;   opt-pass-store-to-load-forward, opt-pass-cons-slot-forward,
;;;;   mem-pass-state helpers, *opt-heap-root-kind-table* integrity,
;;;;   *opt-interval-binop-table*, %mps-pending-uses-reg-p,
;;;;   %mps-flush-if-src-overwritten, %mps-flush-dependent-on-reg.

(in-package :cl-cc/test)

;;; ─── opt-must-alias-p / opt-may-alias-p ─────────────────────────────────

(defun %make-alias-table (&rest entries)
  "Build a :eq hash-table from alternating key/value ENTRIES."
  (let ((ht (make-hash-table :test #'eq)))
    (loop for (k v) on entries by #'cddr do (setf (gethash k ht) v))
    ht))

(defmacro assert-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── opt-slot-alias-key / opt-rewrite-inst-regs ────────────────────────

;;; ─── opt-pass-dead-store-elim ────────────────────────────────────────────

(defun %count-insts-of-type (result type-pred)
  "Count instructions in RESULT matching TYPE-PRED."
  (count-if type-pred result))

;;; ─── opt-pass-store-to-load-forward ──────────────────────────────────────

;;; ─── opt-pass-cons-slot-forward ──────────────────────────────────────────

;;; ─── mem-pass-state struct helpers ──────────────────────────────────────────

;;; ─── *opt-heap-root-kind-table* data integrity ───────────────────────────

;;; ─── *opt-interval-binop-table* / %opt-update-interval-binop ────────────

;;; ─── %mps-pending-uses-reg-p ─────────────────────────────────────────────

;;; ─── %mps-flush-if-src-overwritten ──────────────────────────────────────────

;;; ─── %mps-flush-dependent-on-reg ────────────────────────────────────────────

(it-sequential "dead-store-elim-tbaa-disjoint-read-does-not-observe-slot-store"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :n :value 4)
                        (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1)
                        (make-vm-make-array :dst :arr :size-reg :n
                                            :initial-element nil :fill-pointer nil
                                            :adjustable nil :element-type nil)
                        (make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r2)
                        (cl-cc:make-vm-slot-read :dst :v :obj-reg :arr :slot-name 'x)
                        (make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r3)
                        (make-vm-ret :reg :v)))))
    (expect (= 1 (%count-insts-of-type result (lambda (inst) (typep inst 'cl-cc/vm/clos:vm-slot-write)))) :to-be-truthy)))

(it-sequential "dead-store-elim-unknown-alias-read-observes-slot-store"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-slot-write :obj-reg :a :slot-name 'x :value-reg :r1)
                        (cl-cc:make-vm-slot-read :dst :v :obj-reg :b :slot-name 'x)
                        (make-vm-slot-write :obj-reg :a :slot-name 'x :value-reg :r2)
                        (make-vm-ret :reg :v)))))
    (expect (= 2 (%count-insts-of-type result (lambda (inst) (typep inst 'cl-cc/vm/clos:vm-slot-write)))) :to-be-truthy)))

(it-sequential "optimize-instructions-rewrites-logand-one-eq-zero-to-evenp"
  (let* ((result (cl-cc/optimize:optimize-instructions
                  (list (make-vm-get-global :dst :x :name 'x)
                        (make-vm-const :dst :one :value 1)
                        (make-vm-logand :dst :bit :lhs :x :rhs :one)
                        (make-vm-const :dst :zero :value 0)
                        (make-vm-num-eq :dst :pred :lhs :bit :rhs :zero)
                        (make-vm-ret :reg :pred))
                  :max-iterations 1
                  :pass-pipeline '(:fold :dce)))
         (even-inst (find-if (lambda (inst) (eq :evenp (car (cl-cc/vm:instruction->sexp inst)))) result)))
    (expect even-inst :to-be-truthy)
    (expect (cl-cc/vm:vm-dst even-inst) :to-be :pred)
    (expect (cl-cc/vm:vm-src even-inst) :to-be :x)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-logand)) result) :to-be-falsy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-num-eq)) result) :to-be-falsy)))
