;;;; tests/unit/optimize/optimizer-inline-pass-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp
;;;;
;;;; Covers:
;;;;   Memo utilities  — opt-make-pure-function-memo-table,
;;;;                     opt-pure-function-memo-get, opt-pure-function-memo-put
;;;;   Body analysis   — opt-function-body-instruction-tables (single-pass, returns values)
;;;;   Reachability    — opt-reachable-function-labels

(in-package :cl-cc/test)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

;;; Note: opt-collect-function-defs requires (vm-closure-params inst) to be
;;; non-nil — functions with zero parameters are excluded from collection.
;;; Always give test closures at least one param so they are collected.

(defun make-inline-pass-func-insts (label params body-insts)
  "Return instruction list: vm-closure + vm-label + BODY-INSTS + vm-ret."
  (list* (make-vm-closure :dst :r9 :label label :params params :captured nil)
         (make-vm-label  :name label)
         (append body-insts
                 (list (make-vm-ret :reg (first params))))))

(defun collect-inline-pass-func-defs (label params body-insts)
  "Build func-defs table directly for functions with the given structure."
  (cl-cc/optimize::opt-collect-function-defs
   (make-inline-pass-func-insts label params body-insts)))

;;; ─── opt-make-pure-function-memo-table ──────────────────────────────────────

(it-sequential "opt-memo-table-is-hash-table"
  (let ((ht (cl-cc/optimize::opt-make-pure-function-memo-table)))
    (expect (hash-table-p ht) :to-be-truthy)
    (expect (= 0 (hash-table-count ht)) :to-be-truthy)))

;;; ─── opt-pure-function-memo-get / opt-pure-function-memo-put ────────────────

(it-sequential "opt-memo-get-miss-cases impure-label"
  (destructuring-bind (mark-pure-p) (list nil)
    (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures (make-hash-table :test #'equal)))
    (when mark-pure-p (setf (gethash "fn" pures) t))
    (multiple-value-bind (val found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (expect val :to-be-null)
      (expect found-p :to-be-null)))))

(it-sequential "opt-memo-get-miss-cases pure-no-put"
  (destructuring-bind (mark-pure-p) (list t)
    (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures (make-hash-table :test #'equal)))
    (when mark-pure-p (setf (gethash "fn" pures) t))
    (multiple-value-bind (val found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (expect val :to-be-null)
      (expect found-p :to-be-null)))))

(it-sequential "opt-memo-roundtrip"
  (let ((memo   (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(2) :answer)
    (multiple-value-bind (val found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(2))
      (expect val :to-be :answer)
      (expect found-p :to-be-truthy))))

(it-sequential "opt-memo-put-ignores-impure-label"
  (let ((memo   (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "impure" '() :result)
    (expect (= 0 (hash-table-count memo)) :to-be-truthy)))

(it-sequential "opt-memo-lru-evicts-oldest-entry-at-max-size"
  (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table :max-size 2))
        (pures (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(1) :a)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(2) :b)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(3) :c)
    (expect (= 2 (hash-table-count memo)) :to-be-truthy)
    (multiple-value-bind (v1 f1)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (declare (ignore v1))
      (expect f1 :to-be-falsy))
    (multiple-value-bind (v2 f2)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(2))
      (expect f2 :to-be-truthy)
      (expect v2 :to-be :b))
    (multiple-value-bind (v3 f3)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(3))
      (expect f3 :to-be-truthy)
      (expect v3 :to-be :c))))

(it-sequential "opt-memo-lru-touch-on-get-updates-recentness"
  (let ((memo  (cl-cc/optimize::opt-make-pure-function-memo-table :max-size 2))
        (pures (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(1) :a)
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(2) :b)
    ;; touch key(1): now key(2) becomes LRU
    (multiple-value-bind (_v f)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (declare (ignore _v))
      (expect f :to-be-truthy))
    (cl-cc/optimize::opt-pure-function-memo-put memo pures "fn" '(3) :c)
    (multiple-value-bind (_v f)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(2))
      (declare (ignore _v))
      (expect f :to-be-falsy))
    (multiple-value-bind (v1 f1)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(1))
      (expect f1 :to-be-truthy)
      (expect v1 :to-be :a))
    (multiple-value-bind (v3 f3)
        (cl-cc/optimize::opt-pure-function-memo-get memo pures "fn" '(3))
      (expect f3 :to-be-truthy)
      (expect v3 :to-be :c))))

;;; ─── opt-function-body-instruction-tables ───────────────────────────────────

(it-sequential "opt-body-instruction-set-empty"
  (multiple-value-bind (ht _labels)
      (cl-cc/optimize::opt-function-body-instruction-tables (make-hash-table :test #'equal))
    (expect (= 0 (hash-table-count ht)) :to-be-truthy)))

(it-sequential "opt-body-instruction-set-contains-body-insts"
  (let* ((c1    (make-vm-const :dst :r1 :value 42))
         (ret   (make-vm-ret   :reg :r1))
         (fdefs (make-hash-table :test #'equal)))
    (setf (gethash "f" fdefs)
          (list :closure nil :params nil :body (list c1 ret)))
    (multiple-value-bind (s _labels)
        (cl-cc/optimize::opt-function-body-instruction-tables fdefs)
      (expect (gethash c1  s) :to-be-truthy)
      (expect (gethash ret s) :to-be-truthy))))

(it-sequential "opt-body-instruction-labels-maps-to-label"
  (let* ((c1    (make-vm-const :dst :r0 :value 1))
         (ret   (make-vm-ret   :reg :r0))
         (fdefs (make-hash-table :test #'equal)))
    (setf (gethash "myfn" fdefs)
          (list :closure nil :params nil :body (list c1 ret)))
    (multiple-value-bind (_set inst->label)
        (cl-cc/optimize::opt-function-body-instruction-tables fdefs)
      (expect (gethash c1  inst->label) :to-equal "myfn")
      (expect (gethash ret inst->label) :to-equal "myfn"))))

;;; ─── opt-reachable-function-labels ──────────────────────────────────────────

(it-sequential "opt-reachable-empty-roots"
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (expect (= 0 (hash-table-count r)) :to-be-truthy))))

(it-sequential "opt-reachable-direct-root"
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "fn" graph) nil)
    (setf (gethash "fn" roots) t)
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (expect (gethash "fn" r) :to-be-truthy))))

(it-sequential "opt-reachable-transitive"
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (setf (gethash "b" graph) '("c"))
    (setf (gethash "c" graph) nil)
    (setf (gethash "a" roots) t)
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (expect (gethash "a" r) :to-be-truthy)
      (expect (gethash "b" r) :to-be-truthy)
      (expect (gethash "c" r) :to-be-truthy))))

(it-sequential "opt-reachable-cycle-terminates"
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "x" graph) '("y"))
    (setf (gethash "y" graph) '("x"))
    (setf (gethash "x" roots) t)
    (let ((r (cl-cc/optimize::opt-reachable-function-labels graph roots)))
      (expect (gethash "x" r) :to-be-truthy)
      (expect (gethash "y" r) :to-be-truthy))))
