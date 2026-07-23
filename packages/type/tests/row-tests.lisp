;;;; tests/unit/type/row-tests.lisp — Row Polymorphism Tests
;;;;
;;;; Tests for src/type/row.lisp:
;;;; row-extend, row-restrict, row-select, row-labels,
;;;; row-closed-p, row-open-p,
;;;; effect-row-extend, effect-row-restrict, effect-row-member-p.

(in-package :cl-cc/test)



;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-closed-record (&rest label-type-pairs)
  "Build a closed type-record from alternating label type pairs."
  (make-type-record
   :fields (loop for (l ty) on label-type-pairs by #'cddr
                 collect (cons l ty))
   :row-var nil))

(defun make-open-record (row-var &rest label-type-pairs)
  "Build an open type-record with ROW-VAR."
  (make-type-record
   :fields (loop for (l ty) on label-type-pairs by #'cddr
                 collect (cons l ty))
   :row-var row-var))

;;; ─── row-extend ──────────────────────────────────────────────────────────────

(it-sequential "row-extend-behavior"
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-extend :y type-string r)))
    (expect (type-record-p r2) :to-be-truthy)
    (expect (length (type-record-fields r2)) :to-equal 2))
  (let* ((rv (fresh-type-var))
         (r2 (row-extend :y type-string (make-open-record rv :x type-int))))
    (expect (type-var-p (type-record-row-var r2)) :to-be-truthy))
  (let* ((r (make-closed-record :x type-int))
         (r2 (row-extend :x type-string r)))
    (expect (length (type-record-fields r2)) :to-equal 2)
    (expect (type-equal-p type-string (row-select :x r2)) :to-be-truthy)))

;;; ─── row-restrict ────────────────────────────────────────────────────────────

(it-sequential "row-restrict-behavior"
  (let* ((r (make-closed-record :x type-int :y type-string))
         (r2 (row-restrict :x r)))
    (expect (length (type-record-fields r2)) :to-equal 1)
    (expect (row-select :x r2) :to-be-null)
    (expect (type-equal-p type-string (row-select :y r2)) :to-be-truthy))
  (expect (length (type-record-fields
                             (row-restrict :z (make-closed-record :x type-int)))) :to-equal 1)
  (let* ((rv (fresh-type-var))
         (r2 (row-restrict :x (make-open-record rv :x type-int))))
    (expect (type-var-p (type-record-row-var r2)) :to-be-truthy)))

;;; ─── row-select ──────────────────────────────────────────────────────────────

(it-sequential "row-select-behavior"
  (let ((r (make-closed-record :x type-int :y type-string)))
    (expect (type-equal-p type-int (row-select :x r)) :to-be-truthy)
    (expect (type-equal-p type-string (row-select :y r)) :to-be-truthy))
  (expect (row-select :z (make-closed-record :x type-int)) :to-be-null)
  (let ((v (make-type-variant :cases (list (cons :some type-int) (cons :none type-null))
                              :row-var nil)))
    (expect (type-equal-p type-int  (row-select :some v)) :to-be-truthy)
    (expect (type-equal-p type-null (row-select :none v)) :to-be-truthy)))

;;; ─── row-labels ──────────────────────────────────────────────────────────────

(it-sequential "row-labels-behavior"
  (expect (row-labels (make-closed-record :x type-int :y type-string)) :to-equal '(:x :y))
  (expect (row-labels (make-type-variant :cases (list (cons :a type-int)
                                                                      (cons :b type-string))
                                                         :row-var nil)) :to-equal '(:a :b))
  (expect (row-labels (make-closed-record)) :to-be-null))

;;; ─── row-closed-p / row-open-p ──────────────────────────────────────────────

(it-sequential "row-closed-open-predicate-behavior"
  (let ((r (make-closed-record :x type-int)))
    (expect (row-closed-p r) :to-be-truthy)
    (expect (row-open-p r) :to-be-falsy))
  (let ((r (make-open-record (fresh-type-var) :x type-int)))
    (expect (row-open-p r) :to-be-truthy)
    (expect (row-closed-p r) :to-be-falsy))
  (expect (row-closed-p (make-type-variant :cases nil :row-var nil)) :to-be-truthy)
  (expect (row-open-p   (make-type-variant :cases nil :row-var (fresh-type-var))) :to-be-truthy))

;;; ─── effect-row operations ──────────────────────────────────────────────────

(it-sequential "effect-row-extend-adds-op"
  (let* ((op (cl-cc/type:make-type-effect-op :name :io :args nil))
         (row (make-type-effect-row :effects nil :row-var nil))
         (row2 (cl-cc/type:effect-row-extend op row)))
    (expect (length (type-effect-row-effects row2)) :to-equal 1)))

(it-sequential "effect-row-restrict-removes"
  (let* ((op1 (cl-cc/type:make-type-effect-op :name :io :args nil))
         (op2 (cl-cc/type:make-type-effect-op :name :state :args nil))
         (row (make-type-effect-row :effects (list op1 op2) :row-var nil))
         (row2 (cl-cc/type:effect-row-restrict :io row)))
    (expect (length (type-effect-row-effects row2)) :to-equal 1)))

(it-sequential "effect-row-member-p-behavior"
  (let ((row-with-io (make-type-effect-row :effects (list (cl-cc/type:make-type-effect-op :name :io :args nil))
                                             :row-var nil))
        (empty-row   (make-type-effect-row :effects nil :row-var nil)))
    (expect (cl-cc/type:effect-row-member-p :io row-with-io) :to-be-truthy)
    (expect (cl-cc/type:effect-row-member-p :io empty-row) :to-be-falsy)))

(it-sequential "row-extend-basic"
  (let* ((base (make-type-record :fields (list (cons 'x type-int)) :row-var nil))
         (ext  (row-extend 'y type-string base)))
    (expect (type-record-p ext) :to-be-truthy)
    (expect (= 2 (length (type-record-fields ext))) :to-be-truthy)
    (expect (assoc 'y (type-record-fields ext)) :to-be-truthy)))

(it-sequential "row-restrict-basic"
  (let* ((rec (make-type-record :fields (list (cons 'x type-int)
                                              (cons 'y type-string))
                                :row-var nil))
         (r   (row-restrict 'x rec)))
    (expect (type-record-p r) :to-be-truthy)
    (expect (= 1 (length (type-record-fields r))) :to-be-truthy)
    (expect (assoc 'x (type-record-fields r)) :to-be-null)))

(it-sequential "row-select-basic"
  (let ((rec (make-type-record :fields (list (cons 'name type-string)
                                             (cons 'age  type-int))
                               :row-var nil)))
    (expect (type-equal-p type-string (row-select 'name rec)) :to-be-truthy)
    (expect (type-equal-p type-int    (row-select 'age  rec)) :to-be-truthy)
    (expect (row-select 'missing rec) :to-be-null)))

(it-sequential "row-labels-basic"
  (let ((rec (make-type-record :fields (list (cons 'a type-int)
                                             (cons 'b type-bool))
                               :row-var nil)))
    (let ((labs (row-labels rec)))
      (expect (= 2 (length labs)) :to-be-truthy)
      (expect (member 'a labs) :to-be-truthy)
      (expect (member 'b labs) :to-be-truthy))))

(it-sequential "row-open-closed"
  (let* ((closed (make-type-record :fields (list (cons 'x type-int)) :row-var nil))
         (rv     (fresh-type-var :name 'rho))
         (open   (make-type-record :fields (list (cons 'x type-int)) :row-var rv)))
    (expect (row-closed-p closed) :to-be-truthy)
    (expect (row-open-p   closed) :to-be-falsy)
    (expect (row-closed-p open) :to-be-falsy)
    (expect (row-open-p   open) :to-be-truthy)))
