;;;; tests/unit/expand/defstruct-tests.lisp — expand-defstruct unit tests
;;;;
;;;; Tests for the defstruct → (progn defclass defun defun) expansion.
;;;; Covers: basic structs, :conc-name, :constructor, BOA constructors,
;;;; slot defaults, predicate generation, and accessor-slot-map side effect.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun ds-expand (form)
  "Shorthand: call expand-defstruct and return the expansion."
  (cl-cc/expand::expand-defstruct form))

(defun ds-progn-forms (expansion)
  "Return the body forms of a (progn ...) expansion (cdr)."
  (rest expansion))

(defun ds-assert-deriving-registers (typeclass-name)
  "Assert that a :deriving defstruct registers TYPECLASS-NAME when evaluated."
  (cl-cc/expand:with-fresh-defstruct-registries
    (let ((cl-cc/type::*typeclass-registry* (make-hash-table :test #'eq))
          (cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal))
          (name (gensym "DERIVING-POINT-")))
      (eval (ds-expand `(defstruct (,name (:deriving eq show ord)) x y)))
      (expect (has-typeclass-instance-p typeclass-name name) :to-be-truthy))))

;;; ─── Basic struct ─────────────────────────────────────────────────────────

(it-sequential "ds-basic-expansion-produces-defclass"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp         (ds-expand '(defstruct point x y)))
           (defclass-form (second exp))
           (slot-specs  (fourth defclass-form))
           (first-slot  (first slot-specs)))
      (expect (first exp) :to-equal 'progn)
      (expect (first defclass-form) :to-equal 'defclass)
      (expect (second defclass-form) :to-equal 'point)
      (expect (length slot-specs) :to-equal 2)
      (expect (getf (rest first-slot) :initarg) :to-equal :x)
      (expect (getf (rest first-slot) :initform) :to-equal nil))))

(it-sequential "ds-basic-expansion-generates-constructor"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct point x y)))
           (forms (ds-progn-forms exp))
           (ctor  (second forms))
           (body  (fourth ctor)))
      (expect (first ctor) :to-equal 'defun)
      (expect (second ctor) :to-equal (intern "MAKE-POINT"))
      (expect (first body) :to-equal 'make-instance))))

(it-sequential "ds-basic-expansion-generates-predicate"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct point x y)))
           (forms (ds-progn-forms exp))
           (pred  (third forms)))
      (expect (first pred) :to-equal 'defun)
      (expect (second pred) :to-equal (intern "POINT-P")))))

(it-sequential "ds-basic-expansion-ends-with-quoted-name"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct point x y)))
           (forms (ds-progn-forms exp))
           (last-form (car (last forms))))
      (expect (first last-form) :to-equal 'quote)
      (expect (second last-form) :to-equal 'point))))

;;; ─── Slot defaults and filtering ─────────────────────────────────────────

(it-sequential "ds-slot-default-initform"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp (ds-expand '(defstruct config (timeout 30) (verbose nil))))
           (defclass-form (second exp))
           (slot-specs (fourth defclass-form))
           (first-slot (first slot-specs)))
      (expect (getf (rest first-slot) :initform) :to-equal 30))))

;;; ─── :conc-name option ────────────────────────────────────────────────────

(it-sequential "ds-conc-name-behavior custom"
  (destructuring-bind (form expected-accessor-name) (list '(defstruct (point (:conc-name pt-)) x y) "PT-X")
    (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp  (ds-expand form))
           (slot (first (fourth (second exp)))))
      (expect (getf (rest slot) :accessor) :to-equal (intern expected-accessor-name))))))

(it-sequential "ds-conc-name-behavior default"
  (destructuring-bind (form expected-accessor-name) (list '(defstruct point x) "POINT-X")
    (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp  (ds-expand form))
           (slot (first (fourth (second exp)))))
      (expect (getf (rest slot) :accessor) :to-equal (intern expected-accessor-name))))))

;;; ─── :constructor option ──────────────────────────────────────────────────

(it-sequential "ds-constructor-renamed-by-option"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct (point (:constructor new-point)) x y)))
           (forms (ds-progn-forms exp))
           (ctor  (second forms)))
      (expect (symbol-name (second ctor)) :to-equal "NEW-POINT"))))

(it-sequential "ds-boa-constructor-uses-positional-params"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp    (ds-expand '(defstruct (point (:constructor make-pt (x y))) x y)))
           (forms  (ds-progn-forms exp))
           (ctor   (second forms))
           (params (third ctor)))
      (expect (length params) :to-equal 2)
      (expect (symbol-name (first params)) :to-equal "X")
      (expect (symbol-name (second params)) :to-equal "Y"))))

;;; ─── Docstring filtering ──────────────────────────────────────────────────

(it-sequential "ds-docstring-ignored"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp (ds-expand '(defstruct point "A 2D point." x y)))
           (defclass-form (second exp))
           (slot-specs (fourth defclass-form)))
      (expect (length slot-specs) :to-equal 2))))

;;; ─── *accessor-slot-map* side effect ──────────────────────────────────────

(it-sequential "ds-accessor-slot-map-populated"
  (cl-cc/expand:with-fresh-defstruct-registries
    (ds-expand '(defstruct widget width height))
    (let ((entry (gethash (intern "WIDGET-WIDTH") cl-cc/expand:*accessor-slot-map*)))
      (expect (not (null entry)) :to-be-truthy)
      (expect (car entry) :to-equal 'widget)
      (expect (cdr entry) :to-equal 'width))))

(it-sequential "ds-read-only-accessor-not-registered-for-setf"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp (ds-expand '(defstruct packet (id 0 :read-only t) payload)))
           (slot-specs (fourth (second exp)))
           (id-slot (first slot-specs))
           (payload-slot (second slot-specs))
           (id-accessor (getf (rest id-slot) :reader))
           (payload-accessor (getf (rest payload-slot) :accessor)))
      (expect (null (gethash id-accessor cl-cc/expand:*accessor-slot-map*)) :to-be-truthy)
      (expect (gethash id-accessor cl-cc/expand:*defstruct-read-only-accessor-map*) :to-be-truthy)
      (expect (gethash payload-accessor cl-cc/expand:*accessor-slot-map*) :to-be-truthy))))

(it-sequential "ds-read-only-accessor-setf-signals-error"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp (ds-expand (quote (defstruct packet (id 0 :read-only t) payload))))
           (slot-specs (fourth (second exp)))
           (id-slot (first slot-specs))
           (id-accessor (getf (rest id-slot) :reader)))
      (signals error
        (cl-cc/expand::expand-setf-accessor (list id-accessor (quote packet)) 10)))))

(it-sequential "ds-empty-struct-has-zero-slots"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp (ds-expand '(defstruct empty)))
           (defclass-form (second exp))
           (slot-specs (fourth defclass-form)))
      (expect (length slot-specs) :to-equal 0))))

(it-sequential "ds-deriving-registers-typeclass-instances eq"
  (destructuring-bind (tc-name) (list 'eq)
    (handler-bind ((warning #'muffle-warning))
    (ds-assert-deriving-registers tc-name))))

(it-sequential "ds-deriving-registers-typeclass-instances show"
  (destructuring-bind (tc-name) (list 'show)
    (handler-bind ((warning #'muffle-warning))
    (ds-assert-deriving-registers tc-name))))

(it-sequential "ds-deriving-registers-typeclass-instances ord"
  (destructuring-bind (tc-name) (list 'ord)
    (handler-bind ((warning #'muffle-warning))
    (ds-assert-deriving-registers tc-name))))

;;; ─── %defstruct-extract-boa-parts ────────────────────────────────────────

(it-sequential "ds-extract-boa-parts-normal-only empty"
  (destructuring-bind (boa-args expected) (list nil '(nil . nil))
    (let ((result (cl-cc/expand::%defstruct-extract-boa-parts boa-args)))
    (expect (car result) :to-equal (car expected))
    (expect (cdr result) :to-equal (cdr expected)))))

(it-sequential "ds-extract-boa-parts-normal-only single"
  (destructuring-bind (boa-args expected) (list '(x) '((x) . nil))
    (let ((result (cl-cc/expand::%defstruct-extract-boa-parts boa-args)))
    (expect (car result) :to-equal (car expected))
    (expect (cdr result) :to-equal (cdr expected)))))

(it-sequential "ds-extract-boa-parts-normal-only two"
  (destructuring-bind (boa-args expected) (list '(x y) '((x y) . nil))
    (let ((result (cl-cc/expand::%defstruct-extract-boa-parts boa-args)))
    (expect (car result) :to-equal (car expected))
    (expect (cdr result) :to-equal (cdr expected)))))

(it-sequential "ds-extract-boa-parts-splits-aux-bindings"
  (let ((result (cl-cc/expand::%defstruct-extract-boa-parts '(x y &aux (z 0) (w 1)))))
    (expect (car result) :to-equal '(x y))
    (expect (cdr result) :to-equal '((z 0) (w 1)))))

(it-sequential "ds-extract-boa-parts-promotes-bare-aux-symbol"
  (let ((result (cl-cc/expand::%defstruct-extract-boa-parts '(x &aux z))))
    (expect (car result) :to-equal '(x))
    (expect (cdr result) :to-equal '((z nil)))))

;;; ─── %defstruct-boa-param-names ──────────────────────────────────────────

(it-sequential "ds-boa-param-names-cases simple"
  (destructuring-bind (params expected) (list '(x y z) '(x y z))
    (expect (cl-cc/expand::%defstruct-boa-param-names params) :to-equal expected)))

(it-sequential "ds-boa-param-names-cases skips-keywords"
  (destructuring-bind (params expected) (list '(&optional x &rest y) '(x y))
    (expect (cl-cc/expand::%defstruct-boa-param-names params) :to-equal expected)))

(it-sequential "ds-boa-param-names-cases empty"
  (destructuring-bind (params expected) (list nil nil)
    (expect (cl-cc/expand::%defstruct-boa-param-names params) :to-equal expected)))

;;; ─── :type list / :type vector (FR-546) ─────────────────────────────────

(defun %ds-tree-member (item tree)
  "Return T if ITEM appears anywhere in the nested list TREE."
  (cond ((null tree) nil)
        ((eq tree item) t)
        ((atom tree) nil)
        (t (or (%ds-tree-member item (car tree))
               (%ds-tree-member item (cdr tree))))))

(it-sequential "ds-type-typed-constructor-cases list"
  (destructuring-bind (form expected-ctor) (list '(defstruct (point (:type list)) x y) 'list)
    (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((ctor (first (ds-progn-forms (ds-expand form)))))
      (expect (first ctor) :to-equal 'defun)
      (expect (%ds-tree-member expected-ctor (fourth ctor)) :to-be-truthy)))))

(it-sequential "ds-type-typed-constructor-cases vector"
  (destructuring-bind (form expected-ctor) (list '(defstruct (seg (:type vector)) a b) 'vector)
    (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((ctor (first (ds-progn-forms (ds-expand form)))))
      (expect (first ctor) :to-equal 'defun)
      (expect (%ds-tree-member expected-ctor (fourth ctor)) :to-be-truthy)))))

(it-sequential "ds-type-list-predicate-checks-listp"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct (rect (:type list) (:predicate rect-list-p))
                                width height)))
           (forms (ds-progn-forms exp))
           (pred  (find-if (lambda (f) (and (listp f)
                                            (eq (first f) 'defun)
                                            (eq (second f) 'rect-list-p)))
                           forms)))
      (expect (not (null pred)) :to-be-truthy)
      (expect (%ds-tree-member 'listp pred) :to-be-truthy))))

;;; ─── %defstruct-resolve-slot-values ──────────────────────────────────────

(it-sequential "ds-resolve-slot-values-cases all-bound"
  (destructuring-bind (all-slots bound-names expected) (list '((x 0) (y 0)) '(x y) '(x y))
    (expect (cl-cc/expand::%defstruct-resolve-slot-values all-slots bound-names) :to-equal expected)))

(it-sequential "ds-resolve-slot-values-cases none-bound"
  (destructuring-bind (all-slots bound-names expected) (list '((x 0) (y 1)) '() '(0 1))
    (expect (cl-cc/expand::%defstruct-resolve-slot-values all-slots bound-names) :to-equal expected)))

(it-sequential "ds-resolve-slot-values-cases partial"
  (destructuring-bind (all-slots bound-names expected) (list '((x 0) (y 1)) '(x) '(x 1))
    (expect (cl-cc/expand::%defstruct-resolve-slot-values all-slots bound-names) :to-equal expected)))

;;; ─── %defstruct-build-constructor ────────────────────────────────────────

(it-sequential "ds-basic-expansion-generates-copier"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct point x y)))
           (forms (ds-progn-forms exp))
           (copier (find-if (lambda (f) (and (listp f)
                                              (eq (first f) 'defun)
                                              (eq (second f) (intern "COPY-POINT"))))
                            forms)))
      (expect (not (null copier)) :to-be-truthy))))

(it-sequential "ds-copier-nil-suppresses-copier"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct (point (:copier nil)) x y)))
           (forms (ds-progn-forms exp))
           (copier (find-if (lambda (f) (and (listp f)
                                              (eq (first f) 'defun)
                                              (string= (symbol-name (second f)) "COPY-POINT")))
                            forms)))
      (expect (null copier) :to-be-truthy))))

(it-sequential "ds-copier-custom-name"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct (point (:copier clone-point)) x y)))
           (forms (ds-progn-forms exp))
           (copier (find-if (lambda (f) (and (listp f)
                                               (eq (first f) 'defun)
                                               (eq (second f) 'clone-point)))
                             forms)))
      (expect (not (null copier)) :to-be-truthy))))

(it-sequential "ds-print-object-generates-print-object-method"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct (point (:print-object print-point)) x y)))
           (forms (ds-progn-forms exp))
           (method (find-if (lambda (f) (and (listp f)
                                             (eq (first f) 'defmethod)
                                             (eq (second f) 'print-object)))
                            forms))
           (body (fourth method)))
      (expect (not (null method)) :to-be-truthy)
      (expect (first body) :to-equal 'funcall)
      (expect (second body) :to-equal '(function print-point))
      (expect (length body) :to-equal 4))))

(it-sequential "ds-print-function-generates-print-object-method"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((exp   (ds-expand '(defstruct (point (:print-function print-point)) x y)))
           (forms (ds-progn-forms exp))
           (method (find-if (lambda (f) (and (listp f)
                                             (eq (first f) 'defmethod)
                                             (eq (second f) 'print-object)))
                            forms))
           (body (fourth method)))
      (expect (not (null method)) :to-be-truthy)
      (expect (first body) :to-equal 'funcall)
      (expect (second body) :to-equal '(function print-point))
      (expect (fifth body) :to-equal 0))))

(it-sequential "ds-build-constructor-keyword-form"
  (let* ((slots '((x 0) (y 1)))
         (result (cl-cc/expand::%defstruct-build-constructor
                  'make-pt nil slots
                  (lambda (svs) (cons 'list svs)))))
    (expect (first result) :to-equal 'defun)
    (expect (second result) :to-equal 'make-pt)
    (expect (third result) :to-equal '(&key (x 0) (y 1)))
    (expect (fourth result) :to-equal '(list x y))))

(it-sequential "ds-build-constructor-boa-form"
  (let* ((slots '((x 0) (y 0)))
         (result (cl-cc/expand::%defstruct-build-constructor
                  'make-pt '(a b) slots
                  (lambda (svs) (cons 'list svs)))))
    (expect (first result) :to-equal 'defun)
    (expect (second result) :to-equal 'make-pt)
    (expect (third result) :to-equal '(a b))
    (expect (car (fourth result)) :to-equal 'let*)))
