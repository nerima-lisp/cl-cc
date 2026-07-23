;;;; tests/unit/expand/expander-defstruct-typed-tests.lisp
;;;; Coverage for expander-defstruct-typed.lisp helpers.

(in-package :cl-cc/test)



;;; ── %defstruct-typed-container-form ─────────────────────────────────────

(it-sequential "typed-container-form-dispatcher list"
  (destructuring-bind (struct-type name slot-values expected-head) (list 'list 'my-struct '(a b) 'list)
    (let ((form (cl-cc/expand::%defstruct-typed-container-form struct-type name slot-values)))
    (expect (first form) :to-be expected-head)
    (expect (second form) :to-equal (list 'quote name))
    (expect (cddr form) :to-equal slot-values))))

(it-sequential "typed-container-form-dispatcher vector"
  (destructuring-bind (struct-type name slot-values expected-head) (list 'vector 'my-struct '(a b) 'vector)
    (let ((form (cl-cc/expand::%defstruct-typed-container-form struct-type name slot-values)))
    (expect (first form) :to-be expected-head)
    (expect (second form) :to-equal (list 'quote name))
    (expect (cddr form) :to-equal slot-values))))

;;; ── %defstruct-typed-constructor ────────────────────────────────────────

(it-sequential "typed-constructor-is-defun list"
  (destructuring-bind (struct-type) (list 'list)
    (cl-cc/expand:with-fresh-defstruct-registries
    (let ((form (cl-cc/expand::%defstruct-typed-constructor
                 'make-pt 'pt struct-type nil '((x 0) (y 0)))))
      (expect (first form) :to-be 'defun)
      (expect (second form) :to-be 'make-pt)))))

(it-sequential "typed-constructor-is-defun vector"
  (destructuring-bind (struct-type) (list 'vector)
    (cl-cc/expand:with-fresh-defstruct-registries
    (let ((form (cl-cc/expand::%defstruct-typed-constructor
                 'make-pt 'pt struct-type nil '((x 0) (y 0)))))
      (expect (first form) :to-be 'defun)
      (expect (second form) :to-be 'make-pt)))))

(it-sequential "typed-constructor-list-body-uses-list"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((form (cl-cc/expand::%defstruct-typed-constructor
                  'make-pt 'pt 'list nil '((x 0) (y 0))))
           (body (fourth form)))
      (expect (member 'list body) :to-be-truthy))))

(it-sequential "typed-constructor-vector-body-uses-vector"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((form (cl-cc/expand::%defstruct-typed-constructor
                  'make-seg 'seg 'vector nil '((a 0) (b 0))))
           (body (fourth form)))
      (expect (member 'vector body) :to-be-truthy))))

;;; ── %defstruct-typed-accessors ──────────────────────────────────────────

(it-sequential "typed-accessors-list-uses-nth"
  (let ((forms (cl-cc/expand::%defstruct-typed-accessors 'list 'pt- '((x 0) (y 0)))))
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (first (first forms)) :to-be 'defun)
    (expect (member 'nth (fourth (first forms))) :to-be-truthy)))

(it-sequential "typed-accessors-vector-uses-aref"
  (let ((forms (cl-cc/expand::%defstruct-typed-accessors 'vector 'pt- '((x 0) (y 0)))))
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (member 'aref (fourth (first forms))) :to-be-truthy)))

(it-sequential "typed-accessors-count-matches-slots"
  (let ((forms (cl-cc/expand::%defstruct-typed-accessors 'list 'r- '((a 0) (b 0) (c 0)))))
    (expect (= 3 (length forms)) :to-be-truthy)))

;;; ── %defstruct-typed-predicate ──────────────────────────────────────────

(it-sequential "typed-predicate-list-uses-listp"
  (let* ((form (cl-cc/expand::%defstruct-typed-predicate 'pt-p 'pt 'list 2))
         (body (fourth form)))
    (expect (first form) :to-be 'defun)
    (expect (second form) :to-be 'pt-p)
    (expect (first (second body)) :to-be 'listp)))

(it-sequential "typed-predicate-vector-uses-vectorp"
  (let* ((form (cl-cc/expand::%defstruct-typed-predicate 'seg-p 'seg 'vector 2))
         (body (fourth form)))
    (expect (first form) :to-be 'defun)
    (expect (first (second body)) :to-be 'vectorp)))

(it-sequential "typed-predicate-nil-name-returns-nil"
  (expect (cl-cc/expand::%defstruct-typed-predicate nil 'pt 'list 2) :to-be nil))

;;; ── %defstruct-typed-expansion-forms ────────────────────────────────────

(it-sequential "typed-expansion-forms-includes-quote-name"
  (let ((forms (cl-cc/expand::%defstruct-typed-expansion-forms
                'pt nil '() nil nil)))
    (expect (car (last forms)) :to-equal '(quote pt))))

(it-sequential "typed-expansion-forms-ctor-is-first-when-present"
  (let* ((ctor  '(defun make-pt (&key x y) (list 'pt x y)))
          (forms (cl-cc/expand::%defstruct-typed-expansion-forms 'pt ctor '() nil nil)))
    (expect (first forms) :to-equal ctor)))

(it-sequential "typed-register-accessors-adds-setf-handler"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((slots '((x 0 nil nil)))
           (accessor (cl-cc/expand::%defstruct-accessor-name 'pt- 'x)))
      (cl-cc/expand::%defstruct-register-typed-accessors 'vector 'pt- slots)
      (expect (gethash accessor cl-cc/expand::*setf-compound-place-handlers*) :to-be-truthy))))

(it-sequential "typed-register-accessors-read-only-map"
  (cl-cc/expand:with-fresh-defstruct-registries
    (let* ((slots '((x 0 t nil)))
           (accessor (cl-cc/expand::%defstruct-accessor-name 'pt- 'x)))
      (cl-cc/expand::%defstruct-register-typed-accessors 'list 'pt- slots)
      (expect (gethash accessor cl-cc/expand:*defstruct-read-only-accessor-map*) :to-be-truthy))))

(it-sequential "typed-expansion-forms-pred-precedes-quote"
  (let* ((pred  '(defun pt-p (obj) (listp obj)))
         (forms (cl-cc/expand::%defstruct-typed-expansion-forms 'pt nil '() pred nil)))
    (expect (first (last forms 2)) :to-equal pred)))

;;; ── %defstruct-typed-expansion (integration) ────────────────────────────

(it-sequential "typed-expansion-integration list"
  (destructuring-bind (form) (list '(defstruct (point (:type list)) x y))
    (cl-cc/expand:with-fresh-defstruct-registries
    (let ((exp (cl-cc/expand::expand-defstruct form)))
      (expect (first exp) :to-be 'progn)
      (expect (> (length (rest exp)) 1) :to-be-truthy)))))

(it-sequential "typed-expansion-integration vector"
  (destructuring-bind (form) (list '(defstruct (point (:type vector)) x y))
    (cl-cc/expand:with-fresh-defstruct-registries
    (let ((exp (cl-cc/expand::expand-defstruct form)))
      (expect (first exp) :to-be 'progn)
      (expect (> (length (rest exp)) 1) :to-be-truthy)))))
