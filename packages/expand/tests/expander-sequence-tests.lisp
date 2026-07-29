;;;; tests/unit/expand/expander-sequence-tests.lisp — Sequence expander tests

(in-package :cl-cc/test)



(it-sequential "expander-sequence-predicates-expand-to-let mapcar"
  (destructuring-bind (form) (list '(mapcar #'1+ '(1 2)))
    (expect (car (cl-cc/expand:compiler-macroexpand-all form)) :to-be 'let)))

(it-sequential "expander-sequence-predicates-expand-to-let every"
  (destructuring-bind (form) (list '(every #'evenp '(1 2 3)))
    (expect (car (cl-cc/expand:compiler-macroexpand-all form)) :to-be 'let)))

(it-sequential "expander-sequence-predicates-expand-to-let some"
  (destructuring-bind (form) (list '(some #'oddp '(1 2 3)))
    (expect (car (cl-cc/expand:compiler-macroexpand-all form)) :to-be 'let)))

;;; ── %sequence-build-null-test ─────────────────────────────────────────────

(it-sequential "sequence-build-null-test-forms single"
  (destructuring-bind (vars expected) (list '(l1) '(or (null l1)))
    (expect (cl-cc/expand::%sequence-build-null-test vars) :to-equal expected)))

(it-sequential "sequence-build-null-test-forms two"
  (destructuring-bind (vars expected) (list '(l1 l2) '(or (null l1) (null l2)))
    (expect (cl-cc/expand::%sequence-build-null-test vars) :to-equal expected)))

(it-sequential "sequence-build-null-test-forms three"
  (destructuring-bind (vars expected) (list '(l1 l2 l3) '(or (null l1) (null l2) (null l3)))
    (expect (cl-cc/expand::%sequence-build-null-test vars) :to-equal expected)))

;;; ── %sequence-build-car-args ──────────────────────────────────────────────

(it-sequential "sequence-build-car-args-forms single"
  (destructuring-bind (vars expected) (list '(l1) '((car l1)))
    (expect (cl-cc/expand::%sequence-build-car-args vars) :to-equal expected)))

(it-sequential "sequence-build-car-args-forms two"
  (destructuring-bind (vars expected) (list '(l1 l2) '((car l1) (car l2)))
    (expect (cl-cc/expand::%sequence-build-car-args vars) :to-equal expected)))

;;; ── %sequence-build-cdr-args ──────────────────────────────────────────────

(it-sequential "sequence-build-cdr-args-forms single"
  (destructuring-bind (vars expected) (list '(l1) '((cdr l1)))
    (expect (cl-cc/expand::%sequence-build-cdr-args vars) :to-equal expected)))

(it-sequential "sequence-build-cdr-args-forms two"
  (destructuring-bind (vars expected) (list '(l1 l2) '((cdr l1) (cdr l2)))
    (expect (cl-cc/expand::%sequence-build-cdr-args vars) :to-equal expected)))

;;; ── %sequence-build-single-map-form ──────────────────────────────────────

(it-sequential "sequence-build-single-map-collect-shape"
  (let ((form (cl-cc/expand::%sequence-build-single-map-form :collect '#'1+ 'xs)))
    (expect (first form) :to-be 'let)
    (expect (first (third form)) :to-be 'labels)))

(it-sequential "sequence-build-single-map-side-effect-shape"
  (let ((form (cl-cc/expand::%sequence-build-single-map-form :side-effect '#'print 'xs)))
    (expect (first form) :to-be 'let)
    (expect (first (third form)) :to-be 'labels)))

(it-sequential "sequence-build-single-map-flatmap-shape"
  (let* ((form (cl-cc/expand::%sequence-build-single-map-form :flatmap '#'list 'xs))
         (labels-body (first (second (third form))))
         (rec-body    (third labels-body)))
    (expect (first form) :to-be 'let)
    (expect (first (fourth rec-body)) :to-be 'nconc)))

;;; ── %sequence-build-multi-map-form ───────────────────────────────────────

(it-sequential "sequence-build-multi-map-collect-shape"
  (let ((form (cl-cc/expand::%sequence-build-multi-map-form :collect '#'+ '(l1 l2))))
    (expect (first form) :to-be 'let)
    (expect (first (third form)) :to-be 'labels)))

(it-sequential "sequence-build-multi-map-side-effect-shape"
  (let* ((form (cl-cc/expand::%sequence-build-multi-map-form :side-effect '#'print '(l1)))
         (helper-body (third (first (second (third form))))))
    (expect (first form) :to-be 'let)
    (expect (first helper-body) :to-be 'if)))

;;; ── %sequence-build-single-quantifier-form ───────────────────────────────

(it-sequential "sequence-build-single-quantifier-every-shape"
  (let* ((form (cl-cc/expand::%sequence-build-single-quantifier-form :every '#'evenp 'xs))
         (block-form (third form))
         (dolist-form (third block-form)))
    (expect (first form) :to-be 'let)
    (expect (first block-form) :to-be 'block)
    (expect (first dolist-form) :to-be 'dolist)))

(it-sequential "sequence-build-single-quantifier-some-shape"
  (let* ((form (cl-cc/expand::%sequence-build-single-quantifier-form :some '#'oddp 'xs))
         (block-form (third form))
         (dolist-form (third block-form)))
    (expect (first form) :to-be 'let)
    (expect (first block-form) :to-be 'block)
    (expect (first dolist-form) :to-be 'dolist)))

;;; ── %sequence-build-multi-quantifier-form ────────────────────────────────

(it-sequential "sequence-build-multi-quantifier-every-shape"
  (let ((form (cl-cc/expand::%sequence-build-multi-quantifier-form :every '#'= '(l1 l2))))
    (expect (first form) :to-be 'let)
    (expect (first (third form)) :to-be 'labels)))

(it-sequential "sequence-build-multi-quantifier-some-shape"
  (let ((form (cl-cc/expand::%sequence-build-multi-quantifier-form :some '#'= '(l1 l2))))
    (expect (first form) :to-be 'let)
    (expect (first (third form)) :to-be 'labels)))
