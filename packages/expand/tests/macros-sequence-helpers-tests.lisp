;;;; tests/unit/expand/macros-sequence-helpers-tests.lisp
;;;; Coverage tests for src/expand/macros-sequence-helpers.lisp

(in-package :cl-cc/test)



;;; ── SUBST-IF / SUBST-IF-NOT ────────────────────────────────────────────────

(it-sequential "macros-sequence-helpers-let-body-structure subst-if"
  (destructuring-bind (form inner-op) (list '(subst-if new pred tree) 'labels)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (car (caddr result)) :to-be inner-op))))

(it-sequential "macros-sequence-helpers-let-body-structure member-if"
  (destructuring-bind (form inner-op) (list '(member-if pred lst) 'do)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (car (caddr result)) :to-be inner-op))))

(it-sequential "macros-sequence-helpers-let-body-structure maphash"
  (destructuring-bind (form inner-op) (list '(maphash fn table) 'dolist)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (car (caddr result)) :to-be inner-op))))

(it-sequential "macros-sequence-helpers-complement-delegation subst-if-not"
  (destructuring-bind (form outer-op complement-extractor) (list '(subst-if-not new pred tree) 'subst-if #'caddr)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be outer-op)
    (expect (car (funcall complement-extractor result)) :to-be 'complement))))

(it-sequential "macros-sequence-helpers-complement-delegation member-if-not"
  (destructuring-bind (form outer-op complement-extractor) (list '(member-if-not pred lst) 'member-if #'cadr)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be outer-op)
    (expect (car (funcall complement-extractor result)) :to-be 'complement))))

;;; ── VECTOR ──────────────────────────────────────────────────────────────────

(it-sequential "vector-expansion"
  (let ((empty-result (our-macroexpand-1 '(vector))))
    (expect '(make-array 0) :to-equal empty-result))
  (let ((result (our-macroexpand-1 '(vector a b c))))
    (expect (car result) :to-be 'let)
    (expect (car (cadr (car (second result)))) :to-be 'make-array)))

;;; ── MEMBER-IF / MEMBER-IF-NOT ───────────────────────────────────────────────

;;; ── MAPHASH ─────────────────────────────────────────────────────────────────
