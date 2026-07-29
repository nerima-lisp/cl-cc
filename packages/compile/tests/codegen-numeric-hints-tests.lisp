;;;; packages/compile/tests/codegen-numeric-hints-tests.lisp
;;;; Unit tests for FR-860/FR-861 numeric compile-time helpers
;;;; (packages/compile/src/codegen-numeric-hints.lisp).

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; %codegen-normalize-contagion-type — alist lookup + complex special case
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "normalize-contagion-type-cases fixnum"
  (destructuring-bind (input expected) (list 'fixnum 'integer)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases bignum"
  (destructuring-bind (input expected) (list 'bignum 'integer)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases integer"
  (destructuring-bind (input expected) (list 'integer 'integer)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases ratio"
  (destructuring-bind (input expected) (list 'ratio 'rational)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases rational"
  (destructuring-bind (input expected) (list 'rational 'rational)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases single-float"
  (destructuring-bind (input expected) (list 'single-float 'single-float)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases double-float"
  (destructuring-bind (input expected) (list 'double-float 'double-float)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases float"
  (destructuring-bind (input expected) (list 'float 'double-float)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases complex"
  (destructuring-bind (input expected) (list 'complex 'complex)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases complex-cons"
  (destructuring-bind (input expected) (list '(complex integer) 'complex)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

(it-sequential "normalize-contagion-type-cases unknown"
  (destructuring-bind (input expected) (list 'unknown nil)
    (expect (cl-cc/compile::%codegen-normalize-contagion-type input) :to-equal expected)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; codegen-infer-numeric-contagion-type — two-argument contagion
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "infer-numeric-contagion-type-cases fixnum*fixnum"
  (destructuring-bind (left right expected) (list 'fixnum 'fixnum 'integer)
    (expect (cl-cc/compile::codegen-infer-numeric-contagion-type left right) :to-equal expected)))

(it-sequential "infer-numeric-contagion-type-cases fixnum*single-float"
  (destructuring-bind (left right expected) (list 'fixnum 'single-float 'single-float)
    (expect (cl-cc/compile::codegen-infer-numeric-contagion-type left right) :to-equal expected)))

(it-sequential "infer-numeric-contagion-type-cases double-float*complex"
  (destructuring-bind (left right expected) (list 'double-float 'complex 'complex)
    (expect (cl-cc/compile::codegen-infer-numeric-contagion-type left right) :to-equal expected)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; codegen-inline-arith-dispatch-index — returns non-nil for valid types
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "codegen-inline-arith-dispatch-index-returns-non-nil"
  (let ((index (cl-cc/compile::codegen-inline-arith-dispatch-index '+ 'fixnum 'fixnum)))
    (expect (integerp index) :to-be-truthy)))
