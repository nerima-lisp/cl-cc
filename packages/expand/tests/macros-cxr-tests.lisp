;;;; tests/unit/expand/macros-cxr-tests.lisp — CXR accessor macro tests

(in-package :cl-cc/test)



(it-sequential "macros-cxr-expansion cadr"
  (destructuring-bind (form expected) (list '(cadr x) '(car (cdr x)))
    (expect (our-macroexpand-1 form) :to-equal expected)))

(it-sequential "macros-cxr-expansion caddr"
  (destructuring-bind (form expected) (list '(caddr x) '(car (cdr (cdr x))))
    (expect (our-macroexpand-1 form) :to-equal expected)))

(it-sequential "macros-cxr-expansion cdddr"
  (destructuring-bind (form expected) (list '(cdddr x) '(cdr (cdr (cdr x))))
    (expect (our-macroexpand-1 form) :to-equal expected)))

;;; ─── %expand-cxr (extracted helper) ──────────────────────────────────────

(it-sequential "expand-cxr-helper-cases caar"
  (destructuring-bind (sym arg expected) (list 'caar 'x '(car (car x)))
    (expect (cl-cc/expand::%expand-cxr sym arg) :to-equal expected)))

(it-sequential "expand-cxr-helper-cases cadr"
  (destructuring-bind (sym arg expected) (list 'cadr 'x '(car (cdr x)))
    (expect (cl-cc/expand::%expand-cxr sym arg) :to-equal expected)))

(it-sequential "expand-cxr-helper-cases caddr"
  (destructuring-bind (sym arg expected) (list 'caddr 'x '(car (cdr (cdr x))))
    (expect (cl-cc/expand::%expand-cxr sym arg) :to-equal expected)))

(it-sequential "expand-cxr-helper-cases cadddr"
  (destructuring-bind (sym arg expected) (list 'cadddr 'x '(car (cdr (cdr (cdr x)))))
    (expect (cl-cc/expand::%expand-cxr sym arg) :to-equal expected)))

(it-sequential "expand-cxr-helper-cases cddr"
  (destructuring-bind (sym arg expected) (list 'cddr 'x '(cdr (cdr x)))
    (expect (cl-cc/expand::%expand-cxr sym arg) :to-equal expected)))
