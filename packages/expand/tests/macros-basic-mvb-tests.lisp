;;;; tests/unit/expand/macros-basic-mvb-tests.lisp
;;;; Coverage for src/expand/macros-basic.lisp:
;;;;   psetq, multiple-value-bind, multiple-value-setq, multiple-value-list

(in-package :cl-cc/test)



;;; ─── psetq ────────────────────────────────────────────────────────────────

(it-sequential "psetq-expansion-structure"
  (let* ((exp  (our-macroexpand-1 '(psetq x 1 y 2)))
         (body (cddr exp)))
    (expect (car exp) :to-be 'let)
    (expect (= 2 (length (second exp))) :to-be-truthy)
    (expect (some (lambda (f) (and (consp f) (eq 'setq (car f)))) body) :to-be-truthy)
    (expect (car (last body)) :to-be-null)))

(it-sequential "psetq-empty-is-nil"
  (expect (our-macroexpand-1 '(psetq)) :to-be-null))

(it-sequential "psetq-runtime-parallel-behavior swap-two"
  (destructuring-bind (expected form) (list '(2 1) "(let ((a 1) (b 2)) (psetq a b b a) (list a b))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "psetq-runtime-parallel-behavior rotate-three"
  (destructuring-bind (expected form) (list '(3 1 2) "(let ((x 1) (y 2) (z 3)) (psetq x z y x z y) (list x y z))")
    (expect (run-string form) :to-equal expected)))

;;; ─── multiple-value-bind ──────────────────────────────────────────────────

(it-sequential "mvb-expansion-structure"
  (let* ((exp    (our-macroexpand-1 '(multiple-value-bind (x y) (values 1 2) x))))
    (expect (member (car exp) '(let let*)) :to-be-truthy)))

(it-sequential "mvb-runtime two-values"
  (destructuring-bind (expected form) (list 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "mvb-runtime single-value"
  (destructuring-bind (expected form) (list 42 "(multiple-value-bind (x) (values 42) x)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "mvb-runtime three-values"
  (destructuring-bind (expected form) (list 10 "(multiple-value-bind (a b c) (values 1 3 6) (+ a b c))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; ─── multiple-value-setq ──────────────────────────────────────────────────

(it-sequential "mvsq-expansion-structure"
  (let* ((exp  (our-macroexpand-1 '(multiple-value-setq (x y) (values 1 2))))
         (body (cddr exp)))
    (expect (car exp) :to-be 'let)
    (expect (= 2 (length (remove-if-not (lambda (f) (and (consp f) (eq 'setq (car f)))) body))) :to-be-truthy)))

(it-sequential "mvsq-runtime-behavior sets-vars"
  (destructuring-bind (expected form) (list '(10 20) "(let ((a 0) (b 0)) (multiple-value-setq (a b) (values 10 20)) (list a b))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "mvsq-runtime-behavior returns-first"
  (destructuring-bind (expected form) (list 10 "(let ((a 0) (b 0)) (multiple-value-setq (a b) (values 10 20)))")
    (expect (run-string form) :to-equal expected)))

;;; ─── multiple-value-list ──────────────────────────────────────────────────

(it-sequential "mvl-expansion-structure"
  (let* ((exp  (our-macroexpand-1 '(multiple-value-list (values 1 2 3))))
         )
    (expect (member (car exp) '(list let)) :to-be-truthy)))

(it-sequential "mvl-runtime three-values"
  (destructuring-bind (expected form) (list '(1 2 3) "(multiple-value-list (values 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "mvl-runtime single-value"
  (destructuring-bind (expected form) (list '(42) "(multiple-value-list (values 42))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "mvl-runtime with-floor"
  (destructuring-bind (expected form) (list '(5 2) "(multiple-value-list (floor 17 3))")
    (expect (run-string form) :to-equal expected)))
