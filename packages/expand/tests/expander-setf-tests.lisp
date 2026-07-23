;;;; tests/unit/expand/expander-setf-tests.lisp — Setf expander tests

(in-package :cl-cc/test)



;;; ─── setf expansions ────────────────────────────────────────────────────────

(it-sequential "expander-setf-multi-var-progn"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(setf x 1 y 2))))
    (expect (car result) :to-be 'progn)
    (expect (car (second result)) :to-be 'setq)
    (expect (car (third result)) :to-be 'setq)))

(it-sequential "expander-setf-plain-var-to-setq"
  (let ((result (assert-expansion-head '(setf x 42) 'setq)))
    (expect (second result) :to-be 'x)
    (expect (third result) :to-equal 42)))

(it-sequential "expander-setf-aref-to-aset"
  (let ((result (assert-expansion-head '(setf (aref v i) val) 'cl-cc/expand::aset)))
    (expect (second result) :to-be 'v)
    (expect (third result) :to-be 'i)
    (expect (fourth result) :to-be 'val)))

(it-sequential "expander-setf-multi-place-dispatches-each"
  (let ((result (assert-expansion-head '(setf a 1 b 2) 'progn)))
    (expect (= 2 (length (cdr result))) :to-be-truthy)))
