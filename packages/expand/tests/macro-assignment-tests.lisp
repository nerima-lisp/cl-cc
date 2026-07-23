;;;; tests/unit/expand/macro-assignment-tests.lisp — Macro assignment tests

(in-package :cl-cc/test)



(it-sequential "setf-macro-simple-symbol"
  (expect '(setq x 10) :to-equal (our-macroexpand-1 '(setf x 10))))

(it-sequential "setf-macro-car-place-expands"
  ;; (car x) is a valid CL place; the setf macro expands it (no longer an error).
  (destructuring-bind (form) (list '(setf (car x)  10))
    (expect (our-macroexpand-1 form) :to-be-truthy)))

(it-sequential "setf-macro-accessor-place-falls-back-to-slot-set"
  ;; Unknown accessor places lower through the generic RT-SLOT-SET fallback
  ;; (struct-slot semantics) rather than signaling.
  (destructuring-bind (form) (list '(setf (cons x) 10))
    (expect (our-macroexpand-1 form) :to-be-truthy)))


(it-sequential "psetq-macro-full-expansion"
  (let ((result (our-macroexpand '(psetq a 1 b 2))))
    ;; Expansion: (let ((#:A 1) (#:B 2)) (setq a #:A) (setq b #:B) nil)
    (expect 'let :to-be (car result))
    (expect (consp (cadr result)) :to-be-truthy)
    ;; Body forms are SETQ (not wrapped in PROGN)
    (expect 'setq :to-be (car (caddr result)))))

(it-sequential "psetq-macro-empty-returns-nil"
  (expect (our-macroexpand-1 '(psetq)) :to-be-null))

(it-sequential "psetq-macro-nonzero-produces-let one-pair"
  (destructuring-bind (form) (list '(psetq a 1))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (consp (cadr result)) :to-be-truthy))))

(it-sequential "psetq-macro-nonzero-produces-let three-pair"
  (destructuring-bind (form) (list '(psetq a 1 b 2 c 3))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (consp (cadr result)) :to-be-truthy))))
