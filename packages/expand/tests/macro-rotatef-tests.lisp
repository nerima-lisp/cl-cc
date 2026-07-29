;;;; tests/unit/expand/macro-rotatef-tests.lisp — Rotatef macro tests

(in-package :cl-cc/test)



(it-sequential "rotatef-two-var-structure"
  (let ((result (our-macroexpand-1 '(rotatef x y))))
    (expect 'let :to-be (car result))
    (expect (= (length (cadr result)) 1) :to-be-truthy)
    (let* ((binding (caadr result))
           (tmp-var (first binding))
           (tmp-val (second binding)))
      (expect 'x :to-be tmp-val)
      (expect 'setq :to-be (car (caddr result)))
      (expect 'x :to-be (cadr (caddr result)))
      (expect 'y :to-be (caddr (caddr result)))
      (expect 'setq :to-be (car (cadddr result)))
      (expect 'y :to-be (cadr (cadddr result)))
      (expect tmp-var :to-be (caddr (cadddr result)))
      (expect (car (last result)) :to-be-null)))
  (let ((result (our-macroexpand-1 '(rotatef a b))))
    (expect (car (last result)) :to-be-null)))

(it-sequential "rotatef-single-var-returns-nil"
  (expect (our-macroexpand-1 '(rotatef x)) :to-be-null))

(it-sequential "rotatef-three-var-structure"
  (let ((result (our-macroexpand-1 '(rotatef x y z))))
    (expect 'let :to-be (car result))
    (expect (car (last result)) :to-be-null)))

(it-sequential "rotatef-preserves-places"
  (let ((result (our-macroexpand-1 '(rotatef foo bar))))
    (expect 'foo :to-be (cadr (caadr result)))
    (expect 'foo :to-be (cadr (caddr result)))
    (expect 'bar :to-be (caddr (caddr result)))
    (expect 'bar :to-be (cadr (cadddr result)))))

(it-sequential "integration-rotatef-full-expansion"
  (let ((result (our-macroexpand '(rotatef p q))))
    (expect (search "rotatef" (string-downcase (format nil "~S" result))) :to-be-falsy)))
