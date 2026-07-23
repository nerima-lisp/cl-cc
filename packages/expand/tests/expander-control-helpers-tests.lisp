;;;; tests/unit/expand/expander-control-helpers-tests.lisp — Control helper tests

(in-package :cl-cc/test)



(it-sequential "expand-let-binding-expands-value"
  (expect (cl-cc/expand::expand-let-binding '(x (+ 1 2))) :to-equal '(x (+ 1 2))))

(it-sequential "expand-flet-labels-binding-expands-body"
  (let ((result (cl-cc/expand::expand-flet-labels-binding '(foo (x) (1+ x) (1- x)))))
    (expect (first result) :to-be 'foo)
    (expect (second result) :to-equal '(x))
    (expect (cddr result) :to-equal '((+ x 1) (- x 1)))))

;;; ─── %any-destructuring-let-binding-p ────────────────────────────────────

(it-sequential "any-destructuring-binding-detects-cons-name"
  (expect (cl-cc/expand::%any-destructuring-let-binding-p '(((a b) pair) (x 1))) :to-be-truthy)
  (expect (cl-cc/expand::%any-destructuring-let-binding-p '((x 1) (y 2))) :to-be-falsy))

;;; ─── %expand-handler-case-form ────────────────────────────────────────────

(it-sequential "handler-case-without-no-error-passes-through"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(handler-case expr (error (e) e)))))
    (expect (car result) :to-be 'handler-case)))

(it-sequential "handler-case-no-error-wraps-in-block"
  (let* ((result (cl-cc/expand:compiler-macroexpand-all
                  '(handler-case (ok-fn)
                     (error (e) :bad)
                     (:no-error (v) v))))
         (outer (car result)))
    (expect outer :to-be 'block)))

(it-sequential "handler-case-no-error-error-clauses-return-from-block"
  (let* ((result (cl-cc/expand:compiler-macroexpand-all
                  '(handler-case (ok-fn)
                     (error (e) :bad)
                     (:no-error (v) v))))
         (inner-let   (third result))
         (handler-form (second (first (second inner-let))))
         (error-clause (third handler-form)))
    (expect (car handler-form) :to-be 'handler-case)
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'return-from)))
                       (cddr error-clause)) :to-be-truthy)))

;;; ─── %expand-let-form (destructuring) ────────────────────────────────────

(it-sequential "let-form-with-destructuring-binding-wraps-in-destructuring-bind"
  (let* ((result (cl-cc/expand:compiler-macroexpand-all '(let (((a b) some-pair) (x 1)) body)))
         (has-db (labels ((scan (form)
                            (and (consp form)
                                 (or (eq (car form) 'destructuring-bind)
                                     (some #'scan (cdr form))))))
                   (scan result))))
    (expect has-db :to-be-truthy)))

(it-sequential "let-form-with-empty-bindings-becomes-progn"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(let () body1 body2))))
    (expect (car result) :to-be 'progn)))
