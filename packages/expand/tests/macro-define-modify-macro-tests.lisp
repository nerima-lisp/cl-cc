;;;; tests/unit/expand/macro-define-modify-macro-tests.lisp — DEFINE-MODIFY-MACRO tests

(in-package :cl-cc/test)




(it-sequential "define-modify-macro-lambda-list-has-place-first"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-push (item) cons)))
         (params (caddr result)))
    (expect 'cl-cc:our-defmacro :to-be (car result))
    (expect (= (length params) 2) :to-be-truthy)))

(it-sequential "define-modify-macro-no-extra-args"
  (let ((result (our-macroexpand-1 '(define-modify-macro toggle-flag () not))))
    (expect 'cl-cc:our-defmacro :to-be (car result))
    (expect 'toggle-flag :to-be (cadr result))
    (expect (= (length (caddr result)) 1) :to-be-truthy)))

(it-sequential "define-modify-macro-body-contains-setf"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-incf (n) +)))
         (body (cadddr result)))
    (expect (not (null body)) :to-be-truthy)))

(it-sequential "define-modify-macro-outer-form-shape plain"
  (destructuring-bind (expected-name form) (list 'my-incf '(define-modify-macro my-incf (n) +))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'cl-cc:our-defmacro)
    (expect (cadr result) :to-be expected-name))))

(it-sequential "define-modify-macro-outer-form-shape optional"
  (destructuring-bind (expected-name form) (list 'my-add '(define-modify-macro my-add (&optional (n 1)) +))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'cl-cc:our-defmacro)
    (expect (cadr result) :to-be expected-name))))

(it-sequential "define-modify-macro-outer-form-shape docstring"
  (destructuring-bind (expected-name form) (list 'my-mul '(define-modify-macro my-mul (factor) * "Multiply."))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'cl-cc:our-defmacro)
    (expect (cadr result) :to-be expected-name))))

(it-sequential "define-modify-macro-rest-lambda-list"
  (let* ((result (our-macroexpand-1 '(define-modify-macro my-append (&rest items) append)))
         (params (caddr result)))
    (expect (car result) :to-be 'cl-cc:our-defmacro)
    (expect (member '&rest params) :to-be-truthy)))
