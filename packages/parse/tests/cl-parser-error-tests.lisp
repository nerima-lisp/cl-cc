;;;; tests/unit/parse/cl-parser-error-tests.lisp — CL parser lower-sexp special forms and error tests
;;;;
;;;; Tests: lower-sexp-to-ast special forms, binding forms, function forms,
;;;; type-checking forms, and error/arity signalling cases.
;;;; Requires helpers (parse-one, parse-many, lower) from cl-parser-tests.lisp (loaded first).

(in-package :cl-cc/test)


;;; ─── lower-sexp-to-ast: special forms ───────────────────────────────────────

(it-sequential "lower-control-flow-forms if-with-else"
  (destructuring-bind (form pred) (list '(if x 1 2) #'cl-cc/ast:ast-if-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-control-flow-forms if-without-else"
  (destructuring-bind (form pred) (list '(if x 1) #'cl-cc/ast:ast-if-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-control-flow-forms progn"
  (destructuring-bind (form pred) (list '(progn 1 2 3) #'cl-cc/ast:ast-progn-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-control-flow-forms let"
  (destructuring-bind (form pred) (list '(let ((x 1) (y 2)) x) #'cl-cc/ast:ast-let-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-let-lambda-and-defun-details let-declaration"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(let ((x 1)) (declare (ignore x)) 42))))
               (assert-true (cl-cc/ast:ast-let-p node))
               (assert-equal '((ignore x)) (cl-cc/ast:ast-let-declarations node)))))
    (funcall verify)))

(it-sequential "lower-let-lambda-and-defun-details let-bare-symbol"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(let (x) x))))
               (assert-true (cl-cc/ast:ast-let-p node))
               (let ((binding (first (cl-cc/ast:ast-let-bindings node))))
                 (assert-eq 'x (car binding))
                 (assert-true (cl-cc/ast:ast-quote-p (cdr binding)))))))
    (funcall verify)))

(it-sequential "lower-let-lambda-and-defun-details lambda-params"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(lambda (x y) (+ x y)))))
               (assert-true (cl-cc/ast:ast-lambda-p node))
               (assert-equal '(x y) (cl-cc::ast-lambda-params node))
               (assert-= 1 (length (cl-cc::ast-lambda-body node))))))
    (funcall verify)))

(it-sequential "lower-let-lambda-and-defun-details defun-form"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(defun my-func (a b) (+ a b)))))
               (assert-true (cl-cc/ast:ast-defun-p node))
               (assert-eq 'my-func (cl-cc/ast:ast-defun-name node))
               (assert-equal '(a b) (cl-cc::ast-defun-params node))
               (assert-= 1 (length (cl-cc::ast-defun-body node))))))
    (funcall verify)))

(it-sequential "lower-let-lambda-and-defun-details let-single-element"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(let ((x)) x))))
               (assert-true (cl-cc/ast:ast-let-p node))
               (let ((binding (first (cl-cc/ast:ast-let-bindings node))))
                 (assert-eq 'x (car binding))
                 (assert-true (cl-cc/ast:ast-quote-p (cdr binding)))
                 (assert-null (cl-cc/ast:ast-quote-value (cdr binding)))))))
    (funcall verify)))

(it-sequential "lower-definition-and-binding-forms defvar-with-value"
  (destructuring-bind (form pred) (list '(defvar *x* 42) #'cl-cc/ast:ast-defvar-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-definition-and-binding-forms defvar-no-value"
  (destructuring-bind (form pred) (list '(defvar *x*) #'cl-cc/ast:ast-defvar-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-definition-and-binding-forms setq"
  (destructuring-bind (form pred) (list '(setq x 10) #'cl-cc/ast:ast-setq-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-definition-and-binding-forms setq-multi"
  (destructuring-bind (form pred) (list '(setq a 1 b 2) #'cl-cc/ast:ast-progn-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-definition-and-binding-forms quote"
  (destructuring-bind (form pred) (list '(quote hello) #'cl-cc/ast:ast-quote-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-definition-and-binding-forms block"
  (destructuring-bind (form pred) (list '(block my-block 1 2) #'cl-cc/ast:ast-block-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-definition-and-binding-forms return-from"
  (destructuring-bind (form pred) (list '(return-from my-block 42) #'cl-cc/ast:ast-return-from-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-function-name-forms symbol-ref"
  (destructuring-bind (form expected-name) (list '(function foo) 'foo)
    (let ((node (lower form)))
    (expect (cl-cc/ast:ast-function-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-function-name node) :to-equal expected-name))))

(it-sequential "lower-function-name-forms setf-name"
  (destructuring-bind (form expected-name) (list '(function (setf foo)) '(setf foo))
    (let ((node (lower form)))
    (expect (cl-cc/ast:ast-function-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-function-name node) :to-equal expected-name))))

(it-sequential "lower-function-lambda"
  (let ((node (lower '(function (lambda (x) x)))))
    (expect (cl-cc/ast:ast-lambda-p node) :to-be-truthy)
    (expect (cl-cc::ast-lambda-params node) :to-equal '(x))
    (expect (= 1 (length (cl-cc::ast-lambda-body node))) :to-be-truthy)))

(it-sequential "lower-type-only values"
  (destructuring-bind (form pred) (list '(values 1 2 3) #'cl-cc/ast:ast-values-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only mvb"
  (destructuring-bind (form pred) (list '(multiple-value-bind (a b) (values 1 2) (+ a b)) #'cl-cc/ast:ast-multiple-value-bind-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only go"
  (destructuring-bind (form pred) (list '(go my-tag) #'cl-cc/ast:ast-go-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only catch"
  (destructuring-bind (form pred) (list '(catch 'my-tag 1 2) #'cl-cc/ast:ast-catch-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only throw"
  (destructuring-bind (form pred) (list '(throw 'my-tag 42) #'cl-cc/ast:ast-throw-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only apply"
  (destructuring-bind (form pred) (list '(apply #'foo '(1 2)) #'cl-cc/ast:ast-apply-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only funcall"
  (destructuring-bind (form pred) (list '(funcall #'foo 1 2) #'cl-cc/ast:ast-call-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only tagbody"
  (destructuring-bind (form pred) (list '(tagbody start (print 1) end (print 2)) #'cl-cc/ast:ast-tagbody-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only setf-gethash"
  (destructuring-bind (form pred) (list '(setf (gethash 'k tbl) 42) #'cl-cc/ast:ast-set-gethash-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-type-only mv-prog1"
  (destructuring-bind (form pred) (list '(multiple-value-prog1 (values 1 2) (print 3)) #'cl-cc::ast-multiple-value-prog1-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-unwind-generic-the-cases unwind-protect"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(unwind-protect (risky) (cleanup)))))
               (assert-true (cl-cc/ast:ast-unwind-protect-p node))
               (assert-= 1 (length (cl-cc::ast-unwind-cleanup node))))))
    (funcall verify)))

(it-sequential "lower-unwind-generic-the-cases generic-call"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(my-func 1 2 3))))
               (assert-true (cl-cc/ast:ast-call-p node))
               (assert-= 3 (length (cl-cc/ast:ast-call-args node))))))
    (funcall verify)))

(it-sequential "lower-unwind-generic-the-cases the-form"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(the fixnum x))))
               (assert-true (cl-cc/ast:ast-the-p node))
               (assert-eq 'fixnum (cl-cc/ast:ast-the-type node)))))
    (funcall verify)))

(it-sequential "lower-local-and-binding-forms flet"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(flet ((helper (x) (* x 2))) (helper 5)))))
               (assert-true (cl-cc/ast:ast-flet-p node))
               (assert-= 1 (length (cl-cc::ast-flet-bindings node)))
               (assert-= 1 (length (cl-cc::ast-flet-body node))))))
    (funcall verify)))

(it-sequential "lower-local-and-binding-forms labels"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5)))))
               (assert-true (cl-cc/ast:ast-labels-p node))
               (assert-= 1 (length (cl-cc::ast-labels-bindings node))))))
    (funcall verify)))

(it-sequential "lower-local-and-binding-forms handler-case"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(handler-case (risky) (error (e) (print e))))))
               (assert-true (cl-cc/ast:ast-handler-case-p node))
               (assert-= 1 (length (cl-cc/ast:ast-handler-case-clauses node))))))
    (funcall verify)))

(it-sequential "lower-local-and-binding-forms declare-type"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(defun add1 (x) (declare (type fixnum x)) (+ x 1)))))
               (assert-true (cl-cc/ast:ast-defun-p node))
               (assert-equal '((x fixnum)) (cl-cc::ast-defun-params node)))
             (let ((node (lower '(lambda (x) (declare (type fixnum x)) (+ x 1)))))
               (assert-true (cl-cc/ast:ast-lambda-p node))
               (assert-equal '((x fixnum)) (cl-cc::ast-lambda-params node)))))
    (funcall verify)))

(it-sequential "lower-local-and-binding-forms declare-dynamic"
  (destructuring-bind (verify) (list (lambda ()
             (let ((node (lower '(lambda (&rest args) (declare (dynamic-extent args)) args))))
               (assert-true (cl-cc/ast:ast-lambda-p node))
               (assert-eq 'args (cl-cc::ast-lambda-rest-param node))
               (assert-equal '((dynamic-extent args)) (cl-cc::ast-lambda-declarations node))
               (assert-= 1 (length (cl-cc::ast-lambda-body node))))))
    (funcall verify)))

;;; ─── lower-sexp-to-ast: error cases ────────────────────────────────────────

(it-sequential "lower-sexp-arity-errors if-missing-args"
  (destructuring-bind (form) (list '(if))
    (signals error (lower form))))

(it-sequential "lower-sexp-arity-errors if-too-many-args"
  (destructuring-bind (form) (list '(if a b c d))
    (signals error (lower form))))

(it-sequential "lower-sexp-arity-errors let-no-bindings"
  (destructuring-bind (form) (list '(let))
    (signals error (lower form))))

(it-sequential "lower-sexp-arity-errors defun-no-params"
  (destructuring-bind (form) (list '(defun f))
    (signals error (lower form))))

(it-sequential "lower-sexp-arity-errors setq-odd-args"
  (destructuring-bind (form) (list '(setq x))
    (signals error (lower form))))

(it-sequential "lower-sexp-arity-errors quote-wrong-arity"
  (destructuring-bind (form) (list '(quote))
    (signals error (lower form))))

(it-sequential "lower-sexp-arity-errors function-no-arg"
  (destructuring-bind (form) (list '(function))
    (signals error (lower form))))
