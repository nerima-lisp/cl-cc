;;;; tests/unit/compile/cps-ast-extended-tests.lisp
;;;; Unit tests for src/compile/cps-ast-extended.lisp
;;;;
;;;; Covers: cps-transform-ast for ast-quote, ast-setq, ast-the, ast-values,
;;;;   ast-apply, ast-call (general calls), ast-defun, ast-defmacro,
;;;;   ast-defclass, ast-defgeneric, ast-defmethod; entry points cps-transform-ast*,
;;;;   cps-transform*, cps-transform-eval.
;;;;
;;;; Tests inspect the *structure* of the produced S-expression rather than
;;;; evaluating it (which would require a full runtime environment).

(in-package :cl-cc/test)

;;; Helper: transform a node with a fixed continuation symbol.
(defun cps-with-k (node)
  "Return the CPS form for NODE with continuation symbol K."
  (cl-cc/cps::cps-transform-ast node 'k))

;;; ─── ast-quote ────────────────────────────────────────────────────────────

(it-sequential "cps-quote-symbol-produces-funcall-k-form"
  (let ((result (cps-with-k (cl-cc/ast:make-ast-quote :value 'hello))))
    (expect (car result) :to-be 'funcall)
    (expect (second result) :to-be 'k)
    (expect (third result) :to-equal '(quote hello))))

(it-sequential "cps-quote-list-preserves-list-value"
  (let ((result (cps-with-k (cl-cc/ast:make-ast-quote :value '(a b c)))))
    (expect (third result) :to-equal '(quote (a b c)))))

;;; ─── ast-the ─────────────────────────────────────────────────────────────

(it-sequential "cps-the-wraps-value-with-the-declaration"
  (let* ((node (cl-cc/ast:make-ast-the
                :type 'integer
                :value (cl-cc/ast:make-ast-int :value 5)))
         (result (cps-with-k node)))
    ;; Result is a (cps-transform-ast (ast-int 5) (lambda (v) (funcall k (the integer v))))
    ;; The continuation arg is a lambda containing (the integer ...) and (funcall k ...)
    (expect (consp result) :to-be-truthy)
    ;; Walk into the lambda body to find (the integer ...)
    (labels ((contains-the-p (form)
               (if (consp form)
                   (or (eq (car form) 'the)
                       (some #'contains-the-p form))
                   nil)))
      (expect (contains-the-p result) :to-be-truthy))))

;;; ─── ast-setq ────────────────────────────────────────────────────────────

(it-sequential "cps-setq-contains-setq-and-funcall-k"
  (let* ((node (cl-cc/ast:make-ast-setq
                :var 'x
                :value (cl-cc/ast:make-ast-int :value 0)))
         (result (cps-with-k node)))
    (labels ((contains-sym-p (form sym)
               (if (consp form)
                   (or (eq (car form) sym)
                       (some (lambda (sub) (contains-sym-p sub sym)) (cdr form)))
                   nil)))
      (expect (contains-sym-p result 'setq) :to-be-truthy)
      (expect (contains-sym-p result 'funcall) :to-be-truthy))))

;;; ─── ast-values ──────────────────────────────────────────────────────────

(it-sequential "cps-values-empty-forms-produces-funcall-k-nil"
  (let* ((node (cl-cc/ast:make-ast-values :forms nil))
         (result (cps-with-k node)))
    (expect (car result) :to-be 'funcall)
    (expect (second result) :to-be 'k)
    (expect (third result) :to-be-null)))

(it-sequential "cps-values-single-form-produces-multiple-value-call"
  (let* ((node (cl-cc/ast:make-ast-values
                :forms (list (cl-cc/ast:make-ast-int :value 42))))
         (result (cps-with-k node)))
    (labels ((contains-p (form sym)
               (if (consp form)
                   (or (eq (car form) sym)
                       (some (lambda (sub) (contains-p sub sym)) (cdr form)))
                   nil)))
      (expect (contains-p result 'multiple-value-call) :to-be-truthy))))

;;; ─── ast-apply ───────────────────────────────────────────────────────────

(it-sequential "cps-apply-contains-apply-call"
  (let* ((node (cl-cc/ast:make-ast-apply
                :func (cl-cc/ast:make-ast-var :name 'f)
                :args (list (cl-cc/ast:make-ast-var :name 'args))))
         (result (cps-with-k node)))
    (labels ((contains-apply-p (form)
               (if (consp form)
                   (or (eq (car form) 'apply)
                       (some #'contains-apply-p (cdr form)))
                   nil)))
      (expect (contains-apply-p result) :to-be-truthy))))

;;; ─── ast-call ─────────────────────────────────────────────────────────────

(it-sequential "cps-call-no-args-produces-funcall-k-form"
  (let* ((node (cl-cc/ast:make-ast-call :func 'compute :args nil))
         (result (cps-with-k node)))
    (expect (car result) :to-be 'funcall)
    (expect (second result) :to-be 'k)
    (expect (third result) :to-equal '(compute))))

(it-sequential "cps-call-with-args-threads-args-into-call"
  (let* ((node (cl-cc/ast:make-ast-call
                :func 'add
                :args (list (cl-cc/ast:make-ast-int :value 1)
                            (cl-cc/ast:make-ast-int :value 2))))
         (result (cps-with-k node)))
    ;; Walk the CPS tree to find the innermost (funcall k <call-form>)
    (labels ((find-funcall-k (form)
               (cond
                 ((atom form) nil)
                 ((and (eq (car form) 'funcall) (eq (second form) 'k))
                  form)
                 (t (loop for sub in form
                          for found = (find-funcall-k sub)
                          when found return found)))))
      (let* ((funcall-k (find-funcall-k result))
             (call-form (third funcall-k)))
        ;; The call form must be (add G1 G2) — head is 'add, exactly 2 args
        (expect (consp call-form) :to-be-truthy)
        (expect (car call-form) :to-be 'add)
        (expect (= 2 (length (cdr call-form))) :to-be-truthy)
        ;; Both arg symbols must be distinct gensyms (not NIL, not ADD)
        (expect (symbolp (second call-form)) :to-be-truthy)
        (expect (symbolp (third call-form)) :to-be-truthy)
        (expect (eq (second call-form) (third call-form)) :to-be-falsy)))))

;;; ─── ast-defgeneric ───────────────────────────────────────────────────────

(it-sequential "cps-defun-produces-progn-defun-funcall-k"
  (let* ((node (cl-cc/ast:make-ast-defun
                :name 'square
                :params '(x)
                :body (list (cl-cc/ast:make-ast-binop
                             :op '*
                             :lhs (cl-cc/ast:make-ast-var :name 'x)
                             :rhs (cl-cc/ast:make-ast-var :name 'x)))))
         (result (cps-with-k node)))
    (expect (car result) :to-be 'progn)
    (expect (caadr result) :to-be 'defun)
    (let ((last-form (car (last result))))
      (expect (car last-form) :to-be 'funcall)
      (expect (second last-form) :to-be 'k))))

(it-sequential "cps-defmacro-produces-progn-defmacro-funcall-k"
  (let* ((node (cl-cc/ast:make-ast-defmacro
                :name 'when1
                :lambda-list '(test &body body)
                :body '((list 'if test (cons 'progn body) nil))))
         (result (cps-with-k node)))
    (expect (car result) :to-be 'progn)
    (expect (caadr result) :to-be 'defmacro)
    (let ((last-form (car (last result))))
      (expect (car last-form) :to-be 'funcall)
      (expect (second last-form) :to-be 'k))))

(it-sequential "cps-defgeneric-produces-progn-defgeneric-funcall-k"
  (let* ((node (cl-cc/ast:make-ast-defgeneric :name 'area :params '(shape)))
         (result (cps-with-k node)))
    (expect (car result) :to-be 'progn)
    (expect (caadr result) :to-be 'defgeneric)
    (let ((last-form (car (last result))))
      (expect (car last-form) :to-be 'funcall)
      (expect (second last-form) :to-be 'k))))

;;; ─── cps-transform-ast* ───────────────────────────────────────────────────

(it-sequential "cps-transform-ast-star-returns-lambda-form"
  (let* ((node (cl-cc/ast:make-ast-int :value 7))
         (result (cl-cc/cps::cps-transform-ast* node)))
    (expect (car result) :to-be 'lambda)
    (expect (= 1 (length (second result))) :to-be-truthy)   ; single param
    (expect (symbolp (car (second result))) :to-be-truthy)))  ; param is gensym

;;; ─── cps-transform* ───────────────────────────────────────────────────────

(it-sequential "cps-transform-star-sexp-returns-cons"
  (expect (consp (cl-cc/cps::cps-transform* '42)) :to-be-truthy))

(it-sequential "cps-transform-star-ast-node-returns-lambda-with-one-param"
  (let* ((node (cl-cc/ast:make-ast-quote :value 'x))
         (result (cl-cc/cps::cps-transform* node)))
    (expect (car result) :to-be 'lambda)
    (expect (= 1 (length (second result))) :to-be-truthy)))

;;; ─── %cps-lower-lambda-param ─────────────────────────────────────────────────

(it-sequential "cps-lower-lambda-param-cases no-default"
  (destructuring-bind (slot expected) (list (list 'x nil nil) 'x)
    (expect (cl-cc/cps::%cps-lower-lambda-param slot) :to-equal expected)))

(it-sequential "cps-lower-lambda-param-cases default-only"
  (destructuring-bind (slot expected) (list (list 'y (cl-cc/ast:make-ast-int :value 42) nil) '(y 42))
    (expect (cl-cc/cps::%cps-lower-lambda-param slot) :to-equal expected)))

(it-sequential "cps-lower-lambda-param-cases default-and-svar"
  (destructuring-bind (slot expected) (list (list 'z (cl-cc/ast:make-ast-int :value 0) 'z-p) '(z 0 z-p))
    (expect (cl-cc/cps::%cps-lower-lambda-param slot) :to-equal expected)))

;;; ─── %cps-extended-lambda-list ───────────────────────────────────────────────
;;; Verifies that keyword markers (&optional, &rest, &key) are preserved — this
;;; was a latent bug where (when optional (list '&optional) (mapcar ...)) discarded
;;; the marker because `when` returns only its last form.

(it-sequential "cps-extended-lambda-list-cases required-only"
  (destructuring-bind (required optional rest key expected) (list '(a b) nil nil nil '(a b))
    (expect (cl-cc/cps::%cps-extended-lambda-list required optional rest key) :to-equal expected)))

(it-sequential "cps-extended-lambda-list-cases with-optional"
  (destructuring-bind (required optional rest key expected) (list '(x) (list (list 'y nil nil)) nil nil '(x &optional y))
    (expect (cl-cc/cps::%cps-extended-lambda-list required optional rest key) :to-equal expected)))

(it-sequential "cps-extended-lambda-list-cases with-optional-default"
  (destructuring-bind (required optional rest key expected) (list '(x) (list (list 'y (cl-cc/ast:make-ast-int :value 0) nil)) nil nil '(x &optional (y 0)))
    (expect (cl-cc/cps::%cps-extended-lambda-list required optional rest key) :to-equal expected)))

(it-sequential "cps-extended-lambda-list-cases with-rest"
  (destructuring-bind (required optional rest key expected) (list '(x) nil 'rest nil '(x &rest rest))
    (expect (cl-cc/cps::%cps-extended-lambda-list required optional rest key) :to-equal expected)))

(it-sequential "cps-extended-lambda-list-cases with-key"
  (destructuring-bind (required optional rest key expected) (list '() nil nil (list (list 'k nil nil)) '(&key k))
    (expect (cl-cc/cps::%cps-extended-lambda-list required optional rest key) :to-equal expected)))

(it-sequential "cps-extended-lambda-list-cases combined"
  (destructuring-bind (required optional rest key expected) (list '(a) (list (list 'b nil nil)) 'r (list (list 'c nil nil)) '(a &optional b &rest r &key c))
    (expect (cl-cc/cps::%cps-extended-lambda-list required optional rest key) :to-equal expected)))

;;; ─── cps-transform* shared entrypoint ──────────────────────────────────────

(it-sequential "cps-transform*-handles-ast-node-and-sexp"
  (expect (cl-cc/cps::cps-transform* (cl-cc/ast:make-ast-int :value 1)) :to-be-truthy)
  (expect (cl-cc/cps::cps-transform* '42) :to-be-truthy))

;;; ─── cps-transform-eval ───────────────────────────────────────────────────

(defun %cps-unwrap (v)
  "Apply identity to a CPS-transformed lambda to retrieve the underlying value.
cps-transform-eval returns the raw CPS lambda; other callers depend on that
shape, so we unwrap locally in these tests rather than changing the function."
  (if (functionp v) (funcall v #'identity) v))

(it-sequential "cps-transform-eval-integer-literal-returns-value"
  (expect (= 42 (%cps-unwrap (cl-cc/cps::cps-transform-eval '42))) :to-be-truthy))

(it-sequential "cps-transform-eval-arithmetic-expression-returns-result"
  (expect (= 7 (%cps-unwrap (cl-cc/cps::cps-transform-eval '(+ 3 4)))) :to-be-truthy))
