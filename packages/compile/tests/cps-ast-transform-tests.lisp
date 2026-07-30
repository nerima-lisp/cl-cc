;;;; tests/unit/compile/cps-ast-transform-tests.lisp — CPS Transform Dispatch + Control Tests
;;;;
;;;; Tests for cps-transform* dispatch, sequence/simplify helpers,
;;;; control flow (block/catch/return-from/throw/tagbody), local function
;;;; bindings, and unwind-protect CPS transformation.
;;;; Suite: cps-ast-suite (defined elsewhere, used by in-suite).

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform* dispatcher
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-transform*-dispatch ast-node"
  (destructuring-bind (input) (list (cl-cc:make-ast-int :value 42))
    (expect (is-cps-lambda (cl-cc:cps-transform* input)) :to-be-truthy)))

(it-sequential "cps-transform*-dispatch sexp"
  (destructuring-bind (input) (list '(+ 1 2))
    (expect (is-cps-lambda (cl-cc:cps-transform* input)) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform-sequence edge cases
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-sequence-behavior"
  (let ((k-var (gensym "K")))
    ;; empty: calls continuation with nil
    (let ((sexp (cl-cc:cps-transform-sequence nil k-var)))
      (expect (listp sexp) :to-be-truthy)
      (expect (car sexp) :to-be 'funcall))
    ;; single form: delegates to cps-transform-ast (still a list)
    (let ((sexp (cl-cc:cps-transform-sequence (list (cl-cc:make-ast-int :value 5)) k-var)))
      (expect (listp sexp) :to-be-truthy))))

(it-sequential "cps-simplify-fixed-point-stops-on-stable-form"
  (let ((calls 0))
    (expect (cl-cc/cps::%cps-simplify-fixed-point
                   'start
                   (lambda (form)
                     (incf calls)
                     (if (eq form 'start) 'done 'done))) :to-equal 'done)
    (expect (= 2 calls) :to-be-truthy)))

(it-sequential "cps-dispatch-table-covers-bootstrap-special-forms"
  (dolist (operator '(+ - * if progn let print))
    (expect (functionp (gethash operator cl-cc/cps::*cps-sexp-dispatch-table*)) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for block / return-from
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-control-block-uses-explicit-continuation"
  (let* ((node (cl-cc:make-ast-block
                :name nil
                :body (list (cl-cc:make-ast-return-from
                             :name nil
                             :value (cl-cc:make-ast-int :value 1)))))
         (form (cl-cc:cps-transform-ast* node)))
    (expect (%cps-form-contains-p form 'block) :to-be-falsy)
    (expect (%cps-form-contains-p form 'return-from) :to-be-falsy)))

(it-sequential "cps-control-outer-car catch"
  (destructuring-bind (node expected-car) (list (cl-cc:make-ast-catch :tag  (cl-cc:make-ast-quote :value :done)
                                         :body (list (cl-cc:make-ast-int :value 0))) 'funcall)
    (let* ((k      (gensym "K"))
         (result (cl-cc/cps::cps-transform-ast node k)))
    (expect (car result) :to-be expected-car))))

(it-sequential "cps-control-unbound-return-from-signals-before-value-transform"
  (let ((node (cl-cc:make-ast-return-from
               :name nil
               :value (cl-cc/ast::make-ast-hole))))
    (handler-case
        (progn
          (cl-cc:cps-transform-ast* node)
          (error "Expected unbound CPS block error"))
      (cl-cc/cps::unbound-cps-block (condition)
        (expect (cl-cc/cps::unbound-cps-block-name condition) :to-be nil)))))

(it-sequential "cps-control-contains-token throw"
  (destructuring-bind (node expected-token) (list (cl-cc:make-ast-throw :tag   (cl-cc:make-ast-quote :value :done)
                                               :value (cl-cc:make-ast-int :value 99)) "THROW")
    (let* ((k      (gensym "K"))
         (result (format nil "~S" (cl-cc/cps::cps-transform-ast node k))))
    (expect (search expected-token result) :to-be-truthy))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for tagbody-section
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-tagbody-section-behavior"
  (let ((continue-form '(go next-tag)))
    ;; empty: returns the continuation form directly
    (let ((result (cl-cc/cps::cps-transform-tagbody-section nil continue-form)))
      (expect result :to-equal continue-form))
    ;; single form: CPS wrapper is a funcall
    (let ((result (cl-cc/cps::cps-transform-tagbody-section
                   (list (cl-cc:make-ast-int :value 1)) continue-form)))
      (expect (car result) :to-be 'funcall))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for local function bindings (flet/labels helpers)
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-fn-binding-structure"
  (let* ((k-var 'my-k)
         (binding (list 'my-fn '(a b) (cl-cc:make-ast-int :value 42)))
         (result (cl-cc/cps::cps-transform-fn-binding binding k-var)))
    (expect (first result) :to-be 'my-fn)
    (let ((lambda-list (second result)))
      (expect (first  lambda-list) :to-be 'a)
      (expect (second lambda-list) :to-be 'b)
      (expect (third  lambda-list) :to-be 'my-k))))

(it-sequential "cps-local-fns-outer-is-form-kw flet"
  (destructuring-bind (form-kw) (list 'flet)
    (let* ((k    (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 42))))
    (expect (first (cl-cc/cps::cps-transform-local-fns form-kw nil body k)) :to-be form-kw))))

(it-sequential "cps-local-fns-outer-is-form-kw labels"
  (destructuring-bind (form-kw) (list 'labels)
    (let* ((k    (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 42))))
    (expect (first (cl-cc/cps::cps-transform-local-fns form-kw nil body k)) :to-be form-kw))))

(it-sequential "cps-local-fns-bindings-transformed"
  (let* ((k (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 1)))
         ;; binding = (f (x) <ast-node>)
         (binding (list 'f '(x) (cl-cc:make-ast-int :value 99)))
         (result (cl-cc/cps::cps-transform-local-fns
                  'flet (list binding) body k)))
    ;; second element is the binding list: ((f (x K) ...))
    (expect (first (first (second result))) :to-be 'f)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for unwind-protect
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-unwind-protect-structure"
  (let ((k (gensym "K")))
    (let* ((node (cl-cc:make-ast-unwind-protect
                  :protected (cl-cc:make-ast-int :value 1)
                  :cleanup   (list (cl-cc:make-ast-int :value 2))))
           (result (cl-cc/cps::cps-transform-ast node k)))
      (expect (first result) :to-be 'unwind-protect))
    (let* ((node (cl-cc:make-ast-unwind-protect
                  :protected (cl-cc:make-ast-int :value 1)
                  :cleanup   nil))
           (result (cl-cc/cps::cps-transform-ast node k)))
      (expect (third result) :to-be nil))))
