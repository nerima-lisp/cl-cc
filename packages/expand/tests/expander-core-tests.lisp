;;;; tests/unit/expand/expander-core-tests.lisp — Core expander helper tests

(in-package :cl-cc/test)



(it-sequential "reduce-variadic-op zero-plus"
  (destructuring-bind (op args id expected) (list '+ nil 0 0)
    (expect (cl-cc/expand::reduce-variadic-op op args id) :to-equal expected)))

(it-sequential "reduce-variadic-op zero-mul"
  (destructuring-bind (op args id expected) (list '* nil 1 1)
    (expect (cl-cc/expand::reduce-variadic-op op args id) :to-equal expected)))

(it-sequential "reduce-variadic-op one-arg"
  (destructuring-bind (op args id expected) (list '+ '(x) 0 'x)
    (expect (cl-cc/expand::reduce-variadic-op op args id) :to-equal expected)))

(it-sequential "reduce-variadic-op two-args"
  (destructuring-bind (op args id expected) (list '+ '(a b) 0 '(+ a b))
    (expect (cl-cc/expand::reduce-variadic-op op args id) :to-equal expected)))

(it-sequential "reduce-variadic-op three-args"
  (destructuring-bind (op args id expected) (list '+ '(a b c) 0 '(+ (+ a b) c))
    (expect (cl-cc/expand::reduce-variadic-op op args id) :to-equal expected)))

(it-sequential "reduce-variadic-op four-args"
  (destructuring-bind (op args id expected) (list '* '(a b c d) 1 '(* (* (* a b) c) d))
    (expect (cl-cc/expand::reduce-variadic-op op args id) :to-equal expected)))

(it-sequential "expand-all-atom-passthrough integer"
  (destructuring-bind (form) (list 42)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal form)))

(it-sequential "expand-all-atom-passthrough string"
  (destructuring-bind (form) (list "hello")
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal form)))

(it-sequential "expand-all-atom-passthrough symbol"
  (destructuring-bind (form) (list 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal form)))

(it-sequential "expand-all-quote"
  (expect (cl-cc/expand:compiler-macroexpand-all '(quote (1 2 3))) :to-equal '(quote (1 2 3))))

(it-sequential "expand-all-if"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(if t 1 2))))
    (expect (first result) :to-equal 'if)
    (expect (second result) :to-equal t)
    (expect (third result) :to-equal 1)
    (expect (fourth result) :to-equal 2)))

(it-sequential "expand-all-let"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(let ((x 1)) x))))
    (expect (first result) :to-equal 'let)
    (expect (caar (second result)) :to-equal 'x)))

(it-sequential "expander-variadic-fold-nesting multiply"
  (destructuring-bind (op form) (list '* '(* a b c))
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be op))))

(it-sequential "expander-variadic-fold-nesting append"
  (destructuring-bind (op form) (list 'append '(append a b c))
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be op))))

(it-sequential "expander-variadic-fold-nesting minus"
  (destructuring-bind (op form) (list '- '(- a b c))
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be op))))

(it-sequential "expander-variadic-zero-arg-identity plus"
  (destructuring-bind (form expected) (list '(+) 0)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-variadic-zero-arg-identity times"
  (destructuring-bind (form expected) (list '(*) 1)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

;;; ─── define-expander-for (registration macro) ────────────────────────────

(it-sequential "define-expander-for-installs-handler-in-table"
  (let ((test-head (gensym "EXP-HEAD")))
    ;; Simulate what define-expander-for does at macro expansion
    (setf (gethash test-head cl-cc/expand::*expander-head-table*)
          (lambda (form) (list 'was-handled (second form))))
    (unwind-protect
        (let ((result (cl-cc/expand:compiler-macroexpand-all (list test-head 42))))
          (expect (first result) :to-equal 'was-handled)
          (expect (second result) :to-equal 42))
       (remhash test-head cl-cc/expand::*expander-head-table*))))

(it-sequential "make-macro-expander-returns-descriptor"
  (let ((expander (cl-cc/expand:make-macro-expander '(&body body) '((cons 'progn body)))))
    (expect (getf expander :kind) :to-equal :macro-expander)
    (expect (getf expander :lambda-list) :to-equal '(&body body))
    (expect (listp (getf expander :body)) :to-be-truthy)))

(it-sequential "expander-handler-returning-same-form-does-not-recurse"
  (let ((test-head (gensym "STABLE")))
    (setf (gethash test-head cl-cc/expand::*expander-head-table*)
          (lambda (form) form))  ; idempotent — returns unchanged
    (unwind-protect
        (let ((form (list test-head 1 2)))
          (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal form))
      (remhash test-head cl-cc/expand::*expander-head-table*))))

;;; ─── defmethod expander handler ──────────────────────────────────────────
;;; The defmethod handler is registered by expander.lisp via define-expander-for.

(it-sequential "expander-defmethod-no-qualifier-expands-body"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(defmethod my-noq-method ((x integer)) (+ x 1)))))
    (expect (first result) :to-equal 'defmethod)
    (expect (second result) :to-equal 'my-noq-method)))

(it-sequential "expander-defmethod-with-qualifier-preserved-verbatim"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(defmethod my-around-method :around ((x integer)) x))))
    (expect (first result) :to-equal 'defmethod)
    (expect (second result) :to-equal 'my-around-method)
    (expect (third result) :to-equal :around)))

(it-sequential "expander-defmethod-all-qualifiers-preserved before"
  (destructuring-bind (qualifier) (list :before)
    (let ((result (cl-cc/expand:compiler-macroexpand-all
                 (list 'defmethod 'my-qual-method qualifier '(x) 'x))))
    (expect (third result) :to-equal qualifier))))

(it-sequential "expander-defmethod-all-qualifiers-preserved after"
  (destructuring-bind (qualifier) (list :after)
    (let ((result (cl-cc/expand:compiler-macroexpand-all
                 (list 'defmethod 'my-qual-method qualifier '(x) 'x))))
    (expect (third result) :to-equal qualifier))))

(it-sequential "expander-defmethod-all-qualifiers-preserved around"
  (destructuring-bind (qualifier) (list :around)
    (let ((result (cl-cc/expand:compiler-macroexpand-all
                 (list 'defmethod 'my-qual-method qualifier '(x) 'x))))
    (expect (third result) :to-equal qualifier))))

;;; ─── symbol-macro expansion path ─────────────────────────────────────────

(it-sequential "compiler-macroexpand-all-expands-registered-symbol-macro"
  (let ((key (gensym "SM")))
    (setf (gethash key cl-cc/expand:*symbol-macro-table*) 99)
    (unwind-protect
        (expect (cl-cc/expand:compiler-macroexpand-all key) :to-equal 99)
      (remhash key cl-cc/expand:*symbol-macro-table*))))

(it-sequential "compiler-macroexpand-all-keywords-bypass-symbol-macro"
  (let ((kw :test-keyword))
    (setf (gethash kw cl-cc/expand:*symbol-macro-table*) 'expanded)
    (unwind-protect
        (expect (cl-cc/expand:compiler-macroexpand-all kw) :to-equal kw)
      (remhash kw cl-cc/expand:*symbol-macro-table*))))

;;; ─── accessor slot-map expansion path (FR-120) ───────────────────────────

(it-sequential "compiler-macroexpand-all-accessor-expands-to-slot-value"
  (let ((accessor (gensym "ACC"))
        (class-sym (gensym "CLS")))
    (setf (gethash accessor cl-cc/expand:*accessor-slot-map*) (cons class-sym 'my-slot))
    (unwind-protect
        (let ((result (cl-cc/expand:compiler-macroexpand-all (list accessor 'my-obj))))
          ;; Expected: (slot-value my-obj 'my-slot)
          (expect (first result) :to-equal 'slot-value)
          (expect (second result) :to-equal 'my-obj)
          (expect (third result) :to-equal '(quote my-slot)))
      (remhash accessor cl-cc/expand:*accessor-slot-map*))))

(it-sequential "compiler-macroexpand-all-multi-arg-accessor-does-not-expand"
  (let ((accessor (gensym "ACC2")))
    (setf (gethash accessor cl-cc/expand:*accessor-slot-map*) (cons 'cls 'slot))
    (unwind-protect
        (let ((result (cl-cc/expand:compiler-macroexpand-all
                       (list accessor 'obj 'extra))))
          ;; Should NOT be slot-value (2-arg form only)
          (expect (eq 'slot-value (first result)) :to-be-falsy))
      (remhash accessor cl-cc/expand:*accessor-slot-map*))))
