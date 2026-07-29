;;;; tests/unit/parse/cl-parser-ast-tests.lisp — ast-to-sexp roundtrip and CLOS lowering tests
;;;;
;;;; Tests: ast-to-sexp roundtrip, sexp-head-to-kind, grammar specialized parsers,
;;;; defmacro/defclass/defgeneric/defmethod/make-instance lowering.

(in-package :cl-cc/test)


;;; ─── ast-to-sexp roundtrip ──────────────────────────────────────────────────

(defun ast-roundtrip (sexp)
  "Lower sexp to AST then convert back to sexp."
  (cl-cc/ast:ast-to-sexp (lower sexp)))

(it-sequential "ast-roundtrip-atoms integer"
  (destructuring-bind (input expected) (list 42 42)
    (expect (ast-roundtrip input) :to-equal expected)))

(it-sequential "ast-roundtrip-atoms symbol"
  (destructuring-bind (input expected) (list 'x 'x)
    (expect (ast-roundtrip input) :to-equal expected)))

(it-sequential "ast-roundtrip-wildcard-and-if-structure"
  (expect (symbol-name (ast-roundtrip '_)) :to-equal "_")
  (let ((result (ast-roundtrip '(if x 1 2))))
    (expect (second result) :to-be 'x)
    (expect (= 1 (third result)) :to-be-truthy)
    (expect (= 2 (fourth result)) :to-be-truthy)))

(it-sequential "ast-roundtrip-control-flow if"
  (destructuring-bind (input expected-head) (list '(if x 1 2) 'if)
    (expect (first (ast-roundtrip input)) :to-be expected-head)))

(it-sequential "ast-roundtrip-control-flow progn"
  (destructuring-bind (input expected-head) (list '(progn 1 2 3) 'progn)
    (expect (first (ast-roundtrip input)) :to-be expected-head)))

(it-sequential "ast-roundtrip-control-flow block"
  (destructuring-bind (input expected-head) (list '(block outer 1 2) 'block)
    (expect (first (ast-roundtrip input)) :to-be expected-head)))

(it-sequential "ast-roundtrip-control-flow let"
  (destructuring-bind (input expected-head) (list '(let ((x 10)) x) 'let)
    (expect (first (ast-roundtrip input)) :to-be expected-head)))

(it-sequential "lower-named-let-form"
  (let ((node (lower '(let loop ((i 0) (acc 1))
                       (if (= i 3)
                           acc
                           (loop (+ i 1) (* acc 2)))))))
    (expect (cl-cc/ast:ast-labels-p node) :to-be-truthy)
    (let ((binding (first (cl-cc/ast:ast-labels-bindings node)))
          (body (cl-cc/ast:ast-labels-body node)))
      (expect (first binding) :to-be 'loop)
      (expect (second binding) :to-equal '(i acc))
      (expect (cl-cc/ast:ast-call-p (first body)) :to-be-truthy))))

(it-sequential "ast-roundtrip-exact-forms setq"
  (destructuring-bind (form) (list '(setq x 42))
    (expect (ast-roundtrip form) :to-equal form)))

(it-sequential "ast-roundtrip-exact-forms quote"
  (destructuring-bind (form) (list '(quote hello))
    (expect (ast-roundtrip form) :to-equal form)))

(it-sequential "ast-roundtrip-definition-heads lambda"
  (destructuring-bind (form expected-head expected-name expected-params) (list '(lambda (x y) (+ x y)) 'lambda nil '(x y))
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (when expected-name
      (expect (second result) :to-be expected-name))
    (when expected-params
      (expect (if expected-name (third result) (second result)) :to-equal expected-params)))))

(it-sequential "ast-roundtrip-definition-heads defun"
  (destructuring-bind (form expected-head expected-name expected-params) (list '(defun add (a b) (+ a b)) 'defun 'add '(a b))
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (when expected-name
      (expect (second result) :to-be expected-name))
    (when expected-params
      (expect (if expected-name (third result) (second result)) :to-equal expected-params)))))

(it-sequential "ast-roundtrip-definition-heads defvar"
  (destructuring-bind (form expected-head expected-name expected-params) (list '(defvar *count* 0) 'defvar '*count* nil)
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (when expected-name
      (expect (second result) :to-be expected-name))
    (when expected-params
      (expect (if expected-name (third result) (second result)) :to-equal expected-params)))))

;;; ─── sexp-head-to-kind ───────────────────────────────────────────────────────

(it-sequential "grammar-sexp-head-to-kind defun"
  (destructuring-bind (sym expected) (list 'defun :defun)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind let"
  (destructuring-bind (sym expected) (list 'let :let)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind if"
  (destructuring-bind (sym expected) (list 'if :if)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind lambda"
  (destructuring-bind (sym expected) (list 'lambda :lambda)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind setq"
  (destructuring-bind (sym expected) (list 'setq :setq)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind progn"
  (destructuring-bind (sym expected) (list 'progn :progn)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind defclass"
  (destructuring-bind (sym expected) (list 'defclass :defclass)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

(it-sequential "grammar-sexp-head-to-kind unknown"
  (destructuring-bind (sym expected) (list 'completely-unknown-symbol :call)
    (expect (cl-cc/parse:sexp-head-to-kind sym) :to-be expected)))

;;; ─── Grammar specialized parsers ────────────────────────────────────────────

(it-sequential "grammar-parse-cl-form-atoms integer"
  (destructuring-bind (source expected-value) (list "42" 42)
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-token-p form) :to-be-truthy)
    (expect (cl-cc:cst-token-value form) :to-equal expected-value))))

(it-sequential "grammar-parse-cl-form-atoms string"
  (destructuring-bind (source expected-value) (list "\"hello\"" "hello")
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-token-p form) :to-be-truthy)
    (expect (cl-cc:cst-token-value form) :to-equal expected-value))))

(it-sequential "grammar-parse-cl-form-lists non-empty"
  (destructuring-bind (source expected-child-count) (list "(1 2 3)" 3)
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-interior-p form) :to-be-truthy)
    (expect (= expected-child-count (length (cl-cc:cst-children form))) :to-be-truthy))))

(it-sequential "grammar-parse-cl-form-lists empty"
  (destructuring-bind (source expected-child-count) (list "()" 0)
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-interior-p form) :to-be-truthy)
    (expect (= expected-child-count (length (cl-cc:cst-children form))) :to-be-truthy))))

(it-sequential "grammar-parse-cl-form-reader-macros quote"
  (destructuring-bind (source expected-kind) (list "'foo" :quote)
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-interior-p form) :to-be-truthy)
    (expect (cl-cc:cst-node-kind form) :to-be expected-kind))))

(it-sequential "grammar-parse-cl-form-reader-macros backquote"
  (destructuring-bind (source expected-kind) (list "`foo" :quasiquote)
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-interior-p form) :to-be-truthy)
    (expect (cl-cc:cst-node-kind form) :to-be expected-kind))))

(it-sequential "grammar-parse-cl-form-reader-macros function"
  (destructuring-bind (source expected-kind) (list "#'foo" :function)
    (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc/parse::make-token-stream :tokens tokens :source source))
         (form (cl-cc/parse::parse-cl-form ts)))
    (expect (cl-cc:cst-interior-p form) :to-be-truthy)
    (expect (cl-cc:cst-node-kind form) :to-be expected-kind))))

;;; ─── defmacro lowering ───────────────────────────────────────────────────────

(it-sequential "lower-defmacro-form"
  (let ((node (lower '(defmacro my-mac (x) `(list ,x)))))
    (expect (cl-cc/ast:ast-defmacro-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-defmacro-name node) :to-be 'my-mac)
    (expect (cl-cc/ast:ast-defmacro-lambda-list node) :to-equal '(x))))

;;; ─── defclass lowering ───────────────────────────────────────────────────────

(it-sequential "lower-defclass-form no-superclass"
  (destructuring-bind (form expected-name expected-supers expected-slots) (list '(defclass point () (x y)) 'point '() 2)
    (let ((node (lower form)))
    (expect (cl-cc/ast:ast-defclass-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-defclass-name node) :to-be expected-name)
    (expect (cl-cc/ast:ast-defclass-superclasses node) :to-equal expected-supers)
    (expect (= expected-slots (length (cl-cc/ast:ast-defclass-slots node))) :to-be-truthy))))

(it-sequential "lower-defclass-form with-superclass"
  (destructuring-bind (form expected-name expected-supers expected-slots) (list '(defclass colored-point (point) (color)) 'colored-point '(point) 1)
    (let ((node (lower form)))
    (expect (cl-cc/ast:ast-defclass-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-defclass-name node) :to-be expected-name)
    (expect (cl-cc/ast:ast-defclass-superclasses node) :to-equal expected-supers)
    (expect (= expected-slots (length (cl-cc/ast:ast-defclass-slots node))) :to-be-truthy))))

;;; ─── defgeneric / defmethod lowering ────────────────────────────────────────

(it-sequential "lower-generic-dispatch-forms defgeneric"
  (destructuring-bind (form pred-p get-name get-params expected-name expected-params) (list '(defgeneric area (shape)) #'cl-cc/ast:ast-defgeneric-p #'cl-cc/ast:ast-defgeneric-name #'cl-cc/ast:ast-defgeneric-params 'area '(shape))
    (let ((node (lower form)))
    (expect (funcall pred-p node) :to-be-truthy)
    (expect (funcall get-name node) :to-be expected-name)
    (expect (funcall get-params node) :to-equal expected-params))))

(it-sequential "lower-generic-dispatch-forms defmethod"
  (destructuring-bind (form pred-p get-name get-params expected-name expected-params) (list '(defmethod area ((s circle)) (* pi (expt (slot-value s 'radius) 2))) #'cl-cc/ast:ast-defmethod-p #'cl-cc/ast:ast-defmethod-name #'cl-cc/ast:ast-defmethod-params 'area '(s))
    (let ((node (lower form)))
    (expect (funcall pred-p node) :to-be-truthy)
    (expect (funcall get-name node) :to-be expected-name)
    (expect (funcall get-params node) :to-equal expected-params))))

;;; ─── make-instance lowering ──────────────────────────────────────────────────

(it-sequential "lower-clos-access-forms make-instance"
  (destructuring-bind (form pred) (list '(make-instance 'point :x 1 :y 2) #'cl-cc/ast:ast-make-instance-p)
    (let ((node (lower form)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "lower-clos-access-forms slot-value"
  (destructuring-bind (form pred) (list '(slot-value obj 'name) #'cl-cc/ast:ast-slot-value-p)
    (let ((node (lower form)))
    (expect (funcall pred node) :to-be-truthy))))
