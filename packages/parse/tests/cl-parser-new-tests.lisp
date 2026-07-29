;;;; tests/unit/parse/cl-parser-new-tests.lisp — Additional parser and AST tests
;;;;
;;;; Tests: parse-source atom types, structural forms, vectors, edge cases,
;;;; parse-all-forms additional cases, AST constructors, struct properties,
;;;; lower-sexp extended cases, additional roundtrip tests,
;;;; parse-slot-spec, lambda-list edge cases, parse-then-lower pipeline.

(in-package :cl-cc/test)


(defun %parser-symbol-name-tree (form)
  (cond
    ((null form) nil)
    ((symbolp form) (symbol-name form))
    ((consp form)
     (cons (%parser-symbol-name-tree (car form))
           (%parser-symbol-name-tree (cdr form))))
    ((vectorp form) (map 'vector #'%parser-symbol-name-tree form))
    (t form)))

;;; ─── NEW: parse-source atom parsing ────────────────────────────────────────

(it-sequential "parser-atom-types float"
  (destructuring-bind (source expected) (list "3.14" 3.14)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types zero"
  (destructuring-bind (source expected) (list "0" 0)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types large-int"
  (destructuring-bind (source expected) (list "999999" 999999)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types keyword"
  (destructuring-bind (source expected) (list ":foo" :foo)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types keyword-upper"
  (destructuring-bind (source expected) (list ":BAR" :bar)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types char-a"
  (destructuring-bind (source expected) (list "#\\a" #\a)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types char-space"
  (destructuring-bind (source expected) (list "#\\Space" #\Space)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-atom-types char-newline"
  (destructuring-bind (source expected) (list "#\\Newline" #\Newline)
    (let ((result (parse-one source)))
    (expect result :to-be expected))))

(it-sequential "parser-parse-dotted-pair-produces-cons"
  (let ((result (parse-one "(a . b)")))
    (expect (consp result) :to-be-truthy)
    (expect (symbol-name (car result)) :to-equal "A")
    (expect (symbol-name (cdr result)) :to-equal "B")))

(it-sequential "parser-parse-deeply-nested-list"
  (let ((result (parse-one "((((((1))))))")))
    (expect (consp result) :to-be-truthy)
    (expect (= 1 (first (first (first (first (first (first result))))))) :to-be-truthy)))

(it-sequential "parser-parse-source-vector empty"
  (destructuring-bind (source expected-len) (list "#()" 0)
    (let ((result (parse-one source)))
    (expect (vectorp result) :to-be-truthy)
    (expect (= expected-len (length result)) :to-be-truthy))))

(it-sequential "parser-parse-source-vector three"
  (destructuring-bind (source expected-len) (list "#(1 2 3)" 3)
    (let ((result (parse-one source)))
    (expect (vectorp result) :to-be-truthy)
    (expect (= expected-len (length result)) :to-be-truthy))))

(it-sequential "parser-parse-source-empty-signals-error"
  (signals error (parse-one "")))

(it-sequential "parser-parse-source-quasiquote-produces-cons"
  (let ((result (parse-one "`(a b)")))
    (expect (consp result) :to-be-truthy)
    (expect (symbol-name (first result)) :to-equal "QUASIQUOTE"))
  (let ((result (parse-one "`,x")))
    (expect (%parser-symbol-name-tree result) :to-equal '("QUASIQUOTE" ("UNQUOTE" "X")))))

(it-sequential "parser-parse-source-long-symbol-name-preserved"
  (let* ((long-name (make-string 200 :initial-element #\A))
         (result (parse-one long-name)))
    (expect (symbolp result) :to-be-truthy)
    (expect (= 200 (length (symbol-name result))) :to-be-truthy)))

;;; ─── NEW: parse-all-forms additional cases ─────────────────────────────────

(it-sequential "parser-parse-all-forms-mixed-types"
  (let ((forms (parse-many "42 \"hello\" (+ 1 2)")))
    (expect (= 3 (length forms)) :to-be-truthy)
    (expect (= 42 (first forms)) :to-be-truthy)
    (expect (second forms) :to-equal "hello")
    (expect (consp (third forms)) :to-be-truthy)))

(it-sequential "parser-parse-all-forms-strips-line-comments"
  (let ((forms (parse-many (format nil "; comment~%1 2"))))
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (= 1 (first forms)) :to-be-truthy)
    (expect (= 2 (second forms)) :to-be-truthy)))

(it-sequential "parse-all-forms-length-cases whitespace-only"
  (destructuring-bind (source expected-len) (list (format nil " ~% ") 0)
    (expect (= expected-len (length (parse-many source))) :to-be-truthy)))

(it-sequential "parse-all-forms-length-cases ten-forms"
  (destructuring-bind (source expected-len) (list "1 2 3 4 5 6 7 8 9 10" 10)
    (expect (= expected-len (length (parse-many source))) :to-be-truthy)))

;;; ─── NEW: AST node constructors ────────────────────────────────────────────

(it-sequential "ast-constructor-basic ast-int"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-int :value 5) #'cl-cc/ast:ast-int-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-var"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-var :name 'x) #'cl-cc/ast:ast-var-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-quote"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-quote :value 'hello) #'cl-cc/ast:ast-quote-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-progn"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-progn :forms nil) #'cl-cc/ast:ast-progn-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-print"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-print :expr nil) #'cl-cc/ast:ast-print-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-block"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-block :name 'b :body nil) #'cl-cc/ast:ast-block-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-go"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-go :tag 'done) #'cl-cc/ast:ast-go-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-setq"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-setq :var 'x :value nil) #'cl-cc/ast:ast-setq-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-function"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-function :name 'foo) #'cl-cc/ast:ast-function-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-the"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-the :type 'fixnum :value nil) #'cl-cc/ast:ast-the-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-constructor-basic ast-values"
  (destructuring-bind (node pred) (list (cl-cc/ast:make-ast-values :forms nil) #'cl-cc/ast:ast-values-p)
    (expect (funcall pred node) :to-be-truthy)))

(it-sequential "ast-source-location-fields-stored"
  (let ((node (cl-cc/ast:make-ast-int :value 42
                                       :source-file "test.lisp"
                                       :source-line 10
                                       :source-column 5)))
    (expect (cl-cc::ast-source-file node) :to-equal "test.lisp")
    (expect (= 10 (cl-cc::ast-source-line node)) :to-be-truthy)
    (expect (= 5 (cl-cc::ast-source-column node)) :to-be-truthy)))

(it-sequential "ast-lambda-callable-slots"
  (let ((node (cl-cc/ast:make-ast-lambda :params '(x)
                                          :optional-params '((y nil))
                                          :rest-param 'args
                                          :key-params '((z nil))
                                          :body nil)))
    (expect (cl-cc::ast-lambda-params node) :to-equal '(x))
    (expect (cl-cc::ast-lambda-optional-params node) :to-equal '((y nil)))
    (expect (cl-cc::ast-lambda-rest-param node) :to-be 'args)
    (expect (cl-cc::ast-lambda-key-params node) :to-equal '((z nil)))))

(it-sequential "ast-slot-def-full-options"
  (let ((slot (cl-cc/ast:make-ast-slot-def :name 'x
                                            :initarg :x
                                            :reader 'get-x
                                            :writer 'set-x
                                            :accessor 'x-accessor
                                            :type 'integer)))
    (expect (cl-cc::ast-slot-name     slot) :to-be 'x)
    (expect (cl-cc::ast-slot-initarg   slot) :to-be :x)
    (expect (cl-cc::ast-slot-reader    slot) :to-be 'get-x)
    (expect (cl-cc::ast-slot-writer    slot) :to-be 'set-x)
    (expect (cl-cc::ast-slot-accessor slot) :to-be 'x-accessor)
    (expect (cl-cc::ast-slot-type      slot) :to-be 'integer)))

(it-sequential "ast-location-string-formats with-location"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-int :value 1
                                :source-file "foo.lisp" :source-line 3 :source-column 7) "foo.lisp:3:7")
    (expect (cl-cc/ast:ast-location-string node) :to-equal expected)))

(it-sequential "ast-location-string-formats unknown-location"
  (destructuring-bind (node expected) (list (cl-cc/ast:make-ast-int :value 1) "<unknown location>")
    (expect (cl-cc/ast:ast-location-string node) :to-equal expected)))

;;; ─── NEW: lower-sexp-to-ast additional forms ──────────────────────────────

(it-sequential "lower-binary-operators-produce-ast-binop"
  (dolist (op '(+ - * = < > <= >=))
    (let ((node (lower (list op 1 2))))
      (expect (cl-cc/ast:ast-binop-p node) :to-be-truthy)
      (expect (cl-cc/ast:ast-binop-op node) :to-be op))))

(it-sequential "lower-print-produces-ast-print-with-int-expr"
  (let ((node (lower '(print 42))))
    (expect (cl-cc/ast:ast-print-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-int-p (cl-cc/ast:ast-print-expr node)) :to-be-truthy)))

(it-sequential "lower-defparameter-produces-ast-defvar"
  (let ((node (lower '(defparameter *x* 99))))
    (expect (cl-cc/ast:ast-defvar-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-defvar-name node) :to-be '*x*)))

(it-sequential "lower-extended-lambda-list-params optional"
  (destructuring-bind (form pred-p get-slot expected) (list '(lambda (x &optional (y 0)) (+ x y)) #'cl-cc/ast:ast-lambda-p #'cl-cc::ast-lambda-optional-params 1)
    (let ((node (lower form)))
    (expect (funcall pred-p node) :to-be-truthy)
    (let ((slot-val (funcall get-slot node)))
      (if (eq expected :rest)
          (expect slot-val :to-be 'args)
          (expect (= expected (length slot-val)) :to-be-truthy))))))

(it-sequential "lower-extended-lambda-list-params rest"
  (destructuring-bind (form pred-p get-slot expected) (list '(lambda (x &rest args) args) #'cl-cc/ast:ast-lambda-p #'cl-cc::ast-lambda-rest-param :rest)
    (let ((node (lower form)))
    (expect (funcall pred-p node) :to-be-truthy)
    (let ((slot-val (funcall get-slot node)))
      (if (eq expected :rest)
          (expect slot-val :to-be 'args)
          (expect (= expected (length slot-val)) :to-be-truthy))))))

(it-sequential "lower-extended-lambda-list-params key"
  (destructuring-bind (form pred-p get-slot expected) (list '(defun f (x &key (size 10)) x) #'cl-cc/ast:ast-defun-p #'cl-cc::ast-defun-key-params 1)
    (let ((node (lower form)))
    (expect (funcall pred-p node) :to-be-truthy)
    (let ((slot-val (funcall get-slot node)))
      (if (eq expected :rest)
          (expect slot-val :to-be 'args)
          (expect (= expected (length slot-val)) :to-be-truthy))))))

(it-sequential "lower-return-from-without-value-produces-ast"
  ;; (return-from blk) with no value is valid CL (returns NIL from BLK);
  ;; the lowerer accepts it and produces an AST-RETURN-FROM node.
  (expect (cl-cc/ast:ast-return-from-p (lower '(return-from blk))) :to-be-truthy))

(it-sequential "lower-multiple-value-call-produces-ast-mvc"
  (let ((node (lower '(multiple-value-call #'list (values 1 2) (values 3 4)))))
    (expect (cl-cc/ast:ast-multiple-value-call-p node) :to-be-truthy)
    (expect (= 2 (length (cl-cc::ast-mv-call-args node))) :to-be-truthy)))

(it-sequential "lower-setf-slot-value-produces-ast-set-slot-value"
  (let ((node (lower '(setf (slot-value obj 'x) 10))))
    (expect (cl-cc/ast:ast-set-slot-value-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-set-slot-value-slot node) :to-be 'x)))

(it-sequential "lower-setf-simple-variable-produces-ast-setq"
  (let ((node (lower '(setf x 10))))
    (expect (cl-cc/ast:ast-setq-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-setq-var node) :to-be 'x)))

(it-sequential "lower-quasiquote-expands-unquote"
  (let ((result (cl-cc/ast:ast-to-sexp
                 (lower '(quasiquote (a (unquote x) b))))))
    (expect (%parser-symbol-name-tree result) :to-equal '("CONS" ("QUOTE" "A")
                    ("CONS" "X"
                     ("CONS" ("QUOTE" "B") ("QUOTE" NIL)))))))

(it-sequential "lower-quasiquote-expands-unquote-splicing"
  (let ((result (cl-cc/ast:ast-to-sexp
                 (lower '(quasiquote (a (unquote-splicing xs) b))))))
    (expect (%parser-symbol-name-tree result) :to-equal '("CONS" ("QUOTE" "A")
                    ("APPEND" "XS"
                     ("CONS" ("QUOTE" "B") ("QUOTE" NIL)))))))

;;; ─── NEW: lower-sexp-to-ast error cases ───────────────────────────────────

(it-sequential "lower-sexp-empty-progn-lowers"
  ;; (progn) is valid CL (yields NIL); the lowerer accepts it.
  (destructuring-bind (form) (list '(progn))
    (expect (lower form) :to-be-truthy)))

(it-sequential "lower-sexp-body-errors let-no-body"
  (destructuring-bind (form) (list '(let ()))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors lambda-no-body"
  (destructuring-bind (form) (list '(lambda ()))
    (signals error (lower form))))

(it-sequential "lower-sexp-empty-defun-lowers"
  ;; (defun f ()) has an empty body (yields NIL); valid CL, lowerer accepts it.
  (destructuring-bind (form) (list '(defun f ()))
    (expect (lower form) :to-be-truthy)))

(it-sequential "lower-sexp-empty-block-lowers"
  ;; (block b) with no body is valid CL (yields NIL); lowerer accepts it.
  (destructuring-bind (form) (list '(block b))
    (expect (lower form) :to-be-truthy)))

(it-sequential "lower-sexp-unary-plus-lowers"
  ;; (+ 1) is valid CL (unary + yields its argument); lowerer folds to AST-INT.
  (destructuring-bind (form) (list '(+ 1))
    (expect (lower form) :to-be-truthy)))

(it-sequential "lower-sexp-body-errors go-no-tag"
  (destructuring-bind (form) (list '(go))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors catch-no-body"
  (destructuring-bind (form) (list '(catch 'tag))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors throw-wrong-arity"
  (destructuring-bind (form) (list '(throw 'tag))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors unwind-no-cleanup"
  (destructuring-bind (form) (list '(unwind-protect (x)))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors handler-no-clause"
  (destructuring-bind (form) (list '(handler-case (x)))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors the-wrong-arity"
  (destructuring-bind (form) (list '(the fixnum))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors defclass-no-slots"
  (destructuring-bind (form) (list '(defclass c ()))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors defgeneric-no-ll"
  (destructuring-bind (form) (list '(defgeneric g))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors defmethod-no-body"
  (destructuring-bind (form) (list '(defmethod m ()))
    (signals error (lower form))))

(it-sequential "lower-sexp-body-errors defmacro-no-body"
  (destructuring-bind (form) (list '(defmacro m ()))
    (signals error (lower form))))
