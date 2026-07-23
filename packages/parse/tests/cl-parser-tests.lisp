;;;; tests/unit/parse/cl-parser-tests.lisp — CL parser and grammar unit tests
;;;;
;;;; Tests: parse-source, parse-all-forms, parse-cl-source,
;;;; lower-sexp-to-ast, token-stream operations,
;;;; parse-compiler-lambda-list, lambda-list-has-extended-p.

(in-package :cl-cc/test)


;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun parse-one (source)
  "Parse SOURCE and return the first s-expression."
  (cl-cc:parse-source source))

(defun parse-many (source)
  "Parse SOURCE and return all s-expressions."
  (cl-cc:parse-all-forms source))

(defun lower (sexp)
  "Lower an s-expression to an AST node."
  (cl-cc/parse:lower-sexp-to-ast sexp))

(defmacro assert-parse-boolean-case (expected form)
  `(if ,expected
       (expect ,form :to-be-truthy)
       (expect ,form :to-be-falsy)))

;;; ─── parse-source ───────────────────────────────────────────────────────────

(it-sequential "parser-parse-source-integer positive"
  (destructuring-bind (expected source) (list 42 "42")
    (expect (= expected (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-integer negative"
  (destructuring-bind (expected source) (list -7 "-7")
    (expect (= expected (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases string"
  (destructuring-bind (source pred) (list "\"hello\"" (lambda (r) (string= "hello" r)))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases symbol"
  (destructuring-bind (source pred) (list "foo" (lambda (r) (string= "FOO" (symbol-name r))))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases t-literal"
  (destructuring-bind (source pred) (list "t" (lambda (r) (eq t r)))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases nil-atom"
  (destructuring-bind (source pred) (list "nil" (lambda (r) (null r)))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases empty-list"
  (destructuring-bind (source pred) (list "()" (lambda (r) (null r)))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases simple-list"
  (destructuring-bind (source pred) (list "(+ 1 2)" (lambda (r) (equal '(+ 1 2) r)))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases nested-list"
  (destructuring-bind (source pred) (list "(if (> x 0) x (- x))" (lambda (r) (and (consp r) (eq 'if (first r)))))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

(it-sequential "parser-parse-source-atom-cases quote-sugar"
  (destructuring-bind (source pred) (list "'foo" (lambda (r) (and (consp r) (eq 'quote (first r))
                                           (string= "FOO" (symbol-name (second r))))))
    (expect (funcall pred (parse-one source)) :to-be-truthy)))

;;; ─── parse-all-forms ────────────────────────────────────────────────────────

(it-sequential "parser-parse-all-forms-cases multiple"
  (destructuring-bind (source pred) (list "(defun f (x) x) (defun g (y) y)" (lambda (forms) (and (= 2 (length forms))
                                             (eq 'defun (first (first forms)))
                                             (eq 'defun (first (second forms))))))
    (expect (funcall pred (parse-many source)) :to-be-truthy)))

(it-sequential "parser-parse-all-forms-cases single"
  (destructuring-bind (source pred) (list "(+ 1 2)" (lambda (forms) (and (= 1 (length forms))
                                             (equal '(+ 1 2) (first forms)))))
    (expect (funcall pred (parse-many source)) :to-be-truthy)))

(it-sequential "parser-parse-all-forms-cases empty"
  (destructuring-bind (source pred) (list "" (lambda (forms) (null forms)))
    (expect (funcall pred (parse-many source)) :to-be-truthy)))

(it-sequential "parser-parse-all-forms-cases atoms-seq"
  (destructuring-bind (source pred) (list "1 2 3" (lambda (forms) (and (= 3 (length forms))
                                             (= 1 (first forms)) (= 2 (second forms))
                                             (= 3 (third forms)))))
    (expect (funcall pred (parse-many source)) :to-be-truthy)))

;;; ─── parse-cl-source ────────────────────────────────────────────────────────

(it-sequential "grammar-parse-cl-source-cases single-form-cst"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse:parse-cl-source "(+ 1 2)")
               (declare (ignore diagnostics))
               (assert-= 1 (length cst-list))
               (assert-true (cl-cc:cst-interior-p (first cst-list))))))
    (funcall verify)))

(it-sequential "grammar-parse-cl-source-cases empty-yields-nil"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse:parse-cl-source "")
               (declare (ignore diagnostics))
               (assert-null cst-list))))
    (funcall verify)))

(it-sequential "grammar-parse-cl-source-cases diagnostics-list"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse:parse-cl-source "(+ 1 2)")
               (declare (ignore cst-list))
               (assert-true (listp diagnostics)))))
    (funcall verify)))

(it-sequential "grammar-parse-cl-source-cases multi-form-count"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse:parse-cl-source "1 2 3")
               (declare (ignore diagnostics))
               (assert-= 3 (length cst-list)))))
    (funcall verify)))

;;; ─── token-stream ────────────────────────────────────────────────────────────

(it-sequential "grammar-token-stream-cases struct-fields"
  (destructuring-bind (verify) (list (lambda ()
             (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "test")))
               (assert-true (cl-cc/parse::token-stream-p ts))
               (assert-null (cl-cc/parse::token-stream-tokens ts))
               (assert-string= "test" (cl-cc/parse::token-stream-source ts)))))
    (funcall verify)))

(it-sequential "grammar-token-stream-cases empty-peek"
  (destructuring-bind (verify) (list (lambda ()
             (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
               (assert-null (cl-cc/parse::ts-peek ts))
               (assert-true (cl-cc/parse::ts-at-end-p ts)))))
    (funcall verify)))

(it-sequential "grammar-token-stream-cases eof-returns-nil"
  (destructuring-bind (verify) (list (lambda ()
             (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
               (assert-null (cl-cc/parse::parse-cl-form ts)))))
    (funcall verify)))

;;; ─── parse-compiler-lambda-list ─────────────────────────────────────────────

(it-sequential "parser-lambda-list-parsing required-only"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(x y z))
               (assert-equal '(x y z) required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-null key-params)
               (assert-null aux-params)
               (assert-null whole-param)
               (assert-null environment-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing whole"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(&whole whole-name x &optional y))
               (assert-equal '(x) required)
               (assert-= 1 (length optional))
               (assert-eq 'whole-name whole-param)
               (assert-null rest-param)
               (assert-null key-params)
               (assert-null aux-params)
               (assert-null environment-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing environment"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(a &environment env b))
               (assert-equal '(a b) required)
               (assert-eq 'env environment-param)
               (assert-null optional)
               (assert-null rest-param)
               (assert-null key-params)
               (assert-null aux-params)
               (assert-null whole-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing optional"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(x &optional (y 10)))
               (assert-equal '(x) required)
               (assert-= 1 (length optional))
               (assert-eq 'y (first (first optional)))
               (assert-= 10 (second (first optional)))
               (assert-null rest-param)
               (assert-null key-params)
               (assert-null aux-params)
               (assert-null whole-param)
               (assert-null environment-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing rest"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(x &rest args))
               (assert-equal '(x) required)
               (assert-null optional)
               (assert-eq 'args rest-param)
               (assert-null key-params)
               (assert-null aux-params)
               (assert-null whole-param)
               (assert-null environment-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing key"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(x &key (size 0)))
               (assert-equal '(x) required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-= 1 (length key-params))
               (assert-eq 'size (first (first key-params)))
               (assert-null aux-params)
               (assert-null whole-param)
               (assert-null environment-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing aux"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '(x &aux (y 1) z))
               (assert-equal '(x) required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-null key-params)
               (assert-= 2 (length aux-params))
               (assert-eq 'y (first (first aux-params)))
               (assert-= 1 (second (first aux-params)))
               (assert-eq 'z (first (second aux-params)))
               (assert-null (second (second aux-params)))
               (assert-null whole-param)
               (assert-null environment-param))))
    (funcall verify)))

(it-sequential "parser-lambda-list-parsing empty"
  (destructuring-bind (verify) (list (lambda ()
             (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
                 (cl-cc/parse:parse-compiler-lambda-list '())
               (assert-null required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-null key-params)
               (assert-null aux-params)
               (assert-null whole-param)
               (assert-null environment-param))))
    (funcall verify)))

;;; ─── lambda-list-has-extended-p ──────────────────────────────────────────────

(it-sequential "parser-lambda-list-has-extended-p whole"
  (destructuring-bind (lambda-list expected) (list '(&whole w x) t)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p environment"
  (destructuring-bind (lambda-list expected) (list '(&environment e x) t)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p optional"
  (destructuring-bind (lambda-list expected) (list '(x &optional y) t)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p rest"
  (destructuring-bind (lambda-list expected) (list '(x &rest args) t)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p key"
  (destructuring-bind (lambda-list expected) (list '(x &key y) t)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p aux"
  (destructuring-bind (lambda-list expected) (list '(x &aux y) t)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p simple"
  (destructuring-bind (lambda-list expected) (list '(x y z) nil)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

(it-sequential "parser-lambda-list-has-extended-p empty"
  (destructuring-bind (lambda-list expected) (list '() nil)
    (assert-parse-boolean-case expected
    (cl-cc/parse:lambda-list-has-extended-p lambda-list))))

;;; ─── lower-sexp-to-ast: atoms ────────────────────────────────────────────────

(it-sequential "lower-symbol-and-hole-atoms symbol"
  (destructuring-bind (input verify) (list 'x (lambda (node)
             (assert-true (cl-cc/ast:ast-var-p node))
             (assert-eq 'x (cl-cc/ast:ast-var-name node))))
    (let ((node (lower input)))
    (funcall verify node))))

(it-sequential "lower-symbol-and-hole-atoms underscore"
  (destructuring-bind (input verify) (list '_ (lambda (node)
             (assert-true (cl-cc/ast:ast-hole-p node))))
    (let ((node (lower input)))
    (funcall verify node))))

(it-sequential "lower-arithmetic-cases nary-left-fold"
  (destructuring-bind (form verify) (list '(+ 1 2 3) (lambda (node)
             (assert-true (cl-cc/ast:ast-binop-p node))
             (assert-eq '+ (cl-cc/ast:ast-binop-op node))
             (assert-true (cl-cc/ast:ast-binop-p (cl-cc/ast:ast-binop-lhs node)))
             (assert-= 3 (cl-cc/ast:ast-int-value (cl-cc/ast:ast-binop-rhs node)))))
    (funcall verify (lower form))))

(it-sequential "lower-arithmetic-cases unary-minus"
  (destructuring-bind (form verify) (list '(- 7) (lambda (node)
             (assert-true (cl-cc/ast:ast-binop-p node))
             (assert-eq '- (cl-cc/ast:ast-binop-op node))
             (assert-= 0 (cl-cc/ast:ast-int-value (cl-cc/ast:ast-binop-lhs node)))
             (assert-= 7 (cl-cc/ast:ast-int-value (cl-cc/ast:ast-binop-rhs node)))))
    (funcall verify (lower form))))

(it-sequential "lower-self-eval-produces-ast-quote nil"
  (destructuring-bind (input) (list nil)
    (expect (cl-cc/ast:ast-quote-p (lower input)) :to-be-truthy)))

(it-sequential "lower-self-eval-produces-ast-quote t"
  (destructuring-bind (input) (list t)
    (expect (cl-cc/ast:ast-quote-p (lower input)) :to-be-truthy)))

(it-sequential "lower-self-eval-produces-ast-quote float"
  (destructuring-bind (input) (list 3.14)
    (expect (cl-cc/ast:ast-quote-p (lower input)) :to-be-truthy)))

(it-sequential "lower-literal-values-in-ast-quote string"
  (destructuring-bind (input) (list "hello")
    (let ((node (lower input)))
    (expect (cl-cc/ast:ast-quote-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-quote-value node) :to-equal input))))

(it-sequential "lower-literal-values-in-ast-quote character"
  (destructuring-bind (input) (list #\a)
    (let ((node (lower input)))
    (expect (cl-cc/ast:ast-quote-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-quote-value node) :to-equal input))))
