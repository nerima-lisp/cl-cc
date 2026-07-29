;;;; tests/unit/parse/pratt-pipeline-tests.lisp — Pratt parser pipeline tests
;;;;
;;;; Tests: CST node source positions, parse-cl-source multiple forms,
;;;; parse-all-forms s-expression output, parse-source single form,
;;;; error recovery via cst-error, special form dispatch,
;;;; parse-and-lower pipeline, pratt-parse-expr, pratt-add-diagnostic,
;;;; and pratt-parse-list-until.

(in-package :cl-cc/test)


;;; ─── CST Node Source Positions ───────────────────────────────────────────────

(it-sequential "parse-integer-byte-positions"
  (let ((node (parse-one-cst "42")))
    (expect (= 0 (cl-cc:cst-node-start-byte node)) :to-be-truthy)
    (expect (= 2 (cl-cc:cst-node-end-byte node)) :to-be-truthy)))

(it-sequential "parse-offset-byte-positions"
  (multiple-value-bind (nodes _diag)
      (cl-cc:parse-cl-source "1 99")
    (declare (ignore _diag))
    (let ((second-node (second nodes)))
      (expect (= 2 (cl-cc:cst-node-start-byte second-node)) :to-be-truthy))))

;;; ─── parse-cl-source: Multiple Forms ────────────────────────────────────────

(it-sequential "parse-cl-source-basic-behavior multiple-forms"
  (destructuring-bind (source expected-count check-diagnostics-listp) (list "1 2 3" 3 nil)
    (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source source)
    (expect (= expected-count (length nodes)) :to-be-truthy)
    (when check-diagnostics-listp
      (expect (listp diag) :to-be-truthy)))))

(it-sequential "parse-cl-source-basic-behavior returns-diagnostics"
  (destructuring-bind (source expected-count check-diagnostics-listp) (list "42" 1 t)
    (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source source)
    (expect (= expected-count (length nodes)) :to-be-truthy)
    (when check-diagnostics-listp
      (expect (listp diag) :to-be-truthy)))))

(it-sequential "parse-cl-source-basic-behavior empty-string"
  (destructuring-bind (source expected-count check-diagnostics-listp) (list "" 0 t)
    (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source source)
    (expect (= expected-count (length nodes)) :to-be-truthy)
    (when check-diagnostics-listp
      (expect (listp diag) :to-be-truthy)))))

;;; ─── parse-all-forms: S-expression Output ───────────────────────────────────

(it-sequential "parse-all-forms-value-cases integer"
  (destructuring-bind (source pred) (list "42" (lambda (f) (= f 42)))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-value-cases string"
  (destructuring-bind (source pred) (list "\"hello\"" (lambda (f) (string= f "hello")))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-value-cases nil-list"
  (destructuring-bind (source pred) (list "()" (lambda (f) (null f)))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-value-cases symbol"
  (destructuring-bind (source pred) (list "foo" (lambda (f) (string= "FOO" (symbol-name f))))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-value-cases call"
  (destructuring-bind (source pred) (list "(+ 1 2)" (lambda (f) (and (eq '+ (car f)) (= 1 (cadr f)) (= 2 (caddr f)))))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-value-cases quote"
  (destructuring-bind (source pred) (list "'foo" (lambda (f) (and (eq 'quote (car f)) (string= "FOO" (symbol-name (cadr f))))))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-value-cases nested"
  (destructuring-bind (source pred) (list "(let ((x (+ 1 2))) (* x x))" (lambda (f) (and (eq 'let (car f)) (string= "X" (symbol-name (caaadr f))))))
    (expect (funcall pred (parse-one-sexp source)) :to-be-truthy)))

(it-sequential "parse-all-forms-multiple"
  (let ((forms (cl-cc:parse-all-forms "(defun f (x) x) (f 1)")))
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (caar forms) :to-be 'defun)
    (expect (symbol-name (cadar forms)) :to-equal "F")))

;;; ─── parse-source: Single Form ───────────────────────────────────────────────

(it-sequential "parse-source-returns-one-form"
  (expect (= 99 (cl-cc:parse-source "99")) :to-be-truthy))

(it-sequential "parse-source-errors-on-empty"
  (signals error (cl-cc:parse-source "")))

;;; ─── Error Recovery via cst-error ───────────────────────────────────────────

(it-sequential "parse-unmatched-paren-adds-diagnostic"
  (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source "(+ 1")
    (declare (ignore nodes))
    (expect (not (null diag)) :to-be-truthy)))

;;; ─── Special Forms: sexp-head-to-kind Dispatch ───────────────────────────────

(it-sequential "parse-special-form-head-kind defun"
  (destructuring-bind (source expected-kind) (list "(defun f (x) x)" :defun)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-special-form-head-kind let"
  (destructuring-bind (source expected-kind) (list "(let ((x 1)) x)" :let)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-special-form-head-kind if"
  (destructuring-bind (source expected-kind) (list "(if t 1 2)" :if)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-special-form-head-kind lambda"
  (destructuring-bind (source expected-kind) (list "(lambda (x) x)" :lambda)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-special-form-head-kind unknown"
  (destructuring-bind (source expected-kind) (list "(my-fn a b)" :call)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

;;; ─── parse-and-lower: Full Pipeline ─────────────────────────────────────────

(it-sequential "parse-and-lower-cases returns-list"
  (destructuring-bind (source pred) (list "(+ 1 2)" (lambda (asts) (and (listp asts) (not (null asts)))))
    (expect (funcall pred (cl-cc:parse-and-lower source)) :to-be-truthy)))

(it-sequential "parse-and-lower-cases integer-ast-int"
  (destructuring-bind (source pred) (list "42" (lambda (asts) (cl-cc/ast:ast-int-p (first asts))))
    (expect (funcall pred (cl-cc:parse-and-lower source)) :to-be-truthy)))

(it-sequential "parse-and-lower-cases multiple-3-forms"
  (destructuring-bind (source pred) (list "1 2 3" (lambda (asts) (= 3 (length asts))))
    (expect (funcall pred (cl-cc:parse-and-lower source)) :to-be-truthy)))

;;; ─── pratt-parse-expr: Direct Tests ──────────────────────────────────────────

(it-sequential "pratt-parse-expr-empty-nud-table-returns-error"
  (let* ((ctx  (make-test-ctx "42"))
         (node (cl-cc/parse::pratt-parse-expr ctx)))
    (expect (cl-cc/parse:cst-error-p node) :to-be-truthy)))

(it-sequential "pratt-parse-expr-eof-returns-error"
  (let* ((ctx  (make-test-ctx ""))
         (node (cl-cc/parse::pratt-parse-expr ctx)))
    (expect (cl-cc/parse:cst-error-p node) :to-be-truthy)))

(it-sequential "pratt-parse-expr-node-type integer"
  (destructuring-bind (source pred expected-kind) (list "42" #'cl-cc:cst-token-p nil)
    (let ((node (parse-one-cst source)))
    (expect (funcall pred node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind)))))

(it-sequential "pratt-parse-expr-node-type symbol"
  (destructuring-bind (source pred expected-kind) (list "foo" #'cl-cc:cst-token-p nil)
    (let ((node (parse-one-cst source)))
    (expect (funcall pred node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind)))))

(it-sequential "pratt-parse-expr-node-type list"
  (destructuring-bind (source pred expected-kind) (list "(+ 1 2)" #'cl-cc:cst-interior-p nil)
    (let ((node (parse-one-cst source)))
    (expect (funcall pred node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind)))))

(it-sequential "pratt-parse-expr-node-type quote"
  (destructuring-bind (source pred expected-kind) (list "'x" #'cl-cc:cst-interior-p :quote)
    (let ((node (parse-one-cst source)))
    (expect (funcall pred node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind)))))

(it-sequential "pratt-parse-expr-node-type nested-list"
  (destructuring-bind (source pred expected-kind) (list "(let ((x 1)) x)" #'cl-cc:cst-interior-p nil)
    (let ((node (parse-one-cst source)))
    (expect (funcall pred node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind)))))

;;; ─── pratt-add-diagnostic: Direct Tests ─────────────────────────────────────

(it-sequential "pratt-add-diagnostic-count one"
  (destructuring-bind (n) (list 1)
    (let ((ctx (make-test-ctx "42")))
    (dotimes (i n)
      (cl-cc/parse::pratt-add-diagnostic ctx (format nil "error ~a" i) (cons i (1+ i))))
    (expect (length (cl-cc/parse::pratt-context-diagnostics ctx)) :to-equal n))))

(it-sequential "pratt-add-diagnostic-count two"
  (destructuring-bind (n) (list 2)
    (let ((ctx (make-test-ctx "42")))
    (dotimes (i n)
      (cl-cc/parse::pratt-add-diagnostic ctx (format nil "error ~a" i) (cons i (1+ i))))
    (expect (length (cl-cc/parse::pratt-context-diagnostics ctx)) :to-equal n))))

(it-sequential "pratt-add-diagnostic-records-message"
  (let ((ctx (make-test-ctx "42")))
    (cl-cc/parse::pratt-add-diagnostic ctx "unexpected token" (cons 0 2))
    (let ((diag (first (cl-cc/parse::pratt-context-diagnostics ctx))))
      (expect (not (null diag)) :to-be-truthy))))

;;; ─── pratt-parse-list-until: Direct Tests ───────────────────────────────────

(it-sequential "pratt-parse-list-until-length empty"
  (destructuring-bind (source expected-len) (list "()" 0)
    (let ((ctx (make-test-ctx source)))
    (cl-cc/parse::pratt-advance ctx) ; consume LPAREN
    (let ((items (cl-cc/parse::pratt-parse-list-until ctx :T-RPAREN
                   (lambda (c) (cl-cc/parse::pratt-parse-expr c)))))
      (expect (length items) :to-equal expected-len)))))

(it-sequential "pratt-parse-list-until-length elements"
  (destructuring-bind (source expected-len) (list "(1 2 3)" 3)
    (let ((ctx (make-test-ctx source)))
    (cl-cc/parse::pratt-advance ctx) ; consume LPAREN
    (let ((items (cl-cc/parse::pratt-parse-list-until ctx :T-RPAREN
                   (lambda (c) (cl-cc/parse::pratt-parse-expr c)))))
      (expect (length items) :to-equal expected-len)))))

(it-sequential "pratt-parse-list-until-consumes-terminator"
  (let ((ctx (make-test-ctx "(1) 42")))
    (cl-cc/parse::pratt-advance ctx) ; consume LPAREN
    (cl-cc/parse::pratt-parse-list-until ctx :T-RPAREN
      (lambda (c) (cl-cc/parse::pratt-parse-expr c)))
    ;; Next token should be 42, not RPAREN
    (let ((tok (cl-cc/parse::pratt-peek ctx)))
      (expect (cl-cc:lexer-token-type tok) :to-be :T-INT)
      (expect (cl-cc:lexer-token-value tok) :to-equal 42))))
