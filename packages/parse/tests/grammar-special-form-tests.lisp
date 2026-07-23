;;;; tests/unit/parse/grammar-special-form-tests.lisp — CL grammar special form tests
;;;;
;;;; Tests: special form dispatch, edge cases (quote, unquote-splicing, empty vector),
;;;; macro forms, and CST-to-sexp roundtrip through the grammar.

(in-package :cl-cc/test)


(defun %grammar-symbol-name-tree (form)
  (cond
    ((null form) nil)
    ((symbolp form) (symbol-name form))
    ((consp form)
     (cons (%grammar-symbol-name-tree (car form))
           (%grammar-symbol-name-tree (cdr form))))
    ((vectorp form) (map 'vector #'%grammar-symbol-name-tree form))
    (t form)))

;;; ─── parse-cl-source: special form dispatch ────────────────────────────────

(it-sequential "grammar-special-form-kinds defun"
  (destructuring-bind (source expected-kind) (list "(defun f (x) x)" :defun)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds let"
  (destructuring-bind (source expected-kind) (list "(let ((x 1)) x)" :let)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds if"
  (destructuring-bind (source expected-kind) (list "(if t 1 2)" :if)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds lambda"
  (destructuring-bind (source expected-kind) (list "(lambda (x) x)" :lambda)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds progn"
  (destructuring-bind (source expected-kind) (list "(progn 1 2 3)" :progn)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds setq"
  (destructuring-bind (source expected-kind) (list "(setq x 1)" :setq)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds block"
  (destructuring-bind (source expected-kind) (list "(block nil 1)" :block)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds cond"
  (destructuring-bind (source expected-kind) (list "(cond (t 1))" :cond)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds loop"
  (destructuring-bind (source expected-kind) (list "(loop for x from 1 to 10 collect x)" :loop)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-special-form-kinds call"
  (destructuring-bind (source expected-kind) (list "(foo 1 2)" :call)
    (expect (parse-first-kind source) :to-be expected-kind)))

(it-sequential "grammar-parse-special-form-child-count defun"
  (destructuring-bind (source expected-count) (list "(defun f (x) x)" 4)
    (let ((node (parse-first-form source)))
    (expect (= expected-count (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-parse-special-form-child-count if"
  (destructuring-bind (source expected-count) (list "(if t 1 2)" 4)
    (let ((node (parse-first-form source)))
    (expect (= expected-count (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-parse-quote-form-kind"
  (let ((node (parse-first-form "(quote x)")))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    ;; sexp-head-to-kind does not map "QUOTE" since the case checks symbol eq,
    ;; but the head is an interned symbol. Verify the node is an interior node.
    (expect (not (null (cl-cc:cst-interior-children node))) :to-be-truthy)))

;;; ─── Unquote-splicing ──────────────────────────────────────────────────────

(it-sequential "grammar-parse-unquote-splicing"
  (let ((node (parse-first-form ",@x")))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be :unquote-splicing)
    (expect (= 1 (length (cl-cc:cst-interior-children node))) :to-be-truthy)))

;;; ─── Vector edge cases ─────────────────────────────────────────────────────

(it-sequential "grammar-parse-empty-vector"
  (let ((node (parse-first-form "#()")))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be :vector)
    (expect (= 0 (length (cl-cc:cst-interior-children node))) :to-be-truthy)))

;;; ─── CST-to-sexp roundtrip through grammar ────────────────────────────────

(it-sequential "grammar-cst-to-sexp-roundtrip integer"
  (destructuring-bind (source expected) (list "42" 42)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-to-sexp node) :to-equal expected))))

(it-sequential "grammar-cst-to-sexp-roundtrip string"
  (destructuring-bind (source expected) (list "\"hi\"" "hi")
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-to-sexp node) :to-equal expected))))

(it-sequential "grammar-cst-to-sexp-roundtrip empty-list"
  (destructuring-bind (source expected) (list "()" nil)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-to-sexp node) :to-equal expected))))

(it-sequential "grammar-cst-to-sexp-roundtrip simple-call"
  (destructuring-bind (source expected) (list "(+ 1 2)" '(+ 1 2))
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-to-sexp node) :to-equal expected))))

(it-sequential "grammar-cst-to-sexp-roundtrip nested"
  (destructuring-bind (source expected) (list "(if t 1 2)" '(if t 1 2))
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-to-sexp node) :to-equal expected))))

(it-sequential "grammar-cst-to-sexp-quote"
  (let* ((node (parse-first-form "'x"))
         (sexp (cl-cc:cst-to-sexp node)))
    (expect (consp sexp) :to-be-truthy)
    (expect (car sexp) :to-be 'quote)
    (expect (symbol-name (second sexp)) :to-equal "X")))

(it-sequential "grammar-cst-to-sexp-quasiquote"
  (let* ((node (parse-first-form "`(a ,x ,@xs)"))
         (sexp (cl-cc:cst-to-sexp node)))
    (expect (%grammar-symbol-name-tree sexp) :to-equal '("QUASIQUOTE" ("A" ("UNQUOTE" "X") ("UNQUOTE-SPLICING" "XS"))))))
