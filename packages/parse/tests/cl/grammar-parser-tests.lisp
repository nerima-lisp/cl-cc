;;;; tests/unit/parse/cl/grammar-parser-tests.lisp
;;;; Unit tests for src/parse/cl/grammar.lisp — Form Parsers
;;;;
;;;; Covers: parse-cl-atom (all atom token types),
;;;;   parse-cl-form (quote/backquote/unquote/vector/list dispatch),
;;;;   parse-cl-list-form (empty list, special-form kinds, dotted pairs,
;;;;     generic calls), parse-cl-source (multi-form, nested).

(in-package :cl-cc/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun grammar-parse (source)
  "Parse SOURCE and return the first CST form (or nil)."
  (first (cl-cc::parse-cl-source source)))

(defun grammar-parse-all (source)
  "Parse SOURCE and return the list of all CST forms."
  (cl-cc::parse-cl-source source))

;;; ─── parse-cl-atom — one case per token kind ─────────────────────────────────

(it-sequential "parse-cl-atom-token-kinds integer"
  (destructuring-bind (source expected-kind) (list "42" :T-INT)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-token-kinds float"
  (destructuring-bind (source expected-kind) (list "3.14" :T-FLOAT)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-token-kinds string"
  (destructuring-bind (source expected-kind) (list "\"hi\"" :T-STRING)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-token-kinds keyword"
  (destructuring-bind (source expected-kind) (list ":foo" :T-KEYWORD)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-token-kinds symbol"
  (destructuring-bind (source expected-kind) (list "bar" :T-IDENT)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-token-kinds bool-true"
  (destructuring-bind (source expected-kind) (list "#t" :T-BOOL-TRUE)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-token-kinds bool-false"
  (destructuring-bind (source expected-kind) (list "#f" :T-BOOL-FALSE)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-atom-integer-captures-value"
  (expect (= 99 (cl-cc::cst-token-value (grammar-parse "99"))) :to-be-truthy))

(it-sequential "parse-cl-atom-symbol-has-symbol-value"
  (let ((node (grammar-parse "some-var")))
    (expect (cl-cc::cst-token-p node) :to-be-truthy)
    (expect (symbolp (cl-cc::cst-token-value node)) :to-be-truthy)))

;;; ─── parse-cl-form — prefix sugar forms ─────────────────────────────────────

(it-sequential "parse-cl-form-prefix-kinds quote"
  (destructuring-bind (source expected-kind) (list "'x" :quote)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-form-prefix-kinds backquote"
  (destructuring-bind (source expected-kind) (list "`x" :quasiquote)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-form-prefix-kinds quasiquote-unquote"
  (destructuring-bind (source expected-kind) (list "`(,x)" :quasiquote)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-form-prefix-kinds function"
  (destructuring-bind (source expected-kind) (list "#'car" :function)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-form-vector-with-elements"
  (let ((node (grammar-parse "#(1 2 3)")))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be :vector)
    (expect (= 3 (length (cl-cc::cst-interior-children node))) :to-be-truthy)))

(it-sequential "parse-cl-form-empty-vector"
  (let ((node (grammar-parse "#()")))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be :vector)
    (expect (= 0 (length (cl-cc::cst-interior-children node))) :to-be-truthy)))

;;; ─── parse-cl-list-form — list kinds ────────────────────────────────────────

(it-sequential "parse-cl-list-empty-is-list-kind"
  (let ((node (grammar-parse "()")))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be :list)
    (expect (= 0 (length (cl-cc::cst-interior-children node))) :to-be-truthy)))

(it-sequential "parse-cl-list-call-has-correct-children"
  (let ((node (grammar-parse "(foo 1 2)")))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be :call)
    (expect (= 3 (length (cl-cc::cst-interior-children node))) :to-be-truthy)))

(it-sequential "parse-cl-list-special-form-kinds defun"
  (destructuring-bind (source expected-kind) (list "(defun f (x) x)" :defun)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds let"
  (destructuring-bind (source expected-kind) (list "(let ((x 1)) x)" :let)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds let*"
  (destructuring-bind (source expected-kind) (list "(let* ((x 1)) x)" :let*)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds if"
  (destructuring-bind (source expected-kind) (list "(if t 1 2)" :if)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds lambda"
  (destructuring-bind (source expected-kind) (list "(lambda (x) x)" :lambda)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds block"
  (destructuring-bind (source expected-kind) (list "(block done 1)" :block)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds progn"
  (destructuring-bind (source expected-kind) (list "(progn 1 2)" :progn)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds setq"
  (destructuring-bind (source expected-kind) (list "(setq x 1)" :setq)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-special-form-kinds quote"
  (destructuring-bind (source expected-kind) (list "(quote x)" :call)
    (let ((node (grammar-parse source)))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-cl-list-dotted-pair"
  (let ((node (grammar-parse "(a . b)")))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (cl-cc::cst-node-kind node) :to-be :dotted-list)
    (expect (= 2 (length (cl-cc::cst-interior-children node))) :to-be-truthy)))

(it-sequential "parse-cl-list-nested-forms"
  (let ((node (grammar-parse "(+ (* 2 3) (- 5 1))")))
    (expect (cl-cc::cst-interior-p node) :to-be-truthy)
    (expect (= 3 (length (cl-cc::cst-interior-children node))) :to-be-truthy)
    ;; Second child is (* 2 3)
    (let ((child (second (cl-cc::cst-interior-children node))))
      (expect (cl-cc::cst-interior-p child) :to-be-truthy)
      (expect (= 3 (length (cl-cc::cst-interior-children child))) :to-be-truthy))))

;;; ─── parse-cl-source — multi-form and byte positions ─────────────────────────

(it-sequential "parse-cl-source-multi-and-empty"
  (let ((forms (grammar-parse-all "1 2 3")))
    (expect (= 3 (length forms)) :to-be-truthy)
    (dolist (f forms) (expect (cl-cc::cst-token-p f) :to-be-truthy)))
  (expect (grammar-parse-all "") :to-be-null))

(it-sequential "parse-cl-source-diagnostics-on-error"
  (multiple-value-bind (forms diags)
      (cl-cc::parse-cl-source "(unclosed")
    (declare (ignore forms))
    (expect (listp diags) :to-be-truthy)))

(it-sequential "parse-cl-source-atom-byte-span"
  (let ((node (grammar-parse "42")))
    (expect (= 0 (cl-cc::cst-node-start-byte node)) :to-be-truthy)
    (expect (= 2 (cl-cc::cst-node-end-byte node)) :to-be-truthy)))

(it-sequential "parse-cl-source-list-byte-span"
  (let ((node (grammar-parse "(+ 1 2)")))
    (expect (= 0 (cl-cc::cst-node-start-byte node)) :to-be-truthy)
    (expect (= 7 (cl-cc::cst-node-end-byte node)) :to-be-truthy)))
