;;;; tests/unit/parse/combinator-tests.lisp — Parser Combinator Engine Tests
;;;;
;;;; Tests for the grammar-driven parser combinator engine: token stream
;;;; protocol, grammar rule database, and all combinator primitives.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-tok (type value)
  "Create a plist token."
  (list :type type :value value))

(defun toks (&rest specs)
  "Build a token stream from (type value) pairs.
   Example: (toks :T-INT 1 :T-PLUS \"+\" :T-INT 2)"
  (loop for (type val) on specs by #'cddr
        collect (make-tok type val)))

(defmacro assert-combinator-boolean-case (expected form)
  `(if ,expected
       (expect ,form :to-be-truthy)
       (expect ,form :to-be-falsy)))

;;; ─── Token Stream Protocol ──────────────────────────────────────────────────

(it-sequential "comb-stream-empty-p-behavior nil-stream"
  (destructuring-bind (stream expected) (list nil t)
    (assert-combinator-boolean-case
      expected
    (cl-cc/parse::stream-empty-p stream))))

(it-sequential "comb-stream-empty-p-behavior non-empty-stream"
  (destructuring-bind (stream expected) (list (toks :T-INT 1) nil)
    (assert-combinator-boolean-case
      expected
    (cl-cc/parse::stream-empty-p stream))))

(it-sequential "comb-stream-peek"
  (let ((s (toks :T-INT 42)))
    (let ((tok (cl-cc/parse::stream-peek s)))
      (expect (cl-cc/parse::tok-type tok) :to-equal :T-INT)
      (expect (cl-cc/parse::tok-value tok) :to-equal 42)
      ;; Stream unchanged
      (expect (length s) :to-equal 1))))

(it-sequential "comb-stream-consume"
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (tok rest) (cl-cc/parse::stream-consume s)
      (expect (cl-cc/parse::tok-value tok) :to-equal 1)
      (expect (length rest) :to-equal 1))))

(it-sequential "comb-stream-consume-empty"
  (multiple-value-bind (tok rest) (cl-cc/parse::stream-consume nil)
    (expect tok :to-be-null)
    (expect rest :to-be-null)))

;;; ─── Grammar Rule Database ──────────────────────────────────────────────────

(it-sequential "comb-grammar-rule-lifecycle"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :test-rule cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((rule (cl-cc/parse:query-grammar :test-rule)))
      (expect (consp rule) :to-be-truthy)
      (expect (first rule) :to-equal 'token)))
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (expect (cl-cc/parse:query-grammar :nonexistent) :to-be-null))
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :foo cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (cl-cc/parse:clear-grammar-rules)
    (expect (cl-cc/parse:query-grammar :foo) :to-be-null)))

;;; ─── parse-ok-p ─────────────────────────────────────────────────────────────

(it-sequential "comb-parse-ok-p-behavior integer"
  (destructuring-bind (value expected) (list 42 t)
    (assert-combinator-boolean-case
      expected
    (cl-cc/parse:parse-ok-p value))))

(it-sequential "comb-parse-ok-p-behavior nil"
  (destructuring-bind (value expected) (list nil t)
    (assert-combinator-boolean-case
      expected
    (cl-cc/parse:parse-ok-p value))))

(it-sequential "comb-parse-ok-p-behavior list"
  (destructuring-bind (value expected) (list '(1 2 3) t)
    (assert-combinator-boolean-case
      expected
    (cl-cc/parse:parse-ok-p value))))

(it-sequential "comb-parse-ok-p-behavior fail"
  (destructuring-bind (value expected) (list :fail nil)
    (assert-combinator-boolean-case
      expected
    (cl-cc/parse:parse-ok-p value))))

;;; ─── parse-token* ───────────────────────────────────────────────────────────

(it-sequential "comb-token-match-type"
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* :T-INT nil s)
      (expect ast :to-equal 42)
      (expect rest :to-be-null))))

(it-sequential "comb-token-match-type-and-value"
  (let ((s (toks :T-IDENT "foo")))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* :T-IDENT "foo" s)
      (expect ast :to-equal "foo")
      (expect rest :to-be-null))))

(it-sequential "comb-token-mismatch-cases type-mismatch"
  (destructuring-bind (stream tok-type tok-val) (list (toks :T-INT 1) :T-IDENT nil)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* tok-type tok-val stream)
    (declare (ignore rest))
    (expect ast :to-equal :fail))))

(it-sequential "comb-token-mismatch-cases value-mismatch"
  (destructuring-bind (stream tok-type tok-val) (list (toks :T-IDENT "bar") :T-IDENT "foo")
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* tok-type tok-val stream)
    (declare (ignore rest))
    (expect ast :to-equal :fail))))

(it-sequential "comb-token-mismatch-cases empty-stream"
  (destructuring-bind (stream tok-type tok-val) (list nil :T-INT nil)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* tok-type tok-val stream)
    (declare (ignore rest))
    (expect ast :to-equal :fail))))

;;; ─── parse-literal* ─────────────────────────────────────────────────────────

(it-sequential "comb-literal-match"
  (let ((s (toks :T-IDENT "if")))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-literal* "if" s)
      (expect ast :to-equal "if")
      (expect rest :to-be-null))))

(it-sequential "comb-literal-mismatch"
  (let ((s (toks :T-IDENT "else")))
    (multiple-value-bind (ast _rest) (cl-cc/parse::parse-literal* "if" s)
      (declare (ignore _rest))
      (expect ast :to-equal :fail))))

;;; ─── parse-seq* ─────────────────────────────────────────────────────────────

(it-sequential "comb-seq-all-match"
  (let ((s (toks :T-INT 1 :T-IDENT "+" :T-INT 2)))
    (multiple-value-bind (ast rest)
        (cl-cc/parse::parse-seq* '((token :T-INT) (token :T-IDENT) (token :T-INT)) s)
      (expect ast :to-equal '(1 "+" 2))
      (expect rest :to-be-null))))

(it-sequential "comb-seq-partial-fail"
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (ast rest)
        (cl-cc/parse::parse-seq* '((token :T-INT) (token :T-IDENT)) s)
      (expect ast :to-equal :fail)
      (expect rest :to-be-null))))

(it-sequential "comb-seq-empty"
  (let ((s (toks :T-INT 1)))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-seq* nil s)
      (expect ast :to-equal nil)
      (expect (length rest) :to-equal 1))))

;;; ─── parse-alt* ─────────────────────────────────────────────────────────────

(it-sequential "comb-alt-matching-behavior first-matches"
  (destructuring-bind (tokens alts expected-ast success-p) (list (toks :T-INT 42) '((token :T-INT) (token :T-IDENT)) 42 t)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-alt* alts tokens)
    (expect ast :to-equal expected-ast)
    (when success-p
      (expect rest :to-be-null)))))

(it-sequential "comb-alt-matching-behavior second-matches"
  (destructuring-bind (tokens alts expected-ast success-p) (list (toks :T-IDENT "x") '((token :T-INT) (token :T-IDENT)) "x" t)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-alt* alts tokens)
    (expect ast :to-equal expected-ast)
    (when success-p
      (expect rest :to-be-null)))))

(it-sequential "comb-alt-matching-behavior none-match"
  (destructuring-bind (tokens alts expected-ast success-p) (list (toks :T-IDENT "x") '((token :T-INT) (token :T-PLUS)) :fail nil)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-alt* alts tokens)
    (expect ast :to-equal expected-ast)
    (when success-p
      (expect rest :to-be-null)))))

;;; ─── parse-many* ────────────────────────────────────────────────────────────

(it-sequential "comb-many-behavior zero"
  (destructuring-bind (tokens expected-ast expected-rest-len) (list (toks :T-IDENT "x") nil 1)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-many* '(token :T-INT) tokens)
    (expect ast :to-equal expected-ast)
    (expect (= expected-rest-len (length rest)) :to-be-truthy))))

(it-sequential "comb-many-behavior all"
  (destructuring-bind (tokens expected-ast expected-rest-len) (list (toks :T-INT 1 :T-INT 2 :T-INT 3) '(1 2 3) 0)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-many* '(token :T-INT) tokens)
    (expect ast :to-equal expected-ast)
    (expect (= expected-rest-len (length rest)) :to-be-truthy))))

(it-sequential "comb-many-behavior partial"
  (destructuring-bind (tokens expected-ast expected-rest-len) (list (toks :T-INT 1 :T-INT 2 :T-IDENT "x") '(1 2) 1)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-many* '(token :T-INT) tokens)
    (expect ast :to-equal expected-ast)
    (expect (= expected-rest-len (length rest)) :to-be-truthy))))

;;; ─── parse-many1* ───────────────────────────────────────────────────────────

(it-sequential "comb-many1-behavior one"
  (destructuring-bind (tokens expected-ast expected-rest-len success-p) (list (toks :T-INT 1 :T-IDENT "x") '(1) 1 t)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-many1* '(token :T-INT) tokens)
    (if success-p
        (progn
          (expect ast :to-equal expected-ast)
          (expect (= expected-rest-len (length rest)) :to-be-truthy))
        (expect ast :to-equal :fail)))))

(it-sequential "comb-many1-behavior multiple"
  (destructuring-bind (tokens expected-ast expected-rest-len success-p) (list (toks :T-INT 1 :T-INT 2) '(1 2) 0 t)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-many1* '(token :T-INT) tokens)
    (if success-p
        (progn
          (expect ast :to-equal expected-ast)
          (expect (= expected-rest-len (length rest)) :to-be-truthy))
        (expect ast :to-equal :fail)))))

(it-sequential "comb-many1-behavior zero"
  (destructuring-bind (tokens expected-ast expected-rest-len success-p) (list (toks :T-IDENT "x") :fail nil nil)
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-many1* '(token :T-INT) tokens)
    (if success-p
        (progn
          (expect ast :to-equal expected-ast)
          (expect (= expected-rest-len (length rest)) :to-be-truthy))
        (expect ast :to-equal :fail)))))
