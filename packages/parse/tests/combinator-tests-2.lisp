;;;; combinator-tests-2.lisp — Parser Combinator Engine Tests (continued)
;;;;
;;;; Tests for parse-opt*, parse-rule, parse-combinator dispatcher,
;;;; parse-with-grammar, and multi-rule integration scenarios.

(in-package :cl-cc/test)


;;; ─── parse-opt* ─────────────────────────────────────────────────────────────

(it-sequential "comb-opt-match"
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-opt* '(token :T-INT) s)
      (expect ast :to-equal 42)
      (expect rest :to-be-null))))

(it-sequential "comb-opt-absent"
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-opt* '(token :T-INT) s)
      (expect ast :to-equal :opt-absent)
      (expect (length rest) :to-equal 1))))

;;; ─── parse-rule ─────────────────────────────────────────────────────────────

(it-sequential "comb-parse-rule-defined"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :my-int cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 99)))
      (multiple-value-bind (ast rest) (cl-cc/parse::parse-rule :my-int s)
        (expect ast :to-equal 99)
        (expect rest :to-be-null)))))

(it-sequential "comb-parse-rule-undefined-error"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (signals error (cl-cc/parse::parse-rule :no-such-rule nil))))

;;; ─── parse-combinator (dispatcher) ─────────────────────────────────────────

(it-sequential "comb-dispatch-keyword-shorthand"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :num cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 7)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-combinator :num s)
        (expect ast :to-equal 7)
        (expect rest :to-be-null)))))

(it-sequential "comb-dispatch-token"
  (let ((s (toks :T-INT 5)))
    (multiple-value-bind (ast rest) (cl-cc/parse:parse-combinator '(token :T-INT) s)
      (expect ast :to-equal 5)
      (expect rest :to-be-null))))

(it-sequential "comb-dispatch-unknown-operator-error"
  (signals error (cl-cc/parse:parse-combinator '(bogus :T-INT) nil)))

(it-sequential "comb-dispatch-non-list-non-keyword-error"
  (signals error (cl-cc/parse:parse-combinator 42 nil)))

;;; ─── parse-with-grammar (top-level) ────────────────────────────────────────

(it-sequential "comb-parse-with-grammar-success"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :expr cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 100)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :expr s)
        (expect ast :to-equal 100)
        (expect rest :to-be-null)))))

(it-sequential "comb-parse-with-grammar-failure-error"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :expr cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-IDENT "x")))
      (signals error (cl-cc/parse:parse-with-grammar :expr s)))))

;;; ─── Integration: multi-rule grammar ────────────────────────────────────────

(it-sequential "comb-integration-arithmetic"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    ;; Grammar: expr → INT (PLUS INT)*
    (setf (gethash :add-pair cl-cc/parse:*grammar-rules*)
          '(seq (token :T-PLUS) (token :T-INT)))
    (setf (gethash :expr cl-cc/parse:*grammar-rules*)
          '(seq (token :T-INT) (many :add-pair)))
    (let ((s (toks :T-INT 1 :T-PLUS "+" :T-INT 2 :T-PLUS "+" :T-INT 3)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :expr s)
        ;; ast = (1 (("+" 2) ("+" 3)))
        (expect (listp ast) :to-be-truthy)
        (expect (first ast) :to-equal 1)
        (expect (length (second ast)) :to-equal 2)
        (expect rest :to-be-null)))))

(it-sequential "comb-integration-optional-semicolon"
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :stmt cl-cc/parse:*grammar-rules*)
          '(seq (token :T-INT) (opt (token :T-SEMI))))
    ;; With semicolon
    (let ((s (toks :T-INT 1 :T-SEMI ";")))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :stmt s)
        (expect (first ast) :to-equal 1)
        (expect (second ast) :to-equal ";")
        (expect rest :to-be-null)))
    ;; Without semicolon
    (let ((s (toks :T-INT 1)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :stmt s)
        (expect (first ast) :to-equal 1)
        (expect (second ast) :to-equal :opt-absent)
        (expect rest :to-be-null)))))
