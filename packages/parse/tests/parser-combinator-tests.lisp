;;;; tests/unit/frontend/parser-combinator-tests.lisp
;;;; Unit tests for the grammar-driven parser combinator engine

(in-package :cl-cc/test)


;;; Helpers

(defun make-tok (type value)
  "Create a test token plist."
  (list :type type :value value))

;;; Grammar rule management

(it-sequential "grammar-rule-management"
  (clear-grammar-rules)
  (def-grammar-rule :test-rule (token :T-INT))
  (expect (query-grammar :test-rule) :to-equal '(token :T-INT))
  (expect (query-grammar :no-such-rule) :to-be-null)
  (clear-grammar-rules))

;;; Token matching

(it-sequential "parse-token-succeeds-on-matching-type"
  (let ((stream (list (make-tok :T-INT 42))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect (= 42 ast) :to-be-truthy)
      (expect rest :to-be-null))))

(it-sequential "parse-token-succeeds-with-value-constraint"
  (let ((stream (list (make-tok :T-OP "+") (make-tok :T-INT 1))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-OP "+") stream)
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect ast :to-equal "+")
      (expect (= 1 (length rest)) :to-be-truthy))))

(it-sequential "parse-token-fails-on-wrong-type"
  (let ((stream (list (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (declare (ignore rest))
      (expect (parse-ok-p ast) :to-be-falsy))))

(it-sequential "parse-token-fails-on-empty-stream"
  (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) nil)
    (declare (ignore rest))
    (expect (parse-ok-p ast) :to-be-falsy)))

;;; Sequence

(it-sequential "parse-seq-succeeds-on-full-match"
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-OP "+") (make-tok :T-INT 2))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(seq (token :T-INT) (token :T-OP "+") (token :T-INT)) stream)
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect ast :to-equal '(1 "+" 2))
      (expect rest :to-be-null))))

(it-sequential "parse-seq-fails-on-partial-mismatch"
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(seq (token :T-INT) (token :T-OP "+")) stream)
      (declare (ignore rest))
      (expect (parse-ok-p ast) :to-be-falsy))))

;;; Alternation

(it-sequential "parse-alt-matches-first-branch"
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-INT 42)))
    (declare (ignore rest))
    (expect (parse-ok-p ast) :to-be-truthy)
    (expect (= 42 ast) :to-be-truthy)))

(it-sequential "parse-alt-matches-second-branch"
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-IDENT "foo")))
    (declare (ignore rest))
    (expect (parse-ok-p ast) :to-be-truthy)
    (expect ast :to-equal "foo")))

(it-sequential "parse-alt-fails-when-no-branch-matches"
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-OP "+")))
    (declare (ignore rest))
    (expect (parse-ok-p ast) :to-be-falsy)))

;;; Repetition

(it-sequential "parse-many-returns-nil-on-zero-matches"
  (let ((stream (list (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect ast :to-be-null)
      (expect (= 1 (length rest)) :to-be-truthy))))

(it-sequential "parse-many1-fails-on-zero-matches"
  (multiple-value-bind (ast rest)
      (parse-combinator '(many1 (token :T-INT)) (list (make-tok :T-IDENT "x")))
    (declare (ignore rest))
    (expect (parse-ok-p ast) :to-be-falsy)))

(it-sequential "parse-many-collects-all-matches"
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-INT 2) (make-tok :T-INT 3)
                      (make-tok :T-EOF nil))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect ast :to-equal '(1 2 3))
      (expect (= 1 (length rest)) :to-be-truthy)))
  (multiple-value-bind (ast rest)
      (parse-combinator '(many1 (token :T-INT)) (list (make-tok :T-INT 7) (make-tok :T-IDENT "x")))
    (declare (ignore rest))
    (expect (parse-ok-p ast) :to-be-truthy)
    (expect ast :to-equal '(7))))

;;; Optional

(it-sequential "parse-opt-returns-value-when-present"
  (multiple-value-bind (ast rest)
      (parse-combinator '(opt (token :T-INT)) (list (make-tok :T-INT 5)))
    (expect (parse-ok-p ast) :to-be-truthy)
    (expect (= 5 ast) :to-be-truthy)
    (expect rest :to-be-null)))

(it-sequential "parse-opt-returns-absent-sentinel-when-missing"
  (multiple-value-bind (ast rest)
      (parse-combinator '(opt (token :T-INT)) (list (make-tok :T-IDENT "x")))
    (expect ast :to-be :opt-absent)
    (expect (= 1 (length rest)) :to-be-truthy)))

;;; Named rule reference

(it-sequential "parse-named-rule-and-keyword-shorthand"
  (clear-grammar-rules)
  (def-grammar-rule :my-int (token :T-INT))
  (let ((stream (list (make-tok :T-INT 99))))
    (multiple-value-bind (ast rest) (parse-combinator '(rule :my-int) stream)
      (declare (ignore rest))
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect (= 99 ast) :to-be-truthy)))
  (def-grammar-rule :my-ident (token :T-IDENT))
  (let ((stream (list (make-tok :T-IDENT "hello"))))
    (multiple-value-bind (ast rest) (parse-combinator :my-ident stream)
      (declare (ignore rest))
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect ast :to-equal "hello")))
  (clear-grammar-rules))

;;; Integration: arithmetic expression grammar

(it-sequential "grammar-arithmetic-expression"
  (clear-grammar-rules)
  (def-grammar-rule :atom (alt (token :T-INT) (token :T-IDENT)))
  (def-grammar-rule :addop (alt (token :T-OP "+") (token :T-OP "-")))
  (def-grammar-rule :expr (seq (rule :atom) (many (seq (rule :addop) (rule :atom)))))
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-OP "+")
                      (make-tok :T-INT 2) (make-tok :T-OP "-")
                      (make-tok :T-INT 3))))
    (multiple-value-bind (ast rest) (parse-with-grammar :expr stream)
      (expect (parse-ok-p ast) :to-be-truthy)
      (expect rest :to-be-null)))
  (clear-grammar-rules))
