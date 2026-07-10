;;;; tests/unit/prolog/dcg-tests-core.lisp — DCG Engine Entry-Point Tests
;;;;
;;;; Shared DCG suite definition plus token/phrase entry-point coverage.

(in-package :cl-cc/test)

(in-suite cl-cc-serial-suite)

(defsuite dcg-suite :description "DCG engine unit tests"
  :parent cl-cc-serial-suite)

(in-suite dcg-suite)

(deftest dcg-token-match-value-binds-scalar-token-values
  "dcg-token-match-value binds the token value and preserves the remaining stream."
  (%with-prolog-fixture
    (let ((goal-form (list 'cl-cc/prolog::dcg-token-match-value
                           :T-INT
                           '?value
                           (dcg-input :T-INT 42 :T-EOF nil)
                           '?out)))
      (%with-prolog-goal-results/internal (results goal-form nil (list ?value ?out))
        (assert-equal '((42 ((:T-EOF . nil)))) results)))))

(deftest-each dcg-token-match-fails-when-stream-head-does-not-match
  "dcg-token-match only succeeds when the stream head is a token of the expected type."
  :cases (("wrong token type" (dcg-input :T-IDENT "x"))
          ("malformed stream head" '(not-a-token (:T-INT . 1))))
  (input)
  (%with-prolog-fixture
    (let ((goal-form (list 'cl-cc/prolog::dcg-token-match :T-INT input '?out)))
      (%with-prolog-goal-results/internal (results goal-form nil '?out)
        (assert-true (null results))))))

;;; ─── phrase ─────────────────────────────────────────────────────────────────

(deftest phrase-returns-remaining-input-after-match
  "phrase returns the remaining token list after a successful rule match."
  (with-prolog-facts ((consume-one (?h . ?rest) ?rest))
    (assert-equal '((:T-INT . 2))
                  (cl-cc/prolog:phrase 'consume-one
                                       (dcg-input :T-INT 1 :T-INT 2)))))

(deftest phrase-returns-nil-when-no-rule-matches
  "phrase returns nil when no rule matches the input."
  (with-prolog-facts ((never-match impossible-sentinel ?rest))
    (assert-equal nil
                  (cl-cc/prolog:phrase 'never-match
                                       (dcg-input :T-INT 1)))))

(deftest phrase-returns-first-of-multiple-solutions
  "phrase stops after the first parse result even when more are available."
  (let ((input (dcg-input :T-INT 1 :T-INT 2 :T-INT 3)))
    (with-dcg-rules ((ambiguous (terminal :T-INT) (terminal :T-INT))
                     (ambiguous (terminal :T-INT)))
      (assert-equal (list (cons :T-INT 2)
                          (cons :T-INT 3))
                    (cl-cc/prolog:phrase 'ambiguous input)))))

(deftest phrase-all-multiple
  "phrase-all returns all possible parse results."
  (let ((input (dcg-input :T-INT 1 :T-INT 2 :T-INT 3)))
    (with-dcg-rules ((flexible (terminal :T-INT) (terminal :T-INT))
                     (flexible (terminal :T-INT)))
      (assert-equal '(((:T-INT . 2) (:T-INT . 3))
                      ((:T-INT . 3)))
                    (cl-cc/prolog:phrase-all 'flexible input)))))

(deftest dcg-rule-helpers-roundtrip
  "def-dcg-rule, phrase, and phrase-all agree on a simple rule."
  (let ((input (dcg-input :T-INT 1 :T-EOF nil)))
    (with-dcg-rules ((accept-all))
      (assert-equal input (cl-cc/prolog:phrase 'accept-all input))
      (assert-equal (list input) (cl-cc/prolog:phrase-all 'accept-all input)))))

(deftest dcg-rule-terminal-and-brace-forms
  "def-dcg-rule compiles terminal and brace bodies into a working parser."
  (let ((input (dcg-input :T-INT 1 :T-IDENT "x" :T-EOF nil)))
    (with-dcg-rules ((accept-token-and-check
                      (terminal :T-INT)
                      (brace t)
                      (terminal :T-IDENT)))
      (assert-equal (list (cons :T-EOF nil))
                    (cl-cc/prolog:phrase 'accept-token-and-check input)))))

(deftest dcg-fresh-counter-is-deterministic-from-bound-state
  "dcg-fresh-var is deterministic from a known counter binding."
  (let ((cl-cc/prolog::*dcg-counter* 0))
    (let ((first (cl-cc/prolog::dcg-fresh-var))
          (second (cl-cc/prolog::dcg-fresh-var)))
      (assert-false (eq first second))
      (let ((cl-cc/prolog::*dcg-counter* 0))
        (assert-true (eq first (cl-cc/prolog::dcg-fresh-var)))))))
