;;;; tests/unit/prolog/dcg-tests.lisp — DCG Engine Unit Tests
;;;;
;;;; Tests for the DCG (Definite Clause Grammar) parsing engine:
;;;; builtin predicates and public entry points (phrase, phrase-all).

(in-package :cl-cc/test)

(in-suite cl-cc-serial-suite)

(defsuite dcg-suite :description "DCG engine unit tests"
  :parent cl-cc-serial-suite)


(in-suite dcg-suite)

(deftest dcg-token-match-value-binds-scalar-token-values
  "dcg-token-match-value binds the token value and preserves the remaining stream."
  (%with-prolog-fixture
    (assert-prolog-goal-results=
     (dcg-goal cl-cc/prolog::dcg-token-match-value
               :T-INT '?value
               (dcg-input :T-INT 42 :T-EOF nil)
               '?out)
     '((42 ((:T-EOF . nil))))
     (list ?value ?out))))

;;; ─── phrase ─────────────────────────────────────────────────────────────────

(deftest phrase-returns-remaining-input-after-match
  "phrase returns the remaining token list after a successful rule match."
  (with-prolog-facts ((consume-one (?h . ?rest) ?rest))
    (assert-dcg-phrase= 'consume-one
                        (dcg-input :T-INT 1 :T-INT 2)
                        '((:T-INT . 2)))))

(deftest phrase-returns-nil-when-no-rule-matches
  "phrase returns nil when no rule matches the input."
  (with-prolog-facts ((never-match impossible-sentinel ?rest))
    (assert-null (cl-cc/prolog:phrase 'never-match (dcg-input :T-INT 1)))))

(deftest phrase-all-multiple
  "phrase-all returns all possible parse results."
  (with-prolog-facts ((flexible (?h . ?rest) ?rest)
                      (flexible (?a ?b . ?rest) ?rest))
    (assert-dcg-phrase-all= 'flexible
                            (dcg-input :T-INT 1 :T-INT 2 :T-INT 3)
                            '((( :T-INT . 3))
                              ((:T-INT . 2) (:T-INT . 3))))))

(deftest dcg-rule-helpers-roundtrip
  "def-dcg-rule, phrase, and phrase-all agree on a simple rule."
  (with-dcg-rules ((accept-all))
    (let ((input (dcg-input :T-INT 1 :T-EOF nil)))
      (assert-dcg-phrase= 'accept-all input input)
      (assert-dcg-phrase-all= 'accept-all input (list input)))))

(deftest dcg-rule-terminal-and-brace-forms
  "def-dcg-rule compiles terminal and brace bodies into a working parser."
  (with-dcg-rules ((accept-token-and-check
                    (terminal :T-INT)
                    (brace t)
                    (terminal :T-IDENT)))
    (let ((input (dcg-input :T-INT 1 :T-IDENT "x" :T-EOF nil)))
      (assert-dcg-phrase= 'accept-token-and-check
                          input
                          (list (cons :T-EOF nil))))))

(deftest dcg-fresh-counter-resets
  "dcg-fresh-var is deterministic after dcg-reset-counter."
  (cl-cc/prolog::dcg-reset-counter)
  (let ((first (cl-cc/prolog::dcg-fresh-var))
        (second (cl-cc/prolog::dcg-fresh-var)))
    (assert-false (eq first second))
    (cl-cc/prolog::dcg-reset-counter)
    (assert-eq first (cl-cc/prolog::dcg-fresh-var))))

;;; ─── DCG Builtins via solver helpers ───────────────────────────────────────

(deftest-each dcg-alt-matches-first-or-second-rule
  "dcg-alt succeeds via the first matching rule and falls through to the second."
  :cases (("first rule matches" :result= (dcg-input :T-INT 42) (list nil))
          ("second rule matches" :solves  (dcg-input :T-IDENT "x") nil))
  (assertion input expected)
  (assert-dcg-token-case (assertion goal ((rule-a :T-INT) (rule-b :T-IDENT))
                                   (dcg-goal cl-cc/prolog::dcg-alt
                                             'rule-a 'rule-b input '?out))
    (:result= (assert-prolog-goal-results= goal expected '?out))
    (:solves  (assert-dcg-solves goal '?out))))

(deftest-each dcg-opt-succeeds-with-match-or-epsilon
  "dcg-opt includes nil on a match and leaves the stream unchanged on a mismatch."
  :cases (("match"   :contains (dcg-input :T-INT 1) nil)
          ("mismatch" :solves   (dcg-input :T-IDENT "x") nil))
  (assertion input expected)
  (assert-dcg-token-case (assertion goal ((tok-int :T-INT))
                                   (dcg-goal cl-cc/prolog::dcg-opt
                                             'tok-int input '?out))
    (:contains (assert-dcg-result-contains goal expected '?out))
    (:solves   (assert-dcg-solves goal '?out))))

(deftest-each dcg-star-succeeds-or-collects-matches
  "dcg-star succeeds with zero matches and collects all consecutive matches."
  :cases (("zero matches"   :solves   (dcg-input :T-IDENT "x") nil)
          ("many matches"    :contains (dcg-input :T-INT 1 :T-INT 2 :T-INT 3) nil))
  (assertion input expected)
  (assert-dcg-token-case (assertion goal ((tok-int :T-INT))
                                   (dcg-goal cl-cc/prolog::dcg-star
                                             'tok-int input '?out))
    (:solves   (assert-dcg-solves goal '?out))
    (:contains (assert-dcg-result-contains goal expected '?out))))

(deftest-each dcg-plus-succeeds-or-fails
  "dcg-plus succeeds with at least one match and fails when there are none."
  :cases (("one match"   :solves (dcg-input :T-INT 42))
          ("zero matches" :fails  (dcg-input :T-IDENT "x")))
  (assertion input)
  (assert-dcg-token-case (assertion goal ((tok-int :T-INT))
                                  (dcg-goal cl-cc/prolog::dcg-plus
                                            'tok-int input '?out))
    (:solves (assert-dcg-solves goal '?out))
    (:fails  (assert-prolog-query-count= goal 0))))

;;; ─── dcg-error-recovery ─────────────────────────────────────────────────────

(deftest dcg-error-recovery-skips-to-sync-token
  "dcg-error-recovery skips tokens until it finds a sync token (:T-RPAREN)."
  (assert-prolog-goal-results=
   (dcg-goal cl-cc/prolog::dcg-error-recovery
             (dcg-input :T-INT 1 :T-IDENT "x" :T-RPAREN ")")
             '?out)
   (list (list (cons :T-RPAREN ")")))
   '?out))

(deftest dcg-error-recovery-handles-empty-input
  "dcg-error-recovery succeeds on empty input, returning nil as the remaining stream."
  (assert-dcg-result-contains
   (dcg-goal cl-cc/prolog::dcg-error-recovery nil '?out)
   nil
   '?out))
