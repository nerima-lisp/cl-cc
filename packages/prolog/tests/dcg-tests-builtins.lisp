;;;; tests/unit/prolog/dcg-tests-builtins.lisp — DCG Builtin Tests
;;;;
;;;; Builtin combinators and error-recovery behavior.

(in-package :cl-cc/test)

(in-suite dcg-suite)

;;; ─── DCG Builtins via solver helpers ───────────────────────────────────────

(deftest-each dcg-alt-matches-first-or-second-rule
  "dcg-alt succeeds via the first matching rule and falls through to the second."
  :cases (("first rule matches" :result= (dcg-input :T-INT 42) (list nil))
          ("second rule matches" :solves  (dcg-input :T-IDENT "x") nil))
  (assertion input expected)
  (%with-prolog-fixture
    (cl-cc/prolog:def-rule (rule-a ((:T-INT . ?v) . ?rest) ?rest))
    (cl-cc/prolog:def-rule (rule-b ((:T-IDENT . ?v) . ?rest) ?rest))
    (let ((goal-form (list 'cl-cc/prolog::dcg-alt 'rule-a 'rule-b input '?out)))
      (%with-prolog-goal-results/internal (results goal-form nil '?out)
        (ecase assertion
          (:result=
           (assert-equal expected results))
          (:contains
           (assert-true (member expected results :test #'equal)))
          (:solves
           (assert-true results))
          (:fails
           (assert-prolog-query-count= goal-form 0)))))))

(deftest-each dcg-alt-rejects-malformed-arguments
  "dcg-alt requires at least one alternative plus input/output streams."
  :cases (("missing alternative" '(?in ?out))
          ("missing output stream" '(rule-a ?in)))
  (args)
  (assert-prolog-call-arguments-rejected cl-cc/prolog::%dcg-alt
                                         args
                                         nil
                                         (lambda (env) env)))

(deftest-each dcg-opt-succeeds-with-match-or-epsilon
  "dcg-opt includes nil on a match and leaves the stream unchanged on a mismatch."
  :cases (("match"   :contains (dcg-input :T-INT 1) nil)
          ("mismatch" :solves   (dcg-input :T-IDENT "x") nil))
  (assertion input expected)
  (%with-prolog-fixture
    (cl-cc/prolog:def-rule (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((goal-form (list 'cl-cc/prolog::dcg-opt 'tok-int input '?out)))
      (%with-prolog-goal-results/internal (results goal-form nil '?out)
        (ecase assertion
          (:result=
           (assert-equal expected results))
          (:contains
           (assert-true (member expected results :test #'equal)))
          (:solves
           (assert-true results))
          (:fails
           (assert-prolog-query-count= goal-form 0)))))))

(deftest-each dcg-star-succeeds-or-collects-matches
  "dcg-star succeeds with zero matches and collects all consecutive matches."
  :cases (("zero matches"   :solves   (dcg-input :T-IDENT "x") nil)
          ("many matches"    :contains (dcg-input :T-INT 1 :T-INT 2 :T-INT 3) nil))
  (assertion input expected)
  (%with-prolog-fixture
    (cl-cc/prolog:def-rule (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((goal-form (list 'cl-cc/prolog::dcg-star 'tok-int input '?out)))
      (%with-prolog-goal-results/internal (results goal-form nil '?out)
        (ecase assertion
          (:result=
           (assert-equal expected results))
          (:contains
           (assert-true (member expected results :test #'equal)))
          (:solves
           (assert-true results))
          (:fails
           (assert-prolog-query-count= goal-form 0)))))))

(deftest-each dcg-plus-succeeds-or-fails
  "dcg-plus succeeds with at least one match and fails when there are none."
  :cases (("one match"   :solves (dcg-input :T-INT 42))
          ("zero matches" :fails  (dcg-input :T-IDENT "x")))
  (assertion input)
  (%with-prolog-fixture
    (cl-cc/prolog:def-rule (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((goal-form (list 'cl-cc/prolog::dcg-plus 'tok-int input '?out)))
      (%with-prolog-goal-results/internal (results goal-form nil '?out)
        (ecase assertion
          (:result=
           (assert-equal nil results))
          (:contains
           (assert-true (member nil results :test #'equal)))
          (:solves
           (assert-true results))
          (:fails
           (assert-prolog-query-count= goal-form 0)))))))

;;; ─── dcg-error-recovery ─────────────────────────────────────────────────────

(deftest dcg-error-recovery-skips-to-sync-token
  "dcg-error-recovery skips tokens until it finds a sync token (:T-RPAREN)."
  (let ((goal-form (list 'cl-cc/prolog::dcg-error-recovery
                         (dcg-input :T-INT 1 :T-IDENT "x" :T-RPAREN ")")
                         '?out)))
    (%with-prolog-goal-results/internal (results goal-form nil '?out)
      (assert-equal (list (list (cons :T-RPAREN ")"))) results))))

(deftest dcg-error-recovery-skips-malformed-stream-heads
  "dcg-error-recovery treats malformed stream heads as ordinary skipped input."
  (let ((goal-form (list 'cl-cc/prolog::dcg-error-recovery
                         '(not-a-token (:T-RPAREN . ")"))
                         '?out)))
    (%with-prolog-goal-results/internal (results goal-form nil '?out)
      (assert-equal (list (list (cons :T-RPAREN ")"))) results))))

(deftest dcg-error-recovery-handles-empty-input
  "dcg-error-recovery succeeds on empty input, returning nil as the remaining stream."
  (let ((goal-form (list 'cl-cc/prolog::dcg-error-recovery nil '?out)))
    (%with-prolog-goal-results/internal (results goal-form nil '?out)
      (assert-true (member nil results :test #'equal)))))
