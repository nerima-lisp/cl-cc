(in-package :cl-cc/test)

(in-suite cl-cc-prolog-integration-suite)

;;; Built-in predicates and unification helpers.

(deftest prolog-builtin-unification-succeeds
  "Built-in = unifies and calls continuation once"
  (assert-prolog-goal-results= '(= ?x 42) '(42) ?x))

(deftest-each prolog-non-unification-cases
  "Built-in /=: succeeds when terms differ, fails when equal"
  :cases (("succeeds" '(/= 1 2) 1)
          ("fails"    '(/= 1 1) 0)
          ("unbound vars" '(/= ?x ?y) 0))
  (goal expected-count)
  (assert-prolog-query-count= goal expected-count))

(deftest prolog-builtin-conjunction
  "Built-in 'and' chains goals and accumulates bindings"
  (assert-prolog-goal-results= '(and (= ?x 1) (= ?y 2)) '((1 2)) (list ?x ?y)))

(deftest prolog-builtin-disjunction
  "Built-in 'or' tries each alternative"
  (assert-prolog-query-count= '(or (= ?x 1) (= ?x 2)) 2))

(deftest prolog-unknown-predicate-fails-cleanly
  "An unknown predicate yields no solutions instead of erroring."
  (assert-prolog-query-count= '(nonexistent-predicate-xyz) 0))

(deftest-each prolog-builtin-when
  "Built-in :when: succeeds for truthy, fails for falsy"
  :cases (("true"  '(:when t)   1)
          ("false" '(:when nil) 0))
  (goal expected-count)
  (assert-prolog-query-count= goal expected-count))

(deftest prolog-builtin-when-quotes-symbol-bindings
  "Built-in :when preserves symbol bindings by quoting substituted symbols for CL eval."
  (assert-prolog-query-count= '(and (= ?x foo) (:when (eq ?x 'foo))) 1))

(deftest prolog-unify-handler-behavior
  "prolog-unify-handler succeeds only when terms can be unified."
  (assert-equal '(((?x . 1)))
                (%collect-prolog-projections
                 (lambda (emit)
                   (funcall #'cl-cc/prolog::prolog-unify-handler
                            '(?x 1)
                            nil
                            emit))
                 (lambda (env) env)))
  (assert-equal nil
                (%collect-prolog-projections
                 (lambda (emit)
                   (funcall #'cl-cc/prolog::prolog-unify-handler
                            '(?x (?x))
                            nil
                            emit))
                 (lambda (env) env))))

(deftest prolog-when-unify-succeeds-gates-body-on-success
  "when-unify-succeeds only runs its body when unification succeeds."
  (assert-prolog-unify-gate= :success t '?x 1 :expected-env '((?x . 1)))
  (assert-prolog-unify-gate= :success nil '?x '(?x)))

(deftest prolog-when-unify-fails-gates-body-on-failure
  "when-unify-fails only runs its body when unification fails."
  (assert-prolog-unify-gate= :failure t '?x '(?x))
  (assert-prolog-unify-gate= :failure nil '?x 1))

(deftest-each prolog-logic-var-p
  "logic-var-p recognises ?-prefixed symbols as logic variables."
  :cases (("?x"   '?x   t)
          ("?foo" '?foo t)
          ("x"    'x    nil)
          ("42"   42    nil)
          ("nil"  nil   nil))
  (term expected)
  (if expected
      (assert-true (cl-cc/prolog:logic-var-p term))
      (assert-false (cl-cc/prolog:logic-var-p term))))

(deftest-each prolog-occurs-check-simple-cases
  "occurs-check: same-variable, nested structure, and absent cases with empty env."
  :cases (("self"   t   '?x '?x    nil)
          ("nested" t   '?x '(?x 1) nil)
          ("absent" nil '?x 42      nil))
  (expected var term env)
  (if expected
      (assert-true (cl-cc/prolog::occurs-check var term env))
      (assert-false (cl-cc/prolog::occurs-check var term env))))

(deftest prolog-occurs-check-via-binding
  "occurs-check: follows binding chain when ?y is bound to ?x."
  (let ((env (list (cons '?y '?x))))
    (assert-true (cl-cc/prolog::occurs-check '?x '?y env))))

(deftest-each prolog-unify-atoms
  "unify: atomic terms unify iff they are equal."
  :cases (("same int" 42 42 nil)
          ("same sym" 'a  'a  nil)
          ("diff ints" 1  2   :unify-fail)
          ("int vs sym" 42 'a :unify-fail))
  (t1 t2 expected)
  (assert-equal expected (cl-cc/prolog:unify t1 t2 nil)))

(deftest prolog-unify-variable-binding
  "unify: logic variable binds to atom; occurs-check prevents circular unification."
  (assert-equal '((?x . 42)) (cl-cc/prolog:unify '?x 42 nil))
  (assert-equal :unify-fail (cl-cc/prolog:unify '?x '(?x) nil)))

(deftest prolog-unify-two-vars-aliased
  "unify: two distinct vars become aliased."
  (assert-equal '((?x . ?y)) (cl-cc/prolog:unify '?x '?y nil))
  (let ((env (cl-cc/prolog:unify '?x 99 '((?x . ?y)))))
    (assert-equal 99 (cl-cc/prolog:logic-substitute '?y env))))

(deftest prolog-unify-list-structure
  "unify: cons structures are unified component-wise."
  (assert-prolog-goal-results= '(= (?x 2) (1 ?y)) '((1 2)) (list ?x ?y)))

(deftest-each prolog-logic-substitute-cases
  "logic-substitute handles atoms, bound and unbound variables correctly."
  :cases (("atom-integer" 42       nil                   42)
          ("atom-symbol"  'hello   nil                   'hello)
          ("bound-var"    '?x      (list (cons '?x 99))  99)
          ("unbound-var"  '?unbound nil                   '?unbound))
  (input env expected)
  (assert-equal expected (cl-cc/prolog:logic-substitute input env)))

(deftest prolog-logic-substitute-traverses-structure-and-chains
  "logic-substitute traverses nested cons structure and follows variable chains."
  (let ((env (cl-cc/prolog:unify '?x '(a b c))))
    (assert-equal '((a b c) ?y) (cl-cc/prolog:logic-substitute '(?x ?y) env)))
  (let ((e1 (cl-cc/prolog:unify '?a '?b)))
    (let ((e2 (cl-cc/prolog:unify '?b 7 e1)))
      (assert-equal 7 (cl-cc/prolog:logic-substitute '?a e2)))))

(deftest-each prolog-rename-variables-behavior
  "rename-variables produces fresh symbols distinct from originals and consistent across head/body."
  :cases (("fresh-head-var"
           :fresh-head-var
           '(foo ?x ?y)
           '((bar ?x ?z)))
          ("shared-head-body-var"
           :shared-head-body-var
           '(foo ?x)
           '((bar ?x))))
  (assertion head body)
  (let ((rule (cl-cc/prolog::rename-variables
               (cl-cc/prolog::make-prolog-rule :head head :body body))))
    (ecase assertion
      (:fresh-head-var
       (let ((renamed-var (second (cl-cc/prolog::rule-head rule))))
         (assert-false (eq (second head) renamed-var))
         (assert-true (cl-cc/prolog:logic-var-p renamed-var))))
      (:shared-head-body-var
       (assert-true (eq (second (cl-cc/prolog::rule-head rule))
                        (second (first (cl-cc/prolog::rule-body rule)))))))))
