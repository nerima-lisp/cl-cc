(in-package :cl-cc/test)

(in-suite cl-cc-prolog-integration-suite)

;;; Solver control flow: cut, conjunction, query-all.

(deftest prolog-cut-stops-backtracking
  "Cut (!) stops further alternatives in the parent clause"
  (with-prolog-rules (((first-color ?c) (color ?c) !))
    (with-prolog-facts ((color red) (color green) (color blue))
      (assert-prolog-query-count= '(first-color ?c) 1))))

(deftest prolog-member-behavior
  "member/2: hit, miss, and full enumeration"
  (assert-prolog-query-count= '(member 2  (cons 1 (cons 2 (cons 3 nil)))) 1)
  (assert-prolog-query-count= '(member 99 (cons 1 (cons 2 nil))) 0)
  (assert-prolog-query-count= '(member ?x (cons 1 (cons 2 (cons 3 nil)))) 3))

(deftest-each prolog-append-known-inputs
  "append/3: concatenating two cons-lists yields the expected result"
  :cases (("nil+3"   nil                         '(cons 1 (cons 2 (cons 3 nil))))
          ("one+two" '(cons a nil)               '(cons b (cons c nil))))
  (l1 l2)
  (assert-prolog-query-count= `(append ,l1 ,l2 ?r) 1))

(deftest-each prolog-reverse-queries
  "reverse/2: produces exactly one solution"
  :cases (("empty"     '(reverse nil ?r))
          ("singleton" '(reverse (cons 1 nil) ?r)))
  (goal)
  (assert-prolog-query-count= goal 1))

(deftest-each prolog-length-queries
  "length/2: produces exactly one solution"
  :cases (("empty" '(length nil ?n))
          ("three" '(length (cons a (cons b (cons c nil))) ?n)))
  (goal)
  (assert-prolog-query-count= goal 1))

(deftest prolog-solve-conjunction-empty-succeeds-once
  "solve-conjunction with no goals calls the continuation exactly once"
  (assert-prolog-goal-results= nil '(nil)))

(deftest prolog-solve-conjunction-accumulates-bindings
  "solve-conjunction chains goals, accumulating bindings"
  (assert-prolog-goal-results= '((= ?x 10) (= ?y 20)) '((10 20)) (list ?x ?y)))

(deftest prolog-rule-body-continuation-handles-facts-and-goals
  "Rule body evaluation emits fact envs and delegates non-empty bodies."
  (%with-prolog-goal-results/internal (results nil '((?x . 10)) ?x)
    (assert-equal '(10) results))
  (assert-prolog-goal-results= '((= ?x 20)) '(20) ?x)
  (assert-prolog-goal-results=
   '((= 1 2))
   nil))

(deftest prolog-query-all-count
  "query-all returns every solution"
  (assert-prolog-query-count= '(member ?x (cons 1 (cons 2 (cons 3 (cons 4 nil))))) 4))

(deftest prolog-query-all-accepts-raw-list-goals
  "query-all handles raw list goals through the solver path."
  (with-prolog-facts ((goal-shape 1 1))
    (assert-prolog-query-count= '(goal-shape 1 1) 1)))
