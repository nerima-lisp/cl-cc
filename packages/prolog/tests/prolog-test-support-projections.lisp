(in-package :cl-cc/test)

(defmacro assert-prolog-goal-results= (goal expected &optional projection-form)
  "Assert that GOAL returns EXPECTED as its full projected result list."
  `(%with-prolog-goal-results (results ,goal ,projection-form)
     (assert-equal ,expected results)))

(defmacro assert-prolog-query-count= (goal expected-count)
  "Assert that GOAL yields EXPECTED-COUNT solutions."
  `(%with-prolog-goal-results (results ,goal)
     (assert-= ,expected-count (length results))))

(defmacro assert-prolog-specs= (expected actual)
  "Assert that a Prolog spec table or spec list matches EXPECTED exactly."
  `(assert-equal ,expected ,actual))

(defmacro assert-prolog-rule= (rule expected-head expected-body)
  "Assert that RULE has EXPECTED-HEAD and EXPECTED-BODY."
  `(progn
     (assert-equal ,expected-head (cl-cc/prolog::rule-head ,rule))
     (assert-equal ,expected-body (cl-cc/prolog::rule-body ,rule))))

(defmacro assert-prolog-rule-store= (rules &rest expected-rules)
  "Assert that RULES matches EXPECTED-RULES in order."
  `(progn
     (assert-equal ,(length expected-rules) (length ,rules))
     ,@(loop for expected-rule in expected-rules
             for index from 0
             collect (destructuring-bind (expected-head expected-body)
                         expected-rule
                       `(assert-prolog-rule= (nth ,index ,rules)
                                             ,expected-head
                                             ,expected-body)))))
