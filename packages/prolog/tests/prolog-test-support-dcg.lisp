(in-package :cl-cc/test)

(defmacro assert-dcg-phrase= (rule-name input expected)
  "Assert that PHRASE returns EXPECTED remaining input for RULE-NAME and INPUT."
  `(assert-equal ,expected (cl-cc:phrase ,rule-name ,input)))

(defmacro assert-dcg-phrase-all= (rule-name input expected)
  "Assert that PHRASE-ALL returns EXPECTED parse results for RULE-NAME and INPUT."
  `(assert-equal ,expected (cl-cc:phrase-all ,rule-name ,input)))

(defmacro assert-dcg-token-case ((selector goal rules goal-form) &body clauses)
  "Install token rules, bind GOAL to GOAL-FORM, and dispatch SELECTOR."
  `(with-dcg-token-rules ,rules
     (let ((,goal ,goal-form))
       (ecase ,selector
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (case-key &body body) clause
                       `(,case-key (progn ,@body))))
                   clauses)))))

(defmacro assert-dcg-solves (goal &optional projection-var)
  "Assert that GOAL has at least one substitution."
  `(%with-prolog-goal-results (results ,goal ,projection-var)
     (assert-true results)))

(defmacro assert-dcg-result-contains (goal expected &optional projection-var)
  "Assert that GOAL returns EXPECTED in its remaining streams."
  `(%with-prolog-goal-results (results ,goal ,projection-var)
     (assert-true (member ,expected results :test #'equal))))
