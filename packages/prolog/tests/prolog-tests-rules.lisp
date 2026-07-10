(in-package :cl-cc/test)

(in-suite cl-cc-prolog-integration-suite)

;;; User-defined predicates via def-rule.

(deftest prolog-def-rule-fact-queryable
  "def-rule with an empty body registers a ground fact that query-all can find"
  (with-prolog-facts ((likes alice bob) (likes bob carol))
    (assert-prolog-query-count= '(likes ?who bob) 1)))

(deftest prolog-def-rule-one-level
  "def-rule resolves one inference step"
  (with-prolog-rules (((child ?c ?p) (parent ?p ?c)))
    (with-prolog-facts ((parent tom mary) (parent tom john))
      (assert-prolog-query-count= '(child ?c tom) 2))))

(deftest prolog-def-rule-transitive
  "def-rule supports multi-hop inference"
  (with-prolog-rules (((ancestor ?a ?d) (parent ?a ?d))
                      ((ancestor ?a ?d) (parent ?a ?m) (ancestor ?m ?d)))
    (with-prolog-facts ((parent tom mary) (parent mary ann))
      (assert-prolog-query-count= '(ancestor tom ?d) 2))))
