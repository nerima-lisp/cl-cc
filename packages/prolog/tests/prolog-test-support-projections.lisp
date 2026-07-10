(in-package :cl-cc/test)

(defmacro assert-prolog-goal-results= (goal expected &optional projection-form)
  "Assert that GOAL returns EXPECTED as its full projected result list."
  `(%with-prolog-goal-results/internal (results ,goal nil ,projection-form)
     (assert-equal ,expected results)))

(defmacro assert-prolog-query-count= (goal expected-count)
  "Assert that GOAL yields EXPECTED-COUNT solutions."
  `(%with-prolog-goal-results/internal (results ,goal nil nil)
     (assert-= ,expected-count (length results))))
