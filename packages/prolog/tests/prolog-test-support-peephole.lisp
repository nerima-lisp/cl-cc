(in-package :cl-cc/test)

(defmacro assert-prolog-peephole= (input expected)
  "Assert that applying the peephole optimizer to INPUT yields EXPECTED."
  `(assert-equal ,expected (cl-cc:apply-prolog-peephole ,input)))

(defmacro assert-prolog-peephole-walk= (input out expected)
  "Assert that the internal peephole walker returns EXPECTED for INPUT and OUT."
  `(assert-equal ,expected (cl-cc/optimize::%peephole-walk ,input ,out)))
