(in-package :cl-cc/test)

(in-suite cl-cc-coverage-unstable-unit-suite)

;;; %peephole-walk unit tests (internal helper)

(deftest-each peephole-walk-direct-cases
  "%peephole-walk: nil→nil; singleton→singleton; non-matching pair passes through."
  :cases (("nil"       nil                              nil)
          ("singleton" '((:const :r0 1))                '((:const :r0 1)))
          ("no-match"  '((:add :r1 :r0 :r0) (:sub :r2 :r1 :r1))
                       '((:add :r1 :r0 :r0) (:sub :r2 :r1 :r1))))
  (input expected)
  (assert-equal expected (cl-cc/optimize::%peephole-walk input nil)))

(deftest peephole-walk-matching-pair-fused
  "%peephole-walk: a matching const→move pair is fused into a single const."
  (assert-equal '((:const :r1 99))
                (cl-cc/optimize::%peephole-walk
                 '((:const :r0 99) (:move :r1 :r0))
                 nil)))

(deftest peephole-walk-out-accumulator-is-prepended
  "%peephole-walk: instructions in OUT are prepended (reversed) to the result."
  (assert-equal '((:add :r2 :r1 :r1) (:const :r0 1))
                (cl-cc/optimize::%peephole-walk
                 '((:const :r0 1))
                 '((:add :r2 :r1 :r1)))))
