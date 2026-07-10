(in-package :cl-cc/test)

(in-suite cl-cc-prolog-integration-suite)

(deftest prolog-unify-failure-sentinel-is-preserved
  "The public unification API returns the failure sentinel directly."
  (let ((result (cl-cc/prolog:unify 1 2)))
    (assert-eq :unify-fail result)))
