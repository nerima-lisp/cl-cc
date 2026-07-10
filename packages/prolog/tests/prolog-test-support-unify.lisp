(in-package :cl-cc/test)

(defmacro assert-prolog-unify-gate= (gate expected-ran left right
                                     &key initial-env
                                          ((:expected-env expected-env)
                                           nil
                                           expected-env-p))
  "Assert continuation behavior for when-unify-* gates."
  (ecase gate
    (:success
     `(let ((ran nil)
            (captured-env nil))
        (cl-cc/prolog::when-unify-succeeds (result ,left ,right ,initial-env)
          (setf ran t
                captured-env result))
        (if ,expected-ran
            (assert-true ran)
            (assert-false ran))
        ,@(when expected-env-p
            `((assert-equal ,expected-env captured-env)))))
    (:failure
     `(let ((ran nil))
        (cl-cc/prolog::when-unify-fails (,left ,right ,initial-env)
          (setf ran t))
        (if ,expected-ran
            (assert-true ran)
            (assert-false ran))))))
