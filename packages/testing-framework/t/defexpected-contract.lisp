(in-package :cl-cc/test)

(it-sequential "defexpected passes when body signals an assertion failure"
  (let ((test-function
          (macrolet ((deftest (name docstring &rest body)
                       (declare (ignore name docstring))
                       (loop while (keywordp (first body))
                             do (pop body)
                                (pop body))
                       `(lambda () ,@body)))
            (defexpected expected-failure-contract
              "expected failure"
              :timeout 1
              :depends-on nil
              (cl-weave:fail "expected failure")))))
    (expect (funcall test-function) :to-be t)))

(it-sequential "defexpected reports a clean pass as XPASS"
  (let ((test-function
          (macrolet ((deftest (name docstring &rest body)
                       (declare (ignore name docstring))
                       (loop while (keywordp (first body))
                             do (pop body)
                                (pop body))
                       `(lambda () ,@body)))
            (defexpected unexpected-clean-pass
              "unexpected clean pass"
              :timeout 1
              :depends-on nil
              42))))
    (let ((condition
            (handler-case
                (progn
                  (funcall test-function)
                  nil)
              (cl-weave:assertion-failure (condition)
                condition))))
      (expect condition :to-be-instance-of
               'cl-weave:assertion-failure))))
