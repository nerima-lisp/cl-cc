(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(%php85-register-test 'php85-self-load-executes-registrations
  "Loading the source file replays registration coverage under a fresh registry."
  (lambda ()
    (let* ((entry (persist-lookup *test-registry* 'php84-named-args-to-positional-lowers-named))
           (source-file (getf entry :source-file))
           (loaded-registry nil))
      (assert-true source-file)
      (let ((*php85-self-load-guard* t)
            (*test-registry* (persist-empty)))
        (load source-file)
        (setf loaded-registry *test-registry*))
      (assert-true (persist-lookup loaded-registry 'php84-named-args-to-positional-lowers-named))
      (assert-true (persist-lookup loaded-registry 'php84-enum-with-method-produces-slot-def))
      (assert-null (%php85-run-registered-tests-with-prefix "PHP85-THIS-PREFIX-IS-TOO-LONG")))))

(%php85-register-test 'php85-run-registered-tests-with-prefix-skips-non-symbol-and-excluded
  "The prefix runner ignores non-symbol registry keys and excluded matches."
  (lambda ()
    (let* ((kept-name 'php85-run-registered-tests-with-prefix-kept)
           (excluded-name 'php85-run-registered-tests-with-prefix-excluded)
           (kept-ran nil)
           (excluded-ran nil)
           (symbol-run-count 0)
           (fresh-registry (let ((registry (persist-empty)))
                             (setf registry
                                   (persist-assoc registry 42
                                                  (%test-registry-entry 42
                                                                        :fn (lambda ()
                                                                              (incf symbol-run-count))
                                                                        :suite *current-suite*
                                                                        :source-file (or *compile-file-pathname*
                                                                                         *load-pathname*))))
                             (setf registry
                                   (persist-assoc registry kept-name
                                                  (%test-registry-entry kept-name
                                                                        :fn (lambda ()
                                                                              (setf kept-ran t))
                                                                        :suite *current-suite*
                                                                        :source-file (or *compile-file-pathname*
                                                                                         *load-pathname*))))
                             (setf registry
                                   (persist-assoc registry excluded-name
                                                  (%test-registry-entry excluded-name
                                                                        :fn (lambda ()
                                                                              (setf excluded-ran t))
                                                                        :suite *current-suite*
                                                                        :source-file (or *compile-file-pathname*
                                                                                         *load-pathname*))))
                             registry)))
      (let ((symbol-entry (persist-lookup fresh-registry 42))
            (excluded-entry (persist-lookup fresh-registry excluded-name)))
        (assert-true symbol-entry)
        (assert-true excluded-entry)
        (let ((*test-registry* fresh-registry))
          (assert-equal '(t) (%php85-run-registered-tests-with-prefix "PHP85-RUN-REGISTERED-TESTS-WITH-PREFIX"
                                                                       :exclude (list excluded-name))))
        (assert-true kept-ran)
        (assert-null excluded-ran)
        (funcall (getf symbol-entry :fn))
        (funcall (getf excluded-entry :fn))
        (assert-= 1 symbol-run-count)
        (assert-true excluded-ran)))))

(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests
   :exclude (when *php85-self-load-guard*
              '(php85-self-load-executes-registrations))))
