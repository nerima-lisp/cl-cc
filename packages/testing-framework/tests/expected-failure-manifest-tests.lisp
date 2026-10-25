(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %manifest-test-entry (path status &optional condition-message)
  (append (list :path path
                :expected-status status
                :owner-repository "nerima-lisp/cl-cc-codegen-native"
                :work-order "https://github.com/nerima-lisp/cl-cc-codegen-native/issues/1"
                :condition-class (if (eq status :fail)
                                     "CL-WEAVE:ASSERTION-FAILURE"
                                     "SIMPLE-ERROR"))
          (if (eq status :fail)
              (list :assertion-form "(CL-WEAVE:EXPECT NIL :TO-BE-TRUTHY)"
                    :matcher ":TO-BE-TRUTHY"
                    :actual "NIL"
                    :expected "NIL"
                    :negated "NIL")
              (list :condition-message condition-message))))

(cl-weave:it-sequential "expected-failure manifest rejects unexpected passes"
  (let* ((path "suite > expected failure")
         (manifest (list (%manifest-test-entry path :fail)))
         (results (list (list :path path :status :pass))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest results :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal '(:unexpected-pass)))))

(cl-weave:it-sequential "expected-failure manifest rejects unregistered failures"
  (let ((results (list (list :path "suite > unregistered"
                             :status :fail
                             :condition-class "CL-WEAVE:ASSERTION-FAILURE"
                             :assertion-form "(CL-WEAVE:EXPECT NIL :TO-BE-TRUTHY)"
                             :matcher ":TO-BE-TRUTHY"
                             :actual "NIL"
                             :expected "NIL"
                             :negated "NIL"))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results nil results :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal (quote (:unregistered-failure)))
      (cl-weave:signals error
        (%ensure-no-expected-failure-violations violations)))))

(cl-weave:it-sequential "expected-failure manifest rejects condition drift"
  (let* ((path "suite > changed error")
         (manifest (list (%manifest-test-entry path :error "expected error")))
         (results (list (list :path path
                              :status :error
                              :condition-class "SIMPLE-ERROR"
                              :condition-message "different error"))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest results :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal (quote (:condition-changed)))
      (cl-weave:signals error
        (%ensure-no-expected-failure-violations violations)))))

(cl-weave:it-sequential "expected-failure manifest matches dynamic condition message prefixes"
  (let* ((path "suite > dynamic error")
         (manifest (list (list :path path
                               :expected-status :error
                               :condition-class "SIMPLE-ERROR"
                               :condition-message-prefix "Unbound variable: #:OR"
                               :owner-repository "nerima-lisp/cl-cc"
                               :work-order "https://github.com/nerima-lisp/cl-cc/issues/1")))
         (matching-result (list :path path
                                :status :error
                                :condition-class "SIMPLE-ERROR"
                                :condition-message "Unbound variable: #:OR49592"))
         (mismatching-result (list :path path
                                  :status :error
                                  :condition-class "SIMPLE-ERROR"
                                  :condition-message "Unbound variable: #:AND49592")))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest (list matching-result) :require-complete t)
      (cl-weave:expect (length matched) :to-equal 1)
      (cl-weave:expect violations :to-equal nil))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest (list mismatching-result) :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal '(:condition-changed)))))

(cl-weave:it-sequential "expected-failure manifest rejects tests that did not run"
  (let ((manifest (list (%manifest-test-entry "suite > not run" :fail))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest nil :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal (quote (:not-run)))
      (cl-weave:signals error
        (%ensure-no-expected-failure-violations violations)))))
(cl-weave:it-sequential "expected-failure event normalization preserves pass paths"
  (let* ((event (cl-weave::make-test-event
                  :status :pass
                  :path (list "suite" "passing test")))
         (result (%expected-failure-result-from-event event)))
    (cl-weave:expect (getf result :path) :to-equal "suite > passing test")
    (cl-weave:expect (getf result :status) :to-be :pass)
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results nil (list result) :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect violations :to-equal nil))))
(cl-weave:it-sequential "expected-failure manifest rejects duplicate unrelated passes"
  (let* ((path "suite > passing test")
        (results (list (list :path path :status :pass)
                       (list :path path :status :pass))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results nil results :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal (quote (:duplicate-result)))
      (cl-weave:signals error
        (%ensure-no-expected-failure-violations violations)))))
(cl-weave:it-sequential "expected-failure manifest rejects duplicate expected failures"
  (let* ((path "suite > expected failure")
         (manifest (list (%manifest-test-entry path :fail)))
         (result (copy-list (first manifest)))
         (mismatched-result (copy-list result)))
    (setf (getf result :status) :fail
          (getf mismatched-result :status) :fail
          (getf mismatched-result :actual) "unexpected")
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest
                                        (list mismatched-result result (copy-list result))
                                        :require-complete t)
      (cl-weave:expect (length matched) :to-equal 1)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal '(:condition-changed :duplicate-result :duplicate-result))
      (cl-weave:signals error
        (%ensure-no-expected-failure-violations violations)))))

(cl-weave:it-sequential "full-suite XFAIL audit retains stale manifest entries"
  (let* ((selected-entry (%manifest-test-entry "suite > selected" :fail))
         (stale-entry (%manifest-test-entry "suite > stale" :fail))
         (manifest (list selected-entry stale-entry))
         (selected-paths (list (getf selected-entry :path)))
         (selected-result (append (copy-list selected-entry)
                                  (list :status :fail))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results
         (%select-expected-failure-manifest manifest selected-paths nil)
         (list selected-result)
         :require-complete t)
      (cl-weave:expect (length matched) :to-equal 1)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal '(:not-run))
      (cl-weave:expect (getf (first violations) :path)
                       :to-equal (getf stale-entry :path)))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results
         (%select-expected-failure-manifest manifest selected-paths t)
         (list selected-result)
         :require-complete t)
      (cl-weave:expect (length matched) :to-equal 1)
      (cl-weave:expect violations :to-equal nil))))

(cl-weave:it-sequential "it-sequential routes by source suite rather than leaked current suite"
  (let* ((source #P"/tmp/packages/optimize/tests/runner-taxonomy-tests.lisp")
         (source-key (namestring source)))
    (unwind-protect
         (let ((*compile-file-truename* source)
               (cl-weave::*current-suite* (%ensure-suite-object 'cl-cc-pbt-suite)))
           (remhash source-key *source-suite-assignments*)
           (let ((expansion (macroexpand-1 '(it-sequential "ordinary" nil))))
             (cl-weave:expect (search "CL-CC-UNIT-SUITE"
                                      (prin1-to-string expansion))
                              :to-be-truthy))
           (macroexpand-1 '(in-suite cl-cc-private-tests))
           (let ((expansion (macroexpand-1 '(it-sequential "private" nil))))
             (cl-weave:expect (search "CL-CC-PRIVATE-TESTS"
                                      (prin1-to-string expansion))
                              :to-be-truthy)))
      (remhash source-key *source-suite-assignments*))))

(cl-weave:it-sequential "suite taxonomy and defsuite option validation"
  (cl-weave:signals program-error
    (macroexpand-1 '(defsuite unsupported-suite :parallel nil)))
  (cl-weave:expect
   (%suite-for-test-source #P"/tmp/repo/t/conformance/example.lisp")
   :to-be 'cl-cc-conformance-suite)
  (cl-weave:expect
   (%suite-for-test-source #P"/tmp/repo/t/e2e/example.lisp")
   :to-be 'selfhost-suite)
  (cl-weave:expect
   (%suite-for-test-source #P"/tmp/repo/packages/core/tests/example.lisp")
   :to-be 'cl-cc-unit-suite))
(cl-weave:it-sequential "expected-failure manifest matches assertion failures by condition message"
  (let* ((path "suite > assertion message")
         (message-prefix "Test assertion failed: (CL-WEAVE:EXPECT (NREVERSE CL-CC/TEST::VIOLATIONS)")
         (manifest (list (list :path path
                               :expected-status :fail
                               :condition-class "CL-WEAVE:ASSERTION-FAILURE"
                               :condition-message-prefix message-prefix
                               :owner-repository "nerima-lisp/cl-cc"
                               :work-order "https://github.com/nerima-lisp/cl-cc/issues/69")))
         (form '(cl-weave:expect (nreverse cl-cc/test::violations) :to-equal nil))
         (detail (cl-weave::make-assertion-detail
                  :form form
                  :matcher :to-equal
                  :actual "first violations"
                  :expected nil
                  :negated nil
                  :pass nil))
         (condition (make-condition 'cl-weave:assertion-failure :detail detail))
         (event (cl-weave::make-test-event :status :fail
                                           :path (list "suite" "assertion message")
                                           :condition condition
                                           :assertion detail))
         (first-result (%expected-failure-result-from-event event))
         (changed-actual-result (copy-list first-result))
         (changed-message-result (copy-list first-result)))
    (cl-weave:expect
     (string= message-prefix
              (getf first-result :condition-message)
              :end2 (length message-prefix))
     :to-be-truthy)
    (setf (getf changed-actual-result :actual) "changed violations"
          (getf changed-message-result :condition-message)
          "Different assertion failure")
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest
                                        (list first-result)
                                        :require-complete t)
      (cl-weave:expect (length matched) :to-equal 1)
      (cl-weave:expect violations :to-equal nil))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest
                                        (list changed-actual-result)
                                        :require-complete t)
      (cl-weave:expect (length matched) :to-equal 1)
      (cl-weave:expect violations :to-equal nil))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest
                                        (list changed-message-result)
                                        :require-complete t)
      (cl-weave:expect matched :to-equal nil)
      (cl-weave:expect (mapcar (lambda (violation) (getf violation :kind))
                               violations)
                       :to-equal (list :condition-changed)))))
(in-suite cl-cc-unit-suite)
