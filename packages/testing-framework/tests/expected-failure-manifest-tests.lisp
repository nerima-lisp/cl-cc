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
(in-suite cl-cc-suite)
