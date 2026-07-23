;;;; packages/vm/tests/crash-report-tests.lisp — Crash Report Diagnostic Tests
;;;;
;;;; Tests for the build-crash-report, write-crash-report, format-timestamp,
;;;; and save-crash-dump diagnostic pipeline functions.

(in-package :cl-cc/test)



(it-sequential "crash-report-build-has-required-struct-fields"
  (handler-case (error "test crash for cl-cc")
    (error (c)
      (let ((report (cl-cc/vm::build-crash-report c)))
        (expect (cl-cc/vm::crash-report-p report) :to-be-truthy)
        (expect (cl-cc/vm::cr-condition-type report) :to-be-truthy)
        (expect (stringp (cl-cc/vm::cr-condition-message report)) :to-be-truthy)
        (expect (search "test crash for cl-cc" (cl-cc/vm::cr-condition-message report)) :to-be-truthy)
        (expect (listp (cl-cc/vm::cr-backtrace report)) :to-be-truthy)))))

(it-sequential "crash-report-write-emits-condition-message"
  (handler-case (error "unique-crash-token-42")
    (error (c)
      (let* ((report (cl-cc/vm::build-crash-report c))
             (out    (make-string-output-stream)))
        (cl-cc/vm::write-crash-report report out)
        (let ((text (get-output-stream-string out)))
          (expect (search "unique-crash-token-42" text) :to-be-truthy))))))

(it-sequential "crash-report-format-timestamp-returns-string"
  (let ((ts (cl-cc/vm::format-timestamp (encode-universal-time 0 0 12 1 1 2000))))
    (expect (stringp ts) :to-be-truthy)
    (expect (> (length ts) 0) :to-be-truthy)))

(it-sequential "crash-report-timestamp-is-set-on-build"
  (handler-case (error "ts-test")
    (error (c)
      (let ((report (cl-cc/vm::build-crash-report c)))
        (expect (integerp (cl-cc/vm::cr-timestamp report)) :to-be-truthy)
        (expect (> (cl-cc/vm::cr-timestamp report) 0) :to-be-truthy)))))
