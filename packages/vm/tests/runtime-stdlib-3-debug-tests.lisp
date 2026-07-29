;;; runtime-stdlib-3-debug-tests.lisp — FR-944/945/1085 debug surface

(in-package :cl-cc/test)



(it-sequential "rich-error-report-includes-location-context-and-suggestion"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (loc (cl-cc/vm:make-source-location :file "foo.lisp" :line 2 :column 4))
         (condition (cl-cc/vm::make-vm-unbound-variable
                     state 'pritn
                     :source-location loc
                     :source-text (format nil "(defun f ()~%  pritn~%  42)")
                     :suggestions '(print))))
    (let ((text (format nil "~A" condition)))
      (expect (search "foo.lisp:2:4" text) :to-be-truthy)
      (expect (search "Did you mean" text) :to-be-truthy)
      (expect (search "pritn" text) :to-be-truthy)
      (expect (search "^" text) :to-be-truthy))))

(it-sequential "rich-error-json-output-is-structured"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (loc (cl-cc/vm:make-source-location :file "foo.lisp" :line 1 :column 1))
         (condition (cl-cc/vm::make-vm-undefined-function
                     state 'mapcarr :source-location loc :suggestions '(mapcar))))
    (let ((json (with-output-to-string (out)
                  (cl-cc/vm:format-condition-json condition out))))
      (expect (search "\"location\"" json) :to-be-truthy)
      (expect (search "foo.lisp" json) :to-be-truthy)
      (expect (search "MAPCAR" json) :to-be-truthy))))

(it-sequential "restart-ui-describes-and-invokes-vm-restart-interactively"
  (let ((called 0))
    (let ((restart (cl-cc/vm::make-vm-restart 'use-value
                                              (lambda (value)
                                                (setf called value)))))
      (setf (cl-cc/vm::vm-restart-description restart) "Use supplied value")
      (setf (cl-cc/vm::vm-restart-interactive-function restart) (lambda () (list 42)))
      (expect (cl-cc/vm:describe-restart restart) :to-equal "Use supplied value")
      (cl-cc/vm:vm-invoke-restart-interactively restart)
      (expect (= 42 called) :to-be-truthy))))

(it-sequential "trace-helpers-log-call-and-return"
  (let ((output (make-string-output-stream)))
    (let ((cl-cc/vm:*trace-output* output))
      (cl-cc/vm:vm-untrace)
      (cl-cc/vm:vm-trace 'foo)
      (expect (= 3 (cl-cc/vm:vm-with-trace ('foo '(1 2)) (+ 1 2))) :to-be-truthy))
    (let ((text (get-output-stream-string output)))
      (expect (search "0: (FOO 1 2)" text) :to-be-truthy)
      (expect (search "0: FOO returned 3" text) :to-be-truthy))))

(it-sequential "debugger-helper-surface-is-callable"
  (expect (member 'car (cl-cc/vm:vm-apropos-list "CAR" :cl) :test #'eq) :to-be-truthy)
  (expect (cl-cc/vm:vm-ed 'dummy) :to-be-null)
  (let ((text (with-output-to-string (out)
                (cl-cc/vm:vm-describe 'car out))))
    (expect (search "CAR" text) :to-be-truthy))
  (expect (cl-cc/vm:vm-inspect 'car) :to-be 'car))
