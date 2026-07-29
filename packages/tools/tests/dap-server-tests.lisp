(in-package :cl-cc/test)



(defun dap-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(it-sequential "dap-server-system-loads"
  (expect (asdf:find-system :cl-cc-tools nil) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/dap:make-dap-server) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/dap:dap-server-p) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/dap:dap-server-running-p) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/dap:read-jsonrpc-message) :to-be-truthy)
  (expect (cl-cc/tools/dap:dap-server-p (cl-cc/tools/dap:make-dap-server)) :to-be-truthy))

(it-sequential "dap-jsonrpc-framing-round-trips"
  (let* ((payload '(("seq" . 1) ("type" . "request") ("command" . "initialize")
                    ("arguments" . (("adapterID" . "cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/dap:write-jsonrpc-message out payload))))
    (let ((decoded (cl-cc/tools/dap:read-jsonrpc-message (make-string-input-stream wire))))
      (expect (dap-test-get decoded "type") :to-equal "request")
      (expect (dap-test-get decoded "command") :to-equal "initialize"))))

(it-sequential "dap-initialize-and-breakpoints"
  (let* ((server (cl-cc/tools/dap:make-dap-server))
         (source '(("path" . "/tmp/a.lisp")))
         (breakpoints '((("line" . 10)) (("line" . 12))))
         (init (cl-cc/tools/dap:dap-handle-request
                 server '(("seq" . 1) ("type" . "request") ("command" . "initialize"))))
         (bp (cl-cc/tools/dap:dap-handle-request
              server `(("seq" . 2) ("type" . "request") ("command" . "setBreakpoints")
                       ("arguments" . (("source" . ,source)
                                      ("breakpoints" . ,breakpoints)))))))
    (expect (dap-test-get init "success") :to-be-truthy)
    (expect (dap-test-get (dap-test-get init "body") "supportsConfigurationDoneRequest") :to-be-truthy)
    (expect (length (dap-test-get (dap-test-get bp "body") "breakpoints")) :to-equal 2)
    (expect (every (lambda (breakpoint) (dap-test-get breakpoint "verified"))
                        (dap-test-get (dap-test-get bp "body") "breakpoints")) :to-be-truthy)))

(it-sequential "dap-stack-scopes-variables-and-step-fallbacks"
  (let ((server (cl-cc/tools/dap:make-dap-server)))
    (cl-cc/tools/dap:dap-handle-request
     server '(("seq" . 1) ("type" . "request") ("command" . "setBreakpoints")
              ("arguments" . (("source" . (("path" . "/tmp/a.lisp")))
                             ("breakpoints" . ((("line" . 7))))))))
    (let* ((stack (cl-cc/tools/dap:dap-handle-request
                   server '(("seq" . 2) ("type" . "request") ("command" . "stackTrace")
                            ("arguments" . (("threadId" . 1))))))
           (scopes (cl-cc/tools/dap:dap-handle-request
                    server '(("seq" . 3) ("type" . "request") ("command" . "scopes")
                             ("arguments" . (("frameId" . 1))))))
           (variables (cl-cc/tools/dap:dap-handle-request
                       server '(("seq" . 4) ("type" . "request") ("command" . "variables")
                                ("arguments" . (("variablesReference" . 1))))))
           (next (cl-cc/tools/dap:dap-handle-request
                  server '(("seq" . 5) ("type" . "request") ("command" . "next")))))
      (expect (= 1 (dap-test-get (dap-test-get stack "body") "totalFrames")) :to-be-truthy)
      (expect (dap-test-get (dap-test-get scopes "body") "scopes") :to-be-truthy)
      (expect (dap-test-get (dap-test-get variables "body") "variables") :to-be-truthy)
      (expect (dap-test-get next "success") :to-equal :false)
      (expect (search "not supported" (dap-test-get next "message") :test #'char=) :to-be-truthy))))

(it-sequential "dap-launch-infers-elisp-language-and-evaluate-uses-it"
  (let ((seen-language nil)
        (orig-run (symbol-function 'cl-cc:run-string-repl)))
    (unwind-protect
         (progn
           (sb-ext:without-package-locks
             (setf (symbol-function 'cl-cc:run-string-repl)
                   (lambda (expr &key language &allow-other-keys)
                     (declare (ignore expr))
                     (setf seen-language language)
                     42)))
           (let ((server (cl-cc/tools/dap:make-dap-server :running-p t)))
             (cl-cc/tools/dap:dap-handle-request
              server '(("seq" . 10) ("type" . "request") ("command" . "launch")
                       ("arguments" . (("program" . "/tmp/sample.el")))))
             (let ((response (cl-cc/tools/dap:dap-handle-request
                              server '(("seq" . 11) ("type" . "request") ("command" . "evaluate")
                                       ("arguments" . (("expression" . "(+ 20 22)")))))))
               (expect seen-language :to-be :elisp)
               (assert-output-contains (dap-test-get (dap-test-get response "body") "result") "42"))))
      (sb-ext:without-package-locks
        (setf (symbol-function 'cl-cc:run-string-repl) orig-run)))))
