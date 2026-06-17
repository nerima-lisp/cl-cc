(in-package :cl-cc/test)

(defsuite dap-server-suite
  :description "DAP tools protocol tests"
  :parent cl-cc-unit-suite)

(in-suite dap-server-suite)

(defun dap-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(deftest dap-server-system-loads
  "The DAP source is loadable through :cl-cc-tools."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-tools nil))
  (assert-true (fboundp 'cl-cc/tools/dap:make-dap-server))
  (assert-true (fboundp 'cl-cc/tools/dap:dap-server-p))
  (assert-true (fboundp 'cl-cc/tools/dap:dap-server-running-p))
  (assert-true (fboundp 'cl-cc/tools/dap:read-jsonrpc-message))
  (assert-true (cl-cc/tools/dap:dap-server-p (cl-cc/tools/dap:make-dap-server))))

(deftest dap-jsonrpc-framing-round-trips
  "FR-797: DAP uses the same Content-Length protocol framing."
  :timeout 5
  (let* ((payload '(("seq" . 1) ("type" . "request") ("command" . "initialize")
                    ("arguments" . (("adapterID" . "cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/dap:write-jsonrpc-message out payload))))
    (let ((decoded (cl-cc/tools/dap:read-jsonrpc-message (make-string-input-stream wire))))
      (assert-equal "request" (dap-test-get decoded "type"))
      (assert-equal "initialize" (dap-test-get decoded "command")))))

(deftest dap-initialize-and-breakpoints
  "FR-797: initialize and setBreakpoints return DAP 1.51-style response bodies."
  :timeout 5
  (let* ((server (cl-cc/tools/dap:make-dap-server))
         (source '(("path" . "/tmp/a.lisp")))
         (breakpoints '((("line" . 10)) (("line" . 12))))
         (init (cl-cc/tools/dap:dap-handle-request
                 server '(("seq" . 1) ("type" . "request") ("command" . "initialize"))))
         (bp (cl-cc/tools/dap:dap-handle-request
              server `(("seq" . 2) ("type" . "request") ("command" . "setBreakpoints")
                       ("arguments" . (("source" . ,source)
                                      ("breakpoints" . ,breakpoints)))))))
    (assert-true (dap-test-get init "success"))
    (assert-true (dap-test-get (dap-test-get init "body") "supportsConfigurationDoneRequest"))
    (assert-equal 2 (length (dap-test-get (dap-test-get bp "body") "breakpoints")))
    (assert-true (every (lambda (breakpoint) (dap-test-get breakpoint "verified"))
                        (dap-test-get (dap-test-get bp "body") "breakpoints")))))

(deftest dap-stack-scopes-variables-and-step-fallbacks
  "FR-797: execution inspection works and unsupported step controls are explicit failures."
  :timeout 5
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
      (assert-= 1 (dap-test-get (dap-test-get stack "body") "totalFrames"))
      (assert-true (dap-test-get (dap-test-get scopes "body") "scopes"))
      (assert-true (dap-test-get (dap-test-get variables "body") "variables"))
      (assert-equal :false (dap-test-get next "success"))
      (assert-true (search "not supported" (dap-test-get next "message") :test #'char=)))))

(deftest dap-launch-infers-elisp-language-and-evaluate-uses-it
  "DAP launch should infer :elisp from .el sources and reuse it for evaluate."
  :timeout 5
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
               (assert-eq :elisp seen-language)
               (assert-output-contains (dap-test-get (dap-test-get response "body") "result") "42"))))
      (sb-ext:without-package-locks
        (setf (symbol-function 'cl-cc:run-string-repl) orig-run)))))
