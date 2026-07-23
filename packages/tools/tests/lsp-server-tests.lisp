(in-package :cl-cc/test)



(defun lsp-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(it-sequential "lsp-server-system-loads"
  (expect (asdf:find-system :cl-cc-tools nil) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:make-lsp-server) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:lsp-server-p) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:lsp-server-running-p) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:read-jsonrpc-message) :to-be-truthy)
  (expect (cl-cc/tools/lsp:lsp-server-p (cl-cc/tools/lsp:make-lsp-server)) :to-be-truthy))

(it-sequential "lsp-jsonrpc-framing-round-trips"
  (let* ((payload '(("jsonrpc" . "2.0") ("id" . 7) ("method" . "initialize")
                    ("params" . (("rootUri" . "file:///tmp/cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/lsp:write-jsonrpc-message out payload))))
    (expect (search "Content-Length:" wire :test #'char=) :to-be-truthy)
    (let ((decoded (cl-cc/tools/lsp:read-jsonrpc-message (make-string-input-stream wire))))
      (expect (lsp-test-get decoded "method") :to-equal "initialize")
      (expect (lsp-test-get decoded "id") :to-equal 7))))

(it-sequential "lsp-initialize-advertises-capabilities"
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (cl-cc/tools/lsp:lsp-handle-request
                    server '(("jsonrpc" . "2.0") ("id" . 1) ("method" . "initialize")))))
    (expect (lsp-test-get response "jsonrpc") :to-equal "2.0")
    (expect (lsp-test-get (lsp-test-get response "result") "capabilities") :to-be-truthy)))

(it-sequential "lsp-diagnostics-infer-elisp-language-from-uri"
  (let ((seen-language nil)
        (orig-compile (symbol-function 'cl-cc:compile-string)))
    (unwind-protect
         (progn
           (sb-ext:without-package-locks
             (setf (symbol-function 'cl-cc:compile-string)
                   (lambda (text &rest args &key target language &allow-other-keys)
                     (declare (ignore text target))
                     (setf seen-language language)
                     nil)))
           (cl-cc/tools/lsp:lsp-publish-diagnostics
            (cl-cc/tools/lsp:make-lsp-server)
            "file:///tmp/sample.el")
           (expect seen-language :to-be :elisp))
      (sb-ext:without-package-locks
        (setf (symbol-function 'cl-cc:compile-string) orig-compile)))))

;; SKIP (Nix sandbox): LSP server requires running process

(it-sequential "lsp-publishes-parenthesis-diagnostics"
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///broken.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun broken (x) (+ x 1)")
    (let* ((notification (cl-cc/tools/lsp:lsp-publish-diagnostics server uri))
           (params (lsp-test-get notification "params")))
      (expect (lsp-test-get notification "method") :to-equal "textDocument/publishDiagnostics")
      (expect (lsp-test-get params "diagnostics") :to-be-truthy))))
