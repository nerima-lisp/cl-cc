(in-package :cl-cc/test)

(defsuite lsp-server-suite
  :description "LSP tools protocol tests"
  :parent cl-cc-unit-suite)

(in-suite lsp-server-suite)

(defun lsp-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(deftest lsp-server-system-loads
  "The LSP system is loadable through :cl-cc-tools."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-tools nil))
  (assert-true (fboundp 'cl-cc/tools/lsp:make-lsp-server))
  (assert-true (fboundp 'cl-cc/tools/lsp:lsp-server-p))
  (assert-true (fboundp 'cl-cc/tools/lsp:lsp-server-running-p))
  (assert-true (fboundp 'cl-cc/tools/lsp:read-jsonrpc-message))
  (assert-true (cl-cc/tools/lsp:lsp-server-p (cl-cc/tools/lsp:make-lsp-server))))

(deftest lsp-jsonrpc-framing-round-trips
  "FR-796: Content-Length framing and JSON parsing work without network I/O."
  :timeout 5
  (let* ((payload '(("jsonrpc" . "2.0") ("id" . 7) ("method" . "initialize")
                    ("params" . (("rootUri" . "file:///tmp/cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/lsp:write-jsonrpc-message out payload))))
    (assert-true (search "Content-Length:" wire :test #'char=))
    (let ((decoded (cl-cc/tools/lsp:read-jsonrpc-message (make-string-input-stream wire))))
      (assert-equal "initialize" (lsp-test-get decoded "method"))
      (assert-equal 7 (lsp-test-get decoded "id")))))

(deftest lsp-initialize-advertises-capabilities
  "FR-796: initialize returns LSP capabilities."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (cl-cc/tools/lsp:lsp-handle-request
                    server '(("jsonrpc" . "2.0") ("id" . 1) ("method" . "initialize")))))
    (assert-equal "2.0" (lsp-test-get response "jsonrpc"))
    (assert-true (lsp-test-get (lsp-test-get response "result") "capabilities"))))

(deftest lsp-diagnostics-infer-elisp-language-from-uri
  "LSP diagnostics choose :elisp for .el documents."
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
           (assert-eq :elisp seen-language))
      (sb-ext:without-package-locks
        (setf (symbol-function 'cl-cc:compile-string) orig-compile)))))

;; SKIP (Nix sandbox): LSP server requires running process

(deftest lsp-publishes-parenthesis-diagnostics
  "FR-796: diagnostics are emitted as textDocument/publishDiagnostics notifications."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///broken.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun broken (x) (+ x 1)")
    (let* ((notification (cl-cc/tools/lsp:lsp-publish-diagnostics server uri))
           (params (lsp-test-get notification "params")))
      (assert-equal "textDocument/publishDiagnostics" (lsp-test-get notification "method"))
      (assert-true (lsp-test-get params "diagnostics")))))
