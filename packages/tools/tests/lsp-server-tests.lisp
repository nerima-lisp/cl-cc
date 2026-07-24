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

;;;; ---- JSON codec ---------------------------------------------------------

(deftest-each lsp-encode-json-scalars
  "encode-json renders JSON scalars and literals across all type branches."
  :cases ((":true"   :true                  "true")
          (":false"  :false                 "false")
          ("null"    nil                    "null")
          ("integer" 42                     "42")
          ("negative" -7                    "-7")
          ("string"  "hi"                    "\"hi\"")
          ("symbol"  'hello                  "\"HELLO\""))
  (value expected)
  (assert-equal expected (cl-cc/tools/lsp::encode-json value)))

(deftest lsp-encode-json-escapes-control-characters
  "encode-json escapes quotes, backslashes and control whitespace."
  :timeout 5
  (assert-equal "\"a\\\"b\"" (cl-cc/tools/lsp::encode-json "a\"b"))
  (assert-equal "\"a\\\\b\"" (cl-cc/tools/lsp::encode-json "a\\b"))
  (assert-equal "\"a\\nb\"" (cl-cc/tools/lsp::encode-json (format nil "a~%b")))
  (assert-equal (format nil "\"a\\tb\"") (cl-cc/tools/lsp::encode-json (format nil "a~Ab" #\Tab))))

(deftest lsp-encode-json-arrays-and-objects
  "encode-json distinguishes alists-with-string-keys (objects) from plain lists (arrays)."
  :timeout 5
  (assert-equal "[1,2,3]" (cl-cc/tools/lsp::encode-json '(1 2 3)))
  (assert-equal "{\"a\":1}" (cl-cc/tools/lsp::encode-json '(("a" . 1))))
  (assert-equal "{\"a\":[1,2]}" (cl-cc/tools/lsp::encode-json '(("a" . (1 2)))))
  (assert-equal "{\"nested\":{\"k\":\"v\"}}"
                (cl-cc/tools/lsp::encode-json '(("nested" . (("k" . "v")))))))

(deftest lsp-encode-json-to-supplied-stream-returns-value
  "encode-json with an explicit stream writes to it and returns the original value."
  :timeout 5
  (let* ((value '(("id" . 1)))
         (out (make-string-output-stream))
         (result (cl-cc/tools/lsp::encode-json value out)))
    (assert-eq value result)
    (assert-equal "{\"id\":1}" (get-output-stream-string out))))

(deftest-each lsp-parse-json-scalars
  "parse-json decodes JSON scalars and literals."
  :cases (("true"     "true"    :true)
          ("false"    "false"   :false)
          ("null"     "null"    nil)
          ("integer"  "42"      42)
          ("negative" "-7"      -7)
          ("float"    "3.5"     3.5)
          ("string"   "\"hi\""  "hi"))
  (text expected)
  (assert-equal expected (cl-cc/tools/lsp::parse-json text)))

(deftest lsp-parse-json-string-escapes
  "parse-json interprets backslash escapes inside strings."
  :timeout 5
  (assert-equal (format nil "a~%b") (cl-cc/tools/lsp::parse-json "\"a\\nb\""))
  (assert-equal (format nil "a~Ab" #\Tab) (cl-cc/tools/lsp::parse-json "\"a\\tb\""))
  (assert-equal "a/b" (cl-cc/tools/lsp::parse-json "\"a\\/b\""))
  (assert-equal "a\"b" (cl-cc/tools/lsp::parse-json "\"a\\\"b\"")))

(deftest lsp-parse-json-arrays-and-objects
  "parse-json decodes arrays and nested objects into lists and alists."
  :timeout 5
  (assert-equal '(1 2 3) (cl-cc/tools/lsp::parse-json "[1,2,3]"))
  (assert-equal '(("k" . 1)) (cl-cc/tools/lsp::parse-json "{\"k\":1}"))
  (assert-equal '(("a" . (1 2))) (cl-cc/tools/lsp::parse-json "{\"a\": [1, 2]}"))
  (assert-null (cl-cc/tools/lsp::parse-json "[]"))
  (assert-null (cl-cc/tools/lsp::parse-json "{}")))

(deftest lsp-read-jsonrpc-message-requires-content-length
  "read-jsonrpc-message signals when the Content-Length header is absent."
  :timeout 5
  (assert-signals error
    (cl-cc/tools/lsp:read-jsonrpc-message
     (make-string-input-stream (format nil "X-Header: 1~C~C~C~C{}"
                                       #\Return #\Newline #\Return #\Newline)))))

;;;; ---- Request dispatch ---------------------------------------------------

(defun lsp-request (server method &optional params (id 1))
  (cl-cc/tools/lsp:lsp-handle-request
   server (append `(("jsonrpc" . "2.0") ("id" . ,id) ("method" . ,method))
                  (and params `(("params" . ,params))))))

(deftest lsp-shutdown-marks-server-and-returns-empty-result
  "shutdown records the request and returns a null-result response."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (lsp-request server "shutdown" nil 9)))
    (assert-true (cl-cc/tools/lsp:lsp-server-shutdown-requested-p server))
    (assert-equal 9 (lsp-test-get response "id"))
    (assert-null (lsp-test-get response "result"))))

(deftest lsp-exit-stops-server-and-returns-no-response
  "exit clears running-p and produces no response payload."
  :timeout 5
  (let ((server (cl-cc/tools/lsp:make-lsp-server)))
    (setf (cl-cc/tools/lsp:lsp-server-running-p server) t)
    (assert-null (lsp-request server "exit"))
    (assert-false (cl-cc/tools/lsp:lsp-server-running-p server))))

(deftest lsp-unknown-method-returns-null-result
  "An unrecognized method still yields a well-formed null-result response."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (lsp-request server "textDocument/unknownThing" nil 3)))
    (assert-equal 3 (lsp-test-get response "id"))
    (assert-null (lsp-test-get response "result"))))

(deftest lsp-did-open-indexes-symbols-and-publishes-diagnostics
  "textDocument/didOpen stores the document, indexes its symbols and returns diagnostics."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///a.lisp")
         (notification (lsp-request server "textDocument/didOpen"
                                    `(("textDocument" . (("uri" . ,uri)
                                                         ("text" . "(defun my-func (x) (+ x 1))")))))))
    (assert-equal "textDocument/publishDiagnostics" (lsp-test-get notification "method"))
    (assert-equal "(defun my-func (x) (+ x 1))"
                  (gethash uri (cl-cc/tools/lsp:lsp-server-documents server)))
    (assert-true (gethash uri (cl-cc/tools/lsp:lsp-server-symbols server)))))

(deftest lsp-did-change-replaces-document-text
  "textDocument/didChange applies the last content change and re-indexes."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///b.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun old () 1)")
    (let ((notification (lsp-request server "textDocument/didChange"
                                     `(("textDocument" . (("uri" . ,uri)))
                                       ("contentChanges" . ((("text" . "(defun fresh (y) y)"))))))))
      (assert-equal "textDocument/publishDiagnostics" (lsp-test-get notification "method"))
      (assert-equal "(defun fresh (y) y)"
                    (gethash uri (cl-cc/tools/lsp:lsp-server-documents server))))))

(deftest lsp-completion-includes-keywords-and-document-symbols
  "textDocument/completion offers built-in keywords plus indexed document symbols."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///c.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun my-func (x) x)")
    (let* ((response (lsp-request server "textDocument/completion"))
           (result (lsp-test-get response "result"))
           (labels (mapcar (lambda (item) (lsp-test-get item "label"))
                           (lsp-test-get result "items"))))
      (assert-eq :false (lsp-test-get result "isIncomplete"))
      (assert-true (member "defun" labels :test #'string=))
      (assert-true (member "my-func" labels :test #'string=)))))

(deftest lsp-hover-returns-markdown-for-word-under-cursor
  "textDocument/hover formats the identifier at the cursor as markdown."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///d.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun my-func (x) x)")
    (let* ((response (lsp-request server "textDocument/hover"
                                  `(("textDocument" . (("uri" . ,uri)))
                                    ("position" . (("line" . 0) ("character" . 9))))))
           (contents (lsp-test-get (lsp-test-get response "result") "contents")))
      (assert-equal "markdown" (lsp-test-get contents "kind"))
      (assert-equal "`my-func`" (lsp-test-get contents "value")))))

(deftest lsp-definition-resolves-known-symbol-and-misses-unknown
  "textDocument/definition returns a location for indexed symbols and nil otherwise."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///e.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun my-func (x) x)")
    (let* ((hit (lsp-request server "textDocument/definition"
                             `(("textDocument" . (("uri" . ,uri)))
                               ("position" . (("line" . 0) ("character" . 9))))))
           (location (lsp-test-get hit "result")))
      (assert-equal uri (lsp-test-get location "uri")))
    (cl-cc/tools/lsp:lsp-open-document server uri "(+ 1 2)")
    (let ((miss (lsp-request server "textDocument/definition"
                             `(("textDocument" . (("uri" . ,uri)))
                               ("position" . (("line" . 0) ("character" . 3)))))))
      (assert-null (lsp-test-get miss "result")))))

(deftest lsp-workspace-symbol-filters-by-query
  "workspace/symbol returns document symbols whose names match the query substring."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///f.lisp"))
    (cl-cc/tools/lsp:lsp-open-document
     server uri (format nil "(defun alpha () 1)~%(defvar *beta* 2)"))
    (let ((names (mapcar (lambda (s) (lsp-test-get s "name"))
                         (lsp-test-get (lsp-request server "workspace/symbol"
                                                    '(("query" . "alph")))
                                       "result"))))
      (assert-equal '("alpha") names))
    (let ((all (lsp-test-get (lsp-request server "workspace/symbol"
                                          '(("query" . "")))
                             "result")))
      (assert-= 2 (length all)))))
