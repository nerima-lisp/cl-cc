(in-package :cl-cc/test)



(defun lsp-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(it-sequential "lsp-server-system-loads"
  :timeout
  5
  (expect (asdf:find-system :cl-cc-tools nil) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:make-lsp-server) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:lsp-server-p) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:lsp-server-running-p) :to-be-truthy)
  (expect (fboundp 'cl-cc/tools/lsp:read-jsonrpc-message) :to-be-truthy)
  (expect (cl-cc/tools/lsp:lsp-server-p (cl-cc/tools/lsp:make-lsp-server)) :to-be-truthy))

(it-sequential "lsp-jsonrpc-framing-round-trips"
  :timeout
  5
  (let* ((payload '(("jsonrpc" . "2.0") ("id" . 7) ("method" . "initialize")
                    ("params" . (("rootUri" . "file:///tmp/cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/lsp:write-jsonrpc-message out payload))))
    (expect (search "Content-Length:" wire :test #'char=) :to-be-truthy)
    (let ((decoded (cl-cc/tools/lsp:read-jsonrpc-message (make-string-input-stream wire))))
      (expect (lsp-test-get decoded "method") :to-equal "initialize")
      (expect (lsp-test-get decoded "id") :to-equal 7))))

(it-sequential "lsp-initialize-advertises-capabilities"
  :timeout
  5
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
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///broken.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun broken (x) (+ x 1)")
    (let* ((notification (cl-cc/tools/lsp:lsp-publish-diagnostics server uri))
           (params (lsp-test-get notification "params")))
      (expect (lsp-test-get notification "method") :to-equal "textDocument/publishDiagnostics")
      (expect (lsp-test-get params "diagnostics") :to-be-truthy))))

;;;; ---- JSON codec ---------------------------------------------------------

(it-sequential "lsp-encode-json-scalars :true"
  (destructuring-bind (value expected) (list :true "true")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-scalars :false"
  (destructuring-bind (value expected) (list :false "false")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-scalars null"
  (destructuring-bind (value expected) (list nil "null")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-scalars integer"
  (destructuring-bind (value expected) (list 42 "42")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-scalars negative"
  (destructuring-bind (value expected) (list -7 "-7")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-scalars string"
  (destructuring-bind (value expected) (list "hi" "\"hi\"")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-scalars symbol"
  (destructuring-bind (value expected) (list 'hello "\"HELLO\"")
    (expect (cl-cc/tools/lsp::encode-json value) :to-equal expected)))

(it-sequential "lsp-encode-json-escapes-control-characters"
  :timeout
  5
  (expect (cl-cc/tools/lsp::encode-json "a\"b") :to-equal "\"a\\\"b\"")
  (expect (cl-cc/tools/lsp::encode-json "a\\b") :to-equal "\"a\\\\b\"")
  (expect (cl-cc/tools/lsp::encode-json (format nil "a~%b")) :to-equal "\"a\\nb\"")
  (expect (cl-cc/tools/lsp::encode-json (format nil "a~Ab" #\Tab)) :to-equal (format nil "\"a\\tb\"")))

(it-sequential "lsp-encode-json-arrays-and-objects"
  :timeout
  5
  (expect (cl-cc/tools/lsp::encode-json '(1 2 3)) :to-equal "[1,2,3]")
  (expect (cl-cc/tools/lsp::encode-json '(("a" . 1))) :to-equal "{\"a\":1}")
  (expect (cl-cc/tools/lsp::encode-json '(("a" . (1 2)))) :to-equal "{\"a\":[1,2]}")
  (expect (cl-cc/tools/lsp::encode-json '(("nested" . (("k" . "v"))))) :to-equal "{\"nested\":{\"k\":\"v\"}}"))

(it-sequential "lsp-encode-json-to-supplied-stream-returns-value"
  :timeout
  5
  (let* ((value '(("id" . 1)))
         (out (make-string-output-stream))
         (result (cl-cc/tools/lsp::encode-json value out)))
    (expect result :to-be value)
    (expect (get-output-stream-string out) :to-equal "{\"id\":1}")))

(it-sequential "lsp-parse-json-scalars true"
  (destructuring-bind (text expected) (list "true" :true)
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-scalars false"
  (destructuring-bind (text expected) (list "false" :false)
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-scalars null"
  (destructuring-bind (text expected) (list "null" nil)
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-scalars integer"
  (destructuring-bind (text expected) (list "42" 42)
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-scalars negative"
  (destructuring-bind (text expected) (list "-7" -7)
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-scalars float"
  (destructuring-bind (text expected) (list "3.5" 3.5)
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-scalars string"
  (destructuring-bind (text expected) (list "\"hi\"" "hi")
    (expect (cl-cc/tools/lsp::parse-json text) :to-equal expected)))

(it-sequential "lsp-parse-json-string-escapes"
  :timeout
  5
  (expect (cl-cc/tools/lsp::parse-json "\"a\\nb\"") :to-equal (format nil "a~%b"))
  (expect (cl-cc/tools/lsp::parse-json "\"a\\tb\"") :to-equal (format nil "a~Ab" #\Tab))
  (expect (cl-cc/tools/lsp::parse-json "\"a\\/b\"") :to-equal "a/b")
  (expect (cl-cc/tools/lsp::parse-json "\"a\\\"b\"") :to-equal "a\"b"))

(it-sequential "lsp-parse-json-arrays-and-objects"
  :timeout
  5
  (expect (cl-cc/tools/lsp::parse-json "[1,2,3]") :to-equal '(1 2 3))
  (expect (cl-cc/tools/lsp::parse-json "{\"k\":1}") :to-equal '(("k" . 1)))
  (expect (cl-cc/tools/lsp::parse-json "{\"a\": [1, 2]}") :to-equal '(("a" . (1 2))))
  (expect (cl-cc/tools/lsp::parse-json "[]") :to-be-null)
  (expect (cl-cc/tools/lsp::parse-json "{}") :to-be-null))

(it-sequential "lsp-read-jsonrpc-message-requires-content-length"
  :timeout
  5
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/tools/lsp:read-jsonrpc-message
     (make-string-input-stream (format nil "X-Header: 1~C~C~C~C{}"
                                       #\Return #\Newline #\Return #\Newline)))) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

;;;; ---- Request dispatch ---------------------------------------------------

(defun lsp-request (server method &optional params (id 1))
  (cl-cc/tools/lsp:lsp-handle-request
   server (append `(("jsonrpc" . "2.0") ("id" . ,id) ("method" . ,method))
                  (and params `(("params" . ,params))))))

(it-sequential "lsp-shutdown-marks-server-and-returns-empty-result"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (lsp-request server "shutdown" nil 9)))
    (expect (cl-cc/tools/lsp:lsp-server-shutdown-requested-p server) :to-be-truthy)
    (expect (lsp-test-get response "id") :to-equal 9)
    (expect (lsp-test-get response "result") :to-be-null)))

(it-sequential "lsp-exit-stops-server-and-returns-no-response"
  :timeout
  5
  (let ((server (cl-cc/tools/lsp:make-lsp-server)))
    (setf (cl-cc/tools/lsp:lsp-server-running-p server) t)
    (expect (lsp-request server "exit") :to-be-null)
    (expect (cl-cc/tools/lsp:lsp-server-running-p server) :to-be-falsy)))

(it-sequential "lsp-unknown-method-returns-null-result"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (lsp-request server "textDocument/unknownThing" nil 3)))
    (expect (lsp-test-get response "id") :to-equal 3)
    (expect (lsp-test-get response "result") :to-be-null)))

(it-sequential "lsp-did-open-indexes-symbols-and-publishes-diagnostics"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///a.lisp")
         (notification (lsp-request server "textDocument/didOpen"
                                    `(("textDocument" . (("uri" . ,uri)
                                                         ("text" . "(defun my-func (x) (+ x 1))")))))))
    (expect (lsp-test-get notification "method") :to-equal "textDocument/publishDiagnostics")
    (expect (gethash uri (cl-cc/tools/lsp:lsp-server-documents server)) :to-equal "(defun my-func (x) (+ x 1))")
    (expect (gethash uri (cl-cc/tools/lsp:lsp-server-symbols server)) :to-be-truthy)))

(it-sequential "lsp-did-change-replaces-document-text"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///b.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun old () 1)")
    (let ((notification (lsp-request server "textDocument/didChange"
                                     `(("textDocument" . (("uri" . ,uri)))
                                       ("contentChanges" . ((("text" . "(defun fresh (y) y)"))))))))
      (expect (lsp-test-get notification "method") :to-equal "textDocument/publishDiagnostics")
      (expect (gethash uri (cl-cc/tools/lsp:lsp-server-documents server)) :to-equal "(defun fresh (y) y)"))))

(it-sequential "lsp-completion-includes-keywords-and-document-symbols"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///c.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun my-func (x) x)")
    (let* ((response (lsp-request server "textDocument/completion"))
           (result (lsp-test-get response "result"))
           (labels (mapcar (lambda (item) (lsp-test-get item "label"))
                           (lsp-test-get result "items"))))
      (expect (lsp-test-get result "isIncomplete") :to-be :false)
      (expect (member "defun" labels :test #'string=) :to-be-truthy)
      (expect (member "my-func" labels :test #'string=) :to-be-truthy))))

(it-sequential "lsp-hover-returns-markdown-for-word-under-cursor"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///d.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun my-func (x) x)")
    (let* ((response (lsp-request server "textDocument/hover"
                                  `(("textDocument" . (("uri" . ,uri)))
                                    ("position" . (("line" . 0) ("character" . 9))))))
           (contents (lsp-test-get (lsp-test-get response "result") "contents")))
      (expect (lsp-test-get contents "kind") :to-equal "markdown")
      (expect (lsp-test-get contents "value") :to-equal "`my-func`"))))

(it-sequential "lsp-definition-resolves-known-symbol-and-misses-unknown"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///e.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun my-func (x) x)")
    (let* ((hit (lsp-request server "textDocument/definition"
                             `(("textDocument" . (("uri" . ,uri)))
                               ("position" . (("line" . 0) ("character" . 9))))))
           (location (lsp-test-get hit "result")))
      (expect (lsp-test-get location "uri") :to-equal uri))
    (cl-cc/tools/lsp:lsp-open-document server uri "(+ 1 2)")
    (let ((miss (lsp-request server "textDocument/definition"
                             `(("textDocument" . (("uri" . ,uri)))
                               ("position" . (("line" . 0) ("character" . 3)))))))
      (expect (lsp-test-get miss "result") :to-be-null))))

(it-sequential "lsp-workspace-symbol-filters-by-query"
  :timeout
  5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///f.lisp"))
    (cl-cc/tools/lsp:lsp-open-document
     server uri (format nil "(defun alpha () 1)~%(defvar *beta* 2)"))
    (let ((names (mapcar (lambda (s) (lsp-test-get s "name"))
                         (lsp-test-get (lsp-request server "workspace/symbol"
                                                    '(("query" . "alph")))
                                       "result"))))
      (expect names :to-equal '("alpha")))
    (let ((all (lsp-test-get (lsp-request server "workspace/symbol"
                                          '(("query" . "")))
                             "result")))
      (expect (= 2 (length all)) :to-be-truthy))))
