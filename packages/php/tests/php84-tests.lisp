(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defvar *php85-self-load-guard* nil)

(defvar *php85-name-registry* (make-hash-table :test #'eq)
  "Symbol name -> thunk, supporting %PHP85-RUN-REGISTERED-TESTS-WITH-PREFIX's
dynamic prefix-based lookup/invocation. cl-weave's own suite tree has no
retrieve-by-name-prefix operation, so this stays a small parallel registry;
tests are still separately registered with cl-weave (via DEFTEST/register-test)
for normal RUN-ALL execution and reporting.")

(defun %php85-register-test (name docstring thunk &key timeout depends-on tags)
  "Register a php85 coverage test body under NAME."
  (declare (ignore depends-on))
  (setf (gethash name *php85-name-registry*) thunk)
  (cl-weave::register-test
   (or docstring (string-downcase (symbol-name name)))
   thunk
   :tags tags
   :timeout-ms (when timeout (round (* timeout 1000))))
  name)

(defun %php85-run-registered-tests-with-prefix (prefix &key exclude)
  "Run every registered test whose symbol name starts with PREFIX.
EXCLUDE lists symbols that should not be invoked even if they match PREFIX."
  (let ((results '())
        (prefix-len (length prefix)))
    (maphash (lambda (name thunk)
               (when (and (not (member name exclude :test #'eq))
                          (let ((name-str (symbol-name name)))
                            (and (<= prefix-len (length name-str))
                                 (string= prefix name-str :end2 prefix-len))))
                 (push (funcall thunk) results)))
             *php85-name-registry*)
    (nreverse results)))

(defun %php85-run-current-source-tests (&key exclude)
  (declare (ignore exclude))
  nil)
