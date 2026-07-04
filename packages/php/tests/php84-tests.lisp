(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defvar *php85-self-load-guard* nil)

(defun %php85-register-test (name docstring thunk &key timeout depends-on tags)
  "Register a php85 coverage test body under NAME."
  (setf *test-registry*
        (persist-assoc *test-registry* name
                       (%test-registry-entry name
                                             :fn thunk
                                             :suite *current-suite*
                                             :timeout timeout
                                             :depends-on depends-on
                                             :tags tags
                                             :docstring docstring
                                             :source-file (or *compile-file-pathname*
                                                              *load-pathname*))))
  name)

(defun %php85-run-registered-tests-with-prefix (prefix &key exclude)
  "Run every registered test whose symbol name starts with PREFIX.
EXCLUDE lists symbols that should not be invoked even if they match PREFIX."
  (let ((results '())
        (prefix-len (length prefix)))
    (persist-each *test-registry*
                  (lambda (name plist)
                    (declare (ignore plist))
                    (when (and (symbolp name)
                               (not (member name exclude :test #'eq))
                               (let ((name-str (symbol-name name)))
                                 (and (<= prefix-len (length name-str))
                                      (string= prefix name-str :end2 prefix-len))))
                      (let ((entry (persist-lookup *test-registry* name)))
                        (assert-true entry)
                        (push (funcall (getf entry :fn))
                              results)))))
    (nreverse results)))


(defun %php85-run-current-source-tests (&key exclude)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)
   :exclude exclude))
