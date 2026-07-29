;;;; packages/cli/tests/plugin-tests.lisp — FR-720 plugin architecture tests

(in-package :cl-cc/test)


(defmacro with-clean-plugin-registries (&body body)
  `(let ((cl-cc/cli:*plugin-directory* cl-cc/cli:*plugin-directory*))
     (clrhash cl-cc/cli:*loaded-plugins*)
     (clrhash cl-cc/cli:*repl-command-extensions*)
     (clrhash cl-cc/cli:*compiler-pass-extensions*)
     (clrhash cl-cc/cli:*vm-instruction-extensions*)
     (clrhash cl-cc/cli::*loaded-plugin-files*)
     ,@body))

(defun %plugin-test-handler (&rest args)
  args)

(defvar *plugin-test-command-line* nil)

(defun %plugin-test-command-handler (line)
  (setf *plugin-test-command-line* line))

(it-sequential "define-cl-cc-plugin-registers-extension-points"
  (with-clean-plugin-registries
    (cl-cc/cli:define-cl-cc-plugin fr-720-test
      :description "test plugin"
      :version "1.0"
      :repl-commands ((":hello" #'%plugin-test-command-handler :documentation "hello command"))
      :compiler-passes ((:after-parse #'%plugin-test-handler :phase :after-parse))
      :vm-instructions ((:plugin-op 'plugin-instruction :documentation "plugin op")))
    (expect (getf (gethash "fr-720-test" cl-cc/cli:*loaded-plugins*) :description) :to-equal "test plugin")
    (expect (getf (cl-cc/cli:registered-repl-command ":hello") :plugin) :to-equal 'fr-720-test)
    (setf *plugin-test-command-line* nil)
    (expect (cl-cc/cli:run-repl-command-extension ":hello world") :to-be-truthy)
    (expect *plugin-test-command-line* :to-equal ":hello world")
    (expect (getf (first (cl-cc/cli:registered-compiler-passes :after-parse)) :name) :to-equal :after-parse)
    (expect (getf (cl-cc/cli:registered-vm-instruction :plugin-op) :definition) :to-equal 'plugin-instruction)))

(it-sequential "load-cl-cc-plugins-autoloads-lisp-files"
  (with-clean-plugin-registries
    (let* ((dir (merge-pathnames
                 (format nil "cl-cc-plugin-test-~D/" (random 1000000000))
                 (uiop:temporary-directory)))
           (file (merge-pathnames "autoload-plugin.lisp" dir)))
      (ensure-directories-exist file)
      (unwind-protect
           (progn
             (with-open-file (stream file :direction :output :if-exists :supersede)
               ;; Pin the plugin file's package so its unqualified plugin name
               ;; interns as CL-CC/TEST::AUTOLOADED-PLUGIN regardless of the
               ;; *package* in effect while the test thunk runs.
               (format stream "(in-package :cl-cc/test)~%")
               (format stream "(cl-cc/cli:define-cl-cc-plugin autoloaded-plugin~%")
               (format stream "  :repl-commands ((\"auto\" #'cl:list)))~%"))
             (let ((cl-cc/cli:*plugin-directory* dir))
               (expect (cl-cc/cli:load-cl-cc-plugins :force t) :to-equal (list file))
               (expect (gethash "autoloaded-plugin" cl-cc/cli:*loaded-plugins*) :to-be-truthy)
               (expect (getf (cl-cc/cli:registered-repl-command "auto") :plugin) :to-equal 'autoloaded-plugin)))
        (ignore-errors (delete-file file))
        (ignore-errors (uiop:delete-directory-tree dir :validate t))))))
