;;;; Batch PHPT executor. Python extracts and validates PHPT sections; this
;;;; process loads CL-CC once and runs each source in a fresh VM state.

(progn (require :asdf) (require :sb-posix))

(eval-when (:load-toplevel :execute)
  (unless (every #'find-package
                 '("CL-CC" "CL-CC/PHP" "CL-CC/COMPILE" "CL-CC/VM"
                   "CL-CC/TOOLS/LSP"))
    (error "PHPT driver requires a core with cl-cc and cl-cc-tools loaded"))
  (unless (fboundp 'cl-cc/tools/lsp::parse-json)
    (asdf:load-system :cl-cc-tools))
  (unless (fboundp 'cl-cc/tools/lsp::parse-json)
    (error "PHPT driver requires the cl-cc-tools JSON parser")))

(defun phpt-driver-json-string (string stream)
  (write-char #\" stream)
  (loop for character across string
        for code = (char-code character)
        do (case character
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              (if (< code 32)
                  (format stream "\\u~4,'0X" code)
                  (write-char character stream)))))
  (write-char #\" stream))

(defun phpt-driver-result (id phase status stdout stderr detail stream)
  (write-string "{\"id\":" stream)
  (phpt-driver-json-string id stream)
  (write-string ",\"phase\":" stream)
  (phpt-driver-json-string phase stream)
  (write-string ",\"status\":" stream)
  (phpt-driver-json-string status stream)
  (write-string ",\"stdout\":" stream)
  (phpt-driver-json-string stdout stream)
  (write-string ",\"stderr\":" stream)
  (phpt-driver-json-string stderr stream)
  (write-string ",\"detail\":" stream)
  (phpt-driver-json-string detail stream)
  (write-string "}" stream)
  (terpri stream)
  (force-output stream))

(defun phpt-driver-read-source (path)
  (with-open-file (input path :direction :input :external-format :utf-8)
    (let ((source (make-string-output-stream)))
      (loop for character = (read-char input nil nil)
            while character
            do (write-char character source))
      (get-output-stream-string source))))
(defun phpt-driver-write-progress (path phase stage)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (format stream "~A-~A~%" phase stage)
    (force-output stream)))
(defun phpt-driver-environment-pair (entry)
  (let ((separator (position #\= entry)))
    (when separator
      (cons (subseq entry 0 separator)
            (subseq entry (1+ separator)))))
)
(defun phpt-driver-set-environment-variable (name value)
  (unless (zerop (sb-posix:setenv name value 1))
    (error "Could not set environment variable ~A" name)))
(defun phpt-driver-unset-environment-variable (name)
  (unless (zerop (sb-posix:unsetenv name))
    (error "Could not unset environment variable ~A" name)))
(defun phpt-driver-restore-environment (snapshot phase)
  (declare (ignore phase))
  (dolist (entry (sb-ext:posix-environ))
    (let ((pair (phpt-driver-environment-pair entry)))
      (when pair
        (phpt-driver-unset-environment-variable (car pair)))))
  (dolist (entry snapshot)
    (let ((pair (phpt-driver-environment-pair entry)))
      (when pair
        (phpt-driver-set-environment-variable (car pair) (cdr pair))))))

(defun phpt-driver-run-script
    (phase source-path working-directory progress-path ini-settings capture stdin-path
     environment-snapshot)
  (phpt-driver-write-progress progress-path phase "source")
  (multiple-value-bind (source read-error)
      (handler-case (values (phpt-driver-read-source source-path) nil)
        (error (condition) (values nil condition)))
    (if read-error
        (values "source" "" ""
                (format nil "cannot read ~A source: ~A" phase read-error))
        (let* ((capture-stdin (char= (char capture 0) #\1))
               (capture-stdout (char= (char capture 1) #\1))
               (capture-stderr (char= (char capture 2) #\1))
               (output-buffer (make-string-output-stream))
               (error-buffer (make-string-output-stream))
               (sink (make-broadcast-stream))
               (output-stream
                 (if capture-stdout
                     (make-broadcast-stream output-buffer *standard-output*)
                     sink))
               (error-stream
                 (cond
                   ((and capture-stdout capture-stderr) (make-broadcast-stream output-buffer error-buffer *standard-output* *error-output*))
                   (capture-stderr
                    (make-broadcast-stream error-buffer *standard-output* *error-output*))
                   (t sink)))
               (input-path-p (and stdin-path (plusp (length stdin-path))))
               (input-stream nil))
          (labels ((captured-output ()
                     (cond
                       (capture-stdout (get-output-stream-string output-buffer))
                       (capture-stderr (get-output-stream-string error-buffer))
                       (t "")))
                   (captured-error ()
                     (if capture-stderr
                         (get-output-stream-string error-buffer)
                         "")))
            (handler-case
                (unwind-protect
                     (progn
                       (phpt-driver-restore-environment environment-snapshot phase)
                       (phpt-driver-write-progress progress-path phase "ini")
                       (setf input-stream
                             (cond
                               (input-path-p
                                (open stdin-path :direction :input :external-format :utf-8))
                               (capture-stdin (make-string-input-stream ""))
                               (t *standard-input*)))
                       (let ((*standard-input* input-stream)
                             (*standard-output* output-stream)
                             (*error-output* error-stream)
                             (cl-cc/php::*php-ini-settings* nil)
                             (cl-cc/php::*php-error-reporting-level* 32767)
                             (cl-cc/php::*php-error-handler-stack* nil)
                             (cl-cc/php::*php-exception-handler-stack* nil)
                             (cl-cc/php::*php-default-timezone* "UTC")
                             (cl-cc/php::*php-file-locks* (make-hash-table :test #'equal)))
                         (dolist (setting ini-settings)
                           (cl-cc/php::%php-ini-set (first setting) (second setting)))
                         (phpt-driver-write-progress progress-path phase "compile")
                         (let ((stage "compile"))
                           (handler-case
                               (let* ((result
                                        (cl-cc:compile-string source :target :vm :language :php))
                                      (program
                                        (cl-cc/compile:compilation-result-program result))
                                      (state
                                        (cl-cc/vm:make-vm-state :output-stream output-stream)))
                                 (setf stage "runtime")
                                 (phpt-driver-write-progress progress-path phase "runtime")
                                 (uiop:with-current-directory (working-directory)
                                   (cl-cc:run-compiled program :state state))
                                 (values "passed" (captured-output) (captured-error) ""))
                             (error (condition)
                               (values stage
                                       (captured-output)
                                       (captured-error)
                                       (format nil "~A error: ~A" stage condition)))))))
                  (when (and input-path-p (streamp input-stream))
                    (close input-stream))
                  (phpt-driver-write-progress progress-path phase "cleanup")
                  (phpt-driver-write-progress progress-path phase "done"))
              (error (condition)
                (values "invalid" "" ""
                        (format nil "PHPT setup/cleanup failed: ~A" condition)))))))))

(defun phpt-driver-run-one (request results)
  (let* ((id (cdr (assoc "id" request :test #'string=)))
         (phase (cdr (assoc "phase" request :test #'string=)))
         (source-path (cdr (assoc "source_path" request :test #'string=)))
         (working-directory (cdr (assoc "working_directory" request :test #'string=)))
         (progress-path (cdr (assoc "progress_path" request :test #'string=)))
         (ini-settings (cdr (assoc "ini_settings" request :test #'string=)))
         (capture (cdr (assoc "capture" request :test #'string=)))
         (stdin-path (cdr (assoc "stdin_path" request :test #'string=)))
         (environment-snapshot (sb-ext:posix-environ)))
    (multiple-value-bind (status stdout stderr detail)
        (phpt-driver-run-script
         phase source-path working-directory progress-path ini-settings capture stdin-path
         environment-snapshot)
      (phpt-driver-result id phase status stdout stderr detail results))))


(defun phpt-driver-read-json (path)
  (with-open-file (input path :direction :input :external-format :utf-8)
    (let ((source (with-output-to-string (output)
                    (loop for character = (read-char input nil nil)
                          while character
                          do (write-char character output)))))
      (cl-cc/tools/lsp::parse-json source))))

(defun phpt-driver-main ()
  (let* ((arguments (uiop:command-line-arguments))
         (project-root (first arguments))
         (request-path (second arguments))
         (results-path (third arguments)))
    (unless (and project-root request-path results-path)
      (format *error-output*
              "usage: driver.lisp PROJECT-ROOT REQUEST.json RESULT.json~%")
      (uiop:quit 2))
    (let* ((request (phpt-driver-read-json request-path))
           (allowed-keys
             '("id" "phase" "source_path" "working_directory" "progress_path"
               "ini_settings" "capture" "stdin_path"))
           (id (cdr (assoc "id" request :test #'string=)))
           (phase (cdr (assoc "phase" request :test #'string=)))
           (source-path (cdr (assoc "source_path" request :test #'string=)))
           (working-directory (cdr (assoc "working_directory" request :test #'string=)))
           (progress-path (cdr (assoc "progress_path" request :test #'string=)))
           (ini-settings (cdr (assoc "ini_settings" request :test #'string=)))
           (capture (cdr (assoc "capture" request :test #'string=)))
           (stdin-path (cdr (assoc "stdin_path" request :test #'string=))))
      (unless
          (and
           (listp request)
           (= (length request) 8)
           (every (lambda (entry)
                    (and (consp entry)
                         (stringp (car entry))
                         (member (car entry) allowed-keys :test #'string=)))
                  request)
           (every (lambda (key) (assoc key request :test #'string=)) allowed-keys)
           (stringp id)
           (plusp (length id))
           (member phase '("skipif" "main" "clean") :test #'string=)
           (stringp source-path)
           (plusp (length source-path))
           (stringp working-directory)
           (plusp (length working-directory))
           (stringp progress-path)
           (plusp (length progress-path))
           (listp ini-settings)
           (every (lambda (setting)
                    (and (listp setting)
                         (= (length setting) 2)
                         (every #'stringp setting)))
                  ini-settings)
           (stringp capture)
           (= (length capture) 3)
           (every (lambda (flag) (find flag "01")) capture)
           (or (null stdin-path) (stringp stdin-path)))
        (error "Malformed PHPT JSON request: ~S" request))
      (with-open-file (results results-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
        (phpt-driver-run-one request results))))
  (uiop:quit 0))


(phpt-driver-main)
