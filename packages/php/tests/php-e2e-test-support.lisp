(in-package :cl-cc/test)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-run-capture (source)
    "Compile PHP SOURCE to VM and run it, returning everything it echoed as a
string. compile-string with :language :php registers the PHP host bridges, so a
fresh VM state runs the program end-to-end."
    (let* ((result  (cl-cc:compile-string source :target :vm :language :php))
           (program (cl-cc/compile:compilation-result-program result))
           (out     (make-string-output-stream)))
      (cl-cc/vm:run-compiled program :output-stream out)
      ;; Trim a trailing newline the VM appends when flushing program output.
      (string-right-trim '(#\Newline) (get-output-stream-string out)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-run-capture-io (source &key ini-settings)
    "Compile PHP SOURCE and capture both stdout and error output."
    (let ((cl-cc/php::*php-ini-settings* ini-settings)
          (cl-cc/php::*php-error-reporting-level* 32767))
      (let* ((result  (cl-cc:compile-string source :target :vm :language :php))
             (program (cl-cc/compile:compilation-result-program result))
             (out     (make-string-output-stream))
             (err     (make-string-output-stream)))
        (handler-case
            (let ((*error-output* err))
              (cl-cc/vm:run-compiled program :output-stream out))
          (error (c)
            (declare (ignore c))))
        (values (string-right-trim '(#\Newline)
                                   (get-output-stream-string out))
                (string-right-trim '(#\Newline)
                                   (get-output-stream-string err)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %php-make-ini-settings (&rest pairs)
    "Create a fresh INI settings table for tests."
    (let ((table (make-hash-table :test 'equal)))
      (dolist (entry cl-cc/php::*php-ini-defaults* table)
        (setf (gethash (car entry) table) (cdr entry)))
      (loop for (key value) on pairs by #'cddr
            do (setf (gethash key table) value))
      table)))
