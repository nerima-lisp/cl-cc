;;;; tests/unit/cli/test-support.lisp — shared CLI test helpers

(in-package :cl-cc/test)

(defmacro with-cli-exit (&body body)
  "Run BODY with the CLI system boundary throwing its requested exit code."
  (let ((exit-tag (gensym "CLI-EXIT"))
        (system (gensym "SYSTEM")))
    `(let* ((,system
              (cl-boundary-kit:make-system-boundary
               :exit-fn (lambda (code) (throw ',exit-tag code))))
            (cl-cc/cli::*cli-boundaries*
              (cl-boundary-kit:make-boundary-context :system ,system)))
       (catch ',exit-tag
         ,@body))))

(defun make-cli-parsed (&key command positional flags)
  (let ((parsed (cl-cc/cli::make-parsed-args :command command
                                             :positional positional)))
    (dolist (entry flags parsed)
      (setf (gethash (car entry) (cl-cc/cli:parsed-args-flags parsed))
            (cdr entry)))))
