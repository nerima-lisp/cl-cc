(require :asdf)
(require :uiop)

(let ((arguments (uiop:command-line-arguments)))
  (unless (= (length arguments) 1)
    (format *error-output*
            "Usage: run-pipeline-tests.lisp <cl-cc/test/pipeline-javascript|cl-cc/test/pipeline-php>~%")
    (uiop:quit 2))
  (unless (member (first arguments)
                  '("cl-cc/test/pipeline-javascript"
                    "cl-cc/test/pipeline-php")
                  :test #'string=)
    (format *error-output* "Unsupported pipeline test system: ~A~%"
            (first arguments))
    (uiop:quit 2)))

(let* ((system-name (first (uiop:command-line-arguments)))
       (here (uiop:pathname-directory-pathname
              (or *load-truename* (uiop:getcwd)))))
  (unless (asdf:find-system system-name nil)
    (handler-case
        (asdf:load-asd (merge-pathnames "cl-cc.asd" here))
      (error (condition)
        (format *error-output* "~&FATAL: ~A~%" condition)
        (uiop:quit 1)))))

(setf *default-pathname-defaults* (uiop:getcwd)
      uiop:*temporary-directory* (uiop:temporary-directory))

(format t "# loading ~A~%" (first (uiop:command-line-arguments)))
(handler-case
    (asdf:load-system (first (uiop:command-line-arguments)))
  (error (condition)
    (format *error-output* "~&FATAL: ~A~%" condition)
    (uiop:quit 1)))

(let ((lock (sb-thread:make-mutex :name "macro-eval-lock")))
  (setf cl-cc/expand:*macro-eval-fn*
        (lambda (form) (sb-thread:with-mutex (lock) (eval form)))))

(let ((home (uiop:getenv "HOME")))
  (unless home
    (format *error-output* "FATAL: HOME is required to rebase the stdlib cache.~%")
    (uiop:quit 1))
  (setf cl-cc/pipeline::*stdlib-cache-directory*
        (merge-pathnames #P".cache/cl-cc/"
                         (uiop:ensure-directory-pathname (pathname home)))))

(handler-case
    (let* ((worker-env (uiop:getenv "CL_CC_TEST_WORKERS"))
           (max-workers (if worker-env (parse-integer worker-env) 4)))
      (unless (plusp max-workers)
        (error "CL_CC_TEST_WORKERS must be a positive integer, got ~S."
               worker-env))
      (format t "# warming stdlib cache~%")
      (cl-cc:warm-stdlib-cache)
      (format t "# stdlib cache ready~%")
      (uiop:quit
       (if (cl-weave:run-all :reporter :spec :max-workers max-workers)
           0
           1)))
  (error (condition)
    (format t "~&not ok - run-all fatal error: ~A~%" condition)
    (format *error-output* "~&FATAL: ~A~%" condition)
    (uiop:quit 1)))
