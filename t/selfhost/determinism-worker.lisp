(load
 (merge-pathnames "harness-support.lisp"
                  (make-pathname :defaults (truename *load-truename*) :name nil :type nil)))

(in-package #:cl-cc/selfhost-harness)

(defun run-worker ()
  (let ((arguments (uiop:command-line-arguments)))
    (unless (= (length arguments) 3)
      (error "Worker expects input, output, and cache-root paths; received ~S" arguments))
    (destructuring-bind (input output cache-root) arguments
      (configure-project-asdf)
      (let ((input-path (truename input))
            (output-path (pathname output))
            (cache-path (uiop:ensure-directory-pathname (pathname cache-root))))
        (when (probe-file cache-path)
          (error "Worker cache root was not empty before compilation: ~A" cache-path))
        (when (probe-file output-path)
          (delete-file output-path))
        (when (probe-file output-path)
          (error "Worker could not clear output path before compilation: ~A" output-path))
        (format t "output path absent before compilation: ~A~%" output-path)
        (let ((cl-cc/pipeline:*compile-cache-root* cache-path))
          (cl-cc/pipeline::compile-file-to-native input-path
                                                  :language
                                                  :lisp
                                                  :arch
                                                  :x86-64
                                                  :target-os
                                                  :linux
                                                  :output-file
                                                  output-path))
        (unless
            (and (probe-file output-path)
                 (plusp
                  (with-open-file
                      (stream output-path :direction :input :element-type '(unsigned-byte 8))
                    (file-length stream))))
          (error "Native compilation did not produce a nonempty output: ~A" output-path))
        (unless (probe-file cache-path)
          (error "Native compilation did not create its isolated cache root: ~A" cache-path))
        (format t "compiled ~A to ~A with an initially empty cache root~%" input-path output-path)))))

(handler-case (run-worker)
  (error (condition)
    (format *error-output* "determinism worker error (~A): ~A~%" (type-of condition) condition)
    (uiop:quit 2)))
