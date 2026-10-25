(defpackage #:cl-cc-parity-worker (:use #:cl))

(in-package #:cl-cc-parity-worker)

(defun environment-value (name)
  (or (uiop:getenv name)
      (error "Required environment variable ~A is missing" name)))

(defun find-function (package-name symbol-name)
  (let* ((package
          (or (find-package package-name)
              (error "Package ~A is not loaded" package-name)))
         (symbol
          (or (find-symbol symbol-name package)
              (error "Symbol ~A::~A is not present" package-name symbol-name))))
    (unless (fboundp symbol)
      (error "Symbol ~A::~A is not a function" package-name symbol-name))
    (symbol-function symbol)))

(defun write-octets (path octets)
  (with-open-file
      (stream path
              :direction
              :output
              :if-exists
              :supersede
              :if-does-not-exist
              :create
              :element-type
              '(unsigned-byte 8))
    (write-sequence octets stream)))

(defun write-text (path text)
  (write-octets path (sb-ext:string-to-octets text :external-format :utf-8)))

(defun split-tabs (line)
  (loop with start = 0
        for
        end = (position #\Tab line :start start)
        collect (subseq line start end)
        while
        end
        do (setf start (1+ end))))

(defun case-records (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil nil)
          while line
          unless (or (string= line "") (char= (char line 0) #\#))
            collect (split-tabs line))))

(defun path-in (directory name)
  (format nil "~A/~A" (string-right-trim "/" directory) name))

(defun call-function (package-name symbol-name &rest args)
  (apply (find-function package-name symbol-name) args))

(defun write-failure (directory prefix condition)
  (write-text (path-in directory (format nil "~A.stderr" prefix))
              (format nil "~A~%" condition)))

(defun run-vm (source stdlib source-kind directory)
  (let ((captured (make-string-output-stream))
        (result nil)
        (condition nil)
        (program nil))
    (call-function "CL-CC/COMPILE" "RESET-REPL-STATE")
    (handler-case (let ((*standard-output* captured))
                    (setf result
                          (if (string= source-kind "vm-program")
                              (progn
                                (setf program (eval (read-from-string source)))
                                (call-function "CL-CC/VM"
                                               "RUN-COMPILED"
                                               program))
                              (if (string= stdlib "yes")
                                  (call-function "CL-CC"
                                                 "RUN-STRING"
                                                 source
                                                 :stdlib
                                                 t)
                                  (call-function "CL-CC"
                                                 "RUN-STRING"
                                                 source)))))
      (error (caught)
        (setf condition caught)))
    (if condition
        (progn
          (write-text (path-in directory "vm.status")
                      (format nil "run-failure~%"))
          (write-failure directory "vm" condition))
        (progn
          (unless (integerp result)
            (error "VM result for ~A is not an integer process status: ~S"
                   (environment-value "PARITY_CASE")
                   result))
          (write-text (path-in directory "vm.status") (format nil "ok~%"))
          (write-text (path-in directory "vm.value") (format nil "~S~%" result))
          (write-text (path-in directory "vm.exit")
                      (format nil "~D~%" (logand result #xff)))
          (write-text (path-in directory "vm.stdout")
                      (get-output-stream-string captured))))
    program))

(defun compile-native (source target artifact)
  (let ((arch
         (cond
           ((string= target "x86-64") :x86-64)
           ((string= target "aarch64") :arm64)
           ((string= target "riscv64") :riscv64)
           (t (error "Unknown native parity target ~A" target)))))
    (call-function "CL-CC"
                   "COMPILE-TO-NATIVE"
                   source
                   :arch
                   arch
                   :target-os
                   :linux
                   :output-file
                   artifact)))

(defun compile-wasm (source artifact source-kind direct-program)
  (let* ((program
          (if (string= source-kind "vm-program")
              direct-program
              (let* ((compilation
                      (call-function "CL-CC"
                                     "COMPILE-STRING"
                                     source
                                     :target
                                     :vm))
                     (compiled-program
                      (call-function "CL-CC/COMPILE"
                                     "COMPILATION-RESULT-PROGRAM"
                                     compilation)))
                compiled-program)))
         (result (call-function "CL-CC/CODEGEN" "COMPILE-TO-AOT-WASM" program))
         (bytes (call-function "CL-CC/CODEGEN" "WASM-AOT-RESULT-BYTES" result)))
    (write-octets artifact bytes)))

(defun compile-target (source target artifact source-kind direct-program)
  (if (string= target "wasm")
      (compile-wasm source artifact source-kind direct-program)
      (compile-native source target artifact)))

(defun process-case (record target work-root program-root support-root)
  (destructuring-bind (name category filename stdlib source-kind targets) record
    (declare (ignore category))
    (let* ((directory (path-in work-root name))
           (source-path (path-in program-root filename))
           (artifact
            (path-in directory
                     (if (string= target "wasm")
                         "program.wasm"
                         "program.elf")))
           (original-directory (uiop:getcwd))
           (source (uiop:read-file-string source-path))
           (compile-errors (make-string-output-stream))
           (compile-output (make-string-output-stream))
           (compile-condition nil)
           (direct-program nil)
           (target-applicable-p
            (or (string= targets "all") (string= targets target))))
      (dolist (support-file '("loaded-value.lisp" "loaded-continuation.lisp"))
        (let ((source (path-in support-root support-file))
              (destination (path-in directory support-file)))
          (when (probe-file source)
            (uiop:copy-file source destination))))
      (unwind-protect
          (let ((*default-pathname-defaults*
                 (uiop:ensure-directory-pathname directory)))
            (uiop:chdir directory)
            (setf direct-program (run-vm source stdlib source-kind directory))
            (if target-applicable-p
                (progn
                  (handler-case (let ((*standard-output* compile-output)
                                      (*error-output* compile-errors))
                                  (compile-target source
                                                  target
                                                  artifact
                                                  source-kind
                                                  direct-program))
                    (error (caught)
                      (setf compile-condition caught)))
                  (if compile-condition
                      (progn
                        (write-text (path-in directory "compile.status")
                                    (format nil "compile-failure~%"))
                        (write-text (path-in directory "compile.stdout")
                                    (get-output-stream-string compile-output))
                        (write-text (path-in directory "compile.stderr")
                                    (format nil
                                            "~A~%~A"
                                            compile-condition
                                            (get-output-stream-string
                                             compile-errors))))
                      (progn
                        (write-text (path-in directory "compile.status")
                                    (format nil "compiled~%"))
                        (write-text (path-in directory "compile.stdout")
                                    (get-output-stream-string compile-output))
                        (write-text (path-in directory "compile.stderr")
                                    (get-output-stream-string compile-errors)))))
                (progn
                  (write-text (path-in directory "compile.status")
                              (format nil "not-applicable~%"))
                  (write-text (path-in directory "compile.stdout") "")
                  (write-text (path-in directory "compile.stderr")
                              (format nil "Fixture target set: ~A~%" targets)))))
        (uiop:chdir original-directory))))
)

(defun main ()
  (let* ((target (environment-value "PARITY_TARGET"))
         (cases (case-records (environment-value "PARITY_CASES")))
         (work-root (environment-value "PARITY_WORK_DIR"))
         (program-root (environment-value "PARITY_PROGRAM_DIR"))
         (support-root (environment-value "PARITY_SUPPORT_DIR")))
    (dolist (record cases)
      (let ((*standard-output* (make-string-output-stream))
            (*error-output* (make-string-output-stream)))
        (process-case record target work-root program-root support-root)))
    (write-text (path-in work-root "worker.status") (format nil "ok~%"))))

(handler-case (main)
  (error (condition)
    (let ((work-root (uiop:getenv "PARITY_WORK_DIR")))
      (when work-root
        (write-failure work-root "worker" condition)
        (write-text (path-in work-root "worker.status")
                    (format nil "run-failure~%"))))
    (uiop:quit 1)))
