(load
 (merge-pathnames "harness-support.lisp"
                  (make-pathname :defaults (truename *load-truename*) :name nil :type nil)))

(in-package #:cl-cc/selfhost-harness)

(handler-case (configure-project-asdf)
  (error (condition)
    (format *error-output*
            "stage2 harness initialization error (~A): ~A~%"
            (type-of condition)
            condition)
    (uiop:quit 2)))

(defun fbound-package-symbol (package-name symbol-name)
  (let ((package (find-package package-name)))
    (and package
         (multiple-value-bind (symbol status) (find-symbol symbol-name package)
           (declare (ignore status))
           (and symbol (fboundp symbol) symbol)))))

(defun %artifact-api-name-p (name)
  (and
   ;; Instruction profile output is diagnostic data, not a compiler stage artifact.
   (not (search "PROFILE" name :test (function char-equal)))
   (or
    (and
     (some (lambda (needle)
             (search needle name :test (function char-equal)))
           (list "STAGE1" "STAGE2" "STAGE-1" "STAGE-2" "SELFHOST" "SELF-HOST"))
     (some (lambda (needle)
             (search needle name :test (function char-equal)))
           (list "BUILD" "COMPILE" "SERIAL" "WRITE" "ENCODE" "EMIT"
                 "DUMP" "SAVE" "OCTET")))
    (and
     (some (lambda (needle)
             (search needle name :test (function char-equal)))
           (list "VM-ARTIFACT" "VM-PROGRAM" "VM-STAGE" "VM-BYTECODE"
                 "VM-IMAGE" "BYTECODE-ARTIFACT" "BYTECODE-PROGRAM"))
     (some (lambda (needle)
             (search needle name :test (function char-equal)))
           (list "BUILD" "COMPILE" "SERIAL" "WRITE" "ENCODE" "TO-OCTET"
                 "TO-BYTE" "TO-BYTES" "TO-OCTETS"))))))
(defun vm-artifact-api-candidates ()
  (let ((seen (make-hash-table :test (function eq))))
    (loop for package in (list-all-packages)
          for package-name = (package-name package)
          when (and (search "CL-CC" package-name :test (function char-equal))
                    (not (string= package-name "CL-CC/SELFHOST-HARNESS")))
          append
          (loop for symbol being the symbols of package
                for name = (symbol-name symbol)
                when (and (fboundp symbol)
                          (not (gethash symbol seen))
                          (%artifact-api-name-p name))
                collect
                (progn
                  (setf (gethash symbol seen) t)
                  (format nil "~A::~A"
                          (package-name (symbol-package symbol))
                          name))))))
  (defun find-fbound-api-symbol (package-names symbol-name)
    (loop for package-name in package-names
          for symbol = (fbound-package-symbol package-name symbol-name)
          when symbol
            return symbol))
(defun call-with-counted-vm-host-bridges (body)
  (let* ((vm-package (or (find-package "CL-CC/VM")
                         (error "CL-CC/VM package is unavailable")))
         (table-symbol (find-symbol "*VM-HOST-BRIDGE-FUNCTIONS*" vm-package))
         (register-symbol (find-symbol "VM-REGISTER-HOST-BRIDGE" vm-package))
         (bridge-callable-symbol (find-symbol "VM-BRIDGE-CALLABLE" vm-package))
         (table (and table-symbol
                     (boundp table-symbol)
                     (symbol-value table-symbol)))
         (bridge-count 0)
         (snapshot-complete-p nil)
         (original-entries (and (hash-table-p table)
                                (make-hash-table :test (hash-table-test table))))
         (current-wrappers (and (hash-table-p table)
                                (make-hash-table :test (hash-table-test table)))))
    (unless (and (hash-table-p table)
                 (fboundp register-symbol)
                 (fboundp bridge-callable-symbol))
      (error "VM bridge instrumentation APIs are unavailable"))
    (let ((original-register (symbol-function register-symbol)))
      (labels ((wrap-entry (key)
                 (multiple-value-bind (function present-p) (gethash key table)
                   (when (and present-p
                              (not (eq function (gethash key current-wrappers))))
                     (unless (nth-value 1 (gethash key original-entries))
                       (setf (gethash key original-entries) (cons nil nil)))
                     (unless (functionp function)
                       (error "VM host bridge table has non-function entry ~S" key))
                     (let ((wrapper
                             (let ((target function))
                               (lambda (&rest arguments)
                                 (incf bridge-count)
                                 (multiple-value-prog1
                                     (apply target arguments))))))
                       (setf (gethash key current-wrappers) wrapper
                             (gethash key table) wrapper)))))
               (wrap-all-entries ()
                 (maphash (lambda (key function)
                            (declare (ignore function))
                            (wrap-entry key))
                          table)))
        (unwind-protect
            (progn
              (unless (plusp (hash-table-count table))
                (error "VM host bridge table is empty; dispatch count cannot be measured"))
              (maphash (lambda (key function)
                         (setf (gethash key original-entries)
                               (cons t function)))
                       table)
              (setf snapshot-complete-p t)
              (wrap-all-entries)
              (setf (symbol-function register-symbol)
                    (lambda (&rest arguments)
                      (multiple-value-prog1
                          (apply original-register arguments)
                        (wrap-all-entries))))
              (funcall body
                       (lambda () bridge-count)
                       (lambda () (setf bridge-count 0))))
          (unwind-protect
              (when snapshot-complete-p
                (let ((unrecorded-keys nil))
                  (maphash (lambda (key function)
                             (declare (ignore function))
                             (unless (nth-value 1 (gethash key original-entries))
                               (push key unrecorded-keys)))
                           table)
                  (dolist (key unrecorded-keys)
                    (remhash key table)))
                (maphash
                 (lambda (key record)
                   (if (car record)
                       (setf (gethash key table) (cdr record))
                     (remhash key table)))
                 original-entries))
            (setf (symbol-function register-symbol) original-register)))))))
(defun verify-vm-host-bridge-counter (counter reset)
  (let ((run-string
         (find-fbound-api-symbol
          (list "CL-CC" "CL-CC/PIPELINE" "CL-CC/SELFHOST")
          "RUN-STRING")))
    (unless run-string
      (error "No supported RUN-STRING API is available for bridge counter control"))
    (let ((result
           (cl-cc:with-fresh-repl-state
            (funcall run-string "(run-string \"(+ 1 2)\")"))))
      (unless (eql result 3)
        (error "VM bridge counter control expected RUN-STRING to return 3, got ~S"
               result)))
    (unless (plusp (funcall counter))
      (error "VM bridge counter control did not observe a host bridge dispatch"))
    (format t "VM host bridge counter control: RUN-STRING returned 3 and observed ~D dispatch calls via VM-BRIDGE-CALLABLE.~%"
            (funcall counter))
    (funcall reset)))

(defun run-stage2-check ()
  (assert-matcher-controls)
  (let* ((selfhost-loader (fbound-package-symbol "CL-CC" "OUR-LOAD"))
         (compile-string (fbound-package-symbol "CL-CC/PIPELINE" "COMPILE-STRING"))
         (program-accessor
          (fbound-package-symbol "CL-CC/COMPILE" "COMPILATION-RESULT-PROGRAM"))
         (assembly-accessor
          (fbound-package-symbol "CL-CC/COMPILE" "COMPILATION-RESULT-ASSEMBLY"))
         (artifact-candidates (vm-artifact-api-candidates))
         (source-files (selfhost-all-source-files))
         (loaded-count 0)
         (last-source nil)
         (actual nil)
         (our-load-bridge-count 0))
    (unless (and selfhost-loader compile-string program-accessor assembly-accessor)
      (error "Required selfhost or VM compiler API entrypoints are unavailable"))
    (unless source-files
      (error "selfhost-all-source-files returned no source files"))
    (dolist (source-file source-files)
      (unless (probe-file (cdr source-file))
        (error "Selfhost source file is missing: ~A" (car source-file))))
    (when artifact-candidates
      (error "Review newly found VM artifact API candidates before classifying stage2: ~S"
             artifact-candidates))
    (format t "E2E-equivalent self-load source count: ~D; first=~A; last=~A.~%"
            (length source-files)
            (caar source-files)
            (car (car (last source-files))))
    (format t "Existing CL-CC/PIPELINE::COMPILE-FILE-TO-NATIVE writes a host-native executable via %COMPILE-NATIVE-FILE-SOURCE; it is not a selfhost stage builder or raw VM-byte serializer. No stage-specific VM builder or whole VM artifact byte serializer API names were found in loaded CL-CC packages.~%")
    (call-with-counted-vm-host-bridges
     (lambda (bridge-counter reset-counter)
       (verify-vm-host-bridge-counter bridge-counter reset-counter)
       (let ((original-features (copy-list cl:*features*)))
         (unwind-protect
             (progn
               (pushnew :cl-cc-self-hosting cl:*features*)
               (handler-bind ((warning (function muffle-warning)))
                 (cl-cc:with-fresh-repl-state
                  (let ((cl-cc:*skip-optimizer-passes* t))
                    (dolist (source-file source-files)
                      (setf last-source (car source-file))
                      (handler-case
                          (progn
                            (funcall selfhost-loader (cdr source-file))
                            (incf loaded-count))
                        (error (condition)
                               (format *error-output*
                                       "E2E-equivalent self-load stopped at ~A after ~D / ~D files; host bridge dispatch calls=~D: ~A~%"
                                       last-source loaded-count (length source-files)
                                       (funcall bridge-counter) condition)
                               (format *error-output*
                                       "Stage2 VM host bridge dispatch calls: 0 (stage2 VM did not start; this is not an FR-020 zero-bridge pass).~%")
                               (if (and (zerop loaded-count)
                                        (string= last-source "packages/bootstrap/src/package.lisp")
                                        (search "#:OR" (princ-to-string condition)
                                                :test (function char-equal)))
                                   (progn
                                     (push
                                      (list :check :stage2-fixpoint
                                            :id :self-load-stopped-at-bootstrap-package
                                            :classification :bug
                                            :reason "OUR-LOAD stopped at packages/bootstrap/src/package.lisp with an unbound generated OR variable.")
                                      actual)
                                     (format *error-output*
                                             "Recorded baseline mismatch: :self-load-stopped-at-bootstrap-package (:bug).~%")
                                     (return))
                                 (error condition)))))))))
           (setf cl:*features* original-features)))
       (setf our-load-bridge-count (funcall bridge-counter))))
    (format t "E2E-equivalent our-load progress: loaded ~D / ~D source files; stop=~A.~%"
            loaded-count
            (length source-files)
            (or last-source :complete))
    (format t "Host bridge dispatch calls during self-load: ~D (VM host bridge table callables were wrapped).~%"
            our-load-bridge-count)
    (format t "Stage2 VM host bridge dispatch calls: 0 (stage2 VM did not start; this is not an FR-020 zero-bridge pass).~%")
    (let* ((result (funcall compile-string "(+ 1 2)" :target :vm))
           (program (funcall program-accessor result))
           (assembly (funcall assembly-accessor result)))
      (unless (and program assembly)
        (error "Host VM API probe did not produce both program and assembly"))
      (format t "Host VM API probe: result=~A; program=~A; assembly=~A; stage artifacts=absent.~%"
              (type-of result)
              (type-of program)
              (type-of assembly)))
    (format t
            "Stage2 stop boundary: E2E-equivalent our-load ~A after ~D / ~D source files at ~A; compile-file-to-native emits a host-native executable, not a VM stage artifact. No selfhost stage builder or raw-byte serializer is available, so stage1/stage2 byte comparison did not start.~%"
            (if (= loaded-count (length source-files)) "completed" "stopped")
            loaded-count
            (length source-files)
            (or last-source :complete))
    (push
     (list :check :stage2-fixpoint
           :id :stage2-output-unavailable
           :classification :unimplemented
           :reason "compile-file-to-native produces a host-native executable via %compile-native-file-source; it cannot provide the stage1 VM artifact or raw VM bytes for stage1-to-stage2 comparison, and no selfhost stage builder or serializer is available.")
     actual)
    (unless (check-mismatch-set :stage2-fixpoint actual (read-expected-mismatches))
      (uiop:quit 1))))

(handler-case (run-stage2-check)
  (error (condition)
    (format *error-output* "stage2 harness error (~A): ~A~%" (type-of condition) condition)
    (uiop:quit 2)))
