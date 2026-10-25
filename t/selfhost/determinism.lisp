(load
 (merge-pathnames "harness-support.lisp"
                  (make-pathname :defaults (truename *load-truename*) :name nil :type nil)))

(in-package #:cl-cc/selfhost-harness)

(defun temporary-directory ()
  (let ((directory
         (uiop:ensure-directory-pathname
          (merge-pathnames
           (format nil
                   "cl-cc-determinism-~36R-~36R/"
                   (get-universal-time)
                   (random most-positive-fixnum))
           (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "placeholder" directory))
    directory))

(defun run-native-compile (worker input output cache-root run-label)
  (let ((command
         (list (namestring sb-ext:*runtime-pathname*)
               "--core"
               (namestring sb-ext:*core-pathname*)
               "--noinform"
               "--script"
               (namestring worker)
               (namestring input)
               (namestring output)
               (namestring cache-root))))
    (multiple-value-bind (stdout stderr status)
        (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
      (format t "~A stdout:~%~A" run-label stdout)
      (unless (zerop status)
        (format *error-output* "~A stderr:~%~A" run-label stderr)
        (error "Fresh SBCL compilation process exited with status ~D" status))
      (when (plusp (length stderr))
        (format t "~A stderr:~%~A" run-label stderr)))))

(defun first-differing-offset (left right)
  (loop for index below (min (length left) (length right))
        unless (= (aref left index) (aref right index))
          return index
        finally (return (min (length left) (length right)))))

(defun run-determinism-check ()
  (assert-matcher-controls)
  (configure-project-asdf)
  (let* ((temporary-root (temporary-directory))
         (worker (harness-path "determinism-worker.lisp"))
         (source (harness-path "determinism-input.lisp"))
         (run-one (merge-pathnames "run-one/" temporary-root))
         (run-two (merge-pathnames "run-two/" temporary-root))
         (input-one (merge-pathnames "input.lisp" run-one))
         (input-two (merge-pathnames "input.lisp" run-two))
         (output-one (merge-pathnames "native-output.bin" run-one))
         (output-two (merge-pathnames "native-output.bin" run-two))
         (cache-one (merge-pathnames "cache/" run-one))
         (cache-two (merge-pathnames "cache/" run-two))
         (first-output nil)
         (second-output nil)
         (actual nil))
    (labels ((write-octets (pathname octets)
               (ensure-directories-exist pathname)
               (with-open-file (stream pathname
                                       :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :error
                                       :if-does-not-exist :create)
                 (write-sequence octets stream)))
             (path-offset (octets pathname)
               (let ((path-string (namestring pathname)))
                 (when (every (lambda (character) (< (char-code character) 128))
                              path-string)
                   (let ((needle
                           (map '(vector (unsigned-byte 8))
                                #'char-code
                                path-string)))
                     (loop for start from 0 to (- (length octets) (length needle))
                           when (loop for index below (length needle)
                                      always (= (aref octets (+ start index))
                                                (aref needle index)))
                             return start))))
             (path-pair-at-offset-p (left-output left-path right-output right-path offset)
               (let ((left-offset (path-offset left-output left-path))
                     (right-offset (path-offset right-output right-path))
                     (path-length (length (namestring left-path))))
                 (and left-offset
                      right-offset
                      (= left-offset right-offset)
                      (= path-length (length (namestring right-path)))
                      (<= left-offset offset)
                      (< offset (+ left-offset path-length)))))
             (absolute-path-difference-p (offset)
               (or (path-pair-at-offset-p first-output input-one
                                          second-output input-two offset)
                   (path-pair-at-offset-p first-output output-one
                                          second-output output-two offset)
                   (path-pair-at-offset-p first-output cache-one
                                          second-output cache-two offset))))
      (unwind-protect
          (progn
            (unless (and (probe-file source) (probe-file worker))
              (error "Determinism inputs are missing from the workspace"))
            (let ((source-octets (read-octets source)))
              (write-octets input-one source-octets)
              (write-octets input-two source-octets)
              (unless (and (equalp source-octets (read-octets input-one))
                           (equalp source-octets (read-octets input-two)))
                (error "Copied determinism inputs do not match the fixture bytes")))
            (unless (and (not (equal (namestring input-one) (namestring input-two)))
                         (not (equal (namestring output-one) (namestring output-two))))
              (error "Determinism runs must use distinct input and output paths"))
            (run-native-compile worker input-one output-one cache-one "compile 1")
            (setf first-output (read-octets output-one))
            (run-native-compile worker input-two output-two cache-two "compile 2")
            (setf second-output (read-octets output-two))
            (if (equalp first-output second-output)
                (format t "raw output bytes match: ~D octets~%" (length first-output))
                (let* ((offset (first-differing-offset first-output second-output))
                       (absolute-path-p (absolute-path-difference-p offset)))
                  (format t
                          "raw output bytes differ: first offset ~D, sizes ~D and ~D~%"
                          offset
                          (length first-output)
                          (length second-output))
                  (if absolute-path-p
                      (format t
                              "first differing byte is inside corresponding run-specific absolute paths~%")
                      (format t
                              "difference cause is unclassified (time, gensym, and hash order are not inferred); no normalization was applied~%"))
                  (push
                   (list :check
                         :determinism
                         :id
                         (if absolute-path-p
                             :absolute-path-in-native-output
                             :native-output-bytes-differ)
                         :classification
                         :bug
                         :reason
                         (if absolute-path-p
                             "The first differing bytes are in corresponding run-specific absolute paths."
                             "The same Lisp input produced different native output bytes; cause is unclassified."))
                   actual)))
            (unless (check-mismatch-set :determinism actual (read-expected-mismatches))
              (uiop:quit 1)))
        (when (probe-file temporary-root)
          (uiop:delete-directory-tree temporary-root :validate t :if-does-not-exist :ignore)))))))

(handler-case (run-determinism-check)
  (error (condition)
    (format *error-output* "determinism harness error (~A): ~A~%" (type-of condition) condition)
    (uiop:quit 2)))
