(require :asdf)
(require :uiop)
(handler-case
    (let* ((script-file
             (or *load-truename*
                 (merge-pathnames "scripts/gc-repro.lisp" (uiop:getcwd))))
           (script-directory (uiop:pathname-directory-pathname script-file))
           (project-root
             (make-pathname
              :directory (butlast (pathname-directory script-directory))
              :name nil
              :type nil
              :defaults script-directory)))
      (unless (asdf:find-system "cl-cc/test" nil)
        (asdf:load-asd (merge-pathnames "cl-cc.asd" project-root))))
  (error (condition)
    (format *error-output*
            "GC_REPRO: ERROR phase=system-registry message=~A~%"
            condition)
    (finish-output *error-output*)
    (uiop:quit 1)))

(setf *default-pathname-defaults* (uiop:getcwd))
(setf uiop:*temporary-directory* (uiop:temporary-directory))

(format t "# loading cl-cc/test~%")
(handler-case
    (asdf:load-system "cl-cc/test")
  (error (condition)
    (format *error-output*
            "GC_REPRO: ERROR phase=load message=~A~%"
            condition)
    (finish-output *error-output*)
    (uiop:quit 1)))

(let ((lock (sb-thread:make-mutex :name "macro-eval-lock")))
  (setf cl-cc/expand:*macro-eval-fn*
        (lambda (form)
          (sb-thread:with-mutex (lock) (eval form)))))

(let ((home (uiop:getenv "HOME")))
  (when home
    (setf cl-cc/pipeline::*stdlib-cache-directory*
          (merge-pathnames #P".cache/cl-cc/"
                           (uiop:ensure-directory-pathname (pathname home))))))

(format t "# warming stdlib cache~%")
(cl-cc:warm-stdlib-cache)
(format t "# stdlib cache ready~%")


(defconstant +gc-repro-baseline-bytes+ (* 2 1024 1024 1024))
(defconstant +gc-repro-threshold-bytes+ (* 1 1024 1024))
(defconstant +gc-repro-target-count+ 7)

(defun gc-repro-log (event &rest fields)
  (format *error-output* "GC_REPRO: ~A" event)
  (loop for (key value) on fields by #'cddr
        do (format *error-output* " ~A=~A" key value))
  (terpri *error-output*)
  (finish-output *error-output*))

(defun gc-repro-parse-options (arguments)
  (let ((options (make-hash-table :test #'equal))
        (allowed '("--condition" "--iterations" "--attempt")))
    (unless (evenp (length arguments))
      (error "Expected --condition, --iterations, and --attempt option/value pairs."))
    (loop for tail on arguments by #'cddr
          for option = (first tail)
          for value = (second tail)
          do (unless (member option allowed :test #'equal)
               (error "Unknown option ~S." option))
             (when (gethash option options)
               (error "Option ~S was provided more than once." option))
             (setf (gethash option options) value))
    (dolist (option allowed)
      (unless (gethash option options)
        (error "Missing required option ~S." option)))
    (let ((condition (gethash "--condition" options)))
      (unless (member condition '("baseline" "threshold" "verifier")
                      :test #'equal)
        (error "Condition must be baseline, threshold, or verifier, got ~S."
               condition)))
    (let ((iterations (parse-integer (gethash "--iterations" options)))
          (attempt (parse-integer (gethash "--attempt" options))))
      (unless (plusp iterations)
        (error "--iterations must be a positive integer."))
      (unless (plusp attempt)
        (error "--attempt must be a positive integer."))
      (list :condition (gethash "--condition" options)
            :iterations iterations
            :attempt attempt))))

(defun gc-repro-elapsed-ms (start)
  (floor (* 1000 (- (get-internal-real-time) start))
         internal-time-units-per-second))

(defun gc-repro-configure (condition attempt)
  (let ((expected-bytes
          (if (equal condition "threshold")
              +gc-repro-threshold-bytes+
              +gc-repro-baseline-bytes+)))
    (when (equal condition "threshold")
      (setf (sb-ext:bytes-consed-between-gcs) expected-bytes))
    (unless (= (sb-ext:bytes-consed-between-gcs) expected-bytes)
      (error "GC threshold readback mismatch for ~A: expected ~D, got ~D."
             condition expected-bytes (sb-ext:bytes-consed-between-gcs)))
    (if (equal condition "verifier")
        (progn
          (setf (sb-alien:extern-alien "verify_gens" sb-alien:char) 0)
          (setf (sb-alien:extern-alien "pre_verify_gen_0" sb-alien:int) 1)
          (unless (and (= (sb-alien:extern-alien
                           "verify_gens" sb-alien:char)
                          0)
                       (= (sb-alien:extern-alien
                           "pre_verify_gen_0" sb-alien:int)
                          1))
            (error "GC verifier flag readback mismatch."))
          (gc-repro-log "CONFIG"
                        "condition" condition
                        "attempt" attempt
                        "bytes_cons" expected-bytes
                        "verify_gens" 0
                        "pre_verify_gen_0" 1))
        (gc-repro-log "CONFIG"
                      "condition" condition
                      "attempt" attempt
                      "bytes_cons" expected-bytes))))

(defun gc-repro-target-location-p (entry)
  (let* ((file (getf (cl-weave:test-plan-entry-location entry) :file))
         (path (and file (namestring (pathname file))))
         (suffix "packages/optimize/tests/optimizer-pipeline-tests.lisp")
         (start (and path (- (length path) (length suffix)))))
    (and start
         (not (minusp start))
         (string= suffix path :start2 start))))

(defun gc-repro-plan ()
  (let* ((name-plan (cl-weave:collect-test-plan
                     (cl-weave:root-suite)
                     :name-filter "adaptive-max-iterations"))
         (target-plan (remove-if-not
                       (function gc-repro-target-location-p)
                       name-plan)))
    (unless (= (length target-plan) +gc-repro-target-count+)
      (error "Expected exactly ~D target tests, found ~D among ~D name matches."
             +gc-repro-target-count+ (length target-plan) (length name-plan)))
    (unless (every (function gc-repro-target-location-p) target-plan)
      (error "A selected test is outside the optimizer pipeline test file."))
    (let ((target-file
            (getf (cl-weave:test-plan-entry-location (first target-plan))
                  :file)))
      (unless (every (lambda (entry)
                       (equal (namestring (pathname target-file))
                              (namestring
                               (pathname
                                (getf (cl-weave:test-plan-entry-location entry)
                                      :file)))))
                     target-plan)
        (error "Selected tests have multiple source file pathnames."))
      (values target-plan target-file))))

(defun gc-repro-run (condition attempt iterations selected target-file start)
  (dotimes (index iterations)
    (let ((iteration-start (get-internal-real-time))
          (iteration (1+ index)))
      (gc-repro-log "ITERATION_START"
                    "condition" condition
                    "attempt" attempt
                    "iteration" iteration
                    "selected" selected
                    "elapsed_ms" (gc-repro-elapsed-ms start))
      (let ((passed
              (cl-weave:run-all
               :reporter :json
               :name-filter "adaptive-max-iterations"
               :location-filter (list target-file)
               :max-workers 1
               :pass-with-no-tests nil)))
        (finish-output)
        (unless passed
          (gc-repro-log "ITERATION"
                        "condition" condition
                        "attempt" attempt
                        "iteration" iteration
                        "selected" selected
                        "iteration_elapsed_ms"
                        (gc-repro-elapsed-ms iteration-start)
                        "elapsed_ms" (gc-repro-elapsed-ms start)
                        "status" "failed"
                        "fatal" "false")
          (gc-repro-log "RESULT"
                        "condition" condition
                        "attempt" attempt
                        "iterations" iterations
                        "selected" selected
                        "elapsed_ms" (gc-repro-elapsed-ms start)
                        "status" "failed"
                        "fatal" "false")
          (uiop:quit 1))
        (gc-repro-log "ITERATION"
                      "condition" condition
                      "attempt" attempt
                      "iteration" iteration
                      "selected" selected
                      "iteration_elapsed_ms"
                      (gc-repro-elapsed-ms iteration-start)
                      "elapsed_ms" (gc-repro-elapsed-ms start)
                      "status" "passed"
                      "fatal" "false"))))
  (gc-repro-log "RESULT"
                "condition" condition
                "attempt" attempt
                "iterations" iterations
                "selected" selected
                "elapsed_ms" (gc-repro-elapsed-ms start)
                "status" "passed"))

(defun gc-repro-main ()
  (let* ((start (get-internal-real-time))
         (options (gc-repro-parse-options (uiop:command-line-arguments)))
         (condition (getf options :condition))
         (iterations (getf options :iterations))
         (attempt (getf options :attempt)))
    (gc-repro-configure condition attempt)
    (multiple-value-bind (plan target-file) (gc-repro-plan)
      (let ((selected (length plan)))
        (gc-repro-log "PREFLIGHT"
                      "condition" condition
                      "attempt" attempt
                      "iterations" iterations
                      "selected" selected
                      "configuration" "verified")
        (gc-repro-run condition attempt iterations selected target-file start)))))

(handler-case
    (progn
      (gc-repro-main)
      (uiop:quit 0))
  (error (condition)
    (format *error-output* "GC_REPRO: ERROR message=~A~%" condition)
    (finish-output *error-output*)
    (uiop:quit 1)))
