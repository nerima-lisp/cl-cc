;;;; packages/runtime/tests/continuous-profile-tests.lisp

(in-package :cl-cc/test)



(it-sequential "fr-701-continuous-profiling-records-and-exports-otel"
  (let ((session (cl-cc/runtime:rt-start-continuous-profile
                  :name "fr701" :output nil
                  :attributes '(("service.name" . "cl-cc-test")))))
    (cl-cc/runtime:rt-record-profile-sample '("main" "worker") :count 3 :session session)
    (expect (gethash "main;worker"
                              (cl-cc/runtime:rt-continuous-profile-session-samples session)) :to-equal 3)
    (cl-cc/runtime:rt-stop-continuous-profile session)
    (let ((span (cl-cc/runtime:rt-continuous-profile->otel-span session)))
      (expect (search "fr701" (cl-cc/runtime:rt-otel-span-to-json span)) :to-be-truthy))))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-resolve-perf-symbol
;;; ---------------------------------------------------------------------------

(it-sequential "continuous-profile-resolve-perf-symbol-hit"
  (let ((perf-map '((100 200 "entry-A") (300 400 "entry-B"))))
    ;; Exact start (inclusive)
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol 100 perf-map) :to-equal "entry-A")
    ;; Interior address
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol 150 perf-map) :to-equal "entry-A")
    ;; Second entry, interior
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol 350 perf-map) :to-equal "entry-B")))

(it-sequential "continuous-profile-resolve-perf-symbol-miss"
  (let ((perf-map '((100 200 "entry-A"))))
    ;; Before range
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol 50 perf-map) :to-equal nil)
    ;; Exclusive end
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol 200 perf-map) :to-equal nil)
    ;; Past range
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol 999 perf-map) :to-equal nil)
    ;; Nil address
    (expect (cl-cc/runtime::%rt-profile-resolve-perf-symbol nil perf-map) :to-equal nil)))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-collapsed-stack
;;; ---------------------------------------------------------------------------

(it-sequential "continuous-profile-collapsed-stack-from-frames"
  (let ((stack (list (cl-cc/runtime:make-rt-profile-frame :function "top")
                     (cl-cc/runtime:make-rt-profile-frame :function "middle")
                     (cl-cc/runtime:make-rt-profile-frame :function "bottom"))))
    (expect (cl-cc/runtime::%rt-profile-collapsed-stack stack) :to-equal "top;middle;bottom")))

(it-sequential "continuous-profile-collapsed-stack-from-strings"
  (let ((stack (list "alpha" "beta" "gamma")))
    (expect (cl-cc/runtime::%rt-profile-collapsed-stack stack) :to-equal "alpha;beta;gamma")))

(it-sequential "continuous-profile-collapsed-stack-single-frame"
  (let ((stack (list (cl-cc/runtime:make-rt-profile-frame :function "only"))))
    (expect (cl-cc/runtime::%rt-profile-collapsed-stack stack) :to-equal "only")))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-normalize-frame
;;; ---------------------------------------------------------------------------

(it-sequential "continuous-profile-normalize-frame-string"
  (let ((result (cl-cc/runtime::%rt-profile-normalize-frame "my-fn")))
    (expect (cl-cc/runtime::rt-profile-frame-p result) :to-be-truthy)
    (expect (cl-cc/runtime:rt-profile-frame-function result) :to-equal "my-fn")))

(it-sequential "continuous-profile-normalize-frame-passthrough"
  (let* ((frame (cl-cc/runtime:make-rt-profile-frame :function "existing"))
         (result (cl-cc/runtime::%rt-profile-normalize-frame frame)))
    (expect result :to-be frame)))

(it-sequential "continuous-profile-normalize-frame-perf-map-enrichment"
  (let* ((perf-map '((1000 2000 "perf-symbol-name")))
         (frame (cl-cc/runtime:make-rt-profile-frame :function "fn" :address 1500))
         (result (cl-cc/runtime::%rt-profile-normalize-frame frame perf-map)))
    (expect result :to-be frame)
    (expect (cl-cc/runtime:rt-profile-frame-perf-symbol result) :to-equal "perf-symbol-name")))

;;; ---------------------------------------------------------------------------
;;; %rt-profile-trim-samples
;;; ---------------------------------------------------------------------------

(it-sequential "continuous-profile-trim-samples-under-limit"
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "trim-under"
                  :max-samples 10
                  :sample-log (make-array 4 :adjustable t :fill-pointer 0))))
    (vector-push-extend "a" (cl-cc/runtime:rt-continuous-profile-session-sample-log session))
    (vector-push-extend "b" (cl-cc/runtime:rt-continuous-profile-session-sample-log session))
    (cl-cc/runtime::%rt-profile-trim-samples session)
    (expect (fill-pointer (cl-cc/runtime:rt-continuous-profile-session-sample-log session)) :to-equal 2)))

(it-sequential "continuous-profile-trim-samples-at-limit"
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "trim-at"
                  :max-samples 3
                  :sample-log (make-array 8 :adjustable t :fill-pointer 0))))
    (dolist (x '("a" "b" "c" "d" "e"))
      (vector-push-extend x (cl-cc/runtime:rt-continuous-profile-session-sample-log session)))
    (cl-cc/runtime::%rt-profile-trim-samples session)
    (let ((log (cl-cc/runtime:rt-continuous-profile-session-sample-log session)))
      (expect (fill-pointer log) :to-equal 3)
      (expect (aref log 0) :to-equal "c")
      (expect (aref log 1) :to-equal "d")
      (expect (aref log 2) :to-equal "e"))))

;;; ---------------------------------------------------------------------------
;;; rt-export-continuous-profile
;;; ---------------------------------------------------------------------------

(it-sequential "continuous-profile-export-nil-output-returns-string"
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "export-nil"
                  :output nil
                  :format :otel-json
                  :started-at-nanos 0
                  :stopped-at-nanos 1000000)))
    (let ((result (cl-cc/runtime:rt-export-continuous-profile session :output nil)))
      (expect (stringp result) :to-be-truthy)
      (expect (search "resourceProfiles" result) :to-be-truthy))))

(it-sequential "continuous-profile-export-pprof-format"
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "export-pprof"
                  :output nil
                  :format :pprof-json
                  :started-at-nanos 0
                  :stopped-at-nanos 1000000)))
    (let ((result (cl-cc/runtime:rt-export-continuous-profile session :format :pprof-json :output nil)))
      (expect (stringp result) :to-be-truthy)
      (expect (search "sampleType" result) :to-be-truthy)
      (expect (search "periodType" result) :to-be-truthy))))

(it-sequential "continuous-profile-export-stdout-returns-payload"
  (let ((session (cl-cc/runtime:make-rt-continuous-profile-session
                  :name "export-stdout"
                  :output :stdout
                  :format :otel-json
                  :started-at-nanos 0
                  :stopped-at-nanos 1000000)))
    (let* ((captured (with-output-to-string (*standard-output*)
                       (let ((result (cl-cc/runtime:rt-export-continuous-profile
                                      session :output :stdout)))
                         (expect (stringp result) :to-be-truthy)
                         (expect (search "cl-cc/runtime" result) :to-be-truthy)))))
      (expect (search "cl-cc/runtime" captured) :to-be-truthy))))

(it-sequential "fr-701-continuous-profiler-background-sampler-and-json-exports"
  (let ((session (cl-cc/runtime:rt-start-continuous-profile
                  :name "fr701-sampler"
                  :sample-rate-hz 100
                  :output nil
                  :trace-id "0123456789abcdef0123456789abcdef"
                  :span-id "0123456789abcdef")))
    (expect (cl-cc/runtime:rt-wait-for-continuous-profile-sample session :timeout 1) :to-be-truthy)
    (cl-cc/runtime:rt-record-profile-sample
     (list (cl-cc/runtime:make-rt-profile-frame
            :function "manual-worker"
            :source-file "continuous-profile-tests.lisp"
            :source-line 42))
     :thread-id "test-thread"
     :session session)
    (cl-cc/runtime:rt-stop-continuous-profile session)
    (expect (> (length (cl-cc/runtime:rt-continuous-profile-session-sample-log session)) 0) :to-be-truthy)
    (let ((otel (cl-cc/runtime:rt-continuous-profile-to-otel-json session))
          (pprof (cl-cc/runtime:rt-continuous-profile-to-pprof-json session)))
      (expect (search "\"trace_id\":\"0123456789abcdef0123456789abcdef\"" otel) :to-be-truthy)
      (expect (search "\"stack\"" otel) :to-be-truthy)
      (expect (search "\"sampleType\"" pprof) :to-be-truthy)
      (expect (search "manual-worker" pprof) :to-be-truthy))))
