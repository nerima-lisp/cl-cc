(in-package :cl-cc/test)



(it-sequential "runtime-stdlib-2-runtime-system-loads"
  (expect (asdf:find-system :cl-cc-runtime nil) :to-be-truthy))

(it-sequential "runtime-structured-logging-json-and-context"
  (let ((stream (make-string-output-stream)))
    (let ((cl-cc/runtime:*log-level* cl-cc/runtime:+log-level-trace+)
          (cl-cc/runtime:*log-output* stream)
          (cl-cc/runtime:*log-json-output* t))
      (cl-cc/runtime:with-log-context ((:request-id "r1"))
        (cl-cc/runtime:log-info "hello" :component "test")))
    (let ((line (get-output-stream-string stream)))
      (expect (search "\"level\":\"info\"" line) :to-be-truthy)
      (expect (search "\"message\":\"hello\"" line) :to-be-truthy)
      (expect (search "\"request-id\":\"r1\"" line) :to-be-truthy)
      (expect (search "\"component\":\"test\"" line) :to-be-truthy)
      (expect (search "\"time\":" line) :to-be-truthy))))

(it-sequential "runtime-metrics-prometheus-output"
  (let ((counter (cl-cc/runtime:rt-make-counter :requests :labels '(:route "/")))
        (gauge (cl-cc/runtime:rt-make-gauge :workers))
        (histogram (cl-cc/runtime:rt-make-histogram :latency '(0.1d0 1.0d0))))
    (cl-cc/runtime:rt-counter-increment! counter 3)
    (cl-cc/runtime:rt-gauge-set! gauge 7)
    (cl-cc/runtime:rt-histogram-observe! histogram 0.05d0)
    (cl-cc/runtime:rt-histogram-observe! histogram 2.0d0)
    (let ((text (cl-cc/runtime:prometheus-text-format (list counter gauge histogram))))
      (expect (search "# TYPE requests counter" text) :to-be-truthy)
      (expect (search "requests{route=\"/\"} 3" text) :to-be-truthy)
      (expect (search "workers 7" text) :to-be-truthy)
      (expect (search "latency_bucket" text) :to-be-truthy)
      (expect (search "latency_count 2" text) :to-be-truthy))))

(it-sequential "runtime-perf-counters-condition-path"
  (expect (integerp (cl-cc/runtime:rdtsc)) :to-be-truthy)
  (multiple-value-bind (tsc aux) (cl-cc/runtime:rdtscp)
    (expect (integerp tsc) :to-be-truthy)
    (expect (= 0 aux) :to-be-truthy))
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/runtime:rt-with-perf-counters (:cycles)
      :unreachable)) (cl-cc/runtime:perf-counters-unsupported () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "runtime-arena-allocator-bulk-reset"
  (cl-cc/runtime:with-arena (arena :size-hint 2)
    (let ((a (cl-cc/runtime:arena-alloc arena 1))
          (b (cl-cc/runtime:arena-alloc arena 3)))
      (expect (= 0 (cl-cc/runtime:rt-arena-block-offset a)) :to-be-truthy)
      (expect (= 1 (cl-cc/runtime:rt-arena-block-offset b)) :to-be-truthy)
      (expect (= 4 (cl-cc/runtime:rt-arena-cursor arena)) :to-be-truthy)
      (cl-cc/runtime:arena-reset arena)
      (expect (= 0 (cl-cc/runtime:rt-arena-cursor arena)) :to-be-truthy))))

(it-sequential "runtime-object-pool-two-tier-reuse"
  (let* ((pool (cl-cc/runtime:make-object-pool :test-vector
                                               :min-size 1
                                               :max-size 4
                                               :constructor (lambda () (vector :fresh))))
         (obj (cl-cc/runtime:pool-acquire pool)))
    (cl-cc/runtime:pool-release pool obj)
    (expect (cl-cc/runtime:pool-acquire pool) :to-be obj)))

(it-sequential "runtime-gc-and-vm-runtime-configuration"
  (let ((old-nursery cl-cc/runtime:*gc-nursery-size*)
        (old-adaptive cl-cc/vm:*adaptive-jit-enabled*))
    (unwind-protect
         (progn
           (setf cl-cc/runtime:*gc-nursery-size* 131072
                 cl-cc/runtime:*gc-young-size-words* 131072)
           (cl-cc/vm:vm-record-gc-pause 100.0d0)
           (expect (< cl-cc/runtime:*gc-nursery-size* 131072) :to-be-truthy)
           (setf cl-cc/vm:*adaptive-jit-enabled* nil)
           (cl-cc/vm:vm-handle-runtime-flag '("--adaptive-jit"))
           (expect cl-cc/vm:*adaptive-jit-enabled* :to-be-truthy)
           (cl-cc/vm:enqueue-jit-compilation :cold :hotness 1)
           (cl-cc/vm:enqueue-jit-compilation :hot :hotness 10)
           (expect (cl-cc/vm:dequeue-jit-compilation) :to-equal '(10 . :hot))
           (let ((report (cl-cc/vm:runtime-tuning-report)))
             (expect (getf report :jit-tier1-threshold) :to-be-truthy)
             (expect (getf report :gc-nursery-size) :to-be-truthy)))
      (setf cl-cc/runtime:*gc-nursery-size* old-nursery
            cl-cc/runtime:*gc-young-size-words* old-nursery
            cl-cc/vm:*adaptive-jit-enabled* old-adaptive
            cl-cc/vm:*jit-compilation-queue* nil))))
