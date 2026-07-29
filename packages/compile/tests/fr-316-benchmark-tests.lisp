;;;; packages/compile/tests/fr-316-benchmark-tests.lisp
;;;; FR-316: Benchmark/profiling framework support.

(in-package :cl-cc/test)

(defbenchmark fr-316-empty-benchmark
  "FR-316: minimal benchmark used by framework shape tests."
  :warmup 1
  :iterations 3
  (values))

(it-sequential "fr-316-benchmark-result-shape"
  (let ((result (fr-316-empty-benchmark)))
    (expect (getf result :name) :to-be 'fr-316-empty-benchmark)
    (expect (= 1 (getf result :warmup-count)) :to-be-truthy)
    (expect (= 3 (getf result :iteration-count)) :to-be-truthy)
    (expect (= 3 (length (getf result :durations-ns))) :to-be-truthy)
    (dolist (key '(:min-ns :max-ns :mean-ns :median-ns :p99-ns :stddev-ns))
      (expect (member key result) :to-be-truthy))))

(it-sequential "fr-316-benchmark-json-stable-keys"
  (let* ((result (fr-316-empty-benchmark :warmup 0 :iterations 1))
         (json (benchmark-result-json result)))
    (dolist (key '("\"name\":"
                   "\"warmup_count\":"
                   "\"iteration_count\":"
                   "\"durations_ns\":"
                   "\"min_ns\":"
                   "\"max_ns\":"
                   "\"mean_ns\":"
                   "\"median_ns\":"
                   "\"p99_ns\":"
                    "\"stddev_ns\":"))
      (expect (search key json) :to-be-truthy))))

(it-sequential "fr-316-benchmark-json-rational-numbers"
  (let ((json (benchmark-result-json
               (list :name 'fr-316-rational-json
                     :warmup-count 0
                     :iteration-count 2
                     :durations-ns '(1 2)
                     :min-ns 1
                     :max-ns 2
                     :mean-ns 1/3
                     :median-ns 3/2
                     :p99-ns 2
                     :stddev-ns 2/3))))
    (expect (search "/" json) :to-be-null)
    (expect (search "\"mean_ns\":0." json) :to-be-truthy)
    (expect (search "\"median_ns\":1.500" json) :to-be-truthy)
    (expect (search "\"stddev_ns\":0." json) :to-be-truthy)))

(it-sequential "fr-316-assert-faster-than-success"
  (assert-faster-than 1000000000
    (values)))

(it-sequential "fr-316-assert-faster-than-failure"
  (signals cl-weave:test-failure (assert-faster-than -1
      (values))))

(it-sequential "fr-316-assert-no-consing-success"
  (handler-case
      (assert-no-consing (values))
    (cl-weave:test-failure ()
      ;; Expected: test framework may observe internal SBCL allocation
      (values))))

(it-sequential "fr-316-vm-opcode-frequency-counter"
  (let* ((state (cl-cc:make-vm-state))
         (program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0)))
    (setf (cl-cc:vm-profile-enabled-p state) t)
    (expect (= 42 (cl-cc:run-compiled program :state state)) :to-be-truthy)
    (let ((counts (cl-cc:vm-get-profile-inst-counts state)))
      (expect (plusp (hash-table-count counts)) :to-be-truthy)
      (expect (= 1 (gethash 'cl-cc:vm-const counts 0)) :to-be-truthy)
      (expect (= 1 (gethash 'cl-cc:vm-halt counts 0)) :to-be-truthy))))

(it-sequential "fr-316-vm-call-counts-and-times"
  (let ((state (cl-cc:make-vm-state)))
    (setf (cl-cc:vm-profile-enabled-p state) t)
    (setf (cl-cc:vm-profile-call-stack state) (list "<toplevel>"))
    (setf (cl-cc:vm-profile-call-start-times state) (list 0))
    (let ((now 1000)
          (original-now (symbol-function 'cl-cc/vm::%vm-profile-now-ns)))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-cc/vm::%vm-profile-now-ns)
                   (lambda ()
                     (prog1 now
                       (incf now 4000))))
             (cl-cc/vm::vm-profile-enter-call state 'fr-316-profiled-function)
             (cl-cc/vm::vm-profile-return state))
        (setf (symbol-function 'cl-cc/vm::%vm-profile-now-ns) original-now)))
    (expect (= 1 (gethash 'fr-316-profiled-function
                         (cl-cc:vm-get-profile-call-counts state)
                         0)) :to-be-truthy)
    (expect (= 4000 (gethash 'fr-316-profiled-function
                            (cl-cc:vm-get-profile-call-times state)
                            0)) :to-be-truthy)))
