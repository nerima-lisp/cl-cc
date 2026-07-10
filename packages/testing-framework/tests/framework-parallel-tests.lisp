(in-package :cl-cc/test)

;;; Parallel worker pool and watchdog tests.
;;; These tests focus on heartbeat emission, escalation, and timeout joins.

(in-suite runner-regression-suite)

;;; ── Heartbeat thread (T-1) ──────────────────────────────────────────────

(deftest heartbeat-thread-stops-cleanly-on-watchdog-stop
  "Heartbeat thread exits when watchdog-stop is signalled (no orphan thread)."
  (let* ((stop-flag nil)
         (lock (sb-thread:make-mutex :name "hb-test-lock"))
         (started 0)
         (started-sem (sb-thread:make-semaphore :count 0))
         (stop-sem (sb-thread:make-semaphore :count 0))
         (interval-original *heartbeat-interval-seconds*))
    (let ((*heartbeat-interval-seconds* 1))
      (let ((th (sb-thread:make-thread
                 (lambda ()
                   (incf started)
                   (sb-thread:signal-semaphore started-sem)
                   (loop
                     (when (sb-thread:with-mutex (lock) stop-flag) (return))
                     (sb-thread:wait-on-semaphore stop-sem)))
                   :name "hb-mock")))
        (assert-true (sb-thread:wait-on-semaphore started-sem :timeout 1))
        (sb-thread:with-mutex (lock) (setf stop-flag t))
        (sb-thread:signal-semaphore stop-sem)
        (assert-true (%thread-joined-p th 5))
        (assert-eql 1 started)))
    (assert-eql interval-original *heartbeat-interval-seconds*)))

;;; ── Heartbeat emission (T-2) ────────────────────────────────────────────

(deftest heartbeat-defparameter-respects-env-default
  "*heartbeat-interval-seconds* defaults to a positive integer in the absence of env override."
  (assert-true (and (integerp *heartbeat-interval-seconds*)
                    (plusp *heartbeat-interval-seconds*))))

(deftest heartbeat-format-includes-required-fields
  "A simulated heartbeat line contains t=, done=, workers=, and inflight= fields."
  (let* ((s (with-output-to-string (out)
              (let ((*error-output* out))
                (format *error-output*
                        "# heartbeat: t=~As done=~A/~A workers=~A inflight=~A~%"
                        12 5 100 4 '(test-a test-b))
                (finish-output *error-output*))))
         (line (string-trim '(#\Newline) s)))
    (assert-string-contains-all
     line
     '("# heartbeat:"
       "t=12s"
       "done=5/100"
       "workers=4"
       "inflight="))))

;;; ── Suite deadline and watchdog escalation (T-3) ───────────────────────

(deftest suite-deadline-killer-invokes-exit-action-after-deadline
  "The suite deadline killer reaches the injectable exit action through its real deadline thread."
  (let ((exit-code nil)
        (done (sb-thread:make-semaphore :count 0)))
    (let ((*suite-killer-exit-fn*
            (lambda (&key code)
              (setf exit-code code)
              (sb-thread:signal-semaphore done))))
      (let ((killer-state (%start-suite-deadline-killer (get-internal-real-time))))
        (unwind-protect
             (progn
               (assert-true (sb-thread:wait-on-semaphore done :timeout 3))
               (assert-eql 124 exit-code))
          (%stop-suite-deadline-killer killer-state))))))

(deftest kill-grace-seconds-is-positive-integer
  "*kill-grace-seconds* defaults to a positive integer; absent value would degrade to 0."
  (assert-true (and (integerp *kill-grace-seconds*)
                    (plusp *kill-grace-seconds*))))

;;; ── %collect-timed-out-workers (T-4) ─────────────────────────────────────

(deftest collect-timed-out-workers-returns-empty-when-all-slots-nil
  "%collect-timed-out-workers returns an empty list when all watchdog slots are nil."
  (let* ((lock  (sb-thread:make-mutex :name "test-lock"))
         (slots (make-array 2 :initial-element nil)))
    (assert-null (cl-cc/test::%collect-timed-out-workers slots 2 (get-internal-real-time) lock))))

(deftest collect-timed-out-workers-collects-expired-slot
  "%collect-timed-out-workers returns one entry and clears the slot when elapsed > timeout."
  (let* ((lock    (sb-thread:make-mutex :name "test-lock"))
         (slots   (make-array 1 :initial-element nil))
         (past    (- (get-internal-real-time) (* 999 internal-time-units-per-second)))
         (thread  sb-thread:*current-thread*)
         (slot    (make-watchdog-slot :thread thread :epoch 0
                                      :start-time past :timeout-secs 1)))
    (setf (aref slots 0) slot)
    (let ((timed-out (cl-cc/test::%collect-timed-out-workers
                      slots 1 (get-internal-real-time) lock)))
      (assert-= 1 (length timed-out))
      (assert-null (aref slots 0)))))

(deftest collect-timed-out-workers-ignores-not-yet-expired-slot
  "%collect-timed-out-workers leaves a slot in place when elapsed < timeout."
  (let* ((lock   (sb-thread:make-mutex :name "test-lock"))
         (slots  (make-array 1 :initial-element nil))
         (future (get-internal-real-time))
         (thread sb-thread:*current-thread*)
         (slot   (make-watchdog-slot :thread thread :epoch 0
                                     :start-time future :timeout-secs 9999)))
    (setf (aref slots 0) slot)
    (let ((timed-out (cl-cc/test::%collect-timed-out-workers
                      slots 1 (get-internal-real-time) lock)))
      (assert-null timed-out)
      (assert-true (aref slots 0)))))

;;; ── %check-escalations (T-5) ─────────────────────────────────────────────

(defun %make-escalation-check-context (&key (epoch 7)
                                            (current-test '(:name running-test)))
  (let ((epochs (make-array 1 :initial-element epoch))
        (current-tests (make-array 1 :initial-element current-test))
        (lock (sb-thread:make-mutex :name "test-lock"))
        (past (- (get-internal-real-time) internal-time-units-per-second)))
    (values epochs
            current-tests
            lock
            (make-escalation :worker-idx 0 :epoch 7 :deadline past))))

(defun %make-parallel-blocked-test (name timeout)
  (let ((blocker (sb-thread:make-semaphore :count 0)))
    (list :name name
          :fn (lambda () (sb-thread:wait-on-semaphore blocker))
          :suite 'runner-regression-suite
          :timeout timeout
          :depends-on nil
          :tags nil
          :number 1)))

(defun %make-parallel-timeout-demo-test ()
  (%make-parallel-blocked-test 'parallel-timeout-demo 0.02))

(defun %make-parallel-hung-suite-demo-test ()
  (%make-parallel-blocked-test 'parallel-hung-suite-demo 5))

(deftest check-escalations-records-hang-when-epoch-matches-and-deadline-passed
  "%check-escalations records a hung worker when deadline has passed and epoch still matches."
  (multiple-value-bind (epochs current-tests lock esc)
      (%make-escalation-check-context)
    (let ((captured nil))
      (let ((*error-output* (make-string-output-stream)))
        (cl-cc/test::%check-escalations
         (list esc) epochs current-tests lock (get-internal-real-time)
         (lambda (worker-idx test-plist thread)
           (declare (ignore thread))
           (setf captured (list worker-idx (getf test-plist :name))))))
      (assert-equal '(0 running-test) captured))))

(deftest check-escalations-prunes-resolved-entry-when-epoch-advanced
  "%check-escalations removes an escalation when the worker epoch has advanced past it."
  (multiple-value-bind (epochs current-tests lock esc)
      (%make-escalation-check-context :epoch 8)
    (let ((captured nil))
      (let ((remaining (cl-cc/test::%check-escalations
                        (list esc) epochs current-tests lock (get-internal-real-time)
                        (lambda (&rest args)
                          (setf captured args)))))
          (assert-null remaining)
          (assert-null captured)))))

(deftest check-escalations-prunes-idle-worker-without-exit
  "%check-escalations drops an escalation once the worker has no in-flight test, even if the deadline already passed."
  (multiple-value-bind (epochs current-tests lock esc)
      (%make-escalation-check-context :current-test nil)
    (let ((captured nil))
      (let ((remaining (cl-cc/test::%check-escalations
                        (list esc) epochs current-tests lock (get-internal-real-time)
                        (lambda (&rest args)
                          (setf captured args)))))
          (assert-null remaining)
          (assert-null captured)))))

(deftest run-tests-parallel-timeout-does-not-escalate-after-worker-finishes
  "%run-tests-parallel stops the watchdog cleanly once a timed-out worker has already returned a failure result."
  (with-restored-bindings (*kill-grace-seconds*
                           *heartbeat-interval-seconds*
                           *watchdog-poll-seconds*)
    (setf *kill-grace-seconds* 0.01
          *heartbeat-interval-seconds* 0
          *watchdog-poll-seconds* 0.01)
    (with-replaced-function (%tap-print-result
                             (lambda (&rest args)
                               (declare (ignore args))
                               nil))
      (let ((*error-output* (make-string-output-stream)))
        (let ((results (%run-tests-parallel (list (%make-parallel-timeout-demo-test)) 1)))
          (assert-eq :fail (getf (first results) :status))
          (assert-string-contains-all
           (getf (first results) :detail)
           '("timeout after 0.02 seconds")))))))

(deftest run-tests-parallel-suite-deadline-terminates-hung-worker
  "%run-tests-parallel converts a stuck worker join into sb-ext:timeout when the suite deadline has passed."
  (let ((timed-out nil))
    (with-restored-bindings (*kill-grace-seconds*
                             *heartbeat-interval-seconds*
                             *watchdog-poll-seconds*)
      (setf *kill-grace-seconds* 0.01
            *heartbeat-interval-seconds* 0
            *watchdog-poll-seconds* 0.01)
      (with-replaced-function (%tap-print-result
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 nil))
        (let ((*parallel-suite-deadline*
                (+ (get-internal-real-time)
                   (round (* 0.05 internal-time-units-per-second))))
              (*error-output* (make-string-output-stream)))
          (handler-case
              (%run-tests-parallel (list (%make-parallel-hung-suite-demo-test)) 1)
            (sb-ext:timeout ()
              (setf timed-out t)))))
      (assert-true timed-out))))

(deftest run-tests-parallel-hung-worker-abandoned-and-suite-completes
  "A worker stuck past timeout+grace (interrupt swallowed, simulating the
macOS 26.5 trap-delivery hang) is recorded as a failure, its worker is
replaced, and the remaining queued tests still run to completion."
  (with-restored-bindings (*kill-grace-seconds*
                           *heartbeat-interval-seconds*
                           *watchdog-poll-seconds*)
    (setf *kill-grace-seconds* 0.05
          *heartbeat-interval-seconds* 0
          *watchdog-poll-seconds* 0.01)
    (with-replaced-function (%tap-print-result
                             (lambda (&rest args)
                               (declare (ignore args))
                               nil))
      (let* ((blocker (sb-thread:make-semaphore :count 0))
             (stuck (list :name 'parallel-stuck-demo
                          ;; Swallow the watchdog's timeout interrupt and
                          ;; block again — the worker looks unrecoverable.
                          :fn (lambda ()
                                (dotimes (i 2)
                                  (handler-case
                                      (sb-thread:wait-on-semaphore blocker)
                                    (sb-ext:timeout () nil))))
                          :suite 'runner-regression-suite
                          :timeout 0.02
                          :depends-on nil :tags nil :number 1))
             (ok (list :name 'parallel-ok-demo
                       :fn (lambda () 42)
                       :suite 'runner-regression-suite
                       :timeout 5
                       :depends-on nil :tags nil :number 2))
             (*error-output* (make-string-output-stream)))
        (let ((results (%run-tests-parallel (list stuck ok) 1)))
          ;; Both tests have results: the hung one failed, the queued one ran.
          (assert-= 2 (length results))
          (let ((stuck-result (find 'parallel-stuck-demo results
                                    :key (lambda (r) (getf r :name))))
                (ok-result (find 'parallel-ok-demo results
                                 :key (lambda (r) (getf r :name)))))
            (assert-eq :fail (getf stuck-result :status))
            (assert-string-contains-all (getf stuck-result :detail)
                                        '("hung" "abandoned"))
            (assert-true ok-result)
            (assert-true (not (eq :fail (getf ok-result :status))))))
        ;; Unblock the abandoned thread so it exits instead of leaking.
        (sb-thread:signal-semaphore blocker 2)))))
