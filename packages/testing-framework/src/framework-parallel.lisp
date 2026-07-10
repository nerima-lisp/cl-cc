;;;; tests/framework-parallel.lisp — CL-CC Test Framework (Parallel Worker Pool)
;;;; Persistent worker threads with watchdog-based timeout enforcement.

(in-package :cl-cc/test)

;; Declared here so SBCL treats the worker-local binding as a dynamic special
;; during compile-file, even though framework-runner.lisp defines it later.
(defvar *test-runner-mode*)

(defvar *parallel-suite-deadline* nil
  "Absolute internal-real-time deadline for the active parallel suite run, or NIL.
The parallel worker join loop checks this explicitly because SBCL's
`sb-ext:with-timeout' cannot reliably interrupt a thread blocked in
`sb-thread:join-thread'.")

(defparameter *heartbeat-interval-seconds*
  (or (let ((raw (uiop:getenv "CLCC_HEARTBEAT_SECONDS")))
        (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed)))))
      30)
  "Seconds between heartbeat emits. CLCC_HEARTBEAT_SECONDS env override; 0 disables.")

(defparameter *kill-grace-seconds* 5
  "Seconds to wait after firing a per-test timeout interrupt before escalating
to sb-ext:exit. Workers that don't respond within this window are considered hung.")

(defparameter *watchdog-poll-seconds* 0.5
  "Seconds between watchdog polls while monitoring parallel workers.
Tests may rebind this to a smaller positive real to keep timeout regression
coverage fast without changing the production polling cadence.")

;;; ─────────────────────────────────────────────────────────────────────────
;;; Watchdog state structs
;;; ─────────────────────────────────────────────────────────────────────────

(defstruct watchdog-slot
  "Per-worker entry in the watchdog monitoring table."
  thread epoch start-time timeout-secs)

(defstruct escalation
  "Pending hard-kill record: epoch was captured when the interrupt fired."
  worker-idx epoch deadline thread)

(defun %deadline-expired-p (deadline)
  "Return true when absolute internal-time DEADLINE has passed."
  (and deadline (>= (get-internal-real-time) deadline)))

(defun %join-poll-seconds (deadline)
  "Return a short bounded join timeout, clipped by DEADLINE when present."
  (let ((poll (or (%normalize-positive-timeout *watchdog-poll-seconds*) 0.5)))
    (if deadline
        (let* ((now (get-internal-real-time))
               (remaining (/ (max 0 (- deadline now))
                             (float internal-time-units-per-second))))
          (max 0.001 (min poll remaining)))
        poll)))

(defun %thread-joined-p (thread timeout-seconds)
  "Poll THREAD for termination; return T when it exits, NIL on timeout.
Uses thread-alive-p + sleep instead of join-thread to avoid the GC-safepoint
deadlock on macOS AArch64: join-thread blocks inside without-gcing via
__ulock_wait2, which prevents GC safepoint delivery when workers trigger GC.
sleep is GC-interruptible so the main thread can reach its safepoint normally."
  (unless (sb-thread:thread-alive-p thread)
    (return-from %thread-joined-p t))
  (if (null timeout-seconds)
      (loop
        (sleep 0.005)
        (unless (sb-thread:thread-alive-p thread)
          (return t)))
      (let ((deadline (+ (get-internal-real-time)
                         (round (* timeout-seconds
                                   (float internal-time-units-per-second))))))
        (loop
          (sleep 0.005)
          (unless (sb-thread:thread-alive-p thread)
            (return t))
          (when (>= (get-internal-real-time) deadline)
            (return nil))))))

(defun %terminate-thread-safely (thread)
  "Terminate THREAD when it is still alive, ignoring races during shutdown."
  (when (and thread (sb-thread:thread-alive-p thread))
    (ignore-errors (sb-thread:terminate-thread thread))))

(defun %terminate-live-threads (threads)
  "Terminate every live thread in THREADS without waiting indefinitely."
  (dolist (thread threads)
    (%terminate-thread-safely thread)))

(defun %join-worker-threads-until-deadline (threads deadline &optional hung-thread-p)
  "Join worker THREADS with bounded waits.
HUNG-THREAD-P (when supplied) is a predicate: threads it accepts have been
written off by the watchdog (stuck in an uninterruptible trap) and are
skipped instead of awaited.
Return :SUITE-TIMEOUT when DEADLINE passes before all live workers finish."
  (dolist (thread threads nil)
    (loop
      (when (and hung-thread-p (funcall hung-thread-p thread))
        (return))
      (when (%thread-joined-p thread (%join-poll-seconds deadline))
        (return))
      (when (%deadline-expired-p deadline)
        (return-from %join-worker-threads-until-deadline :suite-timeout)))))

(defun %join-support-thread (thread)
  "Join a watchdog/heartbeat helper without allowing shutdown to hang forever."
  (when thread
    (unless (%thread-joined-p thread (%join-poll-seconds nil))
      (%terminate-thread-safely thread)
      (%thread-joined-p thread (%join-poll-seconds nil)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Parallel Worker Pool
;;; ─────────────────────────────────────────────────────────────────────────

(defun %collect-timed-out-workers (watchdog-slots workers now watchdog-lock)
  "Under WATCHDOG-LOCK, collect slots whose elapsed time exceeds their timeout.
Clears the slot in place. Returns a list of (thread epoch worker-idx) triples."
  (let ((timed-out '()))
    (sb-thread:with-mutex (watchdog-lock)
      (dotimes (i workers)
        (let ((slot (aref watchdog-slots i)))
          (when slot
            (let* ((elapsed (/ (- now (watchdog-slot-start-time slot))
                               (float internal-time-units-per-second))))
              (when (> elapsed (watchdog-slot-timeout-secs slot))
                (setf (aref watchdog-slots i) nil)
                (push (list (watchdog-slot-thread slot)
                            (watchdog-slot-epoch  slot)
                            i)
                      timed-out)))))))
    timed-out))

(defun %fire-timeout-interrupts (timed-out worker-epochs kill-grace now)
  "Signal sb-ext:timeout on each timed-out worker. Returns new escalation records."
  (mapcar (lambda (item)
            (destructuring-bind (thread epoch worker-idx) item
              (ignore-errors
                (sb-thread:interrupt-thread thread
                  (lambda ()
                    (when (= (aref worker-epochs worker-idx) epoch)
                      (error 'sb-ext:timeout)))))
              (make-escalation
               :worker-idx worker-idx
               :epoch      epoch
               :deadline   (+ now (* kill-grace internal-time-units-per-second))
               :thread     thread)))
          timed-out))

(defun %check-escalations (escalations worker-epochs current-tests watchdog-lock now
                           record-hang-fn)
  "Handle workers whose in-flight test is still running after the grace period.
RECORD-HANG-FN is called with (worker-idx test-plist thread): the hung test is
recorded as a failure and its worker abandoned, letting the rest of the suite
finish (a thread stuck in a hardware trap — e.g. the macOS 26.5 trap-delivery
hang — cannot be interrupted or terminated, only written off).
Returns the pruned escalation list (resolved entries removed)."
  (dolist (esc escalations)
    (let* ((worker-idx (escalation-worker-idx esc))
           (still-running
             (sb-thread:with-mutex (watchdog-lock)
               (aref current-tests worker-idx))))
      (when (and (>= now (escalation-deadline esc))
                 still-running
                 (= (aref worker-epochs worker-idx)
                    (escalation-epoch esc)))
        (format *error-output*
                "# WARNING: worker ~A hung on ~A past timeout+~As; abandoning worker~%"
                worker-idx (getf still-running :name) *kill-grace-seconds*)
        (finish-output *error-output*)
        (funcall record-hang-fn worker-idx still-running
                 (escalation-thread esc)))))
  (remove-if (lambda (esc)
               (let* ((worker-idx (escalation-worker-idx esc))
                      (still-running
                        (sb-thread:with-mutex (watchdog-lock)
                          (aref current-tests worker-idx))))
                 (or (>= now (escalation-deadline esc))
                     (not still-running)
                     (/= (aref worker-epochs worker-idx)
                         (escalation-epoch esc)))))
             escalations))

(defun %make-parallel-failure-result (test-plist start-time detail)
  "Build a failure result plist for TEST-PLIST with shared timing metadata."
  (list :name        (getf test-plist :name)
        :status      :fail
        :detail      detail
        :number      (getf test-plist :number)
        :suite       (getf test-plist :suite)
        :source-file (getf test-plist :source-file)
        :duration-ns (%compute-duration-ns start-time (get-internal-real-time))))

(defun %run-test-with-timeout (test-plist worker-idx start-time
                               watchdog-slots worker-epochs current-tests watchdog-lock)
  "Execute TEST-PLIST under watchdog supervision. Returns a result plist."
  (let* ((number  (getf test-plist :number))
         (timeout (%effective-test-timeout test-plist)))
    (when timeout
      (let ((epoch (aref worker-epochs worker-idx)))
        (sb-thread:with-mutex (watchdog-lock)
          (setf (aref watchdog-slots worker-idx)
                (make-watchdog-slot :thread      sb-thread:*current-thread*
                                    :epoch       epoch
                                    :start-time  start-time
                                    :timeout-secs timeout)))))
    (sb-thread:with-mutex (watchdog-lock)
      (setf (aref current-tests worker-idx) test-plist))
    (unwind-protect
         (handler-case
             (%run-single-test test-plist number nil)
           (sb-ext:timeout ()
             (%make-parallel-failure-result
              test-plist start-time
              (format nil "  ---~%  message: \"timeout after ~A seconds (watchdog)\"~%  ..."
                      timeout)))
           (condition (e)
             ;; Keep worker alive when tests intentionally signal custom conditions.
             (%make-parallel-failure-result
              test-plist start-time
              (format nil "  ---~%  message: ~S~%  ..." e))))
      ;; Increment epoch first (invalidates any pending interrupt lambda),
      ;; then clear the slot. without-interrupts prevents a watchdog interrupt
      ;; from landing inside this cleanup path.
      (sb-sys:without-interrupts
        (when timeout
          (incf (aref worker-epochs worker-idx))
          (sb-thread:with-mutex (watchdog-lock)
            (setf (aref watchdog-slots worker-idx) nil)))
        (sb-thread:with-mutex (watchdog-lock)
          (setf (aref current-tests worker-idx) nil))))))

(defun %run-tests-parallel (tests workers &optional prior-timings)
  "Run tests in parallel using WORKERS persistent worker threads.
A single watchdog thread monitors elapsed time per worker and fires a
sb-ext:timeout interrupt when a test exceeds its timeout (O(workers) threads).

Correctness invariants:
 - Epoch counter: each worker-slot registration captures worker-epochs[i] at
   that moment. The interrupt lambda checks the current epoch before signalling,
   making stale interrupts (fired after the test already completed) self-defusing.
 - Interrupt outside lock: timed-out workers are collected inside watchdog-lock,
   then interrupted OUTSIDE it so a slow interrupt delivery cannot block all
   worker threads from registering their next test.
 - sb-sys:without-interrupts around cleanup: prevents the watchdog interrupt from
   landing inside the unwind-protect cleanup path where a second non-local exit
   would leak watchdog-lock."
  (let* ((tests (if prior-timings
                    (%sort-parallel-slow-first tests prior-timings)
                    tests))
         (n              (length tests))
         (results        (make-array n :initial-element nil))
         (results-lock   (sb-thread:make-mutex :name "results-lock"))
         (work-queue     (coerce tests 'vector))
         (queue-index    0)
         (queue-lock     (sb-thread:make-mutex :name "queue-lock"))
         (prolog-snapshot cl-cc/prolog::*prolog-rules*)
         (watchdog-slots  (make-array workers :initial-element nil))
         (worker-epochs   (make-array workers :initial-element 0))
         (current-tests   (make-array workers :initial-element nil))
         (watchdog-lock   (sb-thread:make-mutex :name "watchdog-lock"))
         (watchdog-stop   nil)
         (suite-deadline  *parallel-suite-deadline*)
         (threads         '())
         ;; Threads written off by the watchdog (stuck in an uninterruptible
         ;; trap — e.g. the macOS 26.5 hardware-trap delivery hang) and their
         ;; replacements. Guarded by hung-lock.
         (hung-threads    '())
         (respawned       '())
         (hung-lock       (sb-thread:make-mutex :name "hung-lock")))
    (labels
        ((hung-thread-p (thread)
           (sb-thread:with-mutex (hung-lock)
             (and (member thread hung-threads) t)))

         (record-hang (worker-idx test-plist thread)
           ;; The stuck thread can never publish a result or pick up more
           ;; work: record the test as a failure, write the worker off, and
           ;; respawn a replacement so pool throughput is preserved.
           (let* ((idx (position test-plist work-queue :test #'eq))
                  (result (append (%make-parallel-failure-result
                                   test-plist (get-internal-real-time)
                                   (format nil "  ---~%  message: \"test hung; worker abandoned by watchdog\"~%  ..."))
                                  (list :batch-id idx))))
             (when idx
               (sb-thread:with-mutex (results-lock)
                 (setf (aref results idx) result))
               (%tap-print-result result))
             (sb-thread:with-mutex (watchdog-lock)
               (setf (aref current-tests worker-idx) nil))
             (let ((replacement
                     (sb-thread:make-thread
                      (make-worker worker-idx)
                      :name (format nil "test-worker-~A-respawn" worker-idx))))
               (sb-thread:with-mutex (hung-lock)
                 (push thread hung-threads)
                 (push replacement respawned)))))

         (watchdog ()
           (let ((escalations '()))
             (loop
               (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
               (sleep *watchdog-poll-seconds*)
                (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
                (let* ((now        (get-internal-real-time))
                       (timed-out  (%collect-timed-out-workers
                                    watchdog-slots workers now watchdog-lock))
                       (new-escs   (%fire-timeout-interrupts
                                    timed-out worker-epochs *kill-grace-seconds* now)))
                  (setf escalations
                        (append new-escs
                                (%check-escalations escalations worker-epochs
                                                    current-tests watchdog-lock
                                                    (get-internal-real-time)
                                                    #'record-hang)))))))

         (heartbeat ()
           (let ((start    (get-internal-real-time))
                 (interval *heartbeat-interval-seconds*))
             (when (and interval (plusp interval))
               (loop
                 (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
                 (sleep interval)
                 (when (sb-thread:with-mutex (watchdog-lock) watchdog-stop) (return))
                 (let* ((elapsed-secs (round (/ (- (get-internal-real-time) start)
                                                (float internal-time-units-per-second))))
                        (done (sb-thread:with-mutex (results-lock)
                                (count-if-not #'null results)))
                        (snapshot
                          (sb-thread:with-mutex (watchdog-lock)
                            (copy-seq current-tests)))
                        (inflight-names
                          (loop for slot across snapshot when slot collect (getf slot :name))))
                   (format *error-output*
                           "# heartbeat: t=~As done=~A/~A workers=~A inflight=~A~%"
                           elapsed-secs done n workers inflight-names)
                   (finish-output *error-output*))))))

         (make-worker (worker-idx)
           ;; SBCL worker threads do not inherit parent dynamic bindings;
           ;; *test-runner-mode* and the Prolog rule table are explicitly rebound.
           (lambda ()
               (let ((*test-runner-mode* :parallel)
                     (cl-cc/prolog::*prolog-rules*
                       (%copy-prolog-rules prolog-snapshot)))
               (loop
                 (let ((task nil) (idx nil))
                   (sb-thread:with-mutex (queue-lock)
                     (when (< queue-index n)
                       (setf idx         queue-index
                             task        (aref work-queue queue-index))
                       (incf queue-index)))
                   (unless task (return))
                   (let* ((start-time (get-internal-real-time))
                          (result (%run-test-with-timeout
                                   task worker-idx start-time
                                   watchdog-slots worker-epochs
                                   current-tests watchdog-lock)))
                     (setf result (append result (list :batch-id idx)))
                     (sb-thread:with-mutex (results-lock)
                       (setf (aref results idx) result))
                     (%tap-print-result result))))))))
      (let ((watchdog-thread
               (sb-thread:make-thread #'watchdog :name "test-watchdog"))
            (heartbeat-thread
               (sb-thread:make-thread #'heartbeat :name "test-heartbeat"))
            (suite-timed-out-p nil))
        (unwind-protect
             (progn
               (dotimes (i workers)
                 (push (sb-thread:make-thread (make-worker i)
                                              :name (format nil "test-worker-~A" i))
                       threads))
               (setf threads (nreverse threads))
               (setf suite-timed-out-p
                     (eq (%join-worker-threads-until-deadline
                          threads suite-deadline #'hung-thread-p)
                         :suite-timeout))
               ;; Join respawned replacement workers too; a replacement can
               ;; itself hang and be replaced, so loop until the set is stable.
               (unless suite-timed-out-p
                 (let ((joined '()))
                   (loop
                     (let ((pending (sb-thread:with-mutex (hung-lock)
                                      (set-difference respawned joined))))
                       (when (null pending) (return))
                       (when (eq (%join-worker-threads-until-deadline
                                  pending suite-deadline #'hung-thread-p)
                                 :suite-timeout)
                         (setf suite-timed-out-p t)
                         (return))
                       (setf joined (append pending joined))))))
               (when suite-timed-out-p
                 (format *error-output*
                         "# ERROR: parallel suite deadline reached; terminating workers~%")
                 (finish-output *error-output*)
                 (%terminate-live-threads threads)
                 (error 'sb-ext:timeout)))
          (when (or suite-timed-out-p (%deadline-expired-p suite-deadline))
            (%terminate-live-threads threads)
            (%terminate-live-threads
             (sb-thread:with-mutex (hung-lock) (copy-list respawned))))
          (sb-thread:with-mutex (watchdog-lock)
            (setf watchdog-stop t))
          (%join-support-thread watchdog-thread)
          (%join-support-thread heartbeat-thread))))
    ;; A hang can leave unclaimed queue slots (all workers stuck at once);
    ;; drop the nil placeholders so reporting sees only real result plists.
    (remove nil (coerce results 'list))))
