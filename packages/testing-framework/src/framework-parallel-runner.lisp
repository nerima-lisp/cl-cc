;;;; framework-parallel-runner.lisp — Parallel worker pool, CPU detection, run-suite, run-tests
(in-package :cl-cc/test)

(defparameter *cpu-detect-command-timeout-seconds* 2
  "Timeout in seconds for external CPU-count detection commands.")

(defparameter *suite-killer-exit-fn*
  (lambda (&key code)
    (sb-ext:exit :code code :abort t))
  "Indirection for the suite-deadline-killer exit action so tests can rebind to a recorder.
Default uses sb-ext:exit :code code :abort t.  The :abort t flag calls _exit(2) directly,
bypassing the SBCL thread join that plain (sb-ext:exit) would perform; without :abort t a
GC-safepoint-blocked main thread cannot be terminated by the killer.")

;;; ------------------------------------------------------------
;;; Parallel Worker Pool
;;; ------------------------------------------------------------

(defun %copy-prolog-rules (src)
  "Return a shallow copy of the Prolog rule DB SRC (hash-table).
Per-thread rebinding of `cl-cc/prolog::*prolog-rules*` needs a fresh
table per worker so concurrent test-side rebindings do not race through
the global binding."
  (let ((dst (make-hash-table :test 'eq)))
    (when src
      (maphash (lambda (k v) (setf (gethash k dst) v)) src))
    dst))

(defun %sort-parallel-slow-first (tests prior-timings)
  "Return TESTS stable-sorted by descending prior duration.
Tests absent from PRIOR-TIMINGS get 0 (sorted last — treated as fast or new)."
  (stable-sort (copy-list tests) #'>
               :key (lambda (test)
                      (let ((name (getf test :name)))
                        (or (and name (gethash (symbol-name name) prior-timings))
                            0)))))

(defun %run-tests-sequential (tests &optional results-so-far)
  "Run tests sequentially and return result plists.
RESULTS-SO-FAR is consulted for dependency checks but is not included in the
returned list."
  (let ((results '()))
    (dolist (test tests)
      (let* ((number (getf test :number))
             (result (%run-single-test test number
                                       (append results-so-far (reverse results)))))
        (push result results)
        (%tap-print-result result)))
    (nreverse results)))

(defun %run-tests-mixed (tests workers &optional prior-timings)
  "Run TESTS with a mixed strategy: parallel-safe tests use the worker pool,
while tests from explicitly serial suites and tests with dependencies run
sequentially in dependency-safe input order."
  (let ((results '())
        (tests (%order-tests-for-dependencies tests))
        (parallel-batch '()))
    (flet ((flush-parallel-batch ()
             (when parallel-batch
               (format *error-output* "# batch: ~D parallel tests~%"
                       (length parallel-batch))
               (finish-output *error-output*)
               (setf results
                     (append results
                             (%run-tests-parallel (nreverse parallel-batch)
                                                  workers prior-timings)))
               (setf parallel-batch '()))))
      (dolist (test tests)
        (if (%test-parallel-safe-p test)
            (push test parallel-batch)
            (progn
              (flush-parallel-batch)
              ;; Serial tests run on THIS thread: a hang here is unrecoverable
              ;; (the macOS 26.5 trap-delivery hang blocks interrupts), so
              ;; always announce the test first — the last announced name in
              ;; the log identifies the hung test after a deadline kill.
              (format *error-output* "# serial: ~A~%" (getf test :name))
              (finish-output *error-output*)
              (let ((*test-runner-mode* :parallel))
                (setf results
                      (append results
                              (%run-tests-sequential (list test) results)))))))
      (flush-parallel-batch)
      results)))

(defun %default-run-command-output (cmd)
  "Run CMD and return its captured output as a string, bounded by
*cpu-detect-command-timeout-seconds* to prevent hung subprocesses."
  (with-output-to-string (stream)
    (sb-ext:with-timeout *cpu-detect-command-timeout-seconds*
      (uiop:run-program cmd :output stream :ignore-error-status t))))

(defparameter *run-command-output-fn* #'%default-run-command-output
  "Function of one argument CMD used by `%run-command-output'.")

(defun %run-command-output (cmd)
  "Run CMD through `*run-command-output-fn*' and return its captured output."
  (funcall *run-command-output-fn* cmd))

(defun %parse-command-cpu-count (cmd)
  "Run CMD and parse its output as a positive integer, or NIL."
  (ignore-errors
    (let* ((output (sb-ext:with-timeout *cpu-detect-command-timeout-seconds*
                     (%run-command-output cmd)))
           (n (parse-integer output :junk-allowed t)))
      (and n (plusp n) n))))

;;; CPU detection sources tried left-to-right; first positive integer wins.
(defparameter *cpu-count-sources*
  (list
   ;; 1. Environment override (e.g. CI matrix, Nix sandbox)
   (lambda ()
     (let ((env (uiop:getenv "CL_CC_TEST_WORKERS")))
       (and env (ignore-errors
                  (let ((n (parse-integer env :junk-allowed t)))
                    (and n (plusp n) n))))))
   ;; 2. macOS sysctl
   (lambda () (%parse-command-cpu-count '("sysctl" "-n" "hw.ncpu")))
   ;; 3. Linux nproc
   (lambda () (%parse-command-cpu-count '("nproc"))))
  "Ordered list of CPU-count detection thunks; first truthy result wins.")

(defun %detect-cpu-count ()
  "Detect host CPU count using *cpu-count-sources*. Falls back to 4."
  (or (some #'funcall *cpu-count-sources*) 4))

(defun %default-suite-timeout ()
  "Return the default whole-suite timeout in seconds, or NIL when disabled.
Uses CLCC_SUITE_TIMEOUT when it is a positive integer; otherwise falls back
to 600 seconds. Sized to accommodate a cold-cache full recompile (which
ASDF triggers at ~3-5 minutes when output translations bypass the Nix store
FASLs); warm-cache runs complete in well under a minute."
  (let ((raw (uiop:getenv "CLCC_SUITE_TIMEOUT")))
    (or (and raw
             (ignore-errors
               (let ((parsed (parse-integer raw)))
                 (and (plusp parsed) parsed))))
        600)))

(defun %suite-timeout-deadline (suite-timeout)
  "Convert SUITE-TIMEOUT seconds into an absolute internal-time deadline."
  (and suite-timeout
       (+ (get-internal-real-time)
          (round (* suite-timeout
                    internal-time-units-per-second)))))

(defstruct (%suite-deadline-killer-state
            (:constructor %make-suite-deadline-killer-state (lock sem)))
  (live t :type boolean)
  lock
  sem)

(defun %start-suite-deadline-killer (suite-deadline)
  "Start the deadline-killer thread for SUITE-DEADLINE and return its state."
  (let ((state (%make-suite-deadline-killer-state
                (sb-thread:make-mutex :name "suite-killer-lock")
                (when suite-deadline
                  (sb-thread:make-semaphore :name "suite-killer-sem" :count 0)))))
    (when suite-deadline
      (let ((err *error-output*)
            (exit-fn *suite-killer-exit-fn*)
            (remaining (max 1.0 (/ (- suite-deadline (get-internal-real-time))
                                   (float internal-time-units-per-second)))))
        (sb-thread:make-thread
         (lambda ()
           ;; Poll every 0.1s: sleep marks a GC safepoint before blocking.
           ;; try-semaphore is a non-blocking probe — returns t if signalled
           ;; (suite finished normally, do nothing), nil otherwise.
           (loop with check-interval = 0.1d0
                 for elapsed = 0.0d0 then (+ elapsed check-interval)
                 while (< elapsed remaining)
                 do (sleep check-interval)
                    (when (sb-thread:try-semaphore (%suite-deadline-killer-state-sem state))
                      (return))
                 finally
                 (when (sb-thread:with-mutex ((%suite-deadline-killer-state-lock state))
                         (%suite-deadline-killer-state-live state))
                   (format err "# FATAL: suite deadline killer triggered; aborting~%")
                   (finish-output err)
                   (funcall exit-fn :code 124))))
         :name "suite-deadline-killer")))
    state))

(defun %stop-suite-deadline-killer (state)
  "Mark STATE inactive and wake the killer thread if it is waiting."
  (sb-thread:with-mutex ((%suite-deadline-killer-state-lock state))
    (setf (%suite-deadline-killer-state-live state) nil))
  (when (%suite-deadline-killer-state-sem state)
    (sb-thread:signal-semaphore (%suite-deadline-killer-state-sem state))))

(defun %suite-timeout-result (suite-name timeout quit-p)
  "Handle a whole-suite timeout consistently for CLI and programmatic callers."
  (format *error-output*
          "# ERROR: suite ~A timed out after ~A seconds~%"
          suite-name timeout)
  (if quit-p
      (uiop:quit 124)
      t))

(defun %number-tests (plists)
  "Annotate each test plist in PLISTS with a :number index (1-based)."
  (loop for p in plists for i from 1
        collect (append p (list :number i))))

(defun %normalize-test-filters (filter)
  "Return non-empty test name filters from FILTER.
FILTER may be NIL, a string, or a list of strings. Commas split a single CLI
argument so `--filter php,array` narrows the test set without shell quoting."
  (labels ((parts (value)
             (when value
               (remove ""
                       (mapcar (lambda (part)
                                 (string-trim '(#\Space #\Tab #\Newline #\Return) part))
                               (uiop:split-string (princ-to-string value)
                                                  :separator '(#\,)))
                       :test #'string=))))
    (cond
      ((null filter) nil)
      ((listp filter) (mapcan #'parts filter))
      (t (parts filter)))))

(defun %test-name-matches-filters-p (name filters)
  (and name
       (every (lambda (filter)
                (search filter (symbol-name name) :test #'char-equal))
              filters)))

(defun %filter-tests-by-name (tests filter)
  "Return TESTS whose :NAME contains every filter token, case-insensitively."
  (let ((filters (%normalize-test-filters filter)))
    (if (null filters)
        tests
        (remove-if-not
         (lambda (test)
           (%test-name-matches-filters-p (getf test :name) filters))
         tests))))

(defun %effective-worker-count (ordered-tests parallel workers)
  "Return the effective worker count for ORDERED-TESTS.
Serial runs, serial-only batches, and single-worker requests all collapse to 1.
When WORKERS is NIL, the host CPU count is auto-detected (overridable via
CL_CC_TEST_WORKERS)."
  (let ((w (or workers (%detect-cpu-count))))
    (if (and parallel
             (> w 1)
             (some #'%test-parallel-safe-p ordered-tests))
        w
        1)))

;;; ------------------------------------------------------------
;;; run-suite
;;; ------------------------------------------------------------

(defun run-suite (suite-name &key
                               (parallel t)
                               (random t)
                               (seed nil)
                               (workers nil)
                               (repeat 1)
                               (update-snapshots nil)
                               (tags nil)
                               (exclude-tags nil)
                               (exclude-suites nil)
                               (filter nil)
                               (coverage nil)
                               (warm-stdlib t)
                               (quit-p t))
  "Run all tests in suite-name (and children).
When QUIT-P is true, exits via uiop:quit; otherwise returns whether any test failed."
  (when coverage
    (unless *coverage-reload-in-progress*
      (let ((*coverage-reload-in-progress* t))
        (enable-coverage)
        ;; Coverage app is responsible for loading the desired source systems
        ;; after instrumentation is enabled. Avoid a second force-reload here,
        ;; which otherwise recompiles overlapping test systems in-place and can
        ;; perturb global registries during the coverage phases.
        nil))
    (setf parallel nil)
    (format t "# Coverage mode: parallel disabled~%"))
  (when (or exclude-tags exclude-suites)
    (format t "# Excluding tags: ~S~%# Excluding suites: ~S~%" exclude-tags exclude-suites))
  (when update-snapshots
    (format t "# Snapshot update mode enabled~%"))

  (let* ((suite-timeout (%default-suite-timeout))
         (suite-deadline (%suite-timeout-deadline suite-timeout))
         (killer-state (%start-suite-deadline-killer suite-deadline)))
    ;; Deadline killer: fires (sb-ext:exit :abort t) from a side thread so that
    ;; a GC-safepoint-blocked main thread cannot outlive the suite deadline.
    ;; sb-ext:with-timeout alone is unreliable when the main thread is stuck in GC.
    ;; Poll via sleep (GC-safe safepoint) + try-semaphore (non-blocking) instead of
    ;; wait-on-semaphore :timeout, which uses __ulock_wait2 (NOT a GC safepoint on
    ;; macOS 26 ARM64).  Without this, GC blocks waiting for the killer thread to
    ;; reach a safepoint, while the main thread is already suspended by GC — deadlock.
    ;; Do NOT use sb-ext:with-timeout here — on macOS ARM64 SIGALRM is
    ;; delivered to an arbitrary thread and will kill the watchdog thread
    ;; rather than the main thread, disabling per-test timeout enforcement.
    ;; The semaphore-based killer above handles the suite deadline reliably.
    (unwind-protect
         (let ((*parallel-suite-deadline* suite-deadline)
               (*suite-fixture-cache* (make-hash-table :test #'eq))
               (*suite-parallel-cache* (make-hash-table :test #'eq)))
           ;; Reseed from wall-clock time before generating the random seed.
           ;; The nix core image has a fixed *random-state* that always produces
           ;; seed 1193941380623146742, which triggers a pre-existing deadlock at
           ;; worker 3.  get-internal-real-time advances independently of the
           ;; frozen core image state, so this breaks the determinism.
           (unless seed
             (setf *random-state*
                   (sb-ext:seed-random-state
                    (mod (get-internal-real-time) most-positive-fixnum))))
           (let* ((actual-seed     (or seed (random most-positive-fixnum)))
                  (*random-state*  (sb-ext:seed-random-state actual-seed))
                  (tests-plists    (%filter-tests-by-name
                                    (%collect-all-suite-tests suite-name tags exclude-tags exclude-suites)
                                    filter))
                  (n               (length tests-plists))
                  (test-vec        (coerce (%number-tests tests-plists) 'vector)))
             (format *error-output* "# stage: collected ~D tests~%" n)
             (finish-output *error-output*)
             (when filter
               (format t "# Filtering tests by name: ~A (~D selected)~%" filter n))
             (when random (%fisher-yates-shuffle test-vec))
             (let* ((ordered-tests     (%order-tests-for-dependencies (coerce test-vec 'list)))
                    (effective-workers (%effective-worker-count ordered-tests parallel workers)))
               (format *error-output* "# stage: ordered; workers=~D~%" effective-workers)
               (finish-output *error-output*)
               (%print-tap-header n repeat actual-seed effective-workers)
               (finish-output)
               ;; Second warm call: should be a no-op cache hit if apps.nix pre-warmed.
               ;; No inner sb-ext:with-timeout — see note above about SIGALRM delivery.
               (when warm-stdlib
                 (ignore-errors (cl-cc:warm-stdlib-cache)))
               (format *error-output* "# stage: warm2 done~%")
               (finish-output *error-output*)
               (when coverage (format t "# Coverage report enabled~%"))
               (let* ((prior-timings   (%load-prior-timings))
                      (all-run-results
                        (loop for r from 1 to repeat
                              do (when (> repeat 1) (format t "# Run ~A/~A~%" r repeat))
                              collect (if parallel
                                          (%run-tests-mixed ordered-tests (max 1 effective-workers) prior-timings)
                                          (%run-tests-sequential ordered-tests)))))
                 (when (> repeat 1) (%detect-flaky (reverse all-run-results) repeat))
                 (format t "# To reproduce this run: (run-suite '~A :seed ~A)~%" suite-name actual-seed)
                 (let* ((flat-results (apply #'append (reverse all-run-results)))
                        (any-fail     (%print-result-summary flat-results)))
                   (when coverage (%print-coverage-report flat-results))
                   (%emit-postrun-artifacts flat-results)
                       (if quit-p
                           (uiop:quit (if any-fail 1 0))
                           any-fail)))))
      ;; Cleanup: cancel the deadline-killer thread before calling uiop:quit,
      ;; so it exits promptly rather than blocking on the remaining sleep time.
      (when coverage (disable-coverage))
      (%stop-suite-deadline-killer killer-state)))))

(defun run-tests (&key
                       (tags nil)
                       (exclude-tags nil)
                       (exclude-suites nil)
                       (filter nil)
                       (warm-stdlib t)
                       (parallel t)
                       (random nil)
                       (coverage nil))
  "Run the canonical fast CL-CC test plan.
Integration, end-to-end, conformance, and documentation/evidence checks are run
explicitly by suite taxonomy, not by naming anything slow or auto-loading an
auxiliary system.
When COVERAGE is true, sb-cover instrumentation is enabled and a coverage
report is written to *coverage-report-directory* (default: ./coverage/)."
  (when coverage
    (enable-coverage))
  (unwind-protect
       (run-suite 'cl-cc-suite
                  :parallel parallel
                  :random random
                  :warm-stdlib warm-stdlib
                  :tags tags
                  :exclude-tags exclude-tags
                  :filter filter
                  :coverage coverage
                  :exclude-suites (remove-duplicates
                                   (append exclude-suites
                                           (%default-fast-plan-exclude-suites))
                                   :test #'eq))
    (when coverage
      (disable-coverage)
      (format t "~&Coverage report written to ~A~%" (or *coverage-report-directory* "./coverage/")))))

(defun %default-fast-plan-exclude-suites ()
  '(cl-cc-integration-suite
    cl-cc-e2e-suite
    cl-cc-conformance-suite
    cl-cc-documentation-suite))

(defun %resolve-suite (package-name symbol-name)
  (let* ((pkg (find-package package-name))
         (sym (and pkg (find-symbol symbol-name pkg))))
    (unless sym
      (error "Suite ~A::~A not found (package not loaded?)"
             package-name symbol-name))
    sym))
