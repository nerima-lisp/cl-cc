(in-package :cl-cc/test)

;;; These test the orchestration layer (%run-single-test, %run-tests-sequential,
;;; %effective-worker-count, run-suite, %suite-parallel-p, %test-parallel-safe-p).

(defsuite runner-regression-suite
  :description "Serial regression tests for test runner orchestration"
  :parallel nil
  :parent cl-cc-suite)

(in-suite runner-regression-suite)

(defun %make-blocked-test-plist (name timeout)
  (let ((blocker (sb-thread:make-semaphore :count 0)))
    (list :name name
          :fn (lambda () (sb-thread:wait-on-semaphore blocker))
          :suite 'cl-cc-unit-suite
          :timeout timeout
          :depends-on nil
          :tags nil)))

(defun %make-blocked-thread (name)
  (let ((blocker (sb-thread:make-semaphore :count 0)))
    (sb-thread:make-thread (lambda () (sb-thread:wait-on-semaphore blocker))
                           :name name)))

(defmacro with-clcc-test-timeout-env (&body body)
  `(%with-preserved-env-var ("CLCC_TEST_TIMEOUT") ,@body))

(defmacro with-clcc-suite-timeout-env (&body body)
  `(%with-preserved-env-var ("CLCC_SUITE_TIMEOUT") ,@body))

(deftest suite-parallel-policy-inherits-from-parent
  "Child suites inherit serial execution policy from ancestors."
  (let ((parent (gensym "ULW-PARENT-"))
        (child (gensym "ULW-CHILD-")))
    (with-restored-bindings (*suite-registry*)
      (with-suite-registry-entry (parent
                                   :description "tmp"
                                   :parent nil
                                   :parallel nil)
        (with-suite-registry-entry (child
                                     :description "tmp"
                                     :parent parent
                                     :parallel t)
          (assert-false (%suite-parallel-p child))
          (assert-false (%test-parallel-safe-p (list :suite child :depends-on nil))))))))

(deftest suite-parent-cycle-does-not-hang-discovery-helpers
  "Suite parent cycles terminate in collection, fixture lookup, and parallel checks."
  (let ((suite-a (gensym "ULW-CYCLE-A-"))
        (suite-b (gensym "ULW-CYCLE-B-"))
        (before-a (lambda () :before-a))
        (before-b (lambda () :before-b))
        (after-a (lambda () :after-a))
        (after-b (lambda () :after-b)))
    (with-fresh-registry-state
      (with-suite-registry-entry (suite-a
                                   :description "tmp"
                                   :parent suite-b
                                   :parallel t
                                   :before-each (list before-a)
                                   :after-each (list after-a))
        (with-suite-registry-entry (suite-b
                                     :description "tmp"
                                     :parent suite-a
                                     :parallel t
                                     :before-each (list before-b)
                                     :after-each (list after-b))
          (with-test-registry-entry ('ulw-cycle-test-a
                                      :suite suite-a
                                      :tags nil)
            (with-test-registry-entry ('ulw-cycle-test-b
                                        :suite suite-b
                                        :tags nil)
              (assert-= 2 (length (%collect-all-suite-tests suite-a nil)))
              (multiple-value-bind (before-chain after-chain) (%get-suite-fixtures suite-a)
                (assert-= 2 (length before-chain))
                (assert-= 2 (length after-chain))
                (assert-true (member before-a before-chain :test #'eq))
                (assert-true (member before-b before-chain :test #'eq))
                (assert-true (member after-a after-chain :test #'eq))
                (assert-true (member after-b after-chain :test #'eq)))
              (assert-false (%suite-parallel-p suite-a))
              (assert-false (%test-parallel-safe-p (list :suite suite-a :depends-on nil))))))))))

(deftest dependency-ordering-moves-dependent-after-prerequisite
  "%order-tests-for-dependencies places a dependent test after its prerequisite."
  (let* ((dependency (list :name 'ulw-dependency :depends-on nil))
         (dependent  (list :name 'ulw-dependent  :depends-on 'ulw-dependency))
         (ordered    (%order-tests-for-dependencies (list dependent dependency))))
    (assert-equal '(ulw-dependency ulw-dependent)
                  (mapcar (lambda (test) (getf test :name)) ordered))))

(deftest dependency-ordering-cycle-break-preserves-prefix
  "%order-tests-for-dependencies falls back to preserving non-dependent tests first."
  (let* ((ordered (%order-tests-for-dependencies
                   (list (list :name 'ulw-a :depends-on 'ulw-b)
                         (list :name 'ulw-x :depends-on nil)
                         (list :name 'ulw-b :depends-on 'ulw-a)))))
    (assert-equal '(ulw-x ulw-a ulw-b)
                  (mapcar (lambda (test) (getf test :name)) ordered))))

(deftest run-single-test-skips-when-dependency-failed
  "A test with a failed dependency is reported as skipped without executing its body."
  (let* ((called nil)
         (test-plist (list :name 'needs-dep
                           :fn (lambda () (setf called t))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on 'upstream
                           :tags nil))
         (result (%run-single-test test-plist 1 (list (list :name 'upstream :status :fail)))))
    (assert-false called)
    (assert-eq :skip (getf result :status))))

(deftest run-single-test-pending-condition-returns-pending-with-reason
  "%run-single-test: a pending-condition signals :pending status with the reason in :detail."
  (let* ((test-plist (list :name 'pending-demo
                           :fn (lambda () (error 'pending-condition :reason "later"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
          (result (%run-single-test test-plist 1 '())))
    (assert-eq :pending (getf result :status))
    (assert-string-contains-all (getf result :detail) '("later"))))

(deftest effective-test-timeout-bogus-normalizes-to-default
  "%effective-test-timeout returns the default when given a non-integer keyword."
  (assert-= (%default-test-timeout) (%effective-test-timeout (list :timeout :bogus))))

(deftest effective-test-timeout-preserves-valid-integer
  "%effective-test-timeout returns the given positive integer unchanged."
  (assert-equal 7 (%effective-test-timeout (list :timeout 7))))

(deftest effective-test-timeout-preserves-valid-real
  "%effective-test-timeout returns the given positive real unchanged."
  (assert-equal 0.25 (%effective-test-timeout (list :timeout 0.25))))

(deftest effective-test-timeout-nil-normalizes-to-default
  "%effective-test-timeout returns the default when given nil."
  (assert-= (%default-test-timeout) (%effective-test-timeout (list :timeout nil))))

(deftest default-test-timeout-honors-env-var-and-falls-back-to-10
  "%default-test-timeout: unset→10; set to 14→14; set to bogus→10."
  (with-clcc-test-timeout-env
    (sb-posix:unsetenv "CLCC_TEST_TIMEOUT")
    (assert-= 10 (%default-test-timeout))
    (sb-posix:setenv "CLCC_TEST_TIMEOUT" "14" 1)
    (assert-= 14 (%default-test-timeout))
    (sb-posix:setenv "CLCC_TEST_TIMEOUT" "bogus" 1)
    (assert-= 10 (%default-test-timeout))))

(deftest-each default-test-timeout-rejects-invalid-env-values
  "%default-test-timeout falls back to 10 for non-positive or malformed env values."
  :cases (("zero" "0")
          ("negative" "-1")
          ("malformed" "1abc"))
  (value)
  (with-clcc-test-timeout-env
    (sb-posix:setenv "CLCC_TEST_TIMEOUT" value 1)
    (assert-= 10 (%default-test-timeout))))

(deftest default-test-timeout-env-applies-to-slow-test
  "CLCC_TEST_TIMEOUT=0.05 is used as the per-test default and times out a slow sequential test."
  (with-clcc-test-timeout-env
    (let* ((*test-runner-mode* :sequential)
           (test-plist (%make-blocked-test-plist 'env-slow-demo nil)))
      (sb-posix:setenv "CLCC_TEST_TIMEOUT" "0.05" 1)
      (let ((result (%run-single-test test-plist 1 '())))
        (assert-eq :fail (getf result :status))
        (assert-string-contains-all
         (getf result :detail)
         '("timeout after 0.05 seconds"))))))

(deftest effective-test-timeout-explicit-value-overrides-env-default
  "A valid per-test :timeout overrides CLCC_TEST_TIMEOUT."
  (with-clcc-test-timeout-env
    (sb-posix:setenv "CLCC_TEST_TIMEOUT" "1" 1)
    (assert-equal 3 (%effective-test-timeout (list :timeout 3)))))

(deftest default-suite-timeout-honors-env-var-and-falls-back-to-600
  "%default-suite-timeout: unset→600; set to 90→90; set to bogus→600."
  (with-clcc-suite-timeout-env
    (sb-posix:unsetenv "CLCC_SUITE_TIMEOUT")
    (assert-= 600 (%default-suite-timeout))
    (sb-posix:setenv "CLCC_SUITE_TIMEOUT" "90" 1)
    (assert-= 90 (%default-suite-timeout))
    (sb-posix:setenv "CLCC_SUITE_TIMEOUT" "bogus" 1)
    (assert-= 600 (%default-suite-timeout))))

(deftest-each default-suite-timeout-rejects-invalid-env-values
  "%default-suite-timeout falls back to 600 for non-positive or malformed env values."
  :cases (("zero" "0")
          ("negative" "-1")
          ("malformed" "1abc"))
  (value)
  (with-clcc-suite-timeout-env
    (sb-posix:setenv "CLCC_SUITE_TIMEOUT" value 1)
    (assert-= 600 (%default-suite-timeout))))

(deftest default-suite-timeout-env-drives-suite-deadline
  "CLCC_SUITE_TIMEOUT=1 is accepted, and deadline joins terminate hung workers."
  (let (thread)
    (with-clcc-suite-timeout-env
      (sb-posix:setenv "CLCC_SUITE_TIMEOUT" "1" 1)
      (assert-= 1 (%default-suite-timeout))
      (let ((deadline (+ (get-internal-real-time)
                         (round (* 0.05 internal-time-units-per-second)))))
        (setf thread (%make-blocked-thread "suite-timeout-env-demo"))
        (assert-eq :suite-timeout
                   (%join-worker-threads-until-deadline (list thread) deadline))))
    (when thread (%terminate-thread-safely thread))))

(deftest suite-timeout-result-returns-true-when-quit-p-nil
  "%suite-timeout-result returns t and emits an error log when quit-p is nil."
  (let* ((err-out (make-string-output-stream))
         (*error-output* err-out)
         (result (%suite-timeout-result 'demo-suite 42 nil)))
    (assert-true result)
    (let ((log (get-output-stream-string err-out)))
      (assert-string-contains-all (string-upcase log) '("DEMO-SUITE"))
      (assert-string-contains-all log '("42")))))

(deftest suite-timeout-result-calls-quit-124-when-quit-p-true
  "%suite-timeout-result calls (uiop:quit 124) when quit-p is true."
  (let ((captured nil)
        (err-out (make-string-output-stream)))
    (with-replaced-function (uiop:quit (lambda (&optional code) (setf captured code)))
      (let ((*error-output* err-out))
        (%suite-timeout-result 'other-suite 10 t)))
    (assert-eql 124 captured)
    (assert-string-contains-all
     (string-upcase (get-output-stream-string err-out))
     '("OTHER-SUITE"))))

(deftest run-single-test-skip-condition-returns-skip-with-reason
  "%run-single-test: a skip-condition signals :skip status with the reason in :detail."
  (let* ((test-plist (list :name 'skip-demo
                           :fn (lambda () (error 'skip-condition :reason "not today"))
                           :suite 'cl-cc-unit-suite
                           :timeout nil
                           :depends-on nil
                           :tags nil))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :skip (getf result :status))
    (assert-string-contains-all (getf result :detail) '("not today"))))

(deftest run-single-test-reports-fixture-setup-errors
  "A before-each fixture error is surfaced as a fixture error result."
  (let ((fixture-suite (gensym "ULW-FIXTURE-SUITE-")))
    (with-restored-bindings (*suite-registry*)
      (with-suite-registry-entry (fixture-suite
                                   :description "tmp"
                                   :parent nil
                                   :parallel t
                                   :before-each (list (lambda () (error "fixture boom")))
                                   :after-each '())
        (let* ((test-plist (list :name 'fixture-demo
                                 :fn (lambda () t)
                                 :suite fixture-suite
                                 :timeout nil
                                 :depends-on nil
                                 :tags nil))
               (result (%run-single-test test-plist 1 '())))
          (assert-eq :fail (getf result :status))
          (assert-string-contains-all
           (getf result :detail)
           '("fixture error" "fixture boom")))))))

(deftest run-single-test-times-out-sequentially
  "Sequential runner reports timeout failures when a test exceeds its budget."
  ;; Bind *test-runner-mode* to :sequential so sb-ext:with-timeout is applied.
  ;; The global mode is :parallel (set in apps.nix to suppress SIGALRM delivery
  ;; issues), but this test specifically exercises the sequential timeout path.
  (let* ((*test-runner-mode* :sequential)
         (test-plist (%make-blocked-test-plist 'slow-demo 0.05))
         (result (%run-single-test test-plist 1 '())))
    (assert-eq :fail (getf result :status))
    (assert-string-contains-all (getf result :detail) '("timeout after 0.05 seconds"))))

(deftest run-tests-sequential-returns-ordered-results
  "%run-tests-sequential preserves input order and records pass statuses."
  (let ((results (%run-tests-sequential
                  (list (list :name 'first
                              :fn (lambda () t)
                              :suite 'cl-cc-unit-suite
                              :timeout nil
                              :depends-on nil
                              :tags nil
                              :number 1)
                        (list :name 'second
                              :fn (lambda () t)
                              :suite 'cl-cc-unit-suite
                              :timeout nil
                              :depends-on nil
                              :tags nil
                              :number 2)))))
    (assert-equal '(first second)
                  (mapcar (lambda (result) (getf result :name)) results))
    (assert-true (every (lambda (result) (eq :pass (getf result :status))) results))))

(deftest duplicate-test-names-overwrite-registry-entry
  "deftest overwrites an existing test registry entry when the same name is reloaded."
  (let ((*test-registry* (persist-empty))
        (*current-suite* 'cl-cc-unit-suite)
        (test-name (gensym "DUPLICATE-TEST-NAME-")))
    (eval `(deftest ,test-name t))
    (eval `(deftest ,test-name nil))
    (let ((entry (persist-lookup *test-registry* test-name)))
      (assert-true entry)
      (assert-eq test-name (getf entry :name)))))

(deftest duplicate-suite-names-overwrite-registry-entry
  "defsuite overwrites an existing suite registry entry when the same name is reloaded."
  (let ((*suite-registry* (persist-empty))
        (suite-name (gensym "DUPLICATE-SUITE-NAME-")))
    (eval `(defsuite ,suite-name :description "first"))
    (eval `(defsuite ,suite-name :description "second"))
    (let ((entry (persist-lookup *suite-registry* suite-name)))
      (assert-true entry)
      (assert-string= "second" (getf entry :description)))))

(deftest resolve-suite-returns-symbol-and-signals-for-missing-suite
  "%resolve-suite returns existing suites and errors on missing ones."
  (assert-eq 'cl-cc-unit-suite (%resolve-suite :cl-cc/test "CL-CC-UNIT-SUITE"))
  (handler-case
      (progn
        (%resolve-suite :cl-cc/test "MISSING-SUITE")
        (assert-false t))
    (error (e)
      (assert-string-contains-all
       (princ-to-string e)
       '("Suite CL-CC/TEST::MISSING-SUITE not found")))))

;;; ── New-helper coverage (added after %run-single-test decomposition) ─────

(deftest make-test-result-pass-shape
  "%make-test-result: :pass result has all expected fields and nil detail."
  (let ((r (%make-test-result 'my-test 3 'my-suite :pass nil)))
    (assert-eq 'my-test  (getf r :name))
    (assert-eq :pass     (getf r :status))
    (assert-eq 'my-suite (getf r :suite))
    (assert-= 3          (getf r :number))
    (assert-null         (getf r :detail))))

(deftest make-test-result-fail-carries-detail
  "%make-test-result: :fail result carries the detail string."
  (let ((r (%make-test-result 'my-test 1 'my-suite :fail "oops")))
    (assert-eq :fail      (getf r :status))
    (assert-string= "oops" (getf r :detail))))

(deftest-each format-timeout-detail-rendering
  "%format-timeout-detail renders the configured timeout or the default timeout."
  :cases (("defaulted" nil nil)
          ("numeric"   5   "5 seconds"))
  (timeout expected-substr)
  (assert-string-contains-all
   (%format-timeout-detail timeout)
   (list (or expected-substr
             (format nil "~A seconds" (%default-test-timeout))))))

(deftest-each count-results-by-status-counts-correctly
  "%count-results-by-status filters by exact status keyword."
  :cases (("pass" :pass 2)
          ("fail" :fail 1)
          ("skip" :skip 0))
  (status expected)
  (let ((results (list (list :status :pass) (list :status :fail) (list :status :pass))))
    (assert-= expected (%count-results-by-status results status))))

(deftest-each path-is-cwd-descendant-cases
  "%path-is-cwd-descendant-p correctly identifies descendant paths."
  :cases (("same"      "/home/user/"      "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-true (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("child"     "/home/user/foo/"  "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-true (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("sibling"   "/home/other/"     "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-false (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("nil-path"  nil                  "/home/user/"
           (lambda (path-ns cwd-ns)
             (assert-false (%path-is-cwd-descendant-p path-ns cwd-ns))))
          ("nil-cwd"   "/home/user/"      nil
           (lambda (path-ns cwd-ns)
             (assert-false (%path-is-cwd-descendant-p path-ns cwd-ns)))))
  (path-ns cwd-ns verify)
  (funcall verify path-ns cwd-ns))

(deftest cpu-count-detect-returns-positive-integer
  "%detect-cpu-count always returns a positive integer."
  (let ((*cpu-count-sources* (list (lambda () 8))))
    (let ((n (%detect-cpu-count)))
    (assert-true (integerp n))
      (assert-true (plusp n)))))

(deftest cpu-count-env-source-returns-integer-or-nil
  "The first *cpu-count-sources* thunk returns a positive integer or nil."
  (let* ((env-source (first *cpu-count-sources*))
         (result (funcall env-source)))
    (assert-true (or (null result) (and (integerp result) (plusp result))))))

(deftest number-tests-annotates-with-index
  "%number-tests adds a :number key (1-based) to each plist in the list."
  (let* ((plists (list '(:name a) '(:name b) '(:name c)))
         (result (%number-tests plists)))
    (assert-= 3 (length result))
    (assert-= 1 (getf (first  result) :number))
    (assert-= 2 (getf (second result) :number))
    (assert-= 3 (getf (third  result) :number))))

(deftest number-tests-empty-returns-nil
  "%number-tests on an empty list returns nil."
  (assert-null (%number-tests nil)))

(deftest cpu-count-command-failure-yields-nil
  "%parse-command-cpu-count returns NIL when the command runner fails."
  (let ((*run-command-output-fn* (lambda (&rest args)
                                   (declare (ignore args))
                                   (error "cpu-count failed"))))
    (assert-null (%parse-command-cpu-count '("fake-cpu-count")))))

;;; ── Runner policy / filtering / flaky detection ────────────────────────

(deftest mixed-runner-keeps-serial-suites-out-of-parallel-pool
  "Serial suites and dependent tests are excluded from the parallel worker batch."
  (let ((serial-suite (gensym "ULW-SERIAL-SUITE-"))
        (parallel-suite (gensym "ULW-PARALLEL-SUITE-")))
    (with-restored-binding (*suite-registry*)
      (with-suite-registry-entry (serial-suite
                                   :description "tmp"
                                   :parent nil
                                   :parallel nil)
        (with-suite-registry-entry (parallel-suite
                                     :description "tmp"
                                     :parent nil
                                     :parallel t)
          (assert-false (%test-parallel-safe-p
                         (list :name 'serial-test :suite serial-suite :depends-on nil)))
          (assert-false (%test-parallel-safe-p
                         (list :name 'dependent-test :suite parallel-suite
                               :depends-on 'other-test)))
          (assert-true (%test-parallel-safe-p
                        (list :name 'parallel-test :suite parallel-suite :depends-on nil))))))))

(deftest effective-worker-count-falls-back-to-one-for-serial-batches
  "Worker reporting collapses to 1 when no test in the batch may run in parallel."
  (let ((serial-suite (gensym "ULW-SERIAL-SUITE-")))
    (with-restored-binding (*suite-registry*)
      (with-suite-registry-entry (serial-suite
                                   :description "tmp"
                                   :parent nil
                                   :parallel nil)
        (assert-= 1
                  (%effective-worker-count
                   (list (list :name 'serial-test :suite serial-suite :depends-on nil))
                   t
                   4))
        (assert-= 1
                  (%effective-worker-count
                   (list (list :name 'serial-test :suite serial-suite :depends-on nil))
                   nil
                   4))))))

(deftest effective-worker-count-keeps-requested-workers-for-parallel-safe-batches
  "Worker reporting preserves the requested worker count when at least one test can run in parallel."
  (let ((parallel-suite (gensym "ULW-PARALLEL-SUITE-")))
    (with-restored-binding (*suite-registry*)
      (with-suite-registry-entry (parallel-suite
                                   :description "tmp"
                                   :parent nil
                                   :parallel t)
        (assert-= 4
                  (%effective-worker-count
                   (list (list :name 'parallel-test :suite parallel-suite :depends-on nil))
                   t
                   4))
        (assert-= 2
                  (%effective-worker-count
                   (list (list :name 'parallel-test :suite parallel-suite :depends-on nil))
                   t
                   2))))))

(deftest run-suite-reports-one-worker-for-serial-only-batch
  "run-suite reports one worker when the selected batch has no parallel-safe tests."
  (let ((root (gensym "ULW-ROOT-"))
        (serial-suite (gensym "ULW-SERIAL-"))
        (test-name (gensym "ULW-TEST-")))
    (with-restored-bindings (*suite-registry*
                             *test-registry*
                             ((symbol-function 'cl-cc::warm-stdlib-cache)))
      (with-fresh-registry-state
        (with-suite-registry-entry (root
                                     :description "tmp"
                                     :parent nil
                                     :parallel t)
          (with-suite-registry-entry (serial-suite
                                       :description "tmp"
                                       :parent root
                                       :parallel nil)
            (with-test-registry-entry (test-name
                                        :suite serial-suite
                                        :fn (lambda () t)
                                        :depends-on nil
                                        :timeout nil
                                        :tags nil)
              (setf (symbol-function 'cl-cc::warm-stdlib-cache) (lambda () nil))
              ;; Use quit-p nil so run-suite returns any-fail directly without calling
              ;; uiop:quit — avoids the need to mock uiop:quit and is immune to the
              ;; mocked-quit-returns-NIL ambiguity when the killer thread is active.
              (let ((output (with-output-to-string (s)
                              (let ((*standard-output* s))
                                (assert-false
                                 (run-suite root :parallel t :random nil :workers 4 :quit-p nil))))))
                (assert-string-contains-all output '("Workers: 1"))))))))))

(deftest-each canonical-suite-taxonomy-matches-runner-contract
  "The canonical runner exposes its top-level test classes under the root taxonomy."
  :cases (("unit"        'cl-cc-unit-suite)
          ("integration" 'cl-cc-integration-suite)
          ("e2e"         'cl-cc-e2e-suite)
          ("conformance" 'cl-cc-conformance-suite)
          ("docs"        'cl-cc-documentation-suite))
  (suite-name)
  (assert-eq 'cl-cc-suite
             (getf (persist-lookup *suite-registry* suite-name) :parent)))

(deftest run-tests-excludes-non-unit-suites-by-default
  "The canonical runner keeps non-fast suites outside the default plan."
  (let ((captured nil))
    (with-restored-bindings (*suite-registry*)
      (with-suite-registry-entry ('cl-cc-e2e-suite
                                   :description "tmp"
                                   :parent 'cl-cc-suite
                                   :parallel nil)
        (with-replaced-function (run-suite
                                 (lambda (suite-name &key parallel random warm-stdlib tags exclude-tags exclude-suites filter coverage &allow-other-keys)
                                   (setf captured (list :suite-name suite-name
                                                        :parallel parallel
                                                        :random random
                                                        :warm-stdlib warm-stdlib
                                                        :tags tags
                                                        :exclude-tags exclude-tags
                                                        :exclude-suites exclude-suites
                                                        :filter filter
                                                        :coverage coverage))
                                   0))
          (assert-equal 0 (run-tests :parallel nil :random nil :filter "static"))
          (assert-eq 'cl-cc-suite (getf captured :suite-name))
          (assert-string= "static" (getf captured :filter))
          (assert-true (getf captured :warm-stdlib))
          (assert-true (member 'cl-cc-integration-suite (getf captured :exclude-suites)))
          (assert-true (member 'cl-cc-e2e-suite (getf captured :exclude-suites)))
          (assert-true (member 'cl-cc-conformance-suite (getf captured :exclude-suites)))
          (assert-true (member 'cl-cc-documentation-suite (getf captured :exclude-suites))))))))

(deftest run-tests-forwards-warm-stdlib-option
  "run-tests lets the Nix app skip pre-warming for focused non-stdlib runs."
  (let ((captured nil))
    (with-replaced-function (run-suite
                             (lambda (suite-name &key warm-stdlib &allow-other-keys)
                               (setf captured (list :suite-name suite-name
                                                    :warm-stdlib warm-stdlib))
                               0))
      (assert-equal 0 (run-tests :parallel nil :random nil :warm-stdlib nil))
      (assert-eq 'cl-cc-suite (getf captured :suite-name))
      (assert-false (getf captured :warm-stdlib)))))

(deftest test-name-filter-matches-substrings-case-insensitively
  "The fast-runner filter matches test names by case-insensitive substring."
  (let ((tests (list (list :name 'php-e2e-static-members)
                     (list :name 'js-rt-array))))
    (assert-equal '(php-e2e-static-members)
                  (mapcar (lambda (test) (getf test :name))
                          (%filter-tests-by-name tests "STATIC")))))

(deftest test-name-filter-supports-comma-and-repeated-filters
  "Comma-separated and repeated filters narrow names with AND semantics."
  (let ((tests (list (list :name 'php-e2e-array-pad-key-policy)
                     (list :name 'php-e2e-array-column-index-key)
                     (list :name 'js-rt-array-to-spliced))))
    (assert-equal '(php-e2e-array-pad-key-policy)
                  (mapcar (lambda (test) (getf test :name))
                          (%filter-tests-by-name tests "php,array-pad")))
    (assert-equal '(js-rt-array-to-spliced)
                  (mapcar (lambda (test) (getf test :name))
                          (%filter-tests-by-name tests '("js" "spliced"))))))

(deftest run-tests-does-not-load-e2e-system-implicitly
  "run-tests only dispatches the already-loaded fast suite taxonomy."
  (let ((loaded nil)
        (run-called nil))
    (with-restored-bindings (*suite-registry*)
      (setf *suite-registry* (persist-remove *suite-registry* 'cl-cc-e2e-suite))
      (with-replaced-function (asdf:load-system
                               (lambda (system &key &allow-other-keys)
                                 (setf loaded system)
                                 0))
        (with-replaced-function (run-suite
                                 (lambda (&rest args)
                                   (declare (ignore args))
                                   (setf run-called t)
                                   0))
          (assert-equal 0 (run-tests :parallel nil :random nil))
          (assert-null loaded)
          (assert-true run-called))))))

(deftest fast-plan-filter-is-name-independent
  "Fast-plan selection is based on suite taxonomy, not a slow-name convention."
  (let ((captured nil))
    (with-replaced-function (run-suite
                             (lambda (suite-name &key exclude-suites &allow-other-keys)
                               (setf captured (list suite-name exclude-suites))
                               0))
      (assert-equal 0 (run-tests :parallel nil :random nil))
      (assert-eq 'cl-cc-suite (first captured))
      (assert-true (member 'cl-cc-integration-suite (second captured)))
      (assert-true (member 'cl-cc-e2e-suite (second captured)))
      (assert-true (member 'cl-cc-conformance-suite (second captured)))
      (assert-true (member 'cl-cc-documentation-suite (second captured))))))

(deftest detect-flaky-reports-inconsistent-statuses
  "%detect-flaky prints a summary when a test passes in only some repeated runs."
  (let ((*standard-output* (make-string-output-stream)))
    (%detect-flaky (list (list (list :name 'sometimes :status :pass)
                               (list :name 'always :status :pass))
                         (list (list :name 'sometimes :status :fail)
                               (list :name 'always :status :pass)))
                   2)
    (let ((output (get-output-stream-string *standard-output*)))
      (assert-string-contains-all output '("Flaky tests detected"))
      (assert-string-contains-all (string-upcase output) '("SOMETIMES")))))

(deftest detect-flaky-is-silent-for-consistent-results
  "%detect-flaky emits nothing when every test is consistently pass or fail."
  (let ((*standard-output* (make-string-output-stream)))
    (%detect-flaky (list (list (list :name 'always-pass :status :pass)
                               (list :name 'always-fail :status :fail))
                         (list (list :name 'always-pass :status :pass)
                               (list :name 'always-fail :status :fail)))
                   2)
    (assert-string= "" (get-output-stream-string *standard-output*))))
