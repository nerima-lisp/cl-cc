;;;; Unit tests for the per-test timing harness (FR-001/002/003).
;;;;
;;;; These exercise the private surface of the runner directly:
;;;;   - %run-single-test populates :duration-ns for every terminal status
;;;;   - %tap-print-result emits `duration_ms:` in its YAML diagnostic
;;;;   - %write-timings-tsv writes the frozen 5-column schema
;;;;   - %timings-output-path honors the CLCC_TIMINGS_FILE env override
;;;;
;;;; Durations are never asserted for specific values — wall-clock timing is
;;;; inherently non-deterministic. We assert integrality and non-negativity.

(in-package :cl-cc/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(in-suite cl-cc-unit-suite)

(defun %timing-test-plist (name fn)
  "Build a minimal test-plist consumable by %run-single-test."
  (list :name name
        :fn fn
        :suite 'cl-cc-unit-suite
        :timeout nil
        :depends-on nil
        :tags nil))

(defmacro with-clcc-timings-file-env (value &body body)
  `(%with-preserved-env-var ("CLCC_TIMINGS_FILE")
     (if ,value
         (sb-posix:setenv "CLCC_TIMINGS_FILE" ,value 1)
         (sb-posix:unsetenv "CLCC_TIMINGS_FILE"))
     ,@body))

(deftest-each timing-result-carries-non-negative-duration
  "Every terminal status (pass/fail/skip) carries a non-negative integer :duration-ns."
  :cases (("pass" :pass (lambda () t))
          ("fail" :fail (lambda () (error 'test-failure :message "boom")))
          ("skip" :skip (lambda () (error 'skip-condition :reason "nope"))))
  (expected-status thunk)
  (let* ((plist  (%timing-test-plist 'timing-duration-case thunk))
         (result (%run-single-test plist 1 '()))
         (ns     (getf result :duration-ns)))
    (assert-eq expected-status (getf result :status))
    (assert-true (integerp ns))
    (assert-true (>= ns 0))))

(deftest timing-tap-diagnostic-compact-pass-emits-nothing
  "In compact mode (default), passing tests emit no output."
  (let ((*verbose-tap-mode* :compact))
    (let* ((plist  (%timing-test-plist 'timing-tap-pass (lambda () t)))
           (result (%run-single-test plist 42 '()))
           (output (with-output-to-string (*standard-output*)
                     (%tap-print-result result))))
      (assert-true (zerop (length output))))))

(deftest timing-tap-diagnostic-verbose-pass-contains-duration-ms
  "In verbose TAP mode, a passing test emits duration_ms: in its YAML diagnostic."
  (let ((*verbose-tap-mode* :verbose))
    (let* ((plist  (%timing-test-plist 'timing-tap-pass (lambda () t)))
           (result (%run-single-test plist 42 '()))
           (output (with-output-to-string (*standard-output*)
                     (%tap-print-result result))))
      (assert-string-contains-all output '("duration_ms:" "ok 42 - TIMING-TAP-PASS")))))

(deftest timing-tap-diagnostic-fail-preserves-existing-yaml
  "Failure results still carry their failure YAML and gain duration_ms."
  (let ((*verbose-tap-mode* :compact))
    (let* ((plist (%timing-test-plist
                   'timing-tap-fail
                   (lambda () (error 'test-failure
                                     :message (format nil "  ---~%  message: \"x\"~%  ...")))))
           (result (%run-single-test plist 7 '()))
           (output (with-output-to-string (*standard-output*)
                     (%tap-print-result result))))
      (assert-string-contains-all output '("not ok - " "message:" "duration_ms:")))))

;;; ── %tap-verbose-first-line (T-V) ──────────────────────────────────────────

(deftest-each tap-verbose-first-line-all-statuses
  "%tap-verbose-first-line builds the correct TAP first-line for each status."
  :cases (("pass"    :pass    "ok"     nil   "ok 1 - MY-TEST")
          ("fail"    :fail    "not ok" nil   "not ok 2 - MY-TEST")
          ("skip"    :skip    "ok"     "why" "ok 3 - MY-TEST # SKIP why")
          ("pending" :pending "not ok" "tbd" "not ok 4 - MY-TEST # TODO tbd"))
  (status _prefix detail expected)
  (declare (ignore _prefix))
  (let ((num (ecase status (:pass 1) (:fail 2) (:skip 3) (:pending 4))))
    (assert-string= expected
                    (%tap-verbose-first-line num 'my-test status detail))))

(deftest tap-verbose-first-line-skip-without-detail-falls-back-to-empty
  "%tap-verbose-first-line with NIL detail for :skip uses empty string."
  (assert-string= "ok 5 - TEST # SKIP "
                  (%tap-verbose-first-line 5 'test :skip nil)))

(deftest timing-tsv-row-has-five-tab-separated-columns
  "%write-timings-tsv produces rows with exactly 5 tab-separated columns in
the frozen order suite\\ttest-name\\tduration-ns\\tstatus\\tbatch-id."
  (let ((results (list (list :name 'some-test
                             :suite 'cl-cc-unit-suite
                             :status :pass
                             :duration-ns 123456
                             :batch-id 3)
                       (list :name 'other-test
                             :suite 'cl-cc-unit-suite
                             :status :fail
                             :duration-ns 987
                             :batch-id nil))))
    (uiop:with-temporary-file (:pathname tmp :type "tsv")
      (%write-timings-tsv results tmp)
      (destructuring-bind (first-line second-line)
          (%read-file-lines tmp)
        ;; Split on tab - the separator is #\Tab (character 9).
        (let ((cols1 (uiop:split-string first-line :separator (list #\Tab)))
              (cols2 (uiop:split-string second-line :separator (list #\Tab))))
          (assert-= 5 (length cols1))
          (assert-= 5 (length cols2))
          (assert-string= "CL-CC-UNIT-SUITE" (first cols1))
          (assert-string= "SOME-TEST" (second cols1))
          (assert-string= "123456" (third cols1))
          (assert-string= "passed" (fourth cols1))
          (assert-string= "3" (fifth cols1))
          ;; Sequential row: batch-id absent -> "-"
          (assert-string= "failed" (fourth cols2))
          (assert-string= "-" (fifth cols2)))))))

(deftest timing-output-path-respects-env-override
  "CLCC_TIMINGS_FILE overrides the default ./test-timings.tsv path."
  ;; %timings-output-path rejects absolute paths that don't canonicalize under
  ;; cwd (security guard against arbitrary filesystem writes). Use a cwd-relative
  ;; path so the override is accepted verbatim. The "for-test" suffix avoids
  ;; collision with real test-suite artifacts.
  (with-clcc-timings-file-env "./cl-cc-timings-override-for-test.tsv"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (%timings-output-path))))
      (assert-string= "./cl-cc-timings-override-for-test.tsv" result))))

(deftest timing-output-path-defaults-when-env-unset
  "Without the env var %timings-output-path falls back to the repo-relative default."
  (with-clcc-timings-file-env nil
    (assert-string= "./test-timings.tsv" (%timings-output-path))))

(deftest timing-output-path-rejects-unsafe-absolute-path
  "CLCC_TIMINGS_FILE pointing at /etc/passwd is rejected in favor of default."
  (with-clcc-timings-file-env "/etc/passwd"
    ;; Suppress the warning from %timings-output-path — the assertion is that
    ;; it falls back to the default, not the warning channel.
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (%timings-output-path))))
      (assert-string= "./test-timings.tsv" result))))

(deftest timing-pending-result-carries-duration
  "A test that signals pending-condition still reports :duration-ns."
  (let* ((plist (%timing-test-plist
                 'timing-pending-case
                 (lambda () (error 'pending-condition :reason "soon"))))
         (result (%run-single-test plist 1 '()))
         (ns (getf result :duration-ns)))
    (assert-eq :pending (getf result :status))
    (assert-true (integerp ns))
    (assert-true (>= ns 0))))

(deftest timing-tsv-sanitizes-tab-in-suite-or-name
  "Tab/Newline chars in suite or test name are replaced so each row keeps 5 columns."
  (let* ((bad-name (intern (concatenate 'string "BAD" (string #\Tab) "NAME")
                           :cl-cc/test))
         (bad-suite (intern (concatenate 'string "BAD" (string #\Newline) "SUITE")
                            :cl-cc/test))
         (results (list (list :name bad-name
                              :suite bad-suite
                              :status :pass
                              :duration-ns 1
                              :batch-id nil))))
    (uiop:with-temporary-file (:pathname tmp :type "tsv")
      (%write-timings-tsv results tmp)
      (let* ((line (first (%read-file-lines tmp)))
             (cols (uiop:split-string line :separator (list #\Tab))))
        (assert-= 5 (length cols))
        ;; Sanitizer replaces the tab with a space - exactly one column each.
        (assert-string-contains-all (second cols) '("BAD NAME"))
        (assert-string-contains-all (first cols) '("BAD SUITE"))))))

(deftest-each timing-status-keyword-mapping
  "%status-keyword-to-string maps every framework-emitted status to its frozen TSV token."
  :cases (("pass"      "passed"    :pass)
          ("fail"      "failed"    :fail)
          ("skip"      "skipped"   :skip)
          ("pending"   "pending"   :pending)
          ("errored"   "errored"   :errored)
          ("timed-out" "timed-out" :timed-out))
  (expected kw)
  (assert-string= expected (%status-keyword-to-string kw)))

(deftest timing-load-prior-timings-keeps-maximum-duration-per-test
  "%load-prior-timings keeps the max duration when the TSV contains repeated test names."
  (uiop:with-temporary-file (:pathname tmp :type "tsv")
    (%write-file-lines
     tmp
     (list (format nil "SUITE~CFOO~C10~Cpassed~C-" #\Tab #\Tab #\Tab #\Tab)
           (format nil "SUITE~CFOO~C25~Cpassed~C-" #\Tab #\Tab #\Tab #\Tab)
           (format nil "SUITE~CBAR~C7~Cfailed~C-" #\Tab #\Tab #\Tab #\Tab)))
    (let ((timings (%load-prior-timings tmp)))
      (assert-= 25 (gethash "FOO" timings))
      (assert-= 7 (gethash "BAR" timings)))))

(deftest timing-load-prior-timings-ignores-malformed-rows
  "%load-prior-timings skips rows that do not have a valid test name and duration."
  (uiop:with-temporary-file (:pathname tmp :type "tsv")
    (%write-file-lines
     tmp
     (list
      (format nil "SUITE~CFOO~C10~Cpassed~C-" #\Tab #\Tab #\Tab #\Tab)
      (format nil "SUITE~CFOO~Cnot-an-integer~Cpassed~C-" #\Tab #\Tab #\Tab #\Tab)
      (format nil "SUITE~C~C12~Cpassed~C-" #\Tab #\Tab #\Tab #\Tab)
      "BROKEN\tROW"
      (format nil "SUITE~CBAR~C7~Cfailed~C-" #\Tab #\Tab #\Tab #\Tab)))
    (let ((timings (%load-prior-timings tmp)))
      (assert-= 10 (gethash "FOO" timings))
      (assert-= 7 (gethash "BAR" timings))
      (assert-null (gethash "" timings)))))

(deftest timing-set-test-timeouts-by-prefix-updates-only-matching-tests
  "set-test-timeouts-by-prefix! only rewrites matching registered tests."
  (with-fresh-registry-state
    (with-test-registry-entry ('foo-match :suite 'cl-cc-unit-suite :timeout nil)
      (with-test-registry-entry ('bar-miss :suite 'cl-cc-unit-suite :timeout nil)
        (set-test-timeouts-by-prefix! "FOO-" 17)
        (assert-= 17 (getf (persist-lookup *test-registry* 'foo-match) :timeout))
        (assert-null (getf (persist-lookup *test-registry* 'bar-miss) :timeout))))))

(deftest timing-bulk-timeout-helpers-preserve-explicit-timeouts-by-default
  "Bulk timeout helpers do not overwrite explicit per-test timeouts unless asked."
  (let ((suite 'timing-preserve-suite))
    (with-fresh-registry-state
      (with-suite-registry-entry (suite
                                   :description "preserve"
                                   :parent nil
                                   :parallel t)
        (with-test-registry-entry ('foo-keep :suite suite :timeout 9)
        (with-test-registry-entry ('foo-fill :suite suite :timeout nil)
            (set-test-timeouts-by-prefix! "FOO-" 17)
            (set-suite-test-timeout! suite 23 :recursive t)
            (assert-= 9 (getf (persist-lookup *test-registry* 'foo-keep) :timeout))
            (assert-= 17 (getf (persist-lookup *test-registry* 'foo-fill) :timeout))))))))

(deftest timing-set-suite-test-timeout-can-update-descendants
  "set-suite-test-timeout! optionally updates descendant suite tests too."
  (let ((parent 'timing-timeout-parent)
        (child 'timing-timeout-child))
    (with-fresh-registry-state
      (with-suite-registry-entry (parent
                                   :description "parent"
                                   :parent nil
                                   :parallel t)
      (with-suite-registry-entry (child
                                     :description "child"
                                     :parent parent
                                     :parallel t)
          (with-test-registry-entry ('parent-test :suite parent :timeout nil)
            (with-test-registry-entry ('child-test :suite child :timeout nil)
              (set-suite-test-timeout! parent 23 :recursive t)
              (assert-= 23 (getf (persist-lookup *test-registry* 'parent-test) :timeout))
              (assert-= 23 (getf (persist-lookup *test-registry* 'child-test) :timeout))))))))

(deftest timing-print-result-summary-reports-failures-and-skips
  "%print-result-summary prints aggregated counts and the failed test list."
  (let* ((*verbose-tap-mode* :compact)
         (results (list (list :name 'alpha :status :pass :number 1)
                        (list :name 'beta :status :skip :number 2)
                        (list :name 'gamma :status :fail :number 3)))
         (output (with-output-to-string (*standard-output*)
                    (assert-true (%print-result-summary results)))))
    (assert-string-contains-all output '("1 passed" "1 failed" "1 skipped"))))

;;; ── %detail-ends-with-yaml-terminator-p (T-Y) ──────────────────────────────

(deftest-each detail-ends-with-yaml-terminator-p-cases
  "%detail-ends-with-yaml-terminator-p detects trailing '  ...' correctly."
  :cases (("ends-with-terminator"    "  ---\n  message: x\n  ..."  t)
          ("no-terminator"           "  ---\n  message: x"          nil)
          ("too-short"               "..."                           nil)
          ("nil-input"               nil                             nil)
          ("exact-terminator-only"   "  ..."                        t))
  (detail expected)
  (assert-eq (and expected t)
             (%detail-ends-with-yaml-terminator-p detail)))

;;; ── %duration-ms-from-result (T-D) ─────────────────────────────────────────

(deftest-each duration-ms-from-result-cases
  "%duration-ms-from-result converts :duration-ns to ms or returns 0.0d0 for missing/invalid values."
  :cases (("positive-ns"   1000000   1.0d0)
          ("zero-ns"       0         0.0d0)
          ("nil-ns"        nil       0.0d0))
  (ns expected)
  (let ((result (list :duration-ns ns)))
    (assert-equal expected (%duration-ms-from-result result))))

;;; ── %source-file-display (T-S) ──────────────────────────────────────────────

(deftest source-file-display-nil-returns-nil
  "%source-file-display returns NIL for a NIL path."
  (assert-null (%source-file-display nil)))

(deftest source-file-display-relative-path-stays-relative
  "%source-file-display strips the cwd prefix when the path starts with cwd."
  (let* ((cwd     (namestring (uiop:getcwd)))
         (abs-path (concatenate 'string cwd "tests/foo.lisp")))
    (assert-string= "tests/foo.lisp" (%source-file-display abs-path))))

(deftest source-file-display-unrelated-path-returned-unchanged
  "%source-file-display returns the original namestring for paths outside cwd."
  (let ((path (namestring (merge-pathnames "unrelated.lisp" (uiop:temporary-directory)))))
    (assert-string= path (%source-file-display path)))))
