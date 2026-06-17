;;;; entrypoint-contract-tests.lisp — flake entrypoint contract checks

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %flake-text ()
  (uiop:read-file-string (merge-pathnames #P"nix/apps.nix" (uiop:getcwd))))

(defun %checks-text ()
  (uiop:read-file-string (merge-pathnames #P"nix/checks.nix" (uiop:getcwd))))

(deftest flake-test-app-runs-canonical-suite
  "The single test app dispatches run-tests (canonical fast plan) and exports CLCC timeout vars."
  (let ((text (%flake-text)))
    (assert-string-contains-all
     text
     '("test = mkSbclScript {"
       "CLCC_TEST_TIMEOUT"
       "CLCC_SUITE_TIMEOUT"
       "CL_CC_TEST_WORKERS"
       "CLCC_TEST_TIMEOUT:-10"
       "CLCC_SUITE_TIMEOUT:-600"
       "case \"$CLCC_TEST_TIMEOUT\" in"
       "case \"$CLCC_SUITE_TIMEOUT\" in"
       "*[!0-9]*"
       "CLCC_TEST_TIMEOUT=10"
       "CLCC_SUITE_TIMEOUT=600"
       "(<= workers 1)"
       "run-tests"
       "starting fast test plan"
       "--kill-after=30"))))

(deftest flake-coverage-app-runs-instrumented-suite
  "The coverage app enables sb-cover before force-loading local test systems."
  (let ((text (%flake-text)))
    (assert-string-contains-all
     text
     '("coverage = mkSbclScript"
       "sb-cover:store-coverage-data"
       "initialize-source-registry"
       "initialize-output-translations"
       ":coverage t"))))

(deftest flake-deprecated-smoke-apps-removed
  "test-full / perf-smoke / stability-smoke entrypoints must be removed."
  (let ((text (%flake-text)))
    (assert-string-contains-none
     text
     '("test-full = mkSbclScript"
       "perf-smoke ="
       "stability-smoke ="
       "run-fast-tests"))))

(deftest flake-checks-tests-mirrors-test-app
  "checks.tests must delegate to the test app, ensuring CI matches `nix run .#test`."
  (let ((text (%checks-text)))
    (assert-string-contains-all text '("apps.test.program"))
    (assert-string-contains-none text '("run-fast-tests" "cl-cc-test/clos"))))
