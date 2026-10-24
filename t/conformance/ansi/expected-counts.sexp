; Missing count rows make the first Linux CI run fail; unlisted test failures
; also fail validation until observed cases are recorded.
; Before merging this harness, record one eval row and one compile row for each
; of the 24 chapters.
; CI runs these checks in the "nix flake check" job with --print-build-logs.
; On checker failure, the build log includes checker.log and runner.log, plus the
; suite transcript when present. During baseline collection, missing count rows
; and unlisted failures are expected and do not invalidate a run. Use a row only
; if its ANSI-RESULT chapter/mode matches this derivation, the checker reports no
; runner-exit, impossible-count, or test-marker integrity error, and executed is
; nonzero. The checker verifies loaded = executed + filtered + inactive and
; executed = pass + fail; counts of ANSI-PASSED-TEST, ANSI-FAILED-TEST, and
; ANSI-FILTERED-TEST markers must match pass, fail, and filtered respectively.
; Marker names must be unique within a category and disjoint across categories.
; Copy loaded, executed, filtered, and inactive from the matching ANSI-RESULT.
; Row shape: (:chapter "..." :mode "..." :loaded "..." :executed "..."
;             :filtered "..." :inactive "...")
; :chapter is a test directory name; :mode is "eval" or "compile".
; Count values are decimal strings; checks run only on x86_64-linux.
; :inactive counts tests skipped by a disabled note attached to a test.
; A missing or mismatched tuple fails its chapter/mode check.
()
