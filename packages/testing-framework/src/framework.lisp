;;;; tests/framework.lisp — canonical suite hierarchy
;;;;
;;;; TAP output, timeouts, parallel worker pool, and coverage/discovery
;;;; machinery formerly here are gone: cl-weave's own RUN-ALL reporters
;;;; (:spec/:json/:tap/:github/:junit) and concurrency model replace them.

(in-package :cl-cc/test)

(defsuite cl-cc-suite :description "CL-CC test root suite")

(defsuite cl-cc-unit-suite
  :description "Unit tests"
  :parent cl-cc-suite)

(defsuite cl-cc-integration-suite
  :description "Full-pipeline integration tests"
  :parent cl-cc-suite)

(defsuite cl-cc-integration-serial-suite
  :description "Sequential-only integration tests"
  :parent cl-cc-integration-suite
  :parallel nil)

(defsuite cl-cc-e2e-suite
  :description "End-to-end tests"
  :parent cl-cc-suite)

(defsuite cl-cc-serial-suite
  :description "Sequential-only unit tests"
  :parent cl-cc-unit-suite
  :parallel nil)

(defsuite cl-cc-conformance-suite
  :description "ANSI CL conformance tests (expected-fail for known gaps)"
  :parent cl-cc-suite
  :parallel nil)

(defsuite cl-cc-documentation-suite
  :description "Documentation, roadmap, and implementation evidence checks"
  :parent cl-cc-suite
  :parallel nil)

(in-suite cl-cc-suite)
