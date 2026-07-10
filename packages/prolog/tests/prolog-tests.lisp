;;;; tests/integration/prolog-tests.lisp — Prolog integration tests

(in-package :cl-cc/test)

;; These tests run under the serial integration tree.
(defsuite cl-cc-prolog-integration-suite
  :description "Serial Prolog integration tests with isolated rule DB state"
  :parent cl-cc-integration-serial-suite)

(in-suite cl-cc-prolog-integration-suite)
