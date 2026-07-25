;;;; tests/pbt/suite.lisp - Property-based testing suite definition
;;;
;;; Previously lived at the end of framework-dsl.lisp, alongside the home-grown
;;; property DSL. It outlived that file: MACRO-PBT-SUITE (macro-pbt-tests.lisp)
;;; names CL-CC-PBT-SUITE as its :parent, and every property test file under
;;; pbt/ declares (in-suite cl-cc-pbt-suite) explicitly, so this has to be
;;; defined before any of them load.

(in-package :cl-cc/pbt)

(defsuite cl-cc-pbt-suite
  :description "Property-Based Testing suite for CL-CC"
  :parent cl-cc-integration-suite)
