;;;; js-suite.lisp — JavaScript frontend test suite root

(cl:in-package :cl-cc/test)

(defsuite cl-cc-javascript-suite
  :description "JavaScript frontend tests"
  :parent cl-cc-unit-suite)

(in-suite cl-cc-javascript-suite)
