;;;; cl-cc-testing-framework.asd
;;;; Extracted from tests/framework/ as part of the packages/ reorganization.
;;;; Canonical ASDF system for the CL-CC testing framework.

(asdf:defsystem :cl-cc-testing-framework
  :description "CL-CC testing framework — deftest/assert-*/fixture compatibility
macros preserving the original call surface, backed by the external cl-weave
engine (registration, execution, reporting, parallelism all delegate to
cl-weave; see framework-definitions.lisp for how DEFTEST/DEFSUITE/IN-SUITE
map onto cl-weave's suite tree without requiring lexical restructuring of
every test file)."
  :author "takeokunn"
  :license "MIT"
  :version "0.2.0"
  :depends-on (:cl-cc :cl-weave)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "package-imports-compile-parse")
   (:file "package-imports-core")
   (:file "package-imports-backend")
   (:file "package-exports")
   (:file "framework-conditions-state")
   (:file "framework-definitions")
   (:file "framework")
   (:file "framework-fixtures")
   (:file "framework-assertions")
   (:file "framework-advanced")
   (:file "framework-compiler-run-string")
   (:file "framework-compiler")
   (:file "framework-pbt")
   (:file "framework-meta")
   (:file "framework-fuzz")))
