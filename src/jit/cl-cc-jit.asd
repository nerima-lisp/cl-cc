;;;; cl-cc-jit.asd — JIT compilation subsystem
;;;; Phases 104-105: Runtime JIT infrastructure
(asdf:defsystem "cl-cc-jit"
  :description "JIT compilation subsystem: stack maps, safepoints, write barriers, call stubs, code cache, trace JIT"
  :version "0.1.0"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :depends-on ("sb-posix" "cl-cc-runtime" "cl-cc-vm" "cl-cc-codegen" "cl-cc-compile")
  :serial t
  :components ((:file "package")
    (:file "config")
    (:file "stack-map")
    (:file "safepoints")
    (:file "write-barrier")
    (:file "baseline")
    (:file "call-stubs")
    (:file "cache")
    (:file "trace-jit")))

(asdf:defsystem "cl-cc-jit/tests"
  :description "Tests for the JIT compilation subsystem"
  :depends-on ("cl-cc-jit" "cl-cc-testing-framework")
  :pathname "tests"
  :serial t
  :components ((:file "write-barrier-tests"))
  :perform (asdf:test-op
    (operation component)
    (declare (ignore operation component))
    (unless (uiop:symbol-call :cl-weave :run-all :reporter :spec :pass-with-no-tests nil) (error "cl-cc-jit test suite failed."))))
