;;;; cl-cc-jit.asd — JIT compilation subsystem
;;;; Phases 104-105: Runtime JIT infrastructure

(asdf:defsystem "cl-cc-jit"
  :description "JIT compilation subsystem: stack maps, safepoints, write barriers, call stubs, code cache, trace JIT"
  :version "0.1.0"
  :author "cl-cc team"
  :license "MIT"
  :depends-on ("sb-posix" "cl-cc-runtime" "cl-cc-vm" "cl-cc-codegen" "cl-cc-compile")
  :serial t
  :components
  ((:file "package")
   (:file "config")
   (:file "stack-map")
   (:file "safepoints")
   (:file "write-barrier")
   (:file "baseline")
   (:file "call-stubs")
   (:file "cache")
   (:file "trace-jit")))
