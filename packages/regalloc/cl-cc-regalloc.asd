;;;; cl-cc-regalloc.asd — register allocation feature package
;;;;
;;;; This system owns the current regalloc package, defs/uses analysis, and
;;;; allocation entry points. Keep tests aligned with these production files
;;;; rather than duplicate source fragments in package-local test harnesses.

(asdf:defsystem :cl-cc-regalloc
  :description "Register allocation passes (linear scan, spilling, live-range)"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc-mir :cl-cc-target :cl-cc-optimize)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "regalloc")
   (:file "regalloc-defs-uses")
   (:file "regalloc-allocate")))
