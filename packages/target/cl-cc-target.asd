;;;; cl-cc-target.asd — skeleton for the target feature package
;;;;
;;;; Phase 1 of the ideal package-by-feature redesign (post 2026-05-01 plan).
;;;; This system is intentionally empty — files will migrate here in
;;;; subsequent phases. Currently NOT wired into root cl-cc.asd, so loading
;;;; this system as part of `:cl-cc` is a no-op.

(asdf:defsystem :cl-cc-target
  :description "Target descriptors (target-desc API; absorbs calling-convention.lisp callers in Phase 3)"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "target")))
