(asdf:defsystem :cl-cc-formatter
  :description "FR-320: Minimal Lisp code formatter for CL-CC."
  :author "takeokunn"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :components ((:module "src" :components ((:file "formatter")))))
