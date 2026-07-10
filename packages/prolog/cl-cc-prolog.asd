;;;; cl-cc-prolog.asd — independent ASDF system for the Prolog engine
;;;;
;;;; Files live in the :cl-cc/prolog package. Consumers import the public
;;;; Prolog API from :cl-cc/prolog directly instead of the umbrella :cl-cc
;;;; package.

(asdf:defsystem :cl-cc-prolog
  :description "cl-cc Prolog engine — terms, unification, solver, DCG, peephole rules"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "prolog-builtin-data")
   (:file "prolog-rule-specs")
   (:file "peephole-data")
    (:file "prolog-unification")
    (:file "prolog-rules")
    (:file "prolog-rule-bootstrap")
    (:file "prolog-builtins-eval")
    (:file "prolog-builtins")
    (:file "prolog-solver")
    (:file "dcg-rules")
   (:file "dcg-builtins")
   (:file "dcg-query")))
