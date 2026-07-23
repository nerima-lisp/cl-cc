;;;; cl-cc-prolog-tools.asd — Prolog-based call-graph analysis tools
;;;;
;;;; Built on the external `cl-prolog` engine (github:nerima-lisp/cl-prolog),
;;;; not on cl-cc's own homegrown Prolog engine in packages/prolog
;;;; (:cl-cc-prolog, used internally for peephole-optimization rules). The
;;;; two are unrelated: this package asserts facts about a program's AST
;;;; into a cl-prolog rulebase and queries them for call-graph analyses
;;;; (reachability, dead-code, mutual recursion, FD-constraint graph
;;;; coloring, and a DCG-based grammar for a small textual edge notation).
;;;;
;;;; Leaf system at the cl-cc-ast tier: depends only on :cl-cc-ast and the
;;;; external :cl-prolog system. Its test system additionally depends on the
;;;; external :cl-weave testing framework and is intentionally NOT folded
;;;; into the umbrella cl-cc-test.asd aggregate, since that aggregate is
;;;; driven by cl-cc's own testing-framework runner rather than
;;;; cl-weave:run-all. Run this package's tests independently via
;;;; `(asdf:test-system :cl-cc-prolog-tools)`.

(asdf:defsystem :cl-cc-prolog-tools
  :description "Prolog-based call-graph analysis tools for cl-cc, built on the external cl-prolog engine"
  :author "takeokunn"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-prolog)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "call-graph")
   (:file "edge-dcg")
   (:file "graph-coloring"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-cc-prolog-tools/tests))))

(asdf:defsystem :cl-cc-prolog-tools/tests
  :description "cl-weave test suite for cl-cc-prolog-tools"
  :author "takeokunn"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-prolog-tools :cl-prolog :cl-weave)
  :pathname "tests"
  :serial t
  :components
  ((:file "package")
   (:file "call-graph-tests")
   (:file "edge-dcg-tests")
   (:file "graph-coloring-tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :cl-weave :run-all :reporter :spec)
               (error "cl-cc-prolog-tools test suite failed."))))
