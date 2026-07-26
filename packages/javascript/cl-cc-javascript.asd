;;;; ---------------------------- WARNING ----------------------------
;;;; This system is DOUBLE-DEFINED.
;;;;
;;;; The name "cl-cc-javascript" is defined both here and in the standalone
;;;; repository nerima-lisp/cl-cc-javascript, and the two definitions do not
;;;; agree. Which one ASDF resolves depends on the order of the source
;;;; registry, so the dependency graph of this repository is not currently
;;;; well defined. flake.nix does NOT list this system's standalone repository
;;;; among its inputs, so the Nix build currently resolves to THIS file — but
;;;; nothing enforces that, and any checkout with the standalone repository on
;;;; its source registry may resolve the other way.
;;;;
;;;; Measured 2026-07-26 against the standalone repository's working tree
;;;; (cl-cc-javascript/src vs packages/javascript/src):
;;;;   89 source files here, 93 there
;;;;   49 identical, 39 differing
;;;;   1 only here, 6 only there
;;;;   :depends-on — DIFFERENT: the standalone repo also depends on :cl-cc-vm
;;;;
;;;; WHICH DEFINITION IS AUTHORITATIVE IS UNDECIDED. Do not assume an edit
;;;; here reaches the build, and do not "fix" the divergence by copying one
;;;; side over the other; the two have diverged in both directions. Resolving
;;;; this is a design decision, tracked separately from the packaging
;;;; migration that added this banner.
;;;; -----------------------------------------------------------------

;;;; cl-cc-javascript.asd — JavaScript frontend: lexer, parser, runtime helpers

(asdf:defsystem :cl-cc-javascript
  :description "CL-CC JavaScript frontend: lexer, parser, and runtime helpers"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-cc-bootstrap :cl-cc-parse)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "lexer")
   (:file "lexer-operator")
   (:file "lexer-number")
   (:file "lexer-template")
   (:file "lexer-regex")
   (:file "parser")
   (:file "parser-stmt-binding")
   (:file "parser-expr")
   (:file "parser-expr-args")
   (:file "parser-expr-literal")
   (:file "parser-expr-postfix")
   (:file "parser-expr-unary")
   (:file "parser-expr-primary")
   (:file "parser-arrow")
   (:file "parser-stmt")
   (:file "parser-stmt-fn")
   (:file "parser-stmt-control")
   (:file "parser-class-helpers")
   (:file "parser-stmt-flow")
   (:file "parser-stmt-dispatch")
   (:file "parser-class")
   (:file "parser-class-lower")
   (:file "parser-module")
   (:file "parser-module-export")
   (:file "parser-pattern")
   (:file "parser-pattern-lower")
   ;; NOTE: there is intentionally no separate ast-lower pass. The parser lowers
   ;; JS-specific forms inline (e.g. %js-lower-assignment for &&=/||=/??=, %js-this
   ;; emitted directly), matching the PHP frontend's inline-lowering model. The
   ;; former ast-lower.lisp was dead, uncalled, and inconsistent — removed.
   (:file "runtime")
   (:file "runtime-call")
   (:file "runtime-property")
   (:file "runtime-symbol")
   (:file "runtime-control")
   (:file "runtime-array")
   (:file "runtime-array-core")
   (:file "runtime-array-transforms")
   (:file "runtime-array-es2023")
   (:file "runtime-array-iterators")
   (:file "runtime-array-from")
   (:file "runtime-object")
   (:file "runtime-object-ops")
   (:file "runtime-json")
   (:file "runtime-string")
   (:file "runtime-math")
   (:file "runtime-collections")
   (:file "runtime-collections-set")
   (:file "runtime-collections-zip")
   (:file "runtime-collections-iterators")
   (:file "runtime-async")
   (:file "runtime-map")
   (:file "runtime-weak-collections")
   (:file "runtime-date")
   (:file "runtime-date-methods")
   (:file "runtime-regex")
   (:file "runtime-regex-api")
   (:file "runtime-typed-arrays")
   (:file "runtime-typed-arrays-encoding")
   (:file "runtime-typed-arrays-methods")
   (:file "runtime-typed-arrays-methods-es2023")
   (:file "runtime-class")
   (:file "runtime-module")
   (:file "runtime-ops")
   (:file "runtime-ops-encoding")
   (:file "runtime-temporal")
   (:file "runtime-temporal-duration")
   (:file "runtime-temporal-parse")
   (:file "runtime-temporal-global")
   (:file "runtime-builtins")
   (:file "runtime-builtins-globals")
   (:file "runtime-builtins-intl")
   (:file "runtime-builtins-intl-core")
   (:file "runtime-builtins-intl-number-format")
   (:file "runtime-builtins-intl-date-time-format")
   (:file "runtime-builtins-intl-collator")
   (:file "runtime-builtins-intl-list-format")
   (:file "runtime-builtins-intl-plural-rules")
   (:file "runtime-builtins-platform")
   (:file "runtime-builtins-platform-abort")
   (:file "runtime-builtins-platform-url")
   (:file "runtime-builtins-platform-crypto")
   (:file "runtime-builtins-platform-atomics")
   (:file "runtime-builtins-object")
    (:file "runtime-builtins-table-specs")
    (:file "runtime-builtins-table")
    (:file "runtime-builtins-table-globals")
   (:file "runtime-builtins-prelude")
   (:file "runtime-method-resolver")
   (:file "runtime-method-resolver-core")
   (:file "runtime-method-resolver-tables")
   (:file "runtime-method-resolver-dispatch")
   (:file "backend")))
