;;;; ---------------------------- WARNING ----------------------------
;;;; This system is DOUBLE-DEFINED, and THIS FILE is the one that builds.
;;;;
;;;; The name "cl-cc-type" is also defined in the standalone repository
;;;; nerima-lisp/cl-cc-type, which flake.nix takes as an input and
;;;; nix/asdf-systems.nix injects into the internal system graph. The earlier
;;;; version of this banner said that made the standalone repository win under
;;;; Nix and this file win under a bare `sbcl --load cl-cc.asd`, and left which
;;;; one is authoritative undecided.
;;;;
;;;; Measured 2026-07-26: BOTH resolve to this file. `ensure-system-asd` in
;;;; cl-cc.asd only loads a per-package .asd when FIND-SYSTEM misses, and the
;;;; cl-cc source tree is scanned ahead of the injected derivations, so the
;;;; in-tree definition is found first in either environment. The standalone
;;;; repository has therefore never been compiled by this build.
;;;;
;;;; The evidence is a test, not a reading of the wiring:
;;;; `infer-with-constraints` is defined only in packages/type/src (it was
;;;; deleted from the standalone repository by its module reorganisation, the
;;;; very commit flake.nix pins). The test "infer of a self-applying lambda
;;;; resolves the constraint and returns a fixnum result type" calls it
;;;; unconditionally and PASSES in CI.
;;;;
;;;; Reproducible locally: add the sibling checkouts to the source registry
;;;; alongside this tree and the suite still passes 12192/12192, because these
;;;; files are still what gets compiled. Delete them and the two failures below
;;;; appear.
;;;;
;;;; So this file is authoritative and the standalone repositories are the ones
;;;; that drifted. Building against them today costs exactly two failures,
;;;; measured by pointing the source registry at the sibling checkouts:
;;;;   - cl-cc-type is missing `infer-with-constraints` (an 8-line wrapper over
;;;;     collect-constraints / solve-constraints / zonk).
;;;;   - cl-cc-ast disables no-escape instance scalarization: the standard
;;;;     metaclass case of "An instance of a class with a custom metaclass
;;;;     keeps its allocation and slot reads" stops scalarizing, so VM-SLOT-READ
;;;;     survives where it should not. Suspect the closure.lisp rewrite that
;;;;     merged the AST-LAMBDA and AST-DEFUN clauses into AST-CALLABLE.
;;;;
;;;; Do not delete this file expecting the standalone repository to take over
;;;; silently; it will, and the build will regress. Close those two gaps, pin
;;;; the fixed commits in flake.nix, and confirm the switch actually happened
;;;; before removing anything.
;;;; -----------------------------------------------------------------

;;;; cl-cc-type.asd — independent ASDF system for the type system
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/type package (kind, multiplicity, types, inference, checker, etc.).
;;;; Depends on :cl-cc-ast for AST node types referenced during constraint
;;;; collection and type inference (solver-collect, inference, inference-forms,
;;;; inference-effects).

(asdf:defsystem :cl-cc-type
  :description "cl-cc type system — kinds, multiplicity, HM inference, type classes, effects"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on (:cl-cc-ast)
  :pathname "src"
  :serial t
  :components
   ((:file "package")
    (:file "kind")
    (:file "multiplicity")
     (:file "types-core")
     (:file "types-extended-concurrency")
     (:file "types-extended-units")
     (:file "types-extended-routing-types")
     (:file "types-extended-ffi")
     (:file "types-extended-registries")
     (:file "types-extended-qtt")
     (:file "types-extended-dependent")
     (:file "types-extended-advanced-meta")
     (:file "types-extended-advanced-meta-validators")
     (:file "types-extended-advanced-validators")
     (:file "types-extended-advanced-data")
     (:file "types-extended-advanced-evidence-data")
     (:file "types-extended-advanced-validate")
     (:file "types-extended-advanced-init")
     (:file "types-extended-nodes")
     (:file "types-env")
     (:file "substitution")
   (:file "substitution-schemes")
   (:file "unification")
   (:file "subtyping")
   (:file "effect")
   (:file "row")
   (:file "constraint")
   (:file "parser")
   (:file "parser-extended")
        (:file "parser-typed")
         (:file "typeclass")
         (:file "solver")
   (:file "solver-collect")
   (:file "inference")
   (:file "inference-handlers")
   (:file "inference-forms")
   (:file "inference-forms-advanced")
   (:file "inference-forms-advanced-validators")
   (:file "inference-forms-advanced-init")
   (:file "inference-conditions")
   (:file "inference-effects")
   (:file "bidirectional")
    (:file "checker")
    (:file "printer")
    (:file "printer-unparse")
    (:file "exhaustiveness")
    ;; FR-1602/1701/1702/1803/1804/2202-2206/3303-3305 utility modules
    ;; Keep this order: channels before actors/stm/coroutines/simd.
    (:file "generics")
    (:file "channels")
    (:file "actors")
    (:file "stm")
    (:file "coroutines")
    (:file "simd")
    (:file "routing")
    (:file "utils")))
