;;;; ---------------------------- WARNING ----------------------------
;;;; This system is DOUBLE-DEFINED, and THIS FILE is the one that builds.
;;;;
;;;; The name "cl-cc-ast" is also defined in the standalone repository
;;;; nerima-lisp/cl-cc-ast, which flake.nix takes as an input and
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
;;;; The evidence is a test, not a reading of the wiring. It comes from the
;;;; sibling system cl-cc-type, which is resolved by the same ENSURE-SYSTEM-ASD
;;;; mechanism and therefore settles the order for this one too:
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
;;;;     survives where it should not. DIAGNOSED, and the repository is not the
;;;;     party at fault. It fixes AST-CHILDREN for AST-MAKE-INSTANCE:
;;;;
;;;;       here  (loop for (k v) on initargs by #'cddr collect v)
;;;;       there (mapcar #'cdr initargs)
;;;;
;;;;     INITARGS is an alist, so the in-tree form walks it as a plist and
;;;;     yields (NIL) -- the initarg value expressions are invisible to every
;;;;     generic AST walker. With them visible, some pass between lowering and
;;;;     the let-emit pass rewrites the binding: %LET-NOESCAPE-INSTANCE-SLOTS
;;;;     receives an AST-VAR where it used to receive the AST-MAKE-INSTANCE, so
;;;;     its first gate fails and scalarization is skipped. Traced in-source;
;;;;     the other four gates are identical under both definitions, and raw
;;;;     LOWER-SEXP-TO-AST output is identical too, so the rewrite happens in a
;;;;     pass, not in lowering.
;;;;
;;;;     Closing this means deciding whether that rewrite is correct and the
;;;;     test's expectation should move, or whether the pass has a latent bug
;;;;     that only an accurate AST-CHILDREN exposes. Do not "fix" it by
;;;;     restoring the plist walk: that reinstates a data-layer bug.
;;;;
;;;; Do not delete this file expecting the standalone repository to take over
;;;; silently; it will, and the build will regress. Close those two gaps, pin
;;;; the fixed commits in flake.nix, and confirm the switch actually happened
;;;; before removing anything.
;;;; -----------------------------------------------------------------

;;;; cl-cc-ast.asd — independent ASDF system for the AST node types
;;;;
;;;; Phase 1.2 of the package-by-feature monorepo migration. Files live in
;;;; the :cl-cc/ast package; the facade :cl-cc package (:use :cl-cc/ast) so
;;;; downstream compiler modules see AST symbols unqualified.

(asdf:defsystem :cl-cc-ast
  :description "cl-cc AST node types and protocol (ast-children, ast-bound-names)"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ast")
   (:file "ast-functions")
   (:file "closure")
   (:file "ast-roundtrip")))
