;;;; ---------------------------- WARNING ----------------------------
;;;; This system is DOUBLE-DEFINED.
;;;;
;;;; The name "cl-cc-ast" is defined both here and in the standalone
;;;; repository nerima-lisp/cl-cc-ast, and the two definitions do not
;;;; agree. Which one ASDF resolves depends on the order of the source
;;;; registry, so the dependency graph of this repository is not currently
;;;; well defined. flake.nix takes the standalone repository as an input;
;;;; a plain `sbcl --load cl-cc.asd` from a checkout takes this file.
;;;;
;;;; Measured 2026-07-26 against the standalone repository's working tree
;;;; (cl-cc-ast/src vs packages/ast/src):
;;;;   5 source files here, 5 there
;;;;   0 identical, 5 differing
;;;;   0 only here, 1 only there
;;;;   :depends-on — identical (both empty)
;;;;
;;;; WHICH DEFINITION IS AUTHORITATIVE IS UNDECIDED. Do not assume an edit
;;;; here reaches the build, and do not "fix" the divergence by copying one
;;;; side over the other; the two have diverged in both directions. Resolving
;;;; this is a design decision, tracked separately from the packaging
;;;; migration that added this banner.
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
