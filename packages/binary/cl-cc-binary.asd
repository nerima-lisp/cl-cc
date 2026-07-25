;;;; ---------------------------- WARNING ----------------------------
;;;; This system is DOUBLE-DEFINED.
;;;;
;;;; The name "cl-cc-binary" is defined both here and in the standalone
;;;; repository nerima-lisp/cl-cc-binary, and the two definitions do not
;;;; agree. Which one ASDF resolves depends on the order of the source
;;;; registry, so the dependency graph of this repository is not currently
;;;; well defined. flake.nix does NOT list this system's standalone repository
;;;; among its inputs, so the Nix build currently resolves to THIS file — but
;;;; nothing enforces that, and any checkout with the standalone repository on
;;;; its source registry may resolve the other way.
;;;;
;;;; Measured 2026-07-26 against the standalone repository's working tree
;;;; (cl-cc-binary/src vs packages/binary/src):
;;;;   18 source files here, 24 there
;;;;   9 identical, 7 differing
;;;;   2 only here, 8 only there
;;;;   :depends-on — DIFFERENT: this one has none, the standalone repo depends on "cl-log-kit"
;;;;
;;;; WHICH DEFINITION IS AUTHORITATIVE IS UNDECIDED. Do not assume an edit
;;;; here reaches the build, and do not "fix" the divergence by copying one
;;;; side over the other; the two have diverged in both directions. Resolving
;;;; this is a design decision, tracked separately from the packaging
;;;; migration that added this banner.
;;;; -----------------------------------------------------------------

;;;; cl-cc-binary.asd — independent ASDF system for binary-format emitters
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/binary package and are accessed by callers via the qualified
;;;; cl-cc/binary: prefix. Truly leaf — no dependencies on other cl-cc systems.

(asdf:defsystem :cl-cc-binary
  :description "cl-cc binary-format emitters — Mach-O, ELF, WebAssembly module bytes"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "macho")
   (:file "macho-buffer")
   (:file "macho-fat")
   (:file "macho-serialize")
    (:file "macho-build")
     (:file "elf")
      (:file "icf")
      (:file "got-plt")
      (:file "dwo")
      (:file "patchable-entry")
       (:file "dwarf")
       (:file "dwarf-eh")
       (:file "elf-serialize")
       (:file "elf-emit")
       (:file "dwarf-dwo")
      (:file "pe")
     (:file "wasm")))
