;;; runtime-stdlib-2-completion-tests.lisp — Tests for runtime-stdlib-2 gaps

(in-package :cl-cc/test)



;; ── FR-895: Symbol Table Freeze/Thaw (implementation in vm.lisp) ───────
(it-sequential "runtime-stdlib-2-symbol-table-freeze-thaw-surface"
  (expect (fboundp 'cl-cc/vm::freeze-symbol-table) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::thaw-symbol-table) :to-be-truthy))

;; ── FR-896: Package Lock (implementation in vm.lisp) ───────────────────
;; NOTE: lock-package and package-locked-p are defined in vm.lisp

;; ── FR-917: Reproducible Build ─────────────────────────────────────────
(it-sequential "runtime-stdlib-2-reproducible-build-surface"
  (expect (fboundp 'cl-cc/vm::build-fingerprint) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::source-date-epoch) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::build-timestamp) :to-be-truthy))

;; ── FR-920: Forward References ─────────────────────────────────────────
(it-sequential "runtime-stdlib-2-forward-reference-surface"
  (expect (fboundp 'cl-cc/vm::vm-declare-forward-reference) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::vm-resolve-forward-references) :to-be-truthy))

;; ── FR-820: Print-Circle ───────────────────────────────────────────────
(it-sequential "runtime-stdlib-2-print-circle-surface"
  (expect (find-symbol "*PRINT-CIRCLE*" :cl-cc/vm) :to-be-truthy))
