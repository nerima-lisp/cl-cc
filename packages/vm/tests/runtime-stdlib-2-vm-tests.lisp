;;;; packages/vm/tests/runtime-stdlib-2-vm-tests.lisp
;;;; FR-787–FR-931: Symbol-existence smoke tests for runtime-stdlib-2 features.
;;;;
;;;; Each test asserts that the listed symbol is fboundp / boundp / a loadable package.
;;;; Groups are by check type; FR labels are encoded in the case label strings.

(in-package :cl-cc/test)



;;; ─── Multi-symbol FR groups (cannot collapse to single-entry rows) ─────────

(it-sequential "fr-804-syntax-rules-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/expand::define-syntax) :to-be-truthy)
  (expect (fboundp 'cl-cc/expand::with-gensyms) :to-be-truthy))

;;; ─── Package existence ───────────────────────────────────────────────────────

(it-sequential "fr-package-exists fr-796-lsp"
  (destructuring-bind (pkg) (list :cl-cc/tools/lsp)
    (expect (find-package pkg) :to-be-truthy)))

(it-sequential "fr-package-exists fr-797-dap"
  (destructuring-bind (pkg) (list :cl-cc/tools/dap)
    (expect (find-package pkg) :to-be-truthy)))

(it-sequential "fr-package-exists fr-908-expand"
  (destructuring-bind (pkg) (list :cl-cc/expand)
    (expect (find-package pkg) :to-be-truthy)))

;;; ─── Single-function fboundp checks ─────────────────────────────────────────

(it-sequential "fr-function-exists fr-805-once-only"
  (destructuring-bind (sym) (list 'cl-cc/expand::once-only)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-808-shebang"
  (destructuring-bind (sym) (list 'cl-cc/cli::parse-cli-args)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-809-argv"
  (destructuring-bind (sym) (list 'cl-cc/cli::cl-cc-argv)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-839-iterator"
  (destructuring-bind (sym) (list 'cl-cc/expand::make-iterator)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-842-kahan"
  (destructuring-bind (sym) (list 'cl-cc/vm::kahan-sum)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-856-delay"
  (destructuring-bind (sym) (list 'cl-cc/expand::delay)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-857-memoize"
  (destructuring-bind (sym) (list 'cl-cc/expand::memoize)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-880-custom-hash"
  (destructuring-bind (sym) (list 'cl-cc/vm::define-hash-table-test)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-895-symbol-table"
  (destructuring-bind (sym) (list 'cl-cc/vm::freeze-symbol-table)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-905-tco-unwind"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-check-dynamic-extent)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-911-riscv"
  (destructuring-bind (sym) (list 'cl-cc/emit::riscv64-emit-function)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-914-delimited-cont"
  (destructuring-bind (sym) (list 'cl-cc/vm::call-with-continuation-prompt)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-917-reproducible"
  (destructuring-bind (sym) (list 'cl-cc/cli::cl-cc-deterministic-build-p)
    (expect (fboundp sym) :to-be-truthy)))

;;; ─── Single-variable boundp checks ──────────────────────────────────────────

(it-sequential "fr-variable-exists fr-920-forward-ref"
  (destructuring-bind (sym) (list 'cl-cc/compile::*forward-reference-patch-table*)
    (expect (boundp sym) :to-be-truthy)))
