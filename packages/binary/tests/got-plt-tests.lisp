;;;; packages/binary/tests/got-plt-tests.lisp — GOT/PLT section generation tests
;;;;
;;;; Tests for: add-plt-stubs, add-got-entries, add-dynamic-relocations,
;;;; bind-now-mode, setup-got-plt (got-plt.lisp).
;;;;
;;;; PLT layout (System V AMD64 ABI):
;;;;   PLT[0]  16 bytes: resolver stub  (ff 35 ... ff 25 ...)
;;;;   PLT[n]  16 bytes each: jmpq *GOT[n+3](%rip); push $n; jmpq PLT[0]
;;;;
;;;; GOT layout (.got.plt):
;;;;   GOT[0..2]  reserved (3 × 8 bytes = 24 bytes)
;;;;   GOT[3+i]   per-symbol slot (8 bytes each), filled lazily by ld.so

(in-package :cl-cc/test)



;;; ─── bind-now-mode ───────────────────────────────────────────────────────

;;; ─── add-plt-stubs ───────────────────────────────────────────────────────

;;; ─── add-got-entries ─────────────────────────────────────────────────────

;;; ─── add-dynamic-relocations ─────────────────────────────────────────────

;;; ─── setup-got-plt (integration) ────────────────────────────────────────

