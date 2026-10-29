;;;; tests/unit/optimize/optimizer-speculative-jit-tests.lisp
;;;; Unit tests for speculative JIT helpers — ThinLTO, PGO, tiered JIT, deopt,
;;;; OSR, shape transitions, IC patch, async state machines, channels, STM,
;;;; lock-free, CFI, retpoline, stack canary, shadow stack, Wasm GC,
;;;; DWARF/source-map debug, TLS, atomics, HTM, concurrent GC.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-speculative-jit-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-speculative-jit-tests-suite))

;;; ─── ThinLTO / Tiered JIT / Deopt helpers (FR-301/310/312 partial) ───────

