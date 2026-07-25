;;;; ---------------------------- WARNING ----------------------------
;;;; This system is DOUBLE-DEFINED.
;;;;
;;;; The name "cl-cc-runtime" is defined both here and in the standalone
;;;; repository nerima-lisp/cl-cc-runtime, and the two definitions do not
;;;; agree. Which one ASDF resolves depends on the order of the source
;;;; registry, so the dependency graph of this repository is not currently
;;;; well defined. flake.nix does NOT list this system's standalone repository
;;;; among its inputs, so the Nix build currently resolves to THIS file — but
;;;; nothing enforces that, and any checkout with the standalone repository on
;;;; its source registry may resolve the other way.
;;;;
;;;; Measured 2026-07-26 against the standalone repository's working tree
;;;; (cl-cc-runtime/src vs packages/runtime/src):
;;;;   85 source files here, 93 there
;;;;   54 identical, 30 differing
;;;;   1 only here, 10 only there
;;;;   :depends-on — DIFFERENT: this one has none, the standalone repo depends on
;;;;   "cl-log-kit", "cl-process-kit" and "cl-json-kit"
;;;;
;;;; WHICH DEFINITION IS AUTHORITATIVE IS UNDECIDED. Do not assume an edit
;;;; here reaches the build, and do not "fix" the divergence by copying one
;;;; side over the other; the two have diverged in both directions. Resolving
;;;; this is a design decision, tracked separately from the packaging
;;;; migration that added this banner.
;;;; -----------------------------------------------------------------

;;;; cl-cc-runtime.asd — independent ASDF system for the runtime library
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/runtime package (rt-* function namespace + GC + heap + frame).
;;;; Leaf system: no dependencies on other cl-cc systems after the rt-eval
;;;; backward dependency on :cl-cc was removed.

(asdf:defsystem :cl-cc-runtime
  :description "cl-cc runtime library — rt-* primitives, GC, heap, frame, value codec"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
   (   (:file "package")
    (:file "runtime-region")
    (:file "runtime")
   (:file "runtime-ops")
   (:file "runtime-strings")
   (:file "runtime-math-io")
   (:file "runtime-clos")
   (:file "runtime-io")
    (:file "value")
    (:file "value-codec")
    (:file "frame")
     (:file "heap-data")
     (:file "heap-core")
     (:file "heap-sanitizer")
     (:file "heap-layout")
     (:file "heap-access")
     (:file "heap-free-list")
     (:file "heap-resize")
     (:file "heap-trace")
     (:file "gc-references")
     (:file "gc-profile")
     (:file "gc-data")
     (:file "gc-safepoints")
      (:file "gc-policy")
       (:file "gc-roots-objects")
      (:file "gc-tlab")
     (:file "gc-minor")
       (:file "gc-write-barrier")
       (:file "gc-finalizers")
       (:file "gc-major-mark")
      (:file "gc-workers")
      (:file "gc-major-sweep")
      ;; ── Synchronization & concurrency primitives ──
      (:file "deadlock")
      (:file "sync")
      (:file "portable")
      (:file "lockfree")
     (:file "spsc")
     (:file "ebr")
     (:file "hazard")
     (:file "rcu")
     (:file "qsbr")
     (:file "scheduler")
     (:file "future")
     (:file "channel")
     (:file "actor")
      (:file "task")
      (:file "stm")
      (:file "async")
      (:file "async-generators")
      (:file "effects")
      (:file "fiber")
     (:file "context")
     (:file "image")
     ;; ── OS / I/O / Network ──
     (:file "os")
      (:file "signals")
      (:file "mmap")
      (:file "heap-hugepages")
      (:file "heap-los")
      (:file "ffi")
      (:file "xom")
      (:file "net")
     (:file "io-uring")
     (:file "event-loop")
     (:file "zerocopy")
     (:file "ratelimit")
     ;; ── Debug / Observability / Distributed ──
      (:file "perf")
      (:file "otel")
      (:file "continuous-profile")
      (:file "log")
                             (:file "metrics")
     (:file "consensus")
     (:file "crdt")
     (:file "cluster")
     ;; ── Memory / Topology / Algorithms ──
     (:file "allocator")
     (:file "topology")
     (:file "mvcc")
     (:file "hash-weak")
     (:file "parallel-algo")
     ;; ── GPU / WASM / Reactive / Async ──
     (:file "gpu")
     (:file "reactive")
     ;; ── Phase 116-127: Serialization / Crypto / Compression ──
     (:file "serialize")
     (:file "crypto")
     (:file "compress")
     (:file "gc-advanced-129")))
