# Changelog

All notable changes to cl-cc are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Three CLI commands generated from the cl-cli application spec:
  `cl-cc completion <shell>` (bash, zsh, fish, powershell, nushell, elvish),
  `cl-cc docs [markdown|man|json]`, and `cl-cc version`.

### Changed

Five further nerima-lisp toolkits are adopted the same way cl-prolog and
cl-weave were — pulled in as plain source trees and built with cl-cc's own
`sbcl.buildASDFSystem`, replacing hand-rolled in-tree code with a thin compat
layer over the external library:

- **cl-cli** is now the CLI argument parser: `parse-args` runs
  `cl-cli:parse-argv` against an application spec generated from the CLI's own
  `*flag-spec*` / dispatch table, and still returns the legacy `parsed-args`
  struct so every handler is unchanged.
- **cl-tty-kit** backs all terminal styling: the interactive REPL's banner,
  prompt, results, and errors are colored through `cl-tty-kit:ansi-sgr`
  (gated on an interactive TTY, so captured/piped output stays plain), and the
  IR-dump color constants are built with the same SGR builder.
- **cl-boundary-kit** models every CLI process exit as a swappable system
  boundary (`%cli-exit`), so tests can capture exit codes without terminating
  the image.
- **cl-dataflow** models the `dep-graph` command as a real dataflow graph, with
  DOT / JSON / Mermaid / topological-order rendering from the library.
- **cl-parser-kit** tokenizes and parses the optimizer `--pass-pipeline` spec
  (`sccp,cse,dce`) with a whitespace-tolerant tokenizer + `sep-by` combinator
  instead of a hand-rolled comma split.

## [0.1.0] - 2026-07-11

### Infrastructure

- Green CI on ubuntu-24.04 and macos-15 (10,000-test fast plan, 0 failures)
- Benchmark workflow with regression detection
- OSS governance: LICENSE (MIT), SECURITY.md, CODE_OF_CONDUCT.md, GitHub issue/PR templates

### Compiler Core

- Hand-written incremental lexer and CST parser
- Macro expander: `defstruct`→`defclass`, `defconstant`→`defparameter`, standard macros
- CLOS-based AST definitions
- Continuation-Passing Style (CPS) transform as preferred lowering path
- Register-based VM bytecode code generation (~220 instruction types)
- VM interpreter with meta-circular `eval` support
- MIR/SSA construction (Braun et al. 2013) for native backends
- Native code emission: x86-64 (Mach-O), AArch64, WebAssembly (wasm32)

### Type System

- Hindley-Milner type inference (Algorithm W) with error-sentinel recovery
- Union type narrowing on conditionals
- Parametric types: `(List T)`, `(Option T)`
- Partial typeclass implementation

### Optimizations

- Multi-pass optimizer: CSE, constant folding, copy propagation
- Dead code elimination, strength reduction, jump threading
- Prolog-backed peephole rewrite rules and e-graph rule discovery
- E-graph equality saturation integrated into the main optimizer pipeline
- CFG construction with SSA form
- Register allocation (ML-based + linear scan)

### Runtime

- 2-generation GC: Young (Cheney semi-space) + Old (tri-color mark-sweep)
- SATB write barrier with TLAB allocation
- GC safepoint infrastructure with precise stack maps
- `storage-condition` with heap pressure warnings (80/90/95%)
- Stack overflow guard (`*max-call-stack-depth*`)

### Runtime Subsystems

- **Inline Caches**: monomorphic, polymorphic, megamorphic call site dispatch
- **Type Feedback Vector (TFV)**: runtime type profiling for Tier-1 compilation
- **Concurrency**: green threads (work-stealing), CSP channels, actor model, STM, futures/promises, structured task groups, fibers
- **Memory Reclamation**: EBR, Hazard Pointers, RCU, QSBR, MVCC
- **Lock-Free Structures**: stack, queue, hash map, SPSC ring buffer
- **Synchronization**: mutex, RWLock, semaphore, condition variable, barrier
- **OS Layer**: file I/O, process control, signals, mmap, sockets (TCP/UDP), io_uring stubs, event loop
- **FFI**: foreign function calling, callback trampolines, native struct layout
- **Image**: heap snapshot save/restore with magic/version/CRC32 verification
- **Distributed**: Raft consensus, CRDTs (GCounter, PNCounter, LWWRegister)
- **Observability**: OpenTelemetry spans, performance counters, structured logging, vector clocks, deadlock detector
- **Atomic Operations**: CAS, swap, load, store, incf, memory barriers

### Language Support

- Full ANSI CL special forms (`if`, `progn`, `block`/`return-from`, `tagbody`/`go`, `catch`/`throw`, `unwind-protect`, `let`/`let*`, `flet`/`labels`, `setq`/`setf`, `lambda`/`defun`, `defvar`/`defparameter`, `defmacro`/`macrolet`, `quote`/`the`/`values`, `multiple-value-bind`/`multiple-value-call`, `eval-when`)
- Full CLOS: `defclass`, `defgeneric`, `defmethod`, multiple dispatch, `call-next-method`, inheritance chains
- Closures with lexical variable capture and mutation
- Conditions & error handling: `handler-case`, `ignore-errors`, `error`
- `defstruct` expanding to `defclass` + constructor + predicate
- `loop` macro
- Standard library: lists, sequences, strings, characters, hash tables, arrays, numbers, I/O, pretty printer, streams, Unicode (Latin-1 NFC/NFD), time, random (MT19937), environment queries

### Self-Hosting

- Meta-circular `eval`: `(eval '...)` calls cl-cc compiler pipeline, not host SBCL
- Macro expansion via `our-eval` dispatches to cl-cc bytecode
- Compiler-in-the-compiler: can compile programs that implement parsers, compilers, and evaluators
- Quasiquote in compiled code
- REPL with persistent function, class, and accessor definitions across forms

### CLI

- `cl-cc run <file>` — compile and run
- `cl-cc compile <file>` — compile to native binary (x86-64/AArch64)
- `cl-cc eval "<expr>"` — evaluate expression
- `cl-cc repl` — interactive REPL
- `cl-cc check <file>` — type-check without executing
- `cl-cc selfhost [file]` — self-hosting workload

### Build System

- Nix Flakes with flake-parts for reproducible builds
- ASDF umbrella system with 27 subsystem packages
- `nix develop` / `nix build` / `nix run .#test` / `nix flake check`
