# CL-CC Implementation Roadmap

## Status vocabulary

- ✅ Verified implementation: the referenced implementation and supporting evidence exist in this checkout.
- 🔶 Partial implementation: some implementation evidence exists in this checkout, but completion is not established.
- ❓ Unverified: the claim has not been confirmed in this checkout, or relies only on external evidence.
- ⬜ Not started: the entry is a design or deferred item with no implementation evidence.

This index intentionally contains no FR totals, test totals, performance values,
or dates. Those values drift independently and must not be duplicated here.

## ANSI CL Compliance

- [ansi-cl-lang.md](ansi-cl-lang.md) - Language core - ✅ local evidence recorded per FR
- [ansi-cl-stdlib.md](ansi-cl-stdlib.md) - Standard library - ❓ several entries rely on external or unverified evidence

## Type System

- [type-core.md](type-core.md) - Core type system - ❓ external evidence only
- [type-advanced.md](type-advanced.md) - Advanced type system - ❓ external evidence only

## Runtime

- [runtime-core.md](runtime-core.md) - Runtime core and data structures - ❓ unverified design backlog
- [runtime-subsystem.md](runtime-subsystem.md) - Runtime subsystems - ❓ unverified design backlog
- [runtime-stdlib-1.md](runtime-stdlib-1.md) - Runtime standard library 1 - ❓ unverified design backlog
- [runtime-stdlib-2.md](runtime-stdlib-2.md) - Runtime standard library 2 - ❓ unverified design backlog
- [runtime-stdlib-3.md](runtime-stdlib-3.md) - Runtime standard library 3 - ❓ unverified design backlog

## Memory and WebAssembly

- [memory-gc.md](memory-gc.md) - Memory management and GC - ❓ unverified claims
- [wasm.md](wasm.md) - WebAssembly backend - ⬜ planned/specification notes

## Other roadmaps

The remaining roadmap notes are indexed by filename when their detailed status
has not been audited in this stream. They are not promoted to verified here.

- [optimize-passes.md](optimize-passes.md)
- [optimize-backend.md](optimize-backend.md)
- [native-codegen.md](native-codegen.md)
- [native-advanced.md](native-advanced.md)
- [tooling-compiler.md](tooling-compiler.md)
- [tooling-debug.md](tooling-debug.md)
- [tooling-advanced-1.md](tooling-advanced-1.md)
- [tooling-advanced-2.md](tooling-advanced-2.md)
- [tooling-advanced-3.md](tooling-advanced-3.md)

Status is expressed with the vocabulary above in each roadmap document.
