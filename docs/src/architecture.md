# Architecture

`cl-cc` is a Common Lisp compiler. This checkout contains the pipeline, CLI,
REPL, tooling, and the in-tree compilation engine that transforms AST forms
into VM instructions. It also contains integrations and tests for ASDF systems
maintained in sibling repositories.

`packages/compile` includes the `codegen-*` files for AST-to-VM compilation
and connects that engine to lower-level machine-code facilities.

## Context boundaries

FR-031 defines the target architecture for cl-cc 1.0.0: each child repository
is one bounded context, and dependencies between contexts follow the directions
in this table. Repository names identify source owners; ASDF system names
identify the systems loaded by the umbrella.

| Context | Repository and systems | Role and relationship |
| --- | --- | --- |
| Shared kernel | `cl-cc-bootstrap` | Holds pre-interned symbols and backend registration. The PHP and JavaScript frontends use it; it does not depend on them. |
| Language model | `cl-cc-ast` | Defines the public AST types and traversal API consumed by frontends and transformation contexts. |
| Common Lisp syntax | `cl-cc-parse` | Converts Common Lisp syntax into the AST. The AST does not depend on the parser. |
| PHP and JavaScript | `cl-cc-php`, `cl-cc-javascript` | Frontends that lower source into the AST and use bootstrap's registration protocol. Each depends on the AST and bootstrap. |
| Macro expansion | `cl-cc-expand` | Uses the public AST and type APIs. The AST and type contexts do not depend on expansion. |
| Continuation transformation | `cl-cc-cps` | Transforms AST using the public AST API. The AST does not depend on CPS. |
| Types | `cl-cc-type` | Infers and validates types using the public AST API. The AST does not depend on type inference. |
| Compilation | Umbrella `packages/compile` (`cl-cc-compile`) | In-tree engine that transforms AST into VM instructions and uses AST, CPS, expansion, and VM APIs. Its direct dependencies on parsing, types, optimization, code generation, targets, and register allocation are temporary W3.4.cc.② exceptions. |
| Optimization | `cl-cc-optimize` | Operates on the VM instruction model. The VM does not depend on the optimizer. |
| Machine-code generation | `cl-cc-codegen-native` repository (systems: `cl-cc-codegen`, `cl-cc-emit`, `cl-cc-regalloc`) | Uses the MIR, target, and binary APIs. `cl-cc-codegen` uses register allocation; `cl-cc-emit` uses code generation. MIR and binary do not depend on this context. |
| Intermediate representation and targets | `cl-cc-mir`, `cl-cc-target` | Provide machine-independent IR and target descriptions. Their production systems have no dependencies. |
| Object and bytecode format | `cl-cc-binary` | Provides object and bytecode formats. It uses the logging toolkit; its process-toolkit dependency is a temporary W3.3.binary.② exception. |
| Execution | `cl-cc-vm` | Supporting context that executes VM instructions and calls runtime services. The dependency points from VM to runtime. |
| Runtime foundation | `cl-cc-runtime` | Supporting context for allocation, object representation, I/O, FFI, and other VM services. |
| Pipeline coordination | Umbrella `packages/pipeline` (`cl-cc-pipeline`) | Application layer that composes the context APIs for compilation and evaluation. |
| User interfaces | Umbrella `packages/cli`, `packages/repl`, and `packages/tools` | Interface layer that exposes the compiler and pipeline to users and development workflows. |

Run the check on Linux with `nix build .#checks.x86_64-linux.context-map -L --no-link`.
It loads every production ASDF system and its direct dependencies, checks which
repository contains each system's ASDF source file, and fails for systems
without a context mapping or dependency edges outside the FR-031 directions in
the table above.

Each temporary edge has an exact source-system, target-system, and ticket row
in `t/context-map-exceptions.txt`. Ticket IDs use
`W3.<stage>.<repository>.<phase>`. For example, `W3.4.cc.④` means W3 stage
4, the `cl-cc` repository, phase ④. The un-circled number is the work-order
stage; the circled number is its phase. The repository identifies the owner of
the dependent ASDF system. Completing a phase-④ work order removes both the
dependency and its exception row. Umbrella systems in the `cl-cc` repository,
including compile, pipeline, CLI, REPL, and tools, use `W3.4.cc.④`. The
`cl-cc-compile` dependencies on parse, type, optimize, codegen, target, and
register allocation use `W3.4.cc.②` because phase ② moves these dependencies
to pipeline. The `cl-cc-binary` dependency on `cl-process-kit` uses
`W3.3.binary.②`.

An unallowed dependency from bootstrap, ast, mir, or runtime must be reported
individually: those stage-1 repositories have completed phase ④, so the edge is
either a new violation or a misread of the FR-031 table. Report any edge these
rules do not classify instead of inferring its ticket. Malformed or duplicate
rows fail the check. A stale row names a dependency that is absent from the
current direct ASDF dependencies and also fails the check.

## The pipeline

```text
Source (.lisp, .php, .js, .mjs)
    |
    v
Frontend or parser -> AST -> pipeline -> compiler (cl-cc-compile) -> VM instructions
                                                   |                         |
                                                   |                         +--> VM execution
                                                   |                         +--> bytecode encoding
                                                   |                         `--> native/Wasm lowering
                                                   |                               -> MIR -> target
                                                   |                               -> register allocation
                                                   |                               -> code generation -> emission
```

The VM path executes instructions. Bytecode encoding serializes program data.
Native and Wasm output use the external MIR, target, register-allocation, and
code-generation systems. These are separate output paths; not every build uses
native lowering.

## Packages and external systems

| Package | Role |
| --- | --- |
| `binary` | Integration and tests for the external bytecode system |
| `cli` | Command-line entry point |
| `compile` | In-tree AST-to-VM compiler and machine-code integration |
| `debug` | Debugger and inspection support |
| `docgen` | Documentation generation |
| `emit` | FPGA support and integration/tests for external emission |
| `formatter` | Source formatting |
| `optimize` | Tests for the external optimizer |
| `parse` | Tests for the external parser |
| `pipeline` | High-level compile and evaluation pipeline |
| `prolog-tools` | Call-graph tooling backed by `cl-prolog-kit` |
| `repl` | Interactive REPL |
| `runtime` | Runtime integration/tests and the C header |
| `selfhost` | Self-hosting workloads |
| `stdlib` | Standard library |
| `testing-framework` | Test support |
| `tools` | Development tools |
| `umbrella-tests` | Umbrella test definitions |
| `vm` | Tests for the external VM |

The child repositories and their systems are declared in `flake.nix` and
`nix/asdf-systems.nix`. `cl-cc.asd` is the umbrella ASDF definition. `src/`
contains umbrella source and FFI integration; `nix/` contains the build, test,
and development-shell definitions; `t/` and package-local test directories
contain the test suites.

## Type checking

The external `cl-cc-type` system provides inference. `cl-cc check <file>` runs
it separately from program execution.

## Runtime

The external `cl-cc-runtime` system provides heap management, garbage
collection, object representation, I/O and FFI integration, image support, and
stack-safety facilities used by the VM. `packages/runtime` contains the
in-tree runtime integration and tests.

## Self-hosting

Self-hosting means that the compiler can run its own pipeline inside the VM.
The supported entry point is `cl-cc selfhost [file]`. It runs the selected
self-hosting workload inside the VM; if `[file]` is omitted, the CLI uses
`example/selfhost.lisp`. `--profile` records VM instruction frequencies; its
default output path is `.cl-cc-profile.sexp`.

## Interactive use

The REPL keeps definitions for its session. `cl-cc eval` evaluates one
expression per invocation.
