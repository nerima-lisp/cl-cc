# Architecture

cl-cc is a Common Lisp compiler implemented in Common Lisp. The checked-in
repository contains the pipeline integration, bytecode VM, runtime, emitter
integration/tests, CLI, and tooling. The parser, AST, type, macro-expansion, CPS, MIR,
register-allocation, and code-generation systems are consumed as ASDF/Nix
dependencies from sibling repositories; their implementations are not
directories in this checkout.

## The pipeline

```text
Source (.lisp, .php, .js, .mjs)
    |
    v
Parser and language integration                         external cl-cc-parse
    |
    v
Pipeline and lowering                                   packages/pipeline
    |       external AST/type/expand/CPS systems
    v
Compilation and optimization                            packages/compile
    |       external MIR/regalloc/codegen systems
    +--> VM interpreter                                  packages/vm
    |       bytecode execution and runtime integration
    +--> Native/Wasm emission                            external cl-cc-emit
    |       in-tree integration/tests in packages/emit
    +--> Bytecode encoding                               packages/binary
            portable format
```

The CLI, REPL, runtime, standard library, formatter, debugger, documentation
generator, and test tooling are separate in-tree packages. PHP and JavaScript
frontends enter the shared pipeline through the language integration boundary.

## Packages and external systems

The package directories currently present in this repository are:

| Package | Role |
| --- | --- |
| `binary` | bytecode format |
| `cli` | command-line entry point |
| `compile` | compiler and code-generation integration |
| `debug` | debugger and inspection support |
| `docgen` | documentation generation |
| `emit` | native and Wasm emission |
| `formatter` | source formatting |
| `optimize` | optimization passes |
| `parse` | parser integration and tests; implementation is external |
| `pipeline` | high-level compile/evaluate pipeline |
| `prolog-tools` | call-graph tooling |
| `repl` | interactive REPL |
| `runtime` | runtime support |
| `selfhost` | self-hosting workloads |
| `stdlib` | standard library |
| `testing-framework` | test support |
| `tools` | development tools |
| `umbrella-tests` | umbrella test definitions |
| `vm` | bytecode VM |

The external systems include `cl-cc-parse`, `cl-cc-ast`, `cl-cc-type`,
`cl-cc-expand`, `cl-cc-cps`, `cl-cc-mir`, `cl-cc-regalloc`, and
`cl-cc-codegen`, and `cl-cc-emit`. They are
declared in `flake.nix` and `nix/asdf-systems.nix` and loaded as dependencies.
The Prolog integration is split between external `cl-prolog-kit` and the
in-tree `cl-cc-prolog-tools` system. `cl-cc-prolog` is not the name of a
current in-tree package or public tool.

`cl-cc.asd` is the umbrella ASDF definition. `src/` contains umbrella source
and FFI integration; `nix/` contains the Nix build/test/dev-shell definitions;
`t/` and package-local test directories contain the test suites.

## Type system

Type inference is provided by the external `cl-cc-type` system. It is invoked
independently of execution by `cl-cc check <file>`; this page does not duplicate
that system's implementation details.

## Optimization and emission

The in-tree optimizer is `cl-cc-optimize`, while `cl-cc-compile` connects it to
the external MIR, register-allocation, and code-generation systems. The
in-tree emission boundary is `cl-cc-emit`, which supports the target paths
implemented by the current compiler. `cl-cc-prolog-tools` provides call-graph
analysis using `cl-prolog-kit`; it is tooling, not a compiler package named
`cl-cc-prolog`.

## Runtime

The runtime package is the in-tree boundary for heap management, garbage
collection, object representation, I/O and FFI integration, image support, and
stack-safety facilities used by the VM. The architecture page does not infer
additional runtime interfaces from test names or comments.

## Self-hosting

Self-hosting means that the compiler can run its own pipeline inside the VM.
The supported entry point is `cl-cc selfhost [file]`; `--profile` records VM
instruction frequencies for the workload. The REPL and `eval` commands use
the same pipeline and preserve definitions within their process/session.

The example below exercises the evaluator without describing an internal
package:

```lisp
(defun eval-ast (node)
  (if (integerp node)
      node
      (apply #'+ (mapcar #'eval-ast (cdr node)))))

(eval-ast '(1 (2 (3 4))))
;; => 10
```
