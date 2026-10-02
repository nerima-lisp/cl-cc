# Architecture

cl-cc is a Common Lisp compiler implemented in Common Lisp. The checked-in
repository contains the pipeline integration, integration tests for external
compiler systems, the CLI, and tooling. The parser, AST, type,
macro-expansion, CPS, MIR, register-allocation, code-generation, VM, runtime,
bytecode, and optimization systems are consumed as ASDF/Nix dependencies from
sibling repositories; their implementations are not directories in this
checkout.

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
    |       external optimize/MIR/regalloc/codegen systems
    +--> VM interpreter                                  external cl-cc-vm
    |       integration/tests in packages/vm
    +--> Native/Wasm emission                            external cl-cc-emit
    |       in-tree integration/tests in packages/emit
    +--> Bytecode encoding                               external cl-cc-binary
            integration/tests in packages/binary
```

The CLI, REPL, standard library, formatter, debugger, documentation generator,
and test tooling are separate in-tree packages. Runtime, VM, bytecode, and
optimizer code is consumed from external systems, with integration and test
code kept in the corresponding package directories. PHP and JavaScript
frontends enter the shared pipeline through the language integration boundary.

## Packages and external systems

The package directories currently present in this repository are:

| Package | Role |
| --- | --- |
| `binary` | integration/tests for the external bytecode system |
| `cli` | command-line entry point |
| `compile` | compiler and code-generation integration |
| `debug` | debugger and inspection support |
| `docgen` | documentation generation |
| `emit` | FPGA support plus integration/tests for the external emission system |
| `formatter` | source formatting |
| `optimize` | tests for the external optimizer |
| `parse` | parser tests; implementation is external |
| `pipeline` | high-level compile/evaluate pipeline |
| `prolog-tools` | call-graph tooling |
| `repl` | interactive REPL |
| `runtime` | runtime integration/tests and the C header |
| `selfhost` | self-hosting workloads |
| `stdlib` | standard library |
| `testing-framework` | test support |
| `tools` | development tools |
| `umbrella-tests` | umbrella test definitions |
| `vm` | tests for the external VM |

The external systems include `cl-cc-bootstrap`, `cl-cc-parse`, `cl-cc-ast`,
`cl-cc-type`, `cl-cc-expand`, `cl-cc-cps`, `cl-cc-mir`, `cl-cc-target`,
`cl-cc-regalloc`, `cl-cc-codegen`, `cl-cc-emit`, `cl-cc-binary`,
`cl-cc-optimize`, `cl-cc-vm`, and `cl-cc-runtime`. They are declared in
`flake.nix` and `nix/asdf-systems.nix` and loaded as dependencies.
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

The optimizer is the external `cl-cc-optimize` system, while `cl-cc-compile`
connects it to the external MIR, register-allocation, and code-generation
systems. The external `cl-cc-emit` system provides the emission implementation;
`packages/emit` contains in-tree integration and test code. `cl-cc-prolog-tools`
provides call-graph analysis using `cl-prolog-kit`; it is tooling, not a
compiler package named `cl-cc-prolog`.

## Runtime

The external `cl-cc-runtime` system provides heap management, garbage
collection, object representation, I/O and FFI integration, image support, and
stack-safety facilities used by the VM. `packages/runtime` contains the
in-tree runtime integration and tests.

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
