# CLI

`cl-cc` is the public command-line entry point. The command and option names
below are sourced from `packages/cli/src/main.lisp` and `args.lisp`.

## Commands

| Command | Usage and behavior |
| --- | --- |
| `run <file>` | Compile and run a source file in the VM. |
| `compile <file>` | Produce a host native binary or Wasm; `--system` compiles an ASDF system. |
| `save-core <file>` | Save a CL-CC core image. |
| `eval <expr>` | Evaluate one expression. |
| `repl` | Start the stateful interactive REPL. |
| `check <file>` | Run type inference without executing; `--strict` makes warnings errors. |
| `selfhost [file]` | Run a self-hosting workload; `--profile` records instruction frequencies. |
| `symbols [path]` | Index Lisp definitions; `--fuzzy` searches the index. |
| `profile <folded-stacks.txt>` | Convert folded stacks to an SVG with `--flamegraph <file>`. |
| `compile-commands [path]` | Generate `compile_commands.json`; `-o` changes its output path. |
| `install <system.asd>` | Register or compile a local ASDF system. |
| `uninstall <system>` | Remove a registered local system. |
| `fuzz [--seed N]` | Run compiler fuzzing. |
| `reduce <file>` | Reduce a compiler test case. |
| `audit` | Run the dependency audit handler. |
| `doc <path>` | Generate API documentation; `-o` selects the output. |
| `doctest <path>` | Run docstring examples. |
| `show-types <file>` | Show inferred type signatures. |
| `assert-density <path>` | Analyze assertion density. |
| `abi-dump <file>` | Dump an ABI manifest. |
| `abi-check <old> <new>` | Check two ABI manifests for compatibility. |
| `demangle <name>` | Demangle a C++ ABI symbol. |
| `disasm <wasm>` | Disassemble Wasm; `--wat` emits WAT and `--decompile` uses wasm-decompile. |
| `inspect <wasm>` | Inspect Wasm sections and disassembly. |
| `objdump <file>` | Inspect binary internals. |
| `macrostep <file>` | Step through macro expansion. |
| `bisect [range]` | Find a regression commit. |
| `features` | List feature flags. |
| `dep-graph [--format F]` | Render the ASDF dependency graph. Formats are `dot`, `json`, `mermaid`, and `topo`. |
| `generate <schema>` | Run schema-based code generation. |
| `update [pkg]` | Update dependencies. |
| `completion <shell>` | Emit completion for `bash`, `zsh`, `fish`, `powershell`, `nushell`, or `elvish`. |
| `docs [format]` | Emit CLI reference as `markdown`, `man`, or `json` (default `markdown`). |
| `version` | Print the version. |
| `help [command]` | Show global or command-specific help. |

Source language is inferred from extensions (`.php`, `.el`/`.elisp`, `.js`/`.mjs`;
otherwise Lisp) and can be overridden with `--lang lisp|elisp|php|js|javascript`.

## Target and I/O options

| Option | Meaning |
| --- | --- |
| `-o`, `--output <file>` | Output path. |
| `--arch <value>` | `x86-64`, `x86_64`, `arm64`, `aarch64`, `wasm`, `wasm32`, `wasm64`, or `wasm32-wasi`. |
| `--target <value>` | Target selector such as `wasm32`, `wasm64`, or `wasm32-wasi`; for `compile`, it takes precedence over `--arch`. |
| `--system <name>` | Compile an ASDF system. |
| `--core <file>` | Load a core image before running. |
| `--dump-image <file>` | Dump an initialized image. |
| `--script` | Use script mode. |
| `--seed <N>` | Fuzzing seed. |
| `--lang <value>` | Select the source language. |

The normal architecture default is `x86-64`; `--aot` defaults the Wasm path to
`wasm32`. Native output follows the host/backend format, while Wasm options
produce `.wasm` output.

## Execution and core options

`--stdlib`, `--no-stdlib`, `--watch`, `--timeout <seconds>`, `--no-timeout`,
`--verbose`, `--profile`, `--flamegraph <file>`, `--executable`,
`--toplevel <symbol>`, `--compression <name>` (`none`, `zlib`, `gzip`, `lz4`,
`zstd`), `--gc-min-heap <bytes>`, and `--gc-max-heap <bytes>` control execution,
profiling, saved cores, and runtime limits.

## IR, optimization, and diagnostics

The accepted options are:

```text
--dump-ir <phase>          phase: ast, cps, ssa, vm, opt, asm
--annotate-source         include source locations in IR output
--opt-level <N> | -O <N>  optimization level
--pass-pipeline <spec>    e.g. fold,dce
--opt-bisect-limit <N>    stop optimization after a limit
--opt-remarks <mode>      all, changed, or missed
--optimization-report     print optimization report lines
--tier <N>                compilation tier
--block-compile           enable cross-function/LTO inlining
--print-pass-timings      print per-pass timings
--time-passes             alias for --print-pass-timings
--trace-json <file>       write Chrome trace JSON
--pgo-generate <file>     write optimizer profile data
--pgo-use <file>          load optimizer profile data
--stats                   print optimizer statistics
--trace-emit              print VM/OPT/ASM stages
--verify-transforms       verify transforms
--parallel <value>        parallel compilation setting
--incremental             enable incremental compilation
--lto <value>             LTO setting
--sanitize <value>        sanitizer setting
--strict-no-alloc         strict no-allocation mode
--Werror                  treat compiler warnings as errors
--Werror-category <cat>   treat one warning category as an error
--trace-macros            trace macro expansion
--memoize-macros          memoize macro expansion
--fuzzy <query>           fuzzy symbol search
--coverage[=mcdc]         coverage, optionally MC/DC coverage
```

## Native, Wasm, and hardening options

```text
--debug --retpoline --spectre-mitigations --jit-cache-stats
--stack-protector --shadow-stack --perf-map --bolt
--bolt-profile <file> --build-id <id|auto>
--eh-model <value>        sjlj or table
--asan --msan --tsan --ubsan --hwasan
--aot --streaming --validate --sri --wat --decompile
--memory64 --bigint --source-map --emit-names --debug-info
--emit-debug-info --type-reflection --stack-inspection
--memory-profiler --hot-reload --incremental-repl
--compress --no-compress
--deterministic --reproducible
```

`--reproducible` is an alias for `--deterministic`; `--emit-debug-info` is an
alias for `--debug-info`. `--help` and `-h` display the global or command help.

For the generated authoritative schema, run `cl-cc docs markdown`.
