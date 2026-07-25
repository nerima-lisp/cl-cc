# cl-cc

[![CI](https://github.com/nerima-lisp/cl-cc/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/cl-cc/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/cl-cc/)

A self-hosting Common Lisp compiler and runtime implemented in pure Common
Lisp. cl-cc compiles ANSI Common Lisp to a register-based bytecode VM, and
from there to native x86-64, AArch64, and WebAssembly; PHP and JavaScript
frontends lower to the same AST and reuse the whole backend. There is no
project C source — the VM interpreter includes a minimal SBCL host-backed
CFFI-compatible FFI shim. The compiler's core design (CLOS dispatch,
Prolog-based optimization, CPS transformation, Hindley–Milner type inference)
is implemented using the same language features it compiles.

Full documentation is published at <https://nerima-lisp.github.io/cl-cc/>.
The source for that site lives in [docs/src/](docs/src/).

## Quick Start

```sh
nix develop
```

```sh
cl-cc eval "(+ 1 2)"
cl-cc repl                             # definitions persist across forms
cl-cc run file.lisp
cl-cc run file.php                     # frontend chosen by extension
cl-cc run file.js
cl-cc compile file.lisp -o out --arch x86-64
```

## Install

```nix
# flake.nix
inputs.cl-cc = {
  url = "github:nerima-lisp/cl-cc/v0.1.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Note the pinned tag. Consumers inside this org must pin a release tag rather
than follow the default branch.

## Documentation

- [Getting started](https://nerima-lisp.github.io/cl-cc/getting-started/) —
  install, first program, building from source
- [Core concepts](https://nerima-lisp.github.io/cl-cc/guide/concepts/) — the
  ANSI Common Lisp surface cl-cc implements
- [CLI reference](https://nerima-lisp.github.io/cl-cc/reference/api/) — every
  command and flag
- [Architecture](https://nerima-lisp.github.io/cl-cc/reference/architecture/) —
  the pipeline, the packages, and how self-hosting works
- [Compatibility](https://nerima-lisp.github.io/cl-cc/reference/compatibility/) —
  ANSI conformance status, known limitations, security scope

## Development

```sh
nix develop          # SBCL with every ASDF dependency and the cl-cc CLI
nix run .#test       # the canonical fast unit test plan
nix build            # standalone binary at ./result/bin/cl-cc
nix fmt              # format Nix sources (treefmt)
nix flake check      # tests + formatting + docs, the same gate CI uses
```

Tests live in `t/` and run under
[cl-weave](https://github.com/nerima-lisp/cl-weave), the org's test framework;
the entry point is `./run-tests.lisp`. `nix run .#test` executes the fast unit
plan only — integration, E2E, conformance, and documentation/evidence suites
are selected by suite taxonomy and run explicitly.

Set `CLCC_TEST_TIMEOUT` (default `10`) or `CLCC_SUITE_TIMEOUT` (default `600`)
to override the test timeouts, in seconds. If stale FASLs cause trouble after
switching project paths, clear the cache with
`rm -rf ~/.cache/common-lisp/ && mkdir -p ~/.cache/common-lisp/`.

`docs/notes/` holds the unpublished specification trail for the compiler
subsystems. It is outside the documentation site and is not built.

## Contributing

See the org-wide [CONTRIBUTING](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md)
guide and the [package standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md).

## Support

See [SUPPORT](https://github.com/nerima-lisp/.github/blob/main/SUPPORT.md).
Report security issues privately through
[GitHub Security Advisories](https://github.com/nerima-lisp/cl-cc/security/advisories/new).

## License

MIT. See [LICENSE](LICENSE).
