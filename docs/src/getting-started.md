# Getting started

## Install

cl-cc is distributed as a Nix flake. Add it as an input, pinning a release
tag rather than following the default branch:

```nix
# flake.nix
inputs.cl-cc = {
  url = "github:nerima-lisp/cl-cc/v0.1.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

To get the `cl-cc` executable without adding a flake input:

```sh
nix run github:nerima-lisp/cl-cc -- version
```

If you do not have Nix, follow the
[Nix installation guide](https://nixos.org/download.html) first. Nix is the
only supported way to build cl-cc: the compiler depends on several sibling
nerima-lisp libraries that are resolved through the flake.

## Quick start

Enter the development shell and run a program. `nix develop` puts SBCL, the
`cl-cc` CLI, and every ASDF system on the path.

```sh
nix develop
```

```sh
cl-cc eval "(+ 1 2)"
cl-cc run file.lisp
cl-cc repl
```

The frontend is selected from the file extension, so the same `run` command
handles all three source languages:

```sh
cl-cc run file.lisp   # Common Lisp
cl-cc run file.php    # PHP  (.php)
cl-cc run file.js     # JavaScript (.js, .mjs)
```

Compiling to a native binary takes an explicit architecture:

```sh
cl-cc compile file.lisp -o out --arch x86-64
```

Every command and flag is listed in the [CLI reference](api-reference.md).

## Building and testing

All development tasks go through the flake:

```sh
nix develop          # SBCL with every ASDF dependency and the cl-cc CLI
nix run .#test       # the canonical fast unit test plan
nix build            # standalone binary at ./result/bin/cl-cc
nix fmt              # format Nix sources (treefmt)
nix flake check      # tests + formatting + docs, the same gate CI uses
```

Tests live in `t/` and run under
[cl-weave](https://github.com/nerima-lisp/cl-weave), the org's test framework.
The entry point is `./run-tests.lisp` at the repository root; `nix run .#test`
invokes it and maps to `cl-weave:run-all`.

`nix run .#test` executes the fast unit plan only. Integration, E2E,
conformance, and documentation/evidence suites are selected by suite taxonomy
and run explicitly — they are not selected by name. `nix flake check` invokes
the same fast plan via `checks.tests`.

### Test timeouts

Timeouts are explicit. Two environment variables override the defaults; both
take a positive integer number of seconds, and invalid, zero, or negative
values are ignored in favour of the default.

| Variable | Scope | Default |
|---|---|---|
| `CLCC_TEST_TIMEOUT` | one test | `10` |
| `CLCC_SUITE_TIMEOUT` | the whole suite | `600` |

```sh
CLCC_TEST_TIMEOUT=30 nix run .#test
CLCC_SUITE_TIMEOUT=1200 nix run .#test
```

An individual test may declare a positive `:timeout`, which overrides
`CLCC_TEST_TIMEOUT` for that test.

### Clearing the FASL cache

The runner keeps a warm compilation cache so repeat invocations stay fast.
Switching between project paths can leave stale FASLs behind. Clear them with:

```sh
rm -rf ~/.cache/common-lisp/ && mkdir -p ~/.cache/common-lisp/
```

The `rm` and `mkdir` are separate steps deliberately: recreating the directory
immediately avoids a race in SBCL on macOS.

## Contributing

Branch from `main`, add or update tests for the change, and confirm
`nix run .#test` and `nix fmt` are clean before opening a pull request.
Source code and commit messages are written in English, and commit subjects
use a `feat:` / `fix:` / `docs:` prefix.

The full workflow is the org-wide
[contributing guide](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md).
For the repository layout and what each compiler package does, see
[Architecture](architecture.md).
