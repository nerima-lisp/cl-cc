# Compatibility

This page records what cl-cc does and does not yet implement, and what the
project treats as a security issue.

## Test suite status

The fast test plan passes **9184 tests with 0 failures** in the local
2026-06-14 run (`nix run .#test -- --no-warm-stdlib`).

The fast plan excludes the integration, E2E, conformance, and
documentation/evidence suites by taxonomy, so a green `nix run .#test` is not
a statement about those suites. Native I/O parity tests sit in the E2E
taxonomy while native binary behavior is validated end to end.

## ANSI CL conformance

Conformance suites run explicitly under the conformance taxonomy rather than
in the default fast plan.

**Package system (Ch. 11)** — 18 conformance tests pass as `deftest`. Package
registry metadata, symbol operations (`intern`, `export`, `import`, `shadow`,
`unintern`), `defpackage`, `make-package` and nicknames, `delete-package`,
`rename-package`, `do-symbols`, `list-all-packages`, `gensym`, and
`make-symbol` are implemented.

**`format` directives (Ch. 22)** — 23 of 23 conformance tests pass as
`deftest`. The self-hosted `%vm-format-render` handles `~A`, `~S`, `~D`, `~B`,
`~O`, `~X`, `~R`, `~F`, `~%`, `~~`, `~&`, `~T`, `~C`, `~P`, `~*`, `~?`, `~[`,
`~{`, and `~^`, with column tracking and section parsing. FORMAT in a native
x86-64 binary still falls back to the host SBCL.

**Number tower (Ch. 12)** — 24 of 24 conformance tests pass as `deftest`.
Bignum, ratio, and complex type predicates and arithmetic VM helpers are
implemented. The native x86-64 backend is fixnum-only
(`*x86-64-bignum-calls-enabled*` defaults to `nil`).

**I/O and streams (Ch. 19–21)** — native and E2E conformance tests cover
string streams, predicates, `read-char`/`write-char`, `read-line`/`write-line`,
`read-sequence`/`write-sequence`, namestring, pathname and file operations,
`load`, and compound streams. Native binary parity for `unread-char`,
`write-to-string`, `listen`, `make-pathname`, `load`, and `echo-stream`
remains outside the fast plan until validated end to end.

## Known limitations

### Native backend

`load`, host-backed FFI, host-backed `format`, and host stream bridges are not
yet available in native binaries. Twenty-one pathname, file, compound-stream,
and LOAD runtime functions were added in Wave 4.

### Standalone binaries

The `standalone` CLI option produces a native Mach-O or ELF binary with the
runtime linked in, but full self-hosting — no host CL dependency at all — is
not complete. The self-host pipeline lives in
`packages/selfhost/src/pipeline-selfhost.lisp`.

### Unicode

NFC and NFD normalization is implemented for Latin-1 characters. Full Unicode
15 normalization (NFKC, NFKD, UCA collation) requires the complete Unicode
Character Database.

### Concurrency

Green threads, channels, actors, STM, lock-free structures, and EBR/RCU/QSBR
are implemented as pure-CL primitives suitable for cooperative multitasking.
Native OS thread scheduling and M:N threading are not yet integrated.

### Distributed systems

Raft consensus and the CRDTs are proof-of-concept implementations. They lack
the full RPC integration, log persistence, and network transport layers that
production use would require.

## PHP frontend

The PHP 8.0–8.5 frontend covers the statement and expression forms listed in
[Recipes](recipes.md#the-php-frontend). Not yet supported:

- Full generator coroutine object and resumption semantics. `yield` and
  `yield from` parse to runtime data representations, but coroutine execution
  lowering is not implemented.
- Full stackful Fiber continuation and resumption semantics after
  `Fiber::suspend()`. Construction, `start`, `getReturn`, state queries, and
  the first suspend value do lower to runtime helpers.
- PHP standard library surface beyond the registered runtime helper set.

## JavaScript frontend

The JavaScript frontend tracks ECMAScript 2026 as its latest supported
target. It shares the AST and the entire backend with the other frontends,
so the native backend limitations above apply to it equally.

## Supported versions

| Version | Supported |
| ------- | --------- |
| 0.1.x   | ✓         |

## Security scope

Report vulnerabilities privately through
[GitHub Security Advisories](https://github.com/nerima-lisp/cl-cc/security/advisories/new)
rather than opening a public issue. Include a description of the
vulnerability and its impact, steps to reproduce (a minimal `.lisp`, `.php`,
or `.js` input file if the issue is in a frontend or the VM), and the commit
hash or release version affected. Expect an initial response within 7 days;
the advisory is published together with the patched release.

**cl-cc compiles and executes arbitrary source code by design.** Running
untrusted programs through `cl-cc run` or `cl-cc eval` executes them with the
full privileges of the invoking user. That is expected behavior, not a
vulnerability.

In-scope issues include memory corruption in the runtime or GC, sandbox
escapes from documented isolation features, and miscompilations that silently
produce incorrect binaries.
