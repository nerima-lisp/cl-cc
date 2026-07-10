# Security Policy

## Supported Versions

| Version | Supported |
| ------- | --------- |
| 0.1.x   | ✓         |

## Reporting a Vulnerability

Please report security vulnerabilities privately via
[GitHub Security Advisories](https://github.com/takeokunn/cl-cc/security/advisories/new)
rather than opening a public issue.

Include:

- A description of the vulnerability and its impact
- Steps to reproduce (a minimal `.lisp`, `.php`, or `.js` input file if the
  issue is in a compiler frontend or the VM)
- The commit hash or release version affected

You can expect an initial response within 7 days. Once a fix is available,
the advisory will be published together with the patched release.

## Scope Notes

cl-cc compiles and executes arbitrary source code by design. Running
untrusted programs through `cl-cc run` / `cl-cc eval` executes them with the
full privileges of the invoking user — this is expected behavior, not a
vulnerability. In-scope issues include memory corruption in the runtime/GC,
sandbox escapes from documented isolation features, and miscompilations that
silently produce incorrect binaries.
