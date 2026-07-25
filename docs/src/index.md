# cl-cc

cl-cc is a self-hosting Common Lisp compiler and runtime written in pure
Common Lisp. It compiles ANSI Common Lisp to a register-based bytecode VM,
and from there to native x86-64, AArch64, and WebAssembly. PHP and JavaScript
frontends lower to the same AST and reuse the whole backend.

There is no project C source. The VM interpreter includes a minimal
SBCL host-backed CFFI-compatible FFI shim.

The compiler's core design — CLOS dispatch, Prolog-based optimization, CPS
transformation, Hindley–Milner type inference — is implemented using the same
language features it compiles.

```lisp
(defstruct point (x 0) (y 0))

(let ((p (make-point :x 3 :y 4)))
  (+ (point-x p) (point-y p)))
;; => 7
```

## Where to go next

- [Getting started](getting-started.md) — install cl-cc, run your first
  program, and build the compiler from source.
- [Core concepts](concepts.md) — which parts of ANSI Common Lisp the
  compiler implements, with examples.
- [Recipes](recipes.md) — the REPL, the PHP frontend, and the
  JavaScript frontend.
- [CLI](api-reference.md) — every command and flag.
- [Architecture](architecture.md) — the compiler pipeline, the
  package layout, and how self-hosting works.
- [Compatibility](compatibility.md) — ANSI conformance status,
  known limitations, and the security scope.

## Project

cl-cc is MIT licensed and developed at
[nerima-lisp/cl-cc](https://github.com/nerima-lisp/cl-cc).

Contribution guidelines, the code of conduct, and support channels are
org-wide and live in the
[nerima-lisp/.github](https://github.com/nerima-lisp/.github) repository.
Report security vulnerabilities privately through
[GitHub Security Advisories](https://github.com/nerima-lisp/cl-cc/security/advisories/new);
see [Compatibility](compatibility.md) for what counts as in scope.
