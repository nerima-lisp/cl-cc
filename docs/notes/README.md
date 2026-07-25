# Working notes

This directory holds unpublished working records. It is a sibling of
`docs/src/`, not a subdirectory of it, so it sits outside `docs_dir` and
MkDocs never builds these files. Nothing here appears on
<https://nerima-lisp.github.io/cl-cc/>.

They are kept in the repository rather than deleted because they are the
specification trail for the compiler subsystems. Each file enumerates
functional requirements as `FR-nnn` headings, records which are implemented,
and cites the source file or test that provides the evidence. That trail is
how a subsystem's completeness is argued, so it outlives any single pull
request.

`fr-status.md` is the index: it lists every specification document with its
current FR tally.

Reader-facing documentation lives in `docs/src/` and is linked from the site
nav in `docs/mkdocs.yml`.

## Four specification documents are still at `docs/` top level

`optimize-passes.md`, `optimize-backend.md`, `type-advanced.md`, and `wasm.md`
belong here with the rest, but they cannot be moved yet: the test suite reads
them by literal relative path and asserts that every `FR-` heading has matching
implementation evidence in the Lisp registry.

| Document | Read by | Guarded |
|---|---|---|
| `docs/optimize-passes.md` | `packages/optimize/tests/optimizer-roadmap-tests.lisp` | no |
| `docs/optimize-backend.md` | `packages/optimize/tests/optimizer-roadmap-backend-tests.lisp` | no |
| `docs/type-advanced.md` | `packages/type/tests/type-2026-advanced-registry-tests.lisp` | no |
| `docs/wasm.md` | `packages/emit/tests/wasm-features-tests.lisp` | `probe-file` |

`nix/checks.nix` includes `../docs` in the `checks.tests` source fileset, so
the files reach the sandbox. Moving them without updating those path literals
makes the unguarded reads error and the guarded one pass vacuously.

To finish the move, update the path literals to `docs/notes/...` in the four
test files above, then `mv` the documents into this directory in the same
change.
