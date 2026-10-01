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

## Specification documents and evidence paths

All specification documents tracked by this index live under `docs/notes/`.
Evidence paths below are repository-relative and must exist before they are
used as local implementation evidence.

| Document | Read by | Guarded |
|---|---|---|
| `docs/notes/optimize-passes.md` | `packages/optimize/tests/optimizer-roadmap-tests.lisp` | no |
| `docs/notes/optimize-backend.md` | `packages/optimize/tests/optimizer-roadmap-backend-tests.lisp` | no |
| `docs/notes/type-advanced.md` | external `cl-cc-type` clone | no |
| `docs/notes/wasm.md` | no verified local implementation evidence | no |

When a path or test is not present in this checkout, describe it as external or
unverified instead of presenting it as local implementation evidence.
