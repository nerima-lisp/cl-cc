# Changelog

All notable changes to this project are documented here.

## [0.3.0] - 2026-10-10

### Added

- Wired the CLI documentation, type inspection, ABI, demangling, Wasm, and
  dependency-graph commands to their implementation paths.
- Added strict no-allocation and output-format flag coverage to the CLI tests.

### Changed

- CLI commands now propagate unavailable features and operational failures as
  non-zero exits instead of reporting successful completion.
- Native compilation, execution, and standard-library loading now preserve
  pipeline failures for callers.
- Synchronized the top-level compiler, bundled systems, Nix derivations, and
  CLI version metadata at `0.3.0`.

### Fixed

- Restored CLI standard-library fallback coverage and corrected command
  dispatch/output test bindings.
- Kept native pipeline behavior compatible with the pinned backend inputs.

## [0.2.0] - 2026-10-02

### Added

- Registered the canonical test suites that were present but not included in the flake checks.

### Changed

- Followed the `-kit` renames in the compiler's external dependencies and aligned the Nix dependency graph.
- Refreshed pinned component inputs and `flake.lock`.
- Updated architecture, CLI, and roadmap documentation to match the externalized package boundaries and audited feature evidence.
- Updated the project and bundled subsystem version metadata from `0.1.0` to `0.2.0`.

### Fixed

- Corrected compiler handling for floating-point arithmetic, quoted arithmetic designators, load-time values, deferred/native code generation, and direct lexical exits.
- Preserved Common Lisp truthiness and condition/type results across compiler and optimizer paths.
- Removed stale test components and corrected external roadmap and optimizer evidence paths.

## [0.1.0] - 2026-07-10

Initial release.
