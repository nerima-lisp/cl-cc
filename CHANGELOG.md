# Changelog

All notable changes to this project are documented here.

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
