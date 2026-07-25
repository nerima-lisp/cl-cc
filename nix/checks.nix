# The individual check derivations. They are assembled into the flake's
# `checks` attribute set in flake.nix rather than returned as one here, so that
# `checks.default`, `checks.formatting` and `checks.docs` are readable at the
# point where the flake declares its outputs instead of being hidden behind an
# import.
{
  pkgs,
  lib,
  version,
  sbclWithTests,
  apps,
  clProlog,
  clWeave,
}:
{
  # The canonical fast unit test suite, published as `checks.default`.
  testSuite = pkgs.stdenvNoCC.mkDerivation {
    pname = "cl-cc-tests";
    inherit version;
    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions [
        ../packages
        ../t
        ../nix
        # docs/ is a test INPUT here, not documentation dressing:
        # CL-CC-DOCUMENTATION-SUITE parses the FR specification documents and
        # asserts that every backtick-quoted repository path inside them still
        # resolves. Drop this and 60-odd tests error out on a missing file.
        ../docs
        ../run-tests.lisp
        ../cl-cc.asd
      ];
    };
    nativeBuildInputs = [ sbclWithTests ];
    buildPhase = ''
      export HOME="$TMPDIR"
      ${apps.test.program}
    '';
    installPhase = "mkdir -p $out && touch $out/passed";
    meta.description = "cl-cc canonical fast unit test suite";
  };

  # Standalone cl-weave suite for packages/prolog-tools (the cl-prolog based
  # call-graph analysis tools). Independent of `default` above: that one
  # drives cl-cc's own runner via `nix run .#test`, while this package's tests
  # are cl-weave DESCRIBE/IT specs run through
  # `(asdf:test-system :cl-cc-prolog-tools)`. clProlog and clWeave are plain
  # `sbcl.buildASDFSystem` derivations built from source (see flake.nix) —
  # their installPhase mirrors the source tree directly into $out (e.g.
  # $out/cl-prolog.asd), unlike nixpkgs' curated sbclPackages set which nests
  # under /share/common-lisp/source/, so the registry entry is just
  # "${clProlog}//", not a share/ subpath.
  prologToolsTests =
    pkgs.runCommand "cl-cc-prolog-tools-tests"
      {
        nativeBuildInputs = [ pkgs.sbcl ];
        src = lib.fileset.toSource {
          root = ../.;
          fileset = lib.fileset.unions [
            ../packages/prolog-tools
            ../packages/ast
          ];
        };
      }
      ''
        cp -R "$src" source
        chmod -R u+w source
        cd source
        export HOME="$TMPDIR/home"
        export XDG_CACHE_HOME="$TMPDIR/cache"
        mkdir -p "$HOME" "$XDG_CACHE_HOME"
        export CL_SOURCE_REGISTRY="${clProlog}//:${clWeave}//:$PWD//:"
        sbcl --non-interactive \
          --eval '(require :asdf)' \
          --eval '(asdf:load-asd (truename "packages/ast/cl-cc-ast.asd"))' \
          --eval '(asdf:load-asd (truename "packages/prolog-tools/cl-cc-prolog-tools.asd"))' \
          --eval '(asdf:test-system :cl-cc-prolog-tools)'
        touch $out
      '';
}
