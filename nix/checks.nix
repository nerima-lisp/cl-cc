{
  pkgs,
  lib,
  sbclWithTests,
  apps,
  packagesDefault,
  clProlog,
  clWeave,
}:
{
  checks = {
    tests = pkgs.stdenvNoCC.mkDerivation {
      pname = "cl-cc-tests";
      version = "0.1.0";
      src = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.unions [
          ../packages
          ../tests
          ../docs
          ../nix
          ../cl-cc.asd
          ../cl-cc-test.asd
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

    build = packagesDefault;

    # Standalone cl-weave suite for packages/prolog-tools (the cl-prolog
    # based call-graph analysis tools). Independent of the `tests` check
    # above: that one drives cl-cc's own testing-framework runner via
    # `nix run .#test`, while this package's tests are cl-weave DESCRIBE/IT
    # specs run through `(asdf:test-system :cl-cc-prolog-tools)`. clProlog
    # and clWeave are plain `sbcl.buildASDFSystem` derivations built from
    # source (see flake.nix) — their installPhase mirrors the source tree
    # directly into $out (e.g. $out/cl-prolog.asd), unlike nixpkgs' curated
    # sblcPackages set which nests under /share/common-lisp/source/, so the
    # registry entry is just "${clProlog}//", not a share/ subpath.
    cl-cc-prolog-tools-tests =
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
  };
}
