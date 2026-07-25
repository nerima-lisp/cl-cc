{
  description = "CL-CC — self-hosting Common Lisp compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    # cl-prolog and cl-weave back packages/prolog-tools (call-graph analysis
    # built on the external cl-prolog engine, tested with cl-weave). Pulled
    # in as plain (non-flake) source trees and built with cl-cc's own
    # sbcl.buildASDFSystem below, the same way cl-cc builds its own internal
    # packages — cl-prolog's own flake only declares Linux `systems`, so
    # depending on its `packages.<system>` output would break this flake's
    # aarch64-darwin support; evaluating cl-prolog/cl-weave as flakes at all
    # would also drag in their own transitive inputs (paredit-cli, etc.) for
    # no benefit, since only their source is needed here.
    cl-prolog = {
      url = "github:nerima-lisp/cl-prolog";
      flake = false;
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave";
      flake = false;
    };
    # Five further nerima-lisp toolkits are adopted the same way (plain source
    # trees, built with cl-cc's own sbcl.buildASDFSystem): cl-parser-kit backs
    # packages/parse's tokenizer/combinator/Pratt layer, cl-dataflow backs the
    # packages/pipeline pass-graph orchestration, cl-boundary-kit provides the
    # testable I/O boundaries used by packages/cli + packages/repl, cl-cli is
    # the declarative argument parser behind packages/cli, and cl-tty-kit
    # provides the ANSI/screen/input primitives for the interactive REPL.
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit";
      flake = false;
    };
    cl-dataflow = {
      url = "github:nerima-lisp/cl-dataflow";
      flake = false;
    };
    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit";
      flake = false;
    };
    # cl-log-kit backs cl-boundary-kit's logging boundary as of 0.5.0 (split
    # out of cl-boundary-kit itself); pulled in the same way as the other
    # nerima-lisp toolkits above.
    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit";
      flake = false;
    };
    cl-cli = {
      url = "github:nerima-lisp/cl-cli";
      flake = false;
    };
    cl-tty-kit = {
      url = "github:nerima-lisp/cl-tty-kit";
      flake = false;
    };
    # cl-cc-ast and cl-cc-type are the first subsystems split out of this
    # monorepo (see docs/repo-split-design.md). Adopted the same way as the
    # toolkits above: plain source trees, built with cl-cc's own
    # sbcl.buildASDFSystem and injected into the internal system graph under
    # their original names, so every `deps = [ "cl-cc-ast" ... ]` resolves to
    # the external derivation transparently. cl-cc-type depends on cl-cc-ast.
    cl-cc-ast = {
      url = "github:nerima-lisp/cl-cc-ast";
      flake = false;
    };
    cl-cc-type = {
      url = "github:nerima-lisp/cl-cc-type";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      imports = [ inputs.treefmt-nix.flakeModule ];
      flake.overlays.default = final: _prev: { cl-cc = self.packages.${final.system}.default; };
      perSystem =
        { pkgs, ... }:
        let
          inherit (pkgs) lib;
          dispatchModule = import ./nix/dispatch-sem-fix.nix { inherit pkgs lib; };
          dispatchSemFix = dispatchModule.dispatchSemFix or null;
          sbclModule = import ./nix/sbcl.nix { inherit pkgs; };
          inherit (sbclModule) sbcl sbclBootstrap;
          clProlog = sbcl.buildASDFSystem {
            pname = "cl-prolog";
            version = "0.8.0";
            src = inputs.cl-prolog;
            systems = [ "cl-prolog" ];
          };
          clWeave = sbcl.buildASDFSystem {
            pname = "cl-weave";
            version = "1.0.0";
            src = inputs.cl-weave;
            systems = [ "cl-weave" ];
          };
          # cl-parser-kit is self-contained. cl-boundary-kit pulls the
          # cl-log-kit toolkit above. cl-dataflow, cl-cli, and cl-tty-kit all
          # depend on the external cl-prolog engine in production, so
          # clProlog is threaded into their lispLibs and thereby reaches any
          # cl-cc package that consumes them transitively.
          clParserKit = sbcl.buildASDFSystem {
            pname = "cl-parser-kit";
            version = "0.4.0";
            src = inputs.cl-parser-kit;
            systems = [ "cl-parser-kit" ];
          };
          clDataflow = sbcl.buildASDFSystem {
            pname = "cl-dataflow";
            version = "0.4.0";
            src = inputs.cl-dataflow;
            systems = [ "cl-dataflow" ];
            lispLibs = [ clProlog ];
          };
          clLogKit = sbcl.buildASDFSystem {
            pname = "cl-log-kit";
            version = "1.6.0";
            src = inputs.cl-log-kit;
            systems = [ "cl-log-kit" ];
          };
          clBoundaryKit = sbcl.buildASDFSystem {
            pname = "cl-boundary-kit";
            version = "0.5.0";
            src = inputs.cl-boundary-kit;
            systems = [ "cl-boundary-kit" ];
            lispLibs = [ clLogKit ];
          };
          clCli = sbcl.buildASDFSystem {
            pname = "cl-cli";
            version = "0.3.0";
            src = inputs.cl-cli;
            systems = [ "cl-cli" ];
            lispLibs = [ clProlog ];
          };
          clTtyKit = sbcl.buildASDFSystem {
            pname = "cl-tty-kit";
            version = "0.6.0";
            src = inputs.cl-tty-kit;
            systems = [ "cl-tty-kit" ];
            lispLibs = [ clProlog ];
          };
          # First subsystems split out of the monorepo. clCcAst is a
          # dependency-free leaf; clCcType depends on it. Injected into the
          # internal system graph by name in nix/asdf-systems.nix.
          clCcAst = sbcl.buildASDFSystem {
            pname = "cl-cc-ast";
            version = "0.1.0";
            src = inputs.cl-cc-ast;
            systems = [ "cl-cc-ast" ];
          };
          clCcType = sbcl.buildASDFSystem {
            pname = "cl-cc-type";
            version = "0.1.0";
            src = inputs.cl-cc-type;
            systems = [ "cl-cc-type" ];
            lispLibs = [ clCcAst ];
          };
          asdf = import ./nix/asdf-systems.nix {
            inherit
              pkgs
              lib
              sbcl
              clProlog
              clWeave
              clParserKit
              clDataflow
              clBoundaryKit
              clCli
              clTtyKit
              clCcAst
              clCcType
              ;
          };
          inherit (asdf)
            productionAsdfSystems
            testAsdfSystems
            sbclWithCLCC
            sbclWithJavascriptTests
            sbclWithTests
            cl-cc-prolog-tools
            cl-cc-prolog-tools-test
            ;
          binaryModule = import ./nix/binary.nix { inherit pkgs lib sbclWithCLCC; };
          testImage = import ./nix/sbcl-image.nix {
            inherit
              pkgs
              lib
              sbclWithTests
              dispatchSemFix
              ;
          };
          appsModule = import ./nix/apps.nix {
            inherit
              pkgs
              lib
              sbclWithCLCC
              sbclWithJavascriptTests
              sbclWithTests
              sbclBootstrap
              dispatchSemFix
              testImage
              ;
          };
          checksModule = import ./nix/checks.nix {
            inherit
              pkgs
              lib
              sbclWithTests
              clProlog
              clWeave
              ;
            inherit (appsModule) apps;
            packagesDefault = binaryModule.default;
          };
          devshellModule = import ./nix/devshell.nix { inherit pkgs lib sbclWithCLCC; };
        in
        {
          packages =
            productionAsdfSystems
            // testAsdfSystems
            // {
              inherit (binaryModule) default;
              inherit cl-cc-prolog-tools cl-cc-prolog-tools-test;
              test-image = testImage;
            };
          inherit (appsModule) apps;
          inherit (checksModule) checks;
          inherit (devshellModule) devShells treefmt;
        };
    };
}
