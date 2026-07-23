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
            version = "0.6.0";
            src = inputs.cl-prolog;
            systems = [ "cl-prolog" ];
          };
          clWeave = sbcl.buildASDFSystem {
            pname = "cl-weave";
            version = "0.8.0";
            src = inputs.cl-weave;
            systems = [ "cl-weave" ];
          };
          asdf = import ./nix/asdf-systems.nix { inherit pkgs lib sbcl clProlog clWeave; };
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
            inherit pkgs lib sbclWithTests clProlog clWeave;
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
