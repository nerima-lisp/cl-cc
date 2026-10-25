{
  description = "Self-hosting Common Lisp compiler targeting bytecode, native code, PHP, and JavaScript";

  inputs = {
    # nixos-unstable, not nixpkgs-unstable: it advances only after the NixOS
    # release tests pass, so it is less likely to land a broken build.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.7.4";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    # Every sibling is pinned to an immutable ref — a release tag where one
    # exists, a commit otherwise. A bare `github:nerima-lisp/cl-weave` follows
    # that repository's default branch, so an upstream push to main would break
    # this repository's CI with no change here and no warning.
    #
    # All of them are taken as PLAIN SOURCE TREES (`flake = false`) and built
    # with cl-cc's own `sbcl.buildASDFSystem` below, exactly the way cl-cc
    # builds its own packages/ subsystems. Two reasons, both still current:
    # several of these flakes declare Linux-only `systems`, so consuming their
    # `packages.<system>` output would break aarch64-darwin here; and evaluating
    # them as flakes would drag in their transitive inputs (paredit-cli and so
    # on) to obtain nothing but their source.
    #
    # PACKAGE_STANDARD.md requires `inputs.nixpkgs.follows` on every input. A
    # `flake = false` input HAS no inputs, and Nix rejects an override for one,
    # so the attribute is omitted here. The rule exists to stop each input from
    # locking its own nixpkgs and inflating flake.lock; taking source trees
    # achieves that outcome more completely, since no second nixpkgs enters the
    # lock at all. treefmt-nix and cl-nix-forge, the real flake inputs, carry `follows`.
    cl-prolog-kit = {
      url = "github:nerima-lisp/cl-prolog-kit/v1.5.0";
      flake = false;
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/32671560d3d15d4ecf33b9d8af862ea60737841b";
      flake = false;
    };
    # cl-parser-kit backs packages/parse's tokenizer/combinator/Pratt layer,
    # cl-dataflow-kit backs the packages/pipeline pass-graph orchestration,
    # cl-boundary-kit provides the testable I/O boundaries used by
    # packages/cli + packages/repl, cl-cli is the declarative argument parser
    # behind packages/cli, and cl-tty-kit provides the ANSI/screen/input
    # primitives for the interactive REPL.
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/6379014e6d85bdaecc57157611baa4ddf4b2b206";
      flake = false;
    };
    cl-dataflow-kit = {
      url = "github:nerima-lisp/cl-dataflow-kit/v1.2.0";
      flake = false;
    };
    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/62e4a7df3e52061c38b7ff4f596436bdda985f71";
      flake = false;
    };
    # cl-log-kit backs cl-boundary-kit's logging boundary (split out of
    # cl-boundary-kit itself); pulled in the same way as the toolkits above.
    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit/580ebe23e4106edb8dcc89c0d4732f585c17e3f9";
      flake = false;
    };
    cl-cli = {
      url = "github:nerima-lisp/cl-cli/v1.0.1";
      flake = false;
    };
    cl-tty-kit = {
      url = "github:nerima-lisp/cl-tty-kit/add24964a286720baa9e08722f4b611fcefdf22f";
      flake = false;
    };
    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/5885db990560b1aa3992a88daceea010b9adc1d2";
      flake = false;
    };
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/da83905cad2bcb28518ebb28cac6628c654bb328";
      flake = false;
    };
    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/ec565470b634605e23b0d6eef0645eb15e8e3356";
      flake = false;
    };
    # Additional runtime dependencies, pinned to the revisions in the
    # standalone runtime's lockfile.
    cl-observability-kit = {
      url = "github:nerima-lisp/cl-observability-kit/c7036f43ac8b69457a2979c2cb2c6806452bd728";
      flake = false;
    };
    cl-deflate-kit = {
      url = "github:nerima-lisp/cl-deflate-kit/711eb7540ac43869ae08060b6153e5dc768a8382";
      flake = false;
    };
    cl-crypto-kit = {
      url = "github:nerima-lisp/cl-crypto-kit/41ed7e90f736591aa1961bc6deb0741c73392a60";
      flake = false;
    };
    cl-resilience-kit = {
      url = "github:nerima-lisp/cl-resilience-kit/741d42399ad781244bd36750c6626bd9f4e8b16b";
      flake = false;
    };
    # cl-cc-ast and cl-cc-type are the first subsystems split out of this
    # repository. Injected into the internal system graph under their original
    # names by nix/asdf-systems.nix, so every `deps = [ "cl-cc-ast" ... ]`
    # resolves to the external derivation. cl-cc-type depends on cl-cc-ast.
    #
    # Pinned by commit, not tag: neither repository has cut a release yet.
    # A commit is a stronger pin than a tag (a tag can be moved), so this
    # satisfies the invariant the tag rule exists to protect. Swap in `/v0.1.0`
    # once those releases are tagged.
    cl-cc-ast = {
      url = "github:nerima-lisp/cl-cc-ast/8289d9cec15df0ac73011e3ea872dc3c669f9540";
      flake = false;
    };
    cl-cc-type = {
      url = "github:nerima-lisp/cl-cc-type/1fd5b0afb0245edb965d33cc492988e6bb9f2caf";
      flake = false;
    };
    cl-cc-binary = {
      url = "github:nerima-lisp/cl-cc-binary/3fa4d4b00f4a6298f4b1e78c11930a74f14a8da2";
      flake = false;
    };
    cl-cc-bootstrap = {
      url = "github:nerima-lisp/cl-cc-bootstrap/837e41fd105c632088033be433ef3f1352eda462";
      flake = false;
    };
    cl-cc-vm = {
      url = "github:nerima-lisp/cl-cc-vm/695631a24168ba5d789739d3764308dc7009c805";
      flake = false;
    };
    cl-cc-mir = {
      url = "github:nerima-lisp/cl-cc-mir/3c3cba4b57ac10d7ad52a7bf6aaf421e598dd198";
      flake = false;
    };
    cl-cc-cps = {
      url = "github:nerima-lisp/cl-cc-cps/aa6b7cbc98fa6e5f597104acb7c6b09ba9c2d796";
      flake = false;
    };
    cl-cc-expand = {
      url = "github:nerima-lisp/cl-cc-expand/105aa913ba2680bd0e3ce68725a31dc6ddfcbf56";
      flake = false;
    };
    cl-cc-optimize = {
      url = "github:nerima-lisp/cl-cc-optimize/87c44f3a5ae7075ca1f087e7c9339f70a07bd05a";
      flake = false;
    };
    cl-cc-codegen-native = {
      url = "github:nerima-lisp/cl-cc-codegen-native/3bc0966c82cbd11d83899d1661ea428dc7bb5ee1";
      flake = false;
    };
    cl-cc-parse = {
      url = "github:nerima-lisp/cl-cc-parse/c78ce5307019193523ed1f54ed652e6e8a97eda6";
      flake = false;
    };
    cl-cc-php = {
      url = "github:nerima-lisp/cl-cc-php/d59cdbf72bffacf77a276dfb3492f2be35e8a363";
      flake = false;
    };
    cl-cc-javascript = {
      url = "github:nerima-lisp/cl-cc-javascript/ea2e920ed36ba203943fc62bcac1d70d79dcfa29";
      flake = false;
    };
    cl-cc-runtime = {
      url = "github:nerima-lisp/cl-cc-runtime/ea817be8dc506b45e8e9b4f14073422108a1e7ec";
      flake = false;
    };
    # Dependencies of the standalone cl-cc-runtime that the in-tree copy did
    # not have. Existing toolkit pins above provide logging, process, and JSON;
    # the additional source pins above provide observability, compression,
    # cryptography, and resilience.
    cl-process-kit = {
      url = "github:nerima-lisp/cl-process-kit/1276a8b8db6120ac15a0911c255a586b3a030ca1";
      flake = false;
    };
    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/dace990f643d1dd07639ce57a2559c5143850cce";
      flake = false;
    };
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/021c00df3f4dd18cad7206221125d1596db2fae5";
      flake = false;
    };
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/0ef20d36a0a12800f231172227bd94034611c639";
      flake = false;
    };
  };

  # Plain `outputs` with `forAllSystems`, not flake-parts. The whole flake is
  # visible in one file this way: `nix flake show` output maps directly onto the
  # attribute names written below, with no module system deciding what appears.
  outputs =
    inputs@{
      self,
      nixpkgs,
      treefmt-nix,
      ...
    }:
    let
      # x86_64-linux is verified by CI; aarch64-darwin is verified by the
      # development machine on every local `nix flake check`. aarch64-linux and
      # x86_64-darwin are not declared because nothing runs them (ADR-0078).
      # `ci.yml` deliberately omits --all-systems: the runner is x86_64-linux
      # and would fail evaluating the darwin derivations.
      # Only what is verified, and CI verifies exactly one platform:
      # x86_64-linux. aarch64-darwin was dropped in the 2026-08-01 revision of
      # PACKAGE_STANDARD.md -- its only gate was the maintainer remembering to
      # run `nix flake check` locally, and a gate nobody can observe being
      # skipped is not a gate.
      #
      # Consequence, accepted deliberately: every per-system output --
      # packages, checks, apps AND devShells -- is generated from this one
      # list, so `nix develop` and `nix build` do not work on macOS.
      # Development happens on Linux.
      systems = [
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # Single source of truth for a package's version: the `:version` form in
      # its .asd. Before this, the flake said 0.8.0, cl-cc.asd said 0.1.0 and
      # the newest tag was v0.1.0 — three answers to one question, and nothing
      # to say which was right. A release now edits one line.
      #
      # Nix regexes are whole-string anchored and `.` never spans newlines, so
      # the version is extracted from non-comment lines rather than with a
      # single multi-line match. ASDF metadata may share a line.
      asdVersion =
        asd:
        let
          lines = nixpkgs.lib.splitString "\n" (builtins.readFile asd);
          versionPattern = "[^;]*[[:space:]]*:version \"([^\"]*)\".*";
          versionLine = builtins.head (
            builtins.filter (
              line: builtins.match "[[:space:]]*;.*" line == null && builtins.match versionPattern line != null
            ) lines
          );
        in
        builtins.head (builtins.match versionPattern versionLine);

      version = asdVersion ./cl-cc.asd;

      # The same treatment for every sibling: read the version out of the source
      # tree actually being built, rather than restating it here where it can
      # (and did) fall behind the pinned ref.
      siblingVersion = name: asdVersion "${inputs.${name}}/${name}.asd";

      # treefmt drives `nix fmt` and the `checks.<system>.formatting` gate.
      # Scope is Nix only. Prettier over *.md/*.yml used to be enabled here; it
      # is not any more, because a YAML formatter rewrites the GitHub Actions
      # `on:` key into the boolean `true`, and reformatting Markdown would churn
      # every page under docs/ for no review value.
      treefmtEval = forAllSystems (
        system:
        treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          settings.global.excludes = [
            "flake.lock"
            "LICENSE*"
          ];
        }
      );

      # Everything system-dependent is assembled once per system here, and the
      # top-level outputs below only project attributes out of it. Assembling it
      # separately inside `packages`, `checks`, `apps` and `devShells` would
      # re-evaluate the entire ASDF system graph four times.
      perSystem = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend (
            final: prev: {
              sbcl = final.wrapLisp {
                pkg = prev.sbcl.overrideAttrs (_: {
                  version = "2.6.9";
                  src = final.fetchurl {
                    name = "sbcl-2.6.9-source.tar.bz2";
                    url = "https://sourceforge.net/projects/sbcl/files/sbcl/2.6.9/sbcl-2.6.9-source.tar.bz2/download";
                    hash = "sha256-xv0dc1Vw60/zTK+WCZiMp37QvRK1XQmk/tB1vokNpRM=";
                  };
                });
                faslExt = "fasl";
                flags = [
                  "--dynamic-space-size"
                  "3000"
                ];
              };
            }
          );
          inherit (pkgs) lib;

          dispatchModule = import ./nix/dispatch-sem-fix.nix { inherit pkgs lib; };
          dispatchSemFix = dispatchModule.dispatchSemFix or null;
          sbclModule = import ./nix/sbcl.nix { inherit pkgs; };
          sbclBase = sbclModule.sbcl;
          buildAsdfSystem =
            args:
            (sbclBase.buildASDFSystem args)
            // {
              asdfMetadata = {
                inherit (args) pname src systems;
                lispLibs = args.lispLibs or [ ];
              };
            };
          sbcl = sbclBase // {
            buildASDFSystem = buildAsdfSystem;
          };
          inherit (sbclModule) sbclBootstrap;

          # Sibling toolkits, built from the pinned source trees. The `version`
          # of each mirrors the tag or commit its input is pinned to.
          clPrologKit = sbcl.buildASDFSystem {
            pname = "cl-prolog-kit";
            version = siblingVersion "cl-prolog-kit";
            src = inputs.cl-prolog-kit;
            systems = [
              "cl-prolog-kit"
              "cl-prolog-kit/callgraph"
            ];
          };
          clWeave = sbcl.buildASDFSystem {
            pname = "cl-weave";
            version = siblingVersion "cl-weave";
            src = inputs.cl-weave;
            systems = [ "cl-weave" ];
          };
          # cl-parser-kit is self-contained. cl-boundary-kit uses cl-host-kit.
          # cl-dataflow-kit and cl-cli depend on the external cl-prolog-kit
          # engine in production. cl-tty-kit instead depends on codec and
          # concurrency libraries in its current main system.
          clParserKit = sbcl.buildASDFSystem {
            pname = "cl-parser-kit";
            version = siblingVersion "cl-parser-kit";
            src = inputs.cl-parser-kit;
            systems = [ "cl-parser-kit" ];
          };
          clDataflowKit = sbcl.buildASDFSystem {
            pname = "cl-dataflow-kit";
            version = siblingVersion "cl-dataflow-kit";
            src = inputs.cl-dataflow-kit;
            systems = [ "cl-dataflow-kit" ];
            lispLibs = [
              clBoundaryKit
              clConcurrentKit
              clDateKit
              clHostKit
              clPrologKit
            ];
          };
          clLogKit = sbcl.buildASDFSystem {
            pname = "cl-log-kit";
            version = siblingVersion "cl-log-kit";
            src = inputs.cl-log-kit;
            systems = [ "cl-log-kit" ];
            lispLibs = [
              clBoundaryKit
              clConcurrentKit
              clDateKit
              clHostKit
            ];
          };
          clBoundaryKit = sbcl.buildASDFSystem {
            pname = "cl-boundary-kit";
            version = siblingVersion "cl-boundary-kit";
            src = inputs.cl-boundary-kit;
            systems = [ "cl-boundary-kit" ];
            lispLibs = [ clHostKit ];
          };
          clCli = sbcl.buildASDFSystem {
            pname = "cl-cli";
            version = siblingVersion "cl-cli";
            src = inputs.cl-cli;
            systems = [ "cl-cli" ];
            lispLibs = [ clPrologKit ];
          };
          clTtyKit = sbcl.buildASDFSystem {
            pname = "cl-tty-kit";
            version = siblingVersion "cl-tty-kit";
            src = inputs.cl-tty-kit;
            systems = [ "cl-tty-kit" ];
            lispLibs = [
              clBoundaryKit
              clCodecKit
              clConcurrentKit
              clDateKit
              clHostKit
            ];
          };
          clRegexKit = sbcl.buildASDFSystem {
            pname = "cl-regex-kit";
            version = siblingVersion "cl-regex-kit";
            src = inputs.cl-regex-kit;
            systems = [ "cl-regex-kit" ];
            lispLibs = [
              clBoundaryKit
              clConcurrentKit
              clDateKit
              clHostKit
              clParserKit
            ];
          };
          clHostKit = sbcl.buildASDFSystem {
            pname = "cl-host-kit";
            version = siblingVersion "cl-host-kit";
            src = inputs.cl-host-kit;
            systems = [ "cl-host-kit" ];
          };
          # Subsystems split out of this repository. clCcAst is a
          # dependency-free leaf; clCcType depends on it.
          #
          # These are now the only definitions of those two systems. packages/
          # {ast,type} used to define them as well, and because ASDF resolved
          # whichever the registry reached first -- always the in-tree copy,
          # since this tree is scanned ahead of the injected derivations -- the
          # repositories were never actually compiled and the two sides drifted.
          # The in-tree copies have been removed; packages/{ast,type}/tests
          # remain and now exercise these derivations.
          clCcAst = sbcl.buildASDFSystem {
            pname = "cl-cc-ast";
            version = siblingVersion "cl-cc-ast";
            src = inputs.cl-cc-ast;
            systems = [ "cl-cc-ast" ];
          };
          clCcType = sbcl.buildASDFSystem {
            pname = "cl-cc-type";
            version = siblingVersion "cl-cc-type";
            src = inputs.cl-cc-type;
            systems = [ "cl-cc-type" ];
            lispLibs = [ clCcAst ];
          };
          clProcessKit = sbcl.buildASDFSystem {
            pname = "cl-process-kit";
            version = siblingVersion "cl-process-kit";
            src = inputs.cl-process-kit;
            systems = [ "cl-process-kit" ];
            lispLibs = [
              clBoundaryKit
              clCodecKit
              clConcurrentKit
              clDateKit
              clHostKit
              clLogKit
            ];
          };
          clJsonKit = sbcl.buildASDFSystem {
            pname = "cl-json-kit";
            version = siblingVersion "cl-json-kit";
            src = inputs.cl-json-kit;
            systems = [ "cl-json-kit" ];
          };
          clObservabilityKit = sbcl.buildASDFSystem {
            pname = "cl-observability-kit";
            version = siblingVersion "cl-observability-kit";
            src = inputs.cl-observability-kit;
            systems = [
              "cl-observability-kit"
              "cl-observability-kit/prometheus"
              "cl-observability-kit/otlp"
            ];
            lispLibs = [
              clBoundaryKit
              clConcurrentKit
            ];
          };
          clDeflateKit = sbcl.buildASDFSystem {
            pname = "cl-deflate-kit";
            version = siblingVersion "cl-deflate-kit";
            src = inputs.cl-deflate-kit;
            systems = [ "cl-deflate-kit" ];
          };
          clCryptoKit = sbcl.buildASDFSystem {
            pname = "cl-crypto-kit";
            version = siblingVersion "cl-crypto-kit";
            src = inputs.cl-crypto-kit;
            systems = [ "cl-crypto-kit" ];
          };
          clResilienceKit = sbcl.buildASDFSystem {
            pname = "cl-resilience-kit";
            version = siblingVersion "cl-resilience-kit";
            src = inputs.cl-resilience-kit;
            systems = [ "cl-resilience-kit" ];
            lispLibs = [
              clBoundaryKit
              clConcurrentKit
              clDateKit
            ];
          };
          clDateKit = sbcl.buildASDFSystem {
            pname = "cl-date-kit";
            version = siblingVersion "cl-date-kit";
            src = inputs.cl-date-kit;
            systems = [ "cl-date-kit" ];
          };
          clConcurrentKit = sbcl.buildASDFSystem {
            pname = "cl-concurrent-kit";
            version = siblingVersion "cl-concurrent-kit";
            src = inputs.cl-concurrent-kit;
            systems = [ "cl-concurrent-kit" ];
            lispLibs = [
              clBoundaryKit
              clDateKit
              clHostKit
            ];
          };
          clCodecKit = sbcl.buildASDFSystem {
            pname = "cl-codec-kit";
            version = siblingVersion "cl-codec-kit";
            src = inputs.cl-codec-kit;
            systems = [ "cl-codec-kit" ];
          };
          clCcRuntime = sbcl.buildASDFSystem {
            pname = "cl-cc-runtime";
            version = siblingVersion "cl-cc-runtime";
            src = inputs.cl-cc-runtime;
            systems = [ "cl-cc-runtime" ];
            lispLibs = [
              clLogKit
              clProcessKit
              clJsonKit
              clObservabilityKit
              clDeflateKit
              clCryptoKit
              clResilienceKit
            ];
          };
          clCcBootstrap = sbcl.buildASDFSystem {
            pname = "cl-cc-bootstrap";
            version = siblingVersion "cl-cc-bootstrap";
            src = inputs.cl-cc-bootstrap;
            systems = [ "cl-cc-bootstrap" ];
          };
          clCcVm = sbcl.buildASDFSystem {
            pname = "cl-cc-vm";
            version = siblingVersion "cl-cc-vm";
            src = inputs.cl-cc-vm;
            systems = [ "cl-cc-vm" ];
            lispLibs = [
              clCcBootstrap
              clCcRuntime
              clRegexKit
              clTtyKit
              clHostKit
            ];
          };
          clCcMir = sbcl.buildASDFSystem {
            pname = "cl-cc-mir";
            version = siblingVersion "cl-cc-mir";
            src = inputs.cl-cc-mir;
            systems = [
              "cl-cc-mir"
              "cl-cc-target"
            ];
          };
          clCcTarget = clCcMir;
          clCcCps = sbcl.buildASDFSystem {
            pname = "cl-cc-cps";
            version = siblingVersion "cl-cc-cps";
            src = inputs.cl-cc-cps;
            systems = [ "cl-cc-cps" ];
            lispLibs = [
              clCcBootstrap
              clCcAst
            ];
          };
          clCcExpand = sbcl.buildASDFSystem {
            pname = "cl-cc-expand";
            version = siblingVersion "cl-cc-expand";
            src = inputs.cl-cc-expand;
            systems = [ "cl-cc-expand" ];
            lispLibs = [
              clCcBootstrap
              clCcType
              clCcVm
              clHostKit
            ];
          };
          clCcOptimize = sbcl.buildASDFSystem {
            pname = "cl-cc-optimize";
            version = siblingVersion "cl-cc-optimize";
            src = inputs.cl-cc-optimize;
            systems = [ "cl-cc-optimize" ];
            lispLibs = [
              clCcVm
              clCcType
              clCcAst
              clPrologKit
              clParserKit
              clHostKit
            ];
          };
          # One source tree, three systems -- the repository holds them
          # together because the boundaries between them move. Each is built as
          # its own derivation so dependents keep naming the system they use.
          clCcRegalloc = sbcl.buildASDFSystem {
            pname = "cl-cc-regalloc";
            version = siblingVersion "cl-cc-codegen-native";
            src = inputs.cl-cc-codegen-native;
            systems = [ "cl-cc-regalloc" ];
            lispLibs = [
              clCcVm
              clCcMir
              clCcTarget
              clCcOptimize
            ];
          };
          clCcCodegen = sbcl.buildASDFSystem {
            pname = "cl-cc-codegen";
            version = siblingVersion "cl-cc-codegen-native";
            src = inputs.cl-cc-codegen-native;
            systems = [ "cl-cc-codegen" ];
            lispLibs = [
              clCcBootstrap
              clCcVm
              clCcMir
              clCcTarget
              clCcBinary
              clCcOptimize
              clCcRegalloc
              clProcessKit
              clHostKit
            ];
          };
          clCcEmit = sbcl.buildASDFSystem {
            pname = "cl-cc-emit";
            version = siblingVersion "cl-cc-codegen-native";
            src = inputs.cl-cc-codegen-native;
            systems = [ "cl-cc-emit" ];
            lispLibs = [
              clCcVm
              clCcAst
              clCcMir
              clCcOptimize
              clCcCodegen
            ];
          };
          clCcParse = sbcl.buildASDFSystem {
            pname = "cl-cc-parse";
            version = siblingVersion "cl-cc-parse";
            src = inputs.cl-cc-parse;
            systems = [ "cl-cc-parse" ];
            lispLibs = [
              clCcAst
              clCcBootstrap
            ];
          };
          clCcPhp = sbcl.buildASDFSystem {
            pname = "cl-cc-php";
            version = siblingVersion "cl-cc-php";
            src = inputs.cl-cc-php;
            systems = [ "cl-cc-php" ];
            lispLibs = [
              clCcAst
              clCcBootstrap
              clCcParse
              clCcVm
              clHostKit
              clJsonKit
            ];
          };
          clCcJavascript = sbcl.buildASDFSystem {
            pname = "cl-cc-javascript";
            version = siblingVersion "cl-cc-javascript";
            src = inputs.cl-cc-javascript;
            systems = [ "cl-cc-javascript" ];
            lispLibs = [
              clCcAst
              clCcBootstrap
              clCcParse
              clCcVm
              clBoundaryKit
              clDateKit
              clJsonKit
              clConcurrentKit
              clHostKit
            ];
          };
          clCcBinary = sbcl.buildASDFSystem {
            pname = "cl-cc-binary";
            version = siblingVersion "cl-cc-binary";
            src = inputs.cl-cc-binary;
            systems = [ "cl-cc-binary" ];
            lispLibs = [ clProcessKit ];
          };

          asdf = import ./nix/asdf-systems.nix {
            inherit
              pkgs
              lib
              sbcl
              clPrologKit
              clWeave
              clParserKit
              clDataflowKit
              clBoundaryKit
              clCli
              clTtyKit
              clRegexKit
              clCcAst
              clCcType
              clCcBinary
              clCcRuntime
              clCcBootstrap
              clCcVm
              clCcMir
              clCcTarget
              clCcCps
              clCcExpand
              clCcOptimize
              clCcRegalloc
              clCcCodegen
              clCcEmit
              clCcParse
              clCcPhp
              clCcJavascript
              ;
            buildAsdfSystem = sbcl.buildASDFSystem;
          };
          inherit (asdf)
            productionAsdfSystems
            testAsdfSystems
            sbclWithCLCC
            sbclWithJavascriptTests
            sbclWithJitTests
            sbclWithTests
            cl-cc-prolog-tools
            cl-cc-prolog-tools-test
            ;

          childFlakeSources = [
            {
              name = "bootstrap";
              source = inputs.cl-cc-bootstrap;
            }
            {
              name = "ast";
              source = inputs.cl-cc-ast;
            }
            {
              name = "parse";
              source = inputs.cl-cc-parse;
            }
            {
              name = "expand";
              source = inputs.cl-cc-expand;
            }
            {
              name = "cps";
              source = inputs.cl-cc-cps;
            }
            {
              name = "type";
              source = inputs.cl-cc-type;
            }
            {
              name = "optimize";
              source = inputs.cl-cc-optimize;
            }
            {
              name = "mir";
              source = inputs.cl-cc-mir;
            }
            {
              name = "codegen-native";
              source = inputs.cl-cc-codegen-native;
            }
            {
              name = "binary";
              source = inputs.cl-cc-binary;
            }
            {
              name = "vm";
              source = inputs.cl-cc-vm;
            }
            {
              name = "runtime";
              source = inputs.cl-cc-runtime;
            }
            {
              name = "php";
              source = inputs.cl-cc-php;
            }
            {
              name = "javascript";
              source = inputs.cl-cc-javascript;
            }
          ];

          asdfSystemDerivations = lib.unique (
            builtins.attrValues productionAsdfSystems
            ++ builtins.attrValues testAsdfSystems
            ++ [
              clPrologKit
              clWeave
              clParserKit
              clDataflowKit
              clLogKit
              clBoundaryKit
              clCli
              clTtyKit
              clRegexKit
              clHostKit
              clCcAst
              clCcType
              clProcessKit
              clJsonKit
              clDateKit
              clConcurrentKit
              clCodecKit
              clCcRuntime
              clCcBootstrap
              clCcVm
              clCcMir
              clCcCps
              clCcExpand
              clCcOptimize
              clCcRegalloc
              clCcCodegen
              clCcEmit
              clCcParse
              clCcPhp
              clCcJavascript
              clCcBinary
              cl-cc-prolog-tools
              cl-cc-prolog-tools-test
            ]
          );

          rootLock = builtins.fromJSON (builtins.readFile ./flake.lock);

          binaryModule = import ./nix/binary.nix {
            inherit
              pkgs
              lib
              sbclWithCLCC
              version
              ;
          };
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

          # Rendered documentation site (Material for MkDocs). Builds fully
          # offline: Material bundles its assets, so the Nix sandbox needs no
          # network. --strict promotes a broken link or a page missing from the
          # nav into a build failure.
          docs = pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-cc-docs";
            inherit version;
            src = lib.fileset.toSource {
              root = ./docs;
              fileset = lib.fileset.unions [
                ./docs/mkdocs.yml
                ./docs/src
              ];
            };
            nativeBuildInputs = [ pkgs.python3Packages.mkdocs-material ];
            buildPhase = ''
              runHook preBuild
              mkdocs build --strict --config-file mkdocs.yml --site-dir "$out"
              runHook postBuild
            '';
            dontInstall = true;
            meta = {
              description = "Rendered MkDocs (Material) documentation for cl-cc";
              homepage = "https://github.com/nerima-lisp/cl-cc";
              license = lib.licenses.mit;
            };
          };

          checksModule = import ./nix/checks.nix {
            inherit
              pkgs
              lib
              version
              sbclWithJitTests
              sbclWithTests
              clPrologKit
              clWeave
              clCcAst
              rootLock
              childFlakeSources
              asdfSystemDerivations
              productionAsdfSystems
              ;
            inherit (appsModule) apps;
          };
          devshellModule = import ./nix/devshell.nix { inherit pkgs lib sbclWithCLCC; };
        in
        {
          packages =
            productionAsdfSystems
            // testAsdfSystems
            // {
              inherit (binaryModule) default;
              inherit cl-cc-prolog-tools cl-cc-prolog-tools-test docs;
              test-image = testImage;
            };
          inherit (appsModule) apps;
          inherit (checksModule)
            testSuite
            testSuites
            jitTests
            prologToolsTests
            crossRepoInternalRefs
            contextMap
            toolkitPinConsistency
            lisplibsConsistency
            ;
          inherit (devshellModule) devShells;
        }
      );
    in
    {
      packages = forAllSystems (system: perSystem.${system}.packages);

      # Granularity lives HERE, not in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute below as its own derivation, in
      # parallel, with build caching. Want the test suite, the formatter and the
      # docs reported separately? That is what these three names are for. Adding
      # a second job to ci.yml instead would re-implement the same scheduling
      # and lose cache sharing between the pieces.
      checks = forAllSystems (system: {
        # The SBCL test suite, run through ./run-tests.lisp.
        default = perSystem.${system}.testSuite;
        tests-unit = perSystem.${system}.testSuites.unit;
        tests-integration = perSystem.${system}.testSuites.integration;
        tests-e2e = perSystem.${system}.testSuites.e2e;
        tests-conformance = perSystem.${system}.testSuites.conformance;

        # Fails `nix flake check` when any tracked Nix file is unformatted,
        # which is what makes the formatter a gate rather than a suggestion.
        formatting = treefmtEval.${system}.config.build.check self;

        # `packages.docs` runs `mkdocs build --strict`, so a broken link or a
        # page missing from the nav fails right here. Without this the docs are
        # only ever built by docs.yml, which runs after the merge to main —
        # a break would surface as a failed deploy rather than a failed pull
        # request.
        docs = perSystem.${system}.packages.docs;

        # The standalone binary has to keep linking, not just the library.
        build = perSystem.${system}.packages.default;

        # packages/prolog-tools has its own cl-weave suite, driven by
        # `asdf:test-system` rather than by cl-cc's runner.
        cl-cc-prolog-tools-tests = perSystem.${system}.prologToolsTests;

        cross-repo-internal-refs = perSystem.${system}.crossRepoInternalRefs;

        context-map = perSystem.${system}.contextMap;

        # The JIT write-barrier suite is a secondary ASDF system and must be
        # exercised explicitly; building its FASLs alone does not run tests.
        cl-cc-jit-tests = perSystem.${system}.jitTests;

        toolkit-pin-consistency = perSystem.${system}.toolkitPinConsistency;
        lisplibs-consistency = perSystem.${system}.lisplibsConsistency;
        orphan-tests = inputs.cl-nix-forge.lib.${system}.mkOrphanTestCheck {
          src = ./.;
          testRoots = [
            "packages/binary/tests"
            "packages/cli/tests"
            "packages/compile/tests"
            "packages/debug/tests"
            "packages/docgen/tests"
            "packages/emit/tests"
            "packages/formatter/tests"
            "packages/optimize/tests"
            "packages/parse/tests"
            "packages/pipeline/tests"
            "packages/prolog-tools/tests"
            "packages/testing-framework/tests"
            "packages/tools/tests"
            "packages/vm/tests"
            "src/jit/tests"
            "t"
          ];
          testSystems = [
            "cl-cc/test"
            "cl-cc/test/e2e"
            "cl-cc-debug/tests"
            "cl-cc-docgen/test"
            "cl-cc-jit/tests"
            "cl-cc-prolog-tools/tests"
            "cl-cc-tools/tests"
          ];
          allowlist =
            nixpkgs.lib.filter (path: path != "") (
              nixpkgs.lib.splitString "\n" (builtins.readFile ./t/orphan-allowlist.txt)
            )
            ++ [
              # These files are inputs to standalone Nix checks, not ASDF-registered test files.
              "t/selfhost/determinism-input.lisp"
              "t/selfhost/determinism-worker.lisp"
              "t/selfhost/determinism.lisp"
              "t/selfhost/harness-support.lisp"
              "t/selfhost/stage2-fixpoint.lisp"
            ];
          name = "orphan-tests";
        };
        determinism =
          let
            pkgs = nixpkgs.legacyPackages.${system};
            lib = nixpkgs.lib;
            selfhostSource = builtins.path {
              name = "cl-cc-selfhost-source";
              path = ./.;
              filter =
                path: type:
                let
                  absolutePath = toString path;
                  repositoryRoot = toString ./.;
                  relativePath = lib.removePrefix "${repositoryRoot}/" absolutePath;
                  under = directory: relativePath == directory || lib.hasPrefix "${directory}/" relativePath;
                in
                absolutePath == repositoryRoot
                || relativePath == "cl-cc.asd"
                || under "packages"
                || (relativePath == "src" && type == "directory")
                || relativePath == "src/package.lisp"
                || (relativePath == "src/ffi" && type == "directory")
                || relativePath == "src/ffi/dynlib.lisp"
                || (relativePath == "t" && type == "directory")
                || under "t/selfhost";
            };
          in
          pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-cc-selfhost-determinism";
            version = "1";
            src = selfhostSource;
            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR"
              cat > run-tests.lisp <<'EOF'
              (load "t/selfhost/determinism.lisp")
              EOF
              ${perSystem.${system}.apps.test.program}
              runHook postBuild
            '';
            installPhase = "mkdir -p $out && touch $out/passed";
          };
        stage2-fixpoint =
          let
            pkgs = nixpkgs.legacyPackages.${system};
            lib = nixpkgs.lib;
            selfhostSource = builtins.path {
              name = "cl-cc-selfhost-source";
              path = ./.;
              filter =
                path: type:
                let
                  absolutePath = toString path;
                  repositoryRoot = toString ./.;
                  relativePath = lib.removePrefix "${repositoryRoot}/" absolutePath;
                  under = directory: relativePath == directory || lib.hasPrefix "${directory}/" relativePath;
                in
                absolutePath == repositoryRoot
                || relativePath == "cl-cc.asd"
                || under "packages"
                || (relativePath == "src" && type == "directory")
                || relativePath == "src/package.lisp"
                || (relativePath == "src/ffi" && type == "directory")
                || relativePath == "src/ffi/dynlib.lisp"
                || (relativePath == "t" && type == "directory")
                || under "t/selfhost";
            };
          in
          pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-cc-selfhost-stage2-fixpoint";
            version = "1";
            src = selfhostSource;
            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR"
              cat > run-tests.lisp <<'EOF'
              (load "t/selfhost/stage2-fixpoint.lisp")
              EOF
              ${perSystem.${system}.apps.test.program}
              runHook postBuild
            '';
            installPhase = "mkdir -p $out && touch $out/passed";
          };
      });

      apps = forAllSystems (system: perSystem.${system}.apps);

      devShells = forAllSystems (system: perSystem.${system}.devShells);

      # `nix fmt` entry point.
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);

      overlays.default = final: _prev: { cl-cc = self.packages.${final.system}.default; };
    };
}
