{
  lib,
  sbcl,
  clProlog,
  clWeave,
  clParserKit,
  clDataflow,
  clBoundaryKit,
  clCli,
  clTtyKit,
  ...
}:
let
  projectRoot = ../.;

  # pkgSrc accepts either a `subdir` string (relative to project root) OR a
  # `filesets` list of paths. We use lib.fileset.toSource so each derivation's
  # FASL cache stays independent — touching packages/vm never
  # invalidates packages/bootstrap.
  pkgSrc =
    arg:
    let
      filesets = if builtins.isString arg then [ (projectRoot + "/${arg}") ] else arg;
    in
    lib.fileset.toSource {
      root = projectRoot;
      fileset = lib.fileset.unions filesets;
    };

  # Build an ASDF system via sbcl.buildASDFSystem with shared boilerplate.
  # extraLispLibs threads in external (non cl-cc-*) derivations -- e.g. the
  # external cl-prolog engine -- alongside deps, which only resolves names
  # against allSystems (internal cl-cc-* systems).
  mkAsdfSystem =
    {
      name,
      src,
      deps,
      allSystems,
      extraLispLibs ? [ ],
    }:
    sbcl.buildASDFSystem {
      pname = name;
      version = "0.1.0";
      src = pkgSrc src;
      systems = [ name ];
      lispLibs = (map (n: allSystems.${n}) deps) ++ extraLispLibs;
    };

  # 14 leaf systems — preserved verbatim from the original flake.nix.
  leafSpec = {
    cl-cc-bootstrap = {
      src = "packages/bootstrap";
      deps = [ ];
    };
    cl-cc-ast = {
      src = "packages/ast";
      deps = [ ];
    };
    cl-cc-binary = {
      src = "packages/binary";
      deps = [ ];
    };
    cl-cc-runtime = {
      src = "packages/runtime";
      deps = [ ];
    };
    cl-cc-bytecode = {
      src = "packages/bytecode";
      deps = [ ];
    };
    cl-cc-ir = {
      src = "packages/ir";
      deps = [ ];
    };
    cl-cc-mir = {
      src = "packages/mir";
      deps = [ ];
    };
    cl-cc-type = {
      src = "packages/type";
      deps = [ "cl-cc-ast" ];
    };
    cl-cc-parse = {
      src = "packages/parse";
      deps = [
        "cl-cc-ast"
        "cl-cc-bootstrap"
      ];
    };
    cl-cc-vm = {
      src = "packages/vm";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-runtime"
      ];
    };
    cl-cc-docgen = {
      src = "packages/docgen";
      deps = [ ];
    };
    cl-cc-php = {
      src = "packages/php";
      deps = [
        "cl-cc-ast"
        "cl-cc-bootstrap"
        "cl-cc-parse"
        "cl-cc-vm"
      ];
    };
    cl-cc-javascript = {
      src = "packages/javascript";
      deps = [
        "cl-cc-ast"
        "cl-cc-bootstrap"
        "cl-cc-parse"
        # vm-integration.lisp wires the JS runtime into the VM (§5-1 step B).
        "cl-cc-vm"
      ];
    };
    cl-cc-optimize = {
      src = "packages/optimize";
      deps = [
        "cl-cc-vm"
        "cl-cc-type"
      ];
      # clProlog backs the peephole/e-graph rewrite rules; clParserKit tokenizes
      # and parses the `--pass-pipeline` spec string.
      extraLispLibs = [
        clProlog
        clParserKit
      ];
    };
    cl-cc-target = {
      src = "packages/target";
      deps = [ ];
    };
    cl-cc-regalloc = {
      src = "packages/regalloc";
      deps = [
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-target"
        "cl-cc-binary"
        "cl-cc-optimize"
      ];
    };
    cl-cc-expand = {
      src = "packages/expand";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-type"
        "cl-cc-vm"
      ];
    };
    cl-cc-cps = {
      src = "packages/cps";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
      ];
    };
    cl-cc-codegen = {
      src = "packages/codegen";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-target"
        "cl-cc-optimize"
        "cl-cc-regalloc"
      ];
    };
    cl-cc-emit = {
      src = "packages/emit";
      deps = [
        "cl-cc-vm"
        "cl-cc-mir"
        "cl-cc-optimize"
        "cl-cc-codegen"
      ];
    };
    cl-cc-compile = {
      src = "packages/compile";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
        "cl-cc-parse"
        "cl-cc-type"
        "cl-cc-optimize"
        "cl-cc-vm"
        "cl-cc-expand"
        "cl-cc-cps"
        "cl-cc-codegen"
        "cl-cc-target"
        "cl-cc-regalloc"
      ];
      extraLispLibs = [ clProlog ];
    };
    cl-cc-stdlib = {
      src = "packages/stdlib";
      deps = [ "cl-cc-bootstrap" ];
    };
    cl-cc-pipeline = {
      src = "packages/pipeline";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-ast"
        "cl-cc-parse"
        "cl-cc-php"
        "cl-cc-javascript"
        "cl-cc-type"
        "cl-cc-optimize"
        "cl-cc-vm"
        "cl-cc-expand"
        "cl-cc-emit"
        "cl-cc-stdlib"
        "cl-cc-binary"
        "cl-cc-compile"
      ];
    };
    cl-cc-selfhost = {
      src = "packages/selfhost";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-pipeline"
        "cl-cc-expand"
        "cl-cc-vm"
        "cl-cc-runtime"
        "cl-cc-compile"
        "cl-cc-ast"
        "cl-cc-parse"
        "cl-cc-optimize"
        "cl-cc-emit"
        "cl-cc-stdlib"
      ];
    };
    cl-cc-repl = {
      src = "packages/repl";
      deps = [
        "cl-cc-bootstrap"
        "cl-cc-pipeline"
        "cl-cc-selfhost"
        "cl-cc-expand"
        "cl-cc-vm"
        "cl-cc-parse"
        "cl-cc-compile"
        "cl-cc-runtime"
        "cl-cc-ast"
        "cl-cc-optimize"
        "cl-cc-emit"
        "cl-cc-stdlib"
      ];
      # cl-tty-kit provides ANSI/style/screen + input-decoder primitives for
      # the interactive REPL front-end; cl-boundary-kit models its console/
      # system-exit I/O as swappable, testable boundaries.
      extraLispLibs = [
        clTtyKit
        clBoundaryKit
      ];
    };
  };

  leafNames = builtins.attrNames leafSpec;

  # Umbrella + helpers. cl-cc bundles the umbrella package + compile-pipeline.
  # cl-cc-cli depends on :cl-cc. cl-cc-testing-framework also depends on :cl-cc.
  derivedSpec = {
    cl-cc = {
      src = [
        (projectRoot + "/packages")
        (projectRoot + "/src")
        (projectRoot + "/cl-cc.asd")
        (projectRoot + "/cl-cc-test.asd")
      ];
      deps = leafNames;
    };
    cl-cc-cli = {
      src = "packages/cli";
      deps = [
        "cl-cc"
        "cl-cc-docgen"
      ];
      # cl-cli is the declarative argument parser behind the cl-cc command
      # tree; cl-boundary-kit models console/args/system-exit as testable I/O
      # boundaries at the CLI edge; cl-tty-kit provides the ANSI/style helpers
      # for the interactive REPL and colored IR dumps; cl-dataflow backs the
      # `dep-graph` command's graph model + DOT/Mermaid/topological export.
      extraLispLibs = [
        clCli
        clBoundaryKit
        clTtyKit
        clDataflow
      ];
    };
    cl-cc-testing-framework = {
      src = "packages/testing-framework";
      deps = [
        "cl-cc"
      ];
      extraLispLibs = [ clWeave ];
    };
    cl-cc-tools = {
      src = "packages/tools";
      deps = [ "cl-cc" ];
    };
  };

  productionAsdfSystems = lib.fix (
    sys:
    lib.mapAttrs (
      name:
      {
        src,
        deps,
        extraLispLibs ? [ ],
      }:
      mkAsdfSystem {
        inherit
          name
          src
          deps
          extraLispLibs
          ;
        allSystems = sys;
      }
    ) (leafSpec // derivedSpec)
  );

  # Prolog-based call-graph analysis tools (packages/prolog-tools), built on
  # the external cl-prolog engine — a standalone leaf package registered via
  # `maybe-load-asd` in cl-cc.asd rather than folded into the `:cl-cc`
  # dependency closure, so it is exposed here rather than through
  # `mkAsdfSystem`'s internal-only `deps` lookup (which only resolves names
  # against `productionAsdfSystems`, not external flake inputs).
  cl-cc-prolog-tools = sbcl.buildASDFSystem {
    pname = "cl-cc-prolog-tools";
    version = "0.1.0";
    src = pkgSrc "packages/prolog-tools";
    systems = [ "cl-cc-prolog-tools" ];
    lispLibs = [
      productionAsdfSystems.cl-cc-ast
      clProlog
    ];
  };

  # Its cl-weave test suite is a separate system/derivation so the
  # cl-weave dependency stays test-only, never reaching the production
  # cl-cc-prolog-tools closure above.
  cl-cc-prolog-tools-test = sbcl.buildASDFSystem {
    pname = "cl-cc-prolog-tools-test";
    version = "0.1.0";
    src = pkgSrc "packages/prolog-tools";
    systems = [ "cl-cc-prolog-tools/tests" ];
    lispLibs = [
      productionAsdfSystems.cl-cc-ast
      clProlog
      clWeave
    ];
  };

  # Test systems live in a separate attrset so Nix consumers can opt into
  # the heavier test FASLs only when needed. lispLibs flow from
  # productionAsdfSystems, never recursively from testAsdfSystems.
  testSrc = [
    (projectRoot + "/packages")
    (projectRoot + "/tests")
    (projectRoot + "/src")
    (projectRoot + "/cl-cc.asd")
    (projectRoot + "/cl-cc-test.asd")
  ];

  # Keep self-hosting E2E tests available as an explicit system; the canonical
  # fast test app does not auto-load it.
  testAsdfSystems = {
    "cl-cc-test" = sbcl.buildASDFSystem {
      pname = "cl-cc-test";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-test" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
        cl-cc-tools
      ];
    };
    "cl-cc-javascript-test" = sbcl.buildASDFSystem {
      pname = "cl-cc-javascript-test";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-javascript-test" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
        cl-cc-tools
        cl-cc-php
        cl-cc-javascript
      ];
    };
    "cl-cc-test/e2e" = sbcl.buildASDFSystem {
      pname = "cl-cc-test-e2e";
      version = "0.1.0";
      src = pkgSrc testSrc;
      systems = [ "cl-cc-test/e2e" ];
      lispLibs = with productionAsdfSystems; [
        cl-cc
        cl-cc-cli
        cl-cc-testing-framework
        cl-cc-tools
      ];
    };
  };
in
{
  inherit productionAsdfSystems testAsdfSystems;
  inherit cl-cc-prolog-tools cl-cc-prolog-tools-test;
  sbclWithCLCC = sbcl.withPackages (_: lib.attrValues productionAsdfSystems);
  sbclWithTests = sbcl.withPackages (
    _: (lib.attrValues productionAsdfSystems) ++ [ testAsdfSystems."cl-cc-test" ]
  );
  sbclWithJavascriptTests = sbcl.withPackages (
    _: (lib.attrValues productionAsdfSystems) ++ [ testAsdfSystems."cl-cc-javascript-test" ]
  );
}
