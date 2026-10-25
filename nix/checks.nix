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
  sbclWithJitTests,
  apps,
  clPrologKit,
  clWeave,
  clCcAst,
  rootLock,
  childFlakeSources,
  asdfSystemDerivations,
  productionAsdfSystems,
}:
let
  toolkitNames = [
    "cl-weave"
    "cl-parser-kit"
    "cl-boundary-kit"
    "cl-log-kit"
    "cl-tty-kit"
    "cl-codec-kit"
    "cl-regex-kit"
    "cl-host-kit"
    "cl-process-kit"
    "cl-json-kit"
    "cl-date-kit"
    "cl-concurrent-kit"
    "cl-prolog-kit"
  ];

  resolveInputRef =
    lock: current: reference:
    if builtins.isString reference then
      reference
    else if builtins.isList reference then
      resolveFollow lock current reference
    else
      null;

  resolveFollow =
    lock: current: path:
    let
      startsAtRoot = path != [ ] && builtins.head path == "root";
      start = if startsAtRoot then "root" else current;
      steps = if startsAtRoot then builtins.tail path else path;
      resolveStep =
        nodeName: inputName:
        let
          node = lock.nodes.${nodeName} or { };
          inputs = node.inputs or { };
          reference = inputs.${inputName} or null;
        in
        if reference == null then null else resolveInputRef lock nodeName reference;
    in
    builtins.foldl' (
      nodeName: inputName: if nodeName == null then null else resolveStep nodeName inputName
    ) start steps;

  resolveLockedInput =
    lock: inputName:
    let
      rootInputs = lock.nodes.root.inputs or { };
      nodeName = resolveInputRef lock "root" (rootInputs.${inputName} or null);
    in
    if nodeName == null then null else lock.nodes.${nodeName}.locked or null;

  identity =
    node:
    if node == null then
      null
    else
      {
        owner = node.owner or null;
        repo = node.repo or null;
        rev = node.rev or null;
      };

  toolkitPinComparisons = lib.concatMap (
    child:
    let
      lockFile = child.source + "/flake.lock";
      childLock =
        if builtins.pathExists lockFile then builtins.fromJSON (builtins.readFile lockFile) else null;
    in
    if childLock == null then
      [
        {
          child = child.name;
          toolkit = "flake.lock";
          rootIdentity = null;
          childIdentity = null;
          matches = false;
        }
      ]
    else
      lib.concatMap (
        toolkit:
        let
          childInputs = childLock.nodes.root.inputs or { };
          childReference = childInputs.${toolkit} or null;
          rootIdentity = identity (resolveLockedInput rootLock toolkit);
          childIdentity = identity (resolveLockedInput childLock toolkit);
        in
        if childReference == null then
          [ ]
        else
          [
            {
              inherit (child) name;
              inherit toolkit rootIdentity childIdentity;
              matches = rootIdentity != null && rootIdentity == childIdentity;
            }
          ]
      ) toolkitNames
  ) childFlakeSources;

  asdfSystemMetadata = map (
    package:
    let
      metadata = package.asdfMetadata;
      providerMetadata = provider: {
        pname = provider.asdfMetadata.pname;
        source = provider.asdfMetadata.src;
        systems = provider.asdfMetadata.systems;
      };
    in
    {
      inherit (metadata) pname systems;
      source = metadata.src;
      lispLibs = map providerMetadata metadata.lispLibs;
    }
  ) asdfSystemDerivations;

  contextMapRepositories = [
    {
      name = "umbrella";
      source = ../.;
    }
  ]
  ++ map (child: {
    inherit (child) name;
    source = child.source;
  }) childFlakeSources;

  contextMapRepositorySourceReferences = lib.concatMapStringsSep " " (
    repository: ''"${repository.source}"''
  ) contextMapRepositories;

  contextMapProductionSystems = lib.unique (
    lib.concatMap (package: package.asdfMetadata.systems) (builtins.attrValues productionAsdfSystems)
    ++ lib.concatMap (
      package:
      if package.asdfMetadata.pname == "cl-cc-prolog-tools" then package.asdfMetadata.systems else [ ]
    ) asdfSystemDerivations
  );

  contextMapSystemContextsBase = [
    {
      system = "cl-cc-bootstrap";
      context = "bootstrap";
      repository = "bootstrap";
    }
    {
      system = "cl-cc-ast";
      context = "ast";
      repository = "ast";
    }
    {
      system = "cl-cc-parse";
      context = "parse";
      repository = "parse";
    }
    {
      system = "cl-cc-php";
      context = "php";
      repository = "php";
    }
    {
      system = "cl-cc-javascript";
      context = "javascript";
      repository = "javascript";
    }
    {
      system = "cl-cc-expand";
      context = "expand";
      repository = "expand";
    }
    {
      system = "cl-cc-cps";
      context = "cps";
      repository = "cps";
    }
    {
      system = "cl-cc-type";
      context = "type";
      repository = "type";
    }
    {
      system = "cl-cc-compile";
      context = "compile";
      repository = "umbrella";
    }
    {
      system = "cl-cc-optimize";
      context = "optimize";
      repository = "optimize";
    }
    {
      system = "cl-cc-codegen";
      context = "codegen-native";
      repository = "codegen-native";
    }
    {
      system = "cl-cc-emit";
      context = "codegen-native";
      repository = "codegen-native";
    }
    {
      system = "cl-cc-regalloc";
      context = "codegen-native";
      repository = "codegen-native";
    }
    {
      system = "cl-cc-mir";
      context = "mir";
      repository = "mir";
    }
    {
      system = "cl-cc-target";
      context = "target";
      repository = "mir";
    }
    {
      system = "cl-cc-binary";
      context = "binary";
      repository = "binary";
    }
    {
      system = "cl-cc-vm";
      context = "vm";
      repository = "vm";
    }
    {
      system = "cl-cc-runtime";
      context = "runtime";
      repository = "runtime";
    }
    {
      system = "cl-cc";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-docgen";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-stdlib";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-selfhost";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-jit";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-pipeline";
      context = "pipeline";
      repository = "umbrella";
    }
    {
      system = "cl-cc-cli";
      context = "cli";
      repository = "umbrella";
    }
    {
      system = "cl-cc-repl";
      context = "repl";
      repository = "umbrella";
    }
    {
      system = "cl-cc-tools";
      context = "tools";
      repository = "umbrella";
    }
    {
      system = "cl-cc-testing-framework";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-formatter";
      context = "umbrella";
      repository = "umbrella";
    }
    {
      system = "cl-cc-prolog-tools";
      context = "umbrella";
      repository = "umbrella";
    }
  ];

  contextMapProductionContextSystems = map (mapping: mapping.system) contextMapSystemContextsBase;
  contextMapContextSystems = map (mapping: mapping.system) contextMapSystemContexts;
  contextMapHasUniqueMappings =
    builtins.length contextMapContextSystems == builtins.length (lib.unique contextMapContextSystems);
  contextMapHasCompleteProductionMappings =
    lib.sort builtins.lessThan contextMapProductionSystems
    == lib.sort builtins.lessThan contextMapProductionContextSystems;

  contextMapRepositoryForSystem =
    system:
    let
      mapping = lib.findFirst (entry: entry.system == system) null contextMapSystemContextsBase;
    in
    if mapping == null then "umbrella" else mapping.repository;

  contextMapRepositoryForProvider =
    provider:
    let
      repositories = lib.unique (
        builtins.filter (repository: repository != null) (
          map (
            system:
            let
              mapping = lib.findFirst (entry: entry.system == system) null contextMapSystemContextsBase;
            in
            if mapping == null then null else mapping.repository
          ) provider.systems
        )
      );
    in
    if repositories == [ ] then
      provider.pname
    else if builtins.length repositories == 1 then
      builtins.head repositories
    else
      throw "ASDF provider ${provider.pname} spans multiple mapped repositories";

  contextMapProductionMetadata = builtins.filter (
    package: builtins.any (system: builtins.elem system contextMapProductionSystems) package.systems
  ) asdfSystemMetadata;

  contextMapSystemContexts =
    contextMapSystemContextsBase
    ++ lib.unique (
      lib.concatMap (
        package:
        lib.concatMap (
          provider:
          map
            (system: {
              inherit system;
              context = "external";
              repository = contextMapRepositoryForProvider provider;
            })
            (
              builtins.filter (
                system: !(builtins.elem system contextMapProductionContextSystems)
              ) provider.systems
            )
        ) package.lispLibs
      ) contextMapProductionMetadata
    )
    ++ [
      {
        system = "sb-posix";
        context = "external";
        repository = "sbcl";
      }
    ];

  contextMapSourceRoots = lib.unique (
    [
      {
        name = "umbrella";
        source = ../.;
      }
    ]
    ++ map (child: {
      inherit (child) name;
      source = child.source;
    }) childFlakeSources
    ++ lib.concatMap (
      package:
      map (system: {
        name = contextMapRepositoryForSystem system;
        source = package.source;
      }) (builtins.filter (system: builtins.elem system contextMapProductionSystems) package.systems)
    ) contextMapProductionMetadata
    ++ lib.concatMap (
      package:
      lib.concatMap (provider: [
        {
          name = contextMapRepositoryForProvider provider;
          source = provider.source;
        }
      ]) package.lispLibs
    ) contextMapProductionMetadata
    ++ [
      {
        name = "sbcl";
        source = toString pkgs.sbcl;
      }
    ]
  );

  contextMapSourceRootReferences = lib.concatMapStringsSep " " (
    root: ''"${root.source}"''
  ) contextMapSourceRoots;

  contextMapEdgesFrom = source: targets: map (target: { inherit source target; }) targets;
  contextMapAllowedEdges = lib.concatLists [
    (contextMapEdgesFrom "cl-cc-parse" [ "cl-cc-ast" ])
    (contextMapEdgesFrom "cl-cc-php" [
      "cl-cc-ast"
      "cl-cc-bootstrap"
    ])
    (contextMapEdgesFrom "cl-cc-javascript" [
      "cl-cc-ast"
      "cl-cc-bootstrap"
    ])
    (contextMapEdgesFrom "cl-cc-expand" [
      "cl-cc-ast"
      "cl-cc-type"
    ])
    (contextMapEdgesFrom "cl-cc-cps" [ "cl-cc-ast" ])
    (contextMapEdgesFrom "cl-cc-type" [ "cl-cc-ast" ])
    (contextMapEdgesFrom "cl-cc-compile" [
      "cl-cc-ast"
      "cl-cc-cps"
      "cl-cc-expand"
      "cl-cc-vm"
    ])
    (contextMapEdgesFrom "cl-cc-optimize" [ "cl-cc-vm" ])
    (contextMapEdgesFrom "cl-cc-codegen" [
      "cl-cc-mir"
      "cl-cc-target"
      "cl-cc-binary"
    ])
    (contextMapEdgesFrom "cl-cc-codegen" [ "cl-cc-regalloc" ])
    (contextMapEdgesFrom "cl-cc-emit" [
      "cl-cc-mir"
      "cl-cc-target"
      "cl-cc-binary"
      "cl-cc-codegen"
    ])
    (contextMapEdgesFrom "cl-cc-regalloc" [
      "cl-cc-mir"
      "cl-cc-target"
      "cl-cc-binary"
    ])
    (contextMapEdgesFrom "cl-cc-binary" [ "cl-log-kit" ])
    (contextMapEdgesFrom "cl-cc-vm" [ "cl-cc-runtime" ])
    (contextMapEdgesFrom "cl-cc-runtime" [
      "cl-log-kit"
      "cl-json-kit"
      "cl-host-kit"
      "cl-observability-kit"
      "cl-observability-kit/prometheus"
      "cl-observability-kit/otlp"
      "cl-deflate-kit"
      "cl-crypto-kit"
      "cl-resilience-kit"
    ])
    (contextMapEdgesFrom "cl-cc-pipeline" [
      "cl-cc-bootstrap"
      "cl-cc-ast"
      "cl-cc-parse"
      "cl-cc-php"
      "cl-cc-javascript"
      "cl-cc-expand"
      "cl-cc-cps"
      "cl-cc-type"
      "cl-cc-compile"
      "cl-cc-optimize"
      "cl-cc-codegen"
      "cl-cc-emit"
      "cl-cc-regalloc"
      "cl-cc-mir"
      "cl-cc-target"
      "cl-cc-binary"
      "cl-cc-vm"
      "cl-cc-runtime"
    ])
    (contextMapEdgesFrom "cl-cc-cli" [ "cl-cc-pipeline" ])
    (contextMapEdgesFrom "cl-cc-repl" [ "cl-cc-pipeline" ])
    (contextMapEdgesFrom "cl-cc-tools" [ "cl-cc-pipeline" ])
  ];

  crossRepoInternalRefs =
    pkgs.runCommand "cross-repo-internal-refs"
      {
        nativeBuildInputs = [ pkgs.python3 ];
      }
      ''
        printf '%s\n' ${contextMapRepositorySourceReferences} >/dev/null
        cat > repositories.json <<'JSON'
        ${builtins.toJSON contextMapRepositories}
        JSON
        cat > allowlist.txt <<'ALLOWLIST'
        ${builtins.readFile ../t/cross-repo-allowlist.txt}
        ALLOWLIST
        cat > cross-repo-internal-refs.py <<'PYTHON'
        import json
        import sys
        from collections import Counter
        from pathlib import Path

        package_owners = {
            # The CL-CC facade package is shared by the umbrella and bootstrap.
            "cl-cc": {"umbrella", "bootstrap"},
            "cl-cc/ast": "ast",
            "cl-cc/binary": "binary",
            "cl-cc/bootstrap": "bootstrap",
            "cl-cc/cps": "cps",
            "cl-cc/codegen": "codegen-native",
            "cl-cc/emit": "codegen-native",
            "cl-cc/expand": "expand",
            "cl-cc/javascript": "javascript",
            "cl-cc/mir": "mir",
            "cl-cc/optimize": "optimize",
            "cl-cc/parse": "parse",
            "cl-cc/php": "php",
            "cl-cc/regalloc": "codegen-native",
            "cl-cc/runtime": "runtime",
            "cl-cc/target": "mir",
            "cl-cc/type": "type",
            "cl-cc/vm": "vm",
        }
        child_owners = {
            "ast", "binary", "bootstrap", "cps", "codegen-native", "expand",
            "javascript", "mir", "optimize", "parse", "php", "runtime", "type", "vm",
        }
        source_suffixes = {".asd", ".cl", ".lisp", ".lsp"}
        delimiters = set("()'`,;#" + chr(34))

        def token_end(text, index):
            escaped = False
            in_bars = False
            while index < len(text):
                char = text[index]
                if escaped:
                    escaped = False
                elif char == chr(92):
                    escaped = True
                elif char == "|":
                    in_bars = not in_bars
                elif not in_bars and (char.isspace() or char in delimiters):
                    break
                index += 1
            return index

        def skip_string(text, index):
            escaped = False
            index += 1
            while index < len(text):
                char = text[index]
                if escaped:
                    escaped = False
                elif char == chr(92):
                    escaped = True
                elif char == chr(34):
                    return index + 1
                index += 1
            return index

        def skip_block_comment(text, index):
            depth = 1
            index += 2
            while depth:
                opener = text.find("#|", index)
                closer = text.find("|#", index)
                if closer < 0:
                    raise ValueError("unterminated Lisp block comment")
                if opener >= 0 and opener < closer:
                    depth += 1
                    index = opener + 2
                else:
                    depth -= 1
                    index = closer + 2
            return index

        def skip_character(text, index):
            index += 2
            if index >= len(text):
                return index
            char = text[index]
            if char.isspace() or char in delimiters or char in {'"', "|", chr(92)}:
                return index + 1
            return token_end(text, index)

        def skip_trivia(text, index):
            while index < len(text):
                if text[index].isspace():
                    index += 1
                elif text[index] == ";":
                    newline = text.find(chr(10), index)
                    index = len(text) if newline < 0 else newline + 1
                elif text.startswith("#|", index):
                    index = skip_block_comment(text, index)
                elif text.startswith("#;", index):
                    index = skip_form(text, index + 2)
                else:
                    break
            return index

        def skip_balanced_list(text, index):
            depth = 1
            index += 1
            while index < len(text) and depth:
                if text[index] == ";":
                    newline = text.find(chr(10), index)
                    index = len(text) if newline < 0 else newline + 1
                elif text.startswith("#|", index):
                    index = skip_block_comment(text, index)
                elif text.startswith("#;", index):
                    index = skip_form(text, index + 2)
                elif text.startswith("#" + chr(92), index):
                    index = skip_character(text, index)
                elif text[index] == chr(34):
                    index = skip_string(text, index)
                elif text[index] == "|" or text[index] == chr(92):
                    index = token_end(text, index)
                elif text[index] == "(":
                    depth += 1
                    index += 1
                elif text[index] == ")":
                    depth -= 1
                    index += 1
                else:
                    index += 1
            return index

        def skip_form(text, index):
            index = skip_trivia(text, index)
            if index >= len(text):
                return index
            if text.startswith("#" + chr(92), index):
                return skip_character(text, index)
            if text[index] == chr(34):
                return skip_string(text, index)
            if text[index] == "(":
                return skip_balanced_list(text, index)
            if text.startswith("#+", index) or text.startswith("#-", index):
                return skip_form(text, skip_form(text, index + 2))
            if text.startswith(("#'", "#.", "#,"), index):
                return skip_form(text, index + 2)
            if text[index] in "'`,":
                prefix_length = 2 if text.startswith(",@", index) else 1
                return skip_form(text, index + prefix_length)
            if text[index] == "#":
                dispatch = index + 1
                while dispatch < len(text) and text[dispatch].isdigit():
                    dispatch += 1
                if dispatch > index + 1 and dispatch < len(text) and text[dispatch] in "=#":
                    return (skip_form(text, dispatch + 1) if text[dispatch] == "="
                            else dispatch + 1)
                if dispatch < len(text) and text[dispatch] == "(":
                    return skip_balanced_list(text, dispatch)
                if dispatch < len(text):
                    return skip_form(text, dispatch + 1)
                return dispatch
            return token_end(text, index)

        def lisp_tokens(text):
            index = 0
            while index < len(text):
                if text[index].isspace():
                    index += 1
                elif text[index] == ";":
                    newline = text.find(chr(10), index)
                    index = len(text) if newline < 0 else newline + 1
                elif text.startswith("#|", index):
                    index = skip_block_comment(text, index)
                elif text.startswith("#" + chr(92), index):
                    index = skip_character(text, index)
                elif text.startswith("#;", index):
                    index = skip_form(text, index + 2)
                elif text[index] == chr(34):
                    index = skip_string(text, index)
                elif text[index] in "()'`,":
                    index += 1
                else:
                    end = token_end(text, index)
                    if end == index:
                        index += 1
                    else:
                        yield text[index:end]
                        index = end

        def package_separator(token):
            escaped = False
            in_bars = False
            index = 0
            while index < len(token):
                char = token[index]
                if escaped:
                    escaped = False
                elif char == chr(92):
                    escaped = True
                elif char == "|":
                    in_bars = not in_bars
                elif not in_bars and token.startswith("::", index):
                    return index
                index += 1
            return -1

        def normalized_name(name):
            result = []
            escaped = False
            in_bars = False
            for char in name:
                if escaped:
                    result.append(char)
                    escaped = False
                elif char == chr(92):
                    escaped = True
                elif char == "|":
                    in_bars = not in_bars
                else:
                    result.append(char)
            if escaped or in_bars:
                return None
            return "".join(result).casefold()

        def package_owner(package):
            package = package.casefold()
            if package in package_owners:
                owner = package_owners[package]
                return owner if isinstance(owner, set) else {owner}
            if package.startswith("cl-cc/"):
                context = package.split("/", 1)[1].split("/", 1)[0]
                return {context} if context in child_owners else {"umbrella"}
            return None

        def in_scan_scope(repository, relative):
            parts = relative.parts
            if repository == "umbrella":
                if relative.name == "cl-cc.asd":
                    return True
                if parts and parts[0] in {"src", "t"}:
                    return True
                return bool(parts and parts[0] == "packages"
                            and any(part in {"src", "t", "test", "tests"} for part in parts[:-1]))
            if relative.suffix == ".asd":
                return True
            return any(part in {"src", "t"} for part in parts[:-1])

        def scan(repositories):
            counts = Counter()
            for repository in repositories:
                name = repository["name"]
                root = Path(repository["source"])
                for path in sorted(root.rglob("*")):
                    if not path.is_file() or path.suffix.casefold() not in source_suffixes:
                        continue
                    relative = path.relative_to(root)
                    if not in_scan_scope(name, relative):
                        continue
                    text = path.read_text(encoding="utf-8")
                    if "::" not in text:
                        continue
                    for token in lisp_tokens(text):
                        separator = package_separator(token)
                        if separator <= 0:
                            continue
                        package = normalized_name(token[:separator])
                        symbol = normalized_name(token[separator + 2:])
                        if package is None or symbol is None:
                            continue
                        target = package_owner(package)
                        if target is None or name in target:
                            continue
                        key = (f"{name}/{relative.as_posix()}", f"{package}::{symbol}")
                        counts[key] += 1
            return counts

        def read_allowlist(path):
            rows = {}
            for line_number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                fields = line.split()
                if len(fields) != 3:
                    raise ValueError(f"allowlist line {line_number} must have path, reference, and count")
                key = (fields[0], fields[1])
                if key in rows:
                    raise ValueError(f"duplicate allowlist entry on line {line_number}: {key}")
                rows[key] = int(fields[2])
            return rows

        repositories = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
        current = scan(repositories)
        allowed = read_allowlist(Path(sys.argv[2]))
        new = sorted((key, count) for key, count in current.items() if allowed.get(key) != count)
        stale = sorted((key, count) for key, count in allowed.items() if current.get(key) != count)
        if new or stale:
            for (path, reference), count in new:
                print(f"new or changed reference: {path} {reference} {count}", file=sys.stderr)
            for (path, reference), count in stale:
                print(f"stale or changed allowlist entry: {path} {reference} {count}", file=sys.stderr)
            raise SystemExit(1)
        print(f"checked {len(repositories)} repositories and {sum(current.values())} cross-repository internal references")
        PYTHON
        ${pkgs.python3}/bin/python3 cross-repo-internal-refs.py repositories.json allowlist.txt
        touch "$out"
      '';

  contextMap =
    assert contextMapHasUniqueMappings;
    assert contextMapHasCompleteProductionMappings;
    pkgs.runCommand "context-map"
      {
        nativeBuildInputs = [
          pkgs.jq
          pkgs.sbcl
        ];
      }
      ''
        export HOME="$TMPDIR/home"
        mkdir -p "$HOME"
        printf '%s\n' ${contextMapSourceRootReferences} >/dev/null
        cat > context-map.json <<'JSON'
        ${builtins.toJSON {
          repositories = contextMapSourceRoots;
          systems = contextMapProductionSystems;
          contexts = contextMapSystemContexts;
          allowedEdges = contextMapAllowedEdges;
        }}
        JSON
        cat > asdf-systems.json <<'JSON'
        ${builtins.toJSON asdfSystemMetadata}
        JSON
        export CL_SOURCE_REGISTRY="$(${pkgs.jq}/bin/jq -r '[.[].source, .[].lispLibs[]?.source] | unique | map(. + "//") | join(":")' asdf-systems.json)"
        ${pkgs.jq}/bin/jq -r '.repositories[] | ["R", .name, .source] | @tsv' context-map.json > context-map.tsv
        ${pkgs.jq}/bin/jq -r '.systems[] | ["S", .] | @tsv' context-map.json >> context-map.tsv
        ${pkgs.jq}/bin/jq -r '.contexts[] | ["C", .system, .context, .repository] | @tsv' context-map.json >> context-map.tsv
        ${pkgs.jq}/bin/jq -r '.allowedEdges[] | ["A", .source, .target] | @tsv' context-map.json >> context-map.tsv
        cat > context-map-check.lisp <<'LISP'
        (require :asdf)

        (defun split-on-tab (string)
          (loop with start = 0
                for end = (position #\Tab string :start start)
                collect (subseq string start end)
                while end
                do (setf start (1+ end))))

        (defun read-context-map (path)
          (let ((repositories nil)
                (systems nil)
                (contexts nil)
                (allowed-edges nil))
            (with-open-file (stream path)
              (loop for line = (read-line stream nil nil)
                    while line
                    for fields = (split-on-tab line)
                    do (cond
                         ((string= (first fields) "R")
                          (push (cons (second fields) (third fields)) repositories))
                         ((string= (first fields) "S")
                          (push (second fields) systems))
                         ((string= (first fields) "C")
                          (push (list (second fields) (third fields) (fourth fields)) contexts))
                         ((string= (first fields) "A")
                          (push (list (second fields) (third fields)) allowed-edges)))))
            (values (nreverse repositories)
                    (nreverse systems)
                    (nreverse contexts)
                    (nreverse allowed-edges))))

        (defun path-under-p (file root)
          (let* ((file-name (namestring (truename file)))
                 (root-name (concatenate 'string
                                         (string-right-trim "/" (namestring (truename root)))
                                         "/")))
            (and (>= (length file-name) (length root-name))
                 (string= root-name file-name
                          :end1 (length root-name)
                          :end2 (length root-name)))))

        (defun source-owner (file repositories)
          (let ((owners (remove-if-not
                         (lambda (repository)
                           (path-under-p file (cdr repository)))
                         repositories)))
            (cond
              ((null owners) nil)
              ((= (length owners) 1) (caar owners))
              (t (error "ASDF source ~A has multiple repository owners: ~{~A~^, ~}"
                        file (mapcar #'car owners))))))

        (defun dependency-name (dependency)
          (string-downcase
           (typecase dependency
             (string dependency)
             (symbol (symbol-name dependency))
             (cons (dependency-name
                    (if (member (first dependency) '(:require :version))
                        (second dependency)
                        (first dependency))))
             (t (error "Unsupported ASDF dependency specifier ~S" dependency)))))

        (defun whitespace-char-p (character)
          (find character '(#\Space #\Tab #\Return #\Newline)))

        (defun split-fields (string)
          (loop with cursor = 0
                for start = (position-if-not #'whitespace-char-p string :start cursor)
                while start
                for end = (position-if #'whitespace-char-p string :start start)
                collect (subseq string start end)
                while end
                do (setf cursor (1+ end))))

        (defun ticket-id-p (string)
          (let* ((first-dot (position #\. string))
                 (second-dot (and first-dot
                                  (position #\. string :start (1+ first-dot))))
                 (third-dot (and second-dot
                                 (position #\. string :start (1+ second-dot))))
                 (stage (and second-dot
                             (subseq string (1+ first-dot) second-dot)))
                 (repository (and third-dot
                                  (subseq string (1+ second-dot) third-dot)))
                 (phase (and third-dot (subseq string (1+ third-dot)))))
            (and first-dot
                 second-dot
                 third-dot
                 (= first-dot 2)
                 (string= (subseq string 0 first-dot) "W3")
                 (member (list stage repository)
                         '(("1" "bootstrap") ("1" "ast") ("1" "mir") ("1" "runtime")
                           ("2" "vm") ("2" "type") ("2" "parse") ("2" "cps")
                           ("3" "expand") ("3" "optimize") ("3" "binary")
                           ("4" "codegen-native") ("4" "php") ("4" "javascript") ("4" "cc"))
                         :test #'equal)
                 (every (lambda (character)
                          (or (alphanumericp character) (char= character #\-)))
                        repository)
                 (member phase '("①" "②" "③" "④") :test #'string=))))

        (defun read-exceptions (path)
          (let ((rows nil)
                (seen-pairs nil)
                (errors nil))
            (with-open-file (stream path)
              (loop for line = (read-line stream nil nil)
                    while line
                    for fields = (split-fields line)
                    unless (or (null fields) (char= (char (first fields) 0) #\#))
                      do (cond
                           ((/= (length fields) 3)
                            (push (format nil "exception line must have exactly three fields: ~A" line)
                                  errors))
                           ((not (ticket-id-p (third fields)))
                            (push (format nil "exception line has malformed ticket id: ~A" line)
                                  errors))
                           (t
                            (let ((pair (subseq fields 0 2)))
                              (if (member pair seen-pairs :test #'equal)
                                  (push (format nil "duplicate exception for ~A -> ~A"
                                                (first pair) (second pair))
                                        errors)
                                  (progn
                                    (push pair seen-pairs)
                                    (push fields rows))))))))
            (values (nreverse rows) (nreverse errors))))

        (defun edge-present-p (source target edges)
          (member (list source target) edges :test #'equal))

        (defun exception-for-edge (source target exceptions)
          (find-if (lambda (row)
                     (and (string= source (first row))
                          (string= target (second row))))
                   exceptions))

        (defun resolved-system-source (name)
          (let* ((system (asdf:find-system name nil))
                 (source (and system (asdf:system-source-file system))))
            (unless system
              (error "ASDF cannot resolve dependency system ~A" name))
            (unless (and source (probe-file source))
              (error "ASDF system ~A has no resolvable source file: ~A" name source))
            (values system source)))

        (multiple-value-bind (repositories systems contexts allowed-edges)
            (read-context-map (second sb-ext:*posix-argv*))
          (multiple-value-bind (exceptions exception-errors)
              (read-exceptions (third sb-ext:*posix-argv*))
            (let ((edge-count 0)
                  (used-exceptions nil)
                  (errors exception-errors))
              (dolist (name systems)
                (handler-case
                    (multiple-value-bind (system source) (resolved-system-source name)
                      (let* ((context-info (assoc name contexts :test #'string=))
                             (owner (source-owner source repositories)))
                        (unless owner
                          (push (format nil "production system source is outside the repository map: ~A at ~A"
                                        name source)
                                errors))
                        (unless context-info
                          (push (format nil "production system has no FR-031 context mapping: ~A" name)
                                errors))
                        (when (and context-info owner
                                   (not (string= owner (third context-info))))
                          (push (format nil "production system source owner mismatch: ~A is mapped to ~A but resolves under ~A"
                                        name (third context-info) owner)
                                errors)))
                      (dolist (dependency (asdf:system-depends-on system))
                        (handler-case
                            (let* ((target (dependency-name dependency))
                                   (target-context (assoc target contexts :test #'string=))
                                   (exception (exception-for-edge name target exceptions)))
                              (multiple-value-bind (dependency-system dependency-source)
                                  (resolved-system-source target)
                                (declare (ignore dependency-system))
                                (let ((target-owner (source-owner dependency-source repositories)))
                                  (unless target-context
                                    (push (format nil "dependency system has no context mapping: ~A" target)
                                          errors))
                                  (when (and target-context target-owner
                                             (not (string= target-owner (third target-context))))
                                    (push (format nil "dependency source owner mismatch: ~A is mapped to ~A but resolves under ~A"
                                                  target (third target-context) target-owner)
                                          errors))
                                  (unless target-owner
                                    (push (format nil "dependency source has no repository owner: ~A at ~A"
                                                  target dependency-source)
                                          errors)))
                              (incf edge-count)
                              (cond
                                ((edge-present-p name target allowed-edges))
                                (exception
                                 (push (list name target) used-exceptions))
                                (t
                                 (push (format nil "unassigned production dependency edge: ~A (~A) -> ~A (~A)"
                                               name (or (second (assoc name contexts :test #'string=)) "unclassified")
                                               target (or (second target-context) "unclassified"))
                                       errors)))))
                          (error (condition)
                            (push (format nil "invalid direct dependency of ~A: ~A"
                                          name condition)
                                  errors)))))
                  (error (condition)
                    (push (format nil "invalid production system ~A: ~A" name condition)
                          errors))))
              (dolist (exception exceptions)
                (unless (member (subseq exception 0 2) used-exceptions :test #'equal)
                  (push (format nil "stale context-map exception: ~A -> ~A (~A)"
                                (first exception) (second exception) (third exception))
                        errors)))
              (if errors
                  (progn
                    (dolist (message (nreverse errors))
                      (format *error-output* "~A~%" message))
                    (sb-ext:exit :code 1))
                  (format t "checked ~D production ASDF systems, ~D FR-031 direct edges, and ~D exact exceptions~%"
                          (length systems) edge-count (length exceptions))))))
        LISP
        cat > context-map-exceptions.txt <<'EXCEPTIONS'
        ${builtins.readFile ../t/context-map-exceptions.txt}
        EXCEPTIONS
        sbcl --noinform --script context-map-check.lisp context-map.tsv context-map-exceptions.txt
        touch "$out"
      '';

  toolkitPinConsistency =
    pkgs.runCommand "toolkit-pin-consistency"
      {
        nativeBuildInputs = [ pkgs.jq ];
      }
      ''
        cat > toolkit-pins.json <<'JSON'
        ${builtins.toJSON toolkitPinComparisons}
        JSON
        mismatch_count="$(${pkgs.jq}/bin/jq '[.[] | select(.matches == false)] | length' toolkit-pins.json)"
        if [ "$mismatch_count" -ne 0 ]; then
          ${pkgs.jq}/bin/jq -r '.[] | select(.matches == false) |
            "\(.child) \(.toolkit): child=\(.childIdentity) root=\(.rootIdentity)"' toolkit-pins.json >&2
          exit 1
        fi
        echo "checked ${toString (builtins.length toolkitPinComparisons)} child toolkit pins"
        touch "$out"
      '';

  lisplibsConsistency =
    pkgs.runCommand "lisplibs-consistency"
      {
        nativeBuildInputs = [
          pkgs.jq
          pkgs.sbcl
        ];
      }
      ''
        export HOME="$TMPDIR/home"
        mkdir -p "$HOME"
        cat > asdf-systems.json <<'JSON'
        ${builtins.toJSON asdfSystemMetadata}
        JSON
        export CL_SOURCE_REGISTRY="$(${pkgs.jq}/bin/jq -r '[.[].source, .[].lispLibs[]?.source] | unique | map(. + "//") | join(":")' asdf-systems.json)"
        ${pkgs.jq}/bin/jq -r '
          .[] as $system
          | (["S", $system.pname, ($system.systems | join(",")), $system.source] | @tsv),
            ($system.lispLibs[]? | ["L", $system.pname, .pname, (.systems | join(",")), .source] | @tsv)
        ' asdf-systems.json > asdf-systems.tsv
        cat > lisplibs-check.lisp <<'LISP'
        (require :asdf)

        (defstruct system-spec pname systems source libs)
        (defstruct library-spec pname systems source)

        (defun split-on (delimiter string)
          (loop with start = 0
                for end = (position delimiter string :start start)
                collect (subseq string start end)
                while end
                do (setf start (1+ end))))

        (defun read-specs (path)
          (let ((specs (make-hash-table :test 'equal)))
            (with-open-file (stream path)
              (loop for line = (read-line stream nil nil)
                    while line
                    for fields = (split-on #\Tab line)
                    do (cond
                         ((string= (first fields) "S")
                          (setf (gethash (second fields) specs)
                                (make-system-spec
                                 :pname (second fields)
                                 :systems (split-on #\, (third fields))
                                 :source (fourth fields))))
                         ((string= (first fields) "L")
                          (push (make-library-spec
                                 :pname (third fields)
                                 :systems (split-on #\, (fourth fields))
                                 :source (fifth fields))
                                (system-spec-libs (gethash (second fields) specs))))))
        specs)))

        (defun dependency-name (dependency)
          (string-downcase
           (typecase dependency
             (string dependency)
             (symbol (symbol-name dependency))
            (cons (dependency-name (if (member (first dependency) '(:require :version))
                                       (second dependency)
                                       (first dependency))))
             (t (error "Unsupported ASDF dependency specifier ~S" dependency)))))

        (defun system-source (name)
          (let ((system (asdf:find-system name nil)))
            (unless system
              (error "ASDF cannot resolve system ~A" name))
            (asdf:system-source-file system)))

        (defun sbcl-builtin-system-p (name)
          (or (member name '("asdf" "uiop") :test #'equal)
              (and (>= (length name) 3)
                   (string= "sb-" name :end1 3 :end2 3))))

        (defun path-under-p (file root)
          (let* ((file-name (namestring (truename file)))
                 (root-name (concatenate 'string
                                         (string-right-trim "/" (namestring (truename root)))
                                         "/")))
            (and (>= (length file-name) (length root-name))
                 (string= root-name file-name
                          :end1 (length root-name)
                          :end2 (length root-name)))))

        (defun system-under-source-p (name source)
          (let ((file (system-source name)))
            (and file (path-under-p file source))))

        (defun collect-dependencies (roots)
          (let ((seen (make-hash-table :test 'equal)))
            (labels ((visit (name)
                       (unless (gethash name seen)
                         (setf (gethash name seen) t)
                     (unless (sbcl-builtin-system-p name)
                       (let ((system (asdf:find-system name nil)))
                         (unless system
                           (error "ASDF cannot resolve system ~A" name))
                         (dolist (dependency (asdf:system-depends-on system))
                           (visit (dependency-name dependency))))))))
              (dolist (root roots)
                (visit root)))
            (loop for name being the hash-keys of seen collect name)))

        (defun provider-map (specs)
          (let ((providers (make-hash-table :test 'equal)))
            (maphash
             (lambda (pname spec)
               (dolist (system (system-spec-systems spec))
                 (pushnew pname (gethash (string-downcase system) providers) :test #'equal)))
             specs)
            providers))

        (defun declared-library-closure (spec specs)
          (let ((seen (make-hash-table :test 'equal)))
            (labels ((visit (pname)
                       (unless (gethash pname seen)
                         (let ((provider (gethash pname specs)))
                           (unless provider
                             (error "No metadata for lispLibs provider ~A" pname))
                           (setf (gethash pname seen) t)
                           (dolist (library (system-spec-libs provider))
                             (visit (library-spec-pname library)))))))
              (dolist (library (system-spec-libs spec))
                (visit (library-spec-pname library))))
            seen))

        (defun source-known-p (file sources)
          (and file (some (lambda (source) (path-under-p file source)) sources)))

        (defun check-spec (spec specs providers sources)
          (let* ((roots (mapcar #'string-downcase (system-spec-systems spec)))
                 (missing-roots (remove-if (lambda (name) (asdf:find-system name nil)) roots)))
            (cond
              (missing-roots
               (error "ASDF cannot resolve selected systems for ~A: ~{~A~^, ~}"
                      (system-spec-pname spec) missing-roots))
              (t
               (let* ((closure (collect-dependencies roots))
                      (required (make-hash-table :test 'equal))
                      (actual (declared-library-closure spec specs))
                      (errors nil))
                 (dolist (name closure)
                   (unless (or (member name roots :test #'equal)
                               (sbcl-builtin-system-p name)
                               (system-under-source-p name (system-spec-source spec)))
                     (let* ((source (system-source name))
                            (owners (gethash name providers)))
                       (cond
                         ((and owners (= (length owners) 1))
                          (setf (gethash (first owners) required) t))
                         ((and owners (> (length owners) 1))
                          (push (format nil "~A has ambiguous providers for ~A: ~{~A~^, ~}"
                                        (system-spec-pname spec) name owners)
                                errors))
                         ((source-known-p source sources)
                          (push (format nil "~A depends on unprovided ASDF system ~A"
                                        (system-spec-pname spec) name)
                        errors))))))
                 (let ((missing (loop for name being the hash-keys of required
                                      unless (gethash name actual) collect name)))
                   (when missing
                     (push (format nil "~A is missing lispLibs providers: ~{~A~^, ~}"
                                   (system-spec-pname spec) (sort missing #'string<))
                           errors)))
                 errors)))))

        (let* ((specs (read-specs (second sb-ext:*posix-argv*)))
               (providers (provider-map specs))
               (sources (loop for spec being the hash-values of specs
                              collect (system-spec-source spec))))
          (maphash
           (lambda (pname spec)
             (declare (ignore pname))
             (dolist (library (system-spec-libs spec))
               (pushnew (library-spec-source library) sources :test #'equal)))
           specs)
          (let ((errors nil))
            (maphash
             (lambda (pname spec)
               (declare (ignore pname))
               (setf errors (nconc (check-spec spec specs providers sources) errors)))
             specs)
            (if errors
                (progn
                  (dolist (message (nreverse errors))
                    (format *error-output* "~A~%" message))
                  (sb-ext:exit :code 1))
                (format t "checked ~D ASDF system builds~%"
                        (hash-table-count specs)))))
        LISP
        sbcl --noinform --script lisplibs-check.lisp asdf-systems.tsv
        touch "$out"
      '';
  testSuiteFor =
    taxonomy:
    pkgs.stdenvNoCC.mkDerivation {
      pname = "cl-cc-tests-${taxonomy}";
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
          # resolves.
          ../docs
          ../run-tests.lisp
          ../cl-cc.asd
        ];
      };
      nativeBuildInputs = [ sbclWithTests ];
      buildPhase = ''
        export HOME="$TMPDIR"
        ${apps.test.program} --suite ${taxonomy}
      '';
      installPhase = "mkdir -p $out && touch $out/passed";
      meta.description = "cl-cc ${taxonomy} test suite";
    };

  testSuites = {
    unit = testSuiteFor "unit";
    integration = testSuiteFor "integration";
    e2e = testSuiteFor "e2e";
    conformance = testSuiteFor "conformance";
  };
in
{
  inherit
    crossRepoInternalRefs
    contextMap
    toolkitPinConsistency
    lisplibsConsistency
    testSuites
    ;

  # The canonical fast unit suite remains available as checks.default.
  testSuite = testSuites.unit;

  jitTests = pkgs.runCommand "cl-cc-jit-tests" { nativeBuildInputs = [ sbclWithJitTests ]; } ''
    export HOME="$TMPDIR/home"
    export XDG_CACHE_HOME="$TMPDIR/cache"
    mkdir -p "$HOME" "$XDG_CACHE_HOME"
    sbcl --non-interactive \
      --eval '(require :asdf)' \
      --eval '(asdf:test-system "cl-cc-jit/tests")'
    touch $out
  '';

  # Standalone cl-weave suite for packages/prolog-tools (the cl-prolog-kit based
  # call-graph analysis tools). Independent of `default` above: that one
  # drives cl-cc's own runner via `nix run .#test`, while this package's tests
  # are cl-weave DESCRIBE/IT specs run through
  # `(asdf:test-system :cl-cc-prolog-tools)`. clPrologKit and clWeave are plain
  # `sbcl.buildASDFSystem` derivations built from source (see flake.nix) —
  # their installPhase mirrors the source tree directly into $out (e.g.
  # $out/cl-prolog-kit.asd), unlike nixpkgs' curated sbclPackages set which nests
  # under /share/common-lisp/source/, so the registry entry is just
  # "${clPrologKit}//", not a share/ subpath.
  prologToolsTests =
    pkgs.runCommand "cl-cc-prolog-tools-tests"
      {
        nativeBuildInputs = [ pkgs.sbcl ];
        src = lib.fileset.toSource {
          root = ../.;
          fileset = lib.fileset.unions [
            ../packages/prolog-tools
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
        # cl-cc-prolog-tools :depends-on :cl-cc-ast, which used to be satisfied
        # by packages/ast in this tree. That copy is gone and the standalone
        # repository is authoritative, so the derivation supplies it.
        export CL_SOURCE_REGISTRY="${clPrologKit}//:${clWeave}//:${clCcAst}//:$PWD//:"
        sbcl --non-interactive \
          --eval '(require :asdf)' \
          --eval '(asdf:load-asd (truename "packages/prolog-tools/cl-cc-prolog-tools.asd"))' \
          --eval '(asdf:test-system :cl-cc-prolog-tools)'
        touch $out
      '';

}
