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
        source = toString provider.asdfMetadata.src;
        systems = provider.asdfMetadata.systems;
      };
    in
    {
      inherit (metadata) pname systems;
      source = toString metadata.src;
      lispLibs = map providerMetadata metadata.lispLibs;
    }
  ) asdfSystemDerivations;

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
  testSuiteFor = taxonomy: pkgs.stdenvNoCC.mkDerivation {
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
  inherit toolkitPinConsistency lisplibsConsistency testSuites;

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
