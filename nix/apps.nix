{
  pkgs,
  lib,
  sbclWithCLCC,
  sbclWithJavascriptTests,
  sbclWithTests,
  sbclBootstrap,
  dispatchSemFix ? null,
  testImage,
}:
let
  sbclFlags = "--dynamic-space-size 8192";

  cwdGuard = ''
    if [ ! -f ./cl-cc.asd ]; then
      echo "cl-cc: run from the project root (cl-cc.asd not found in $PWD)" >&2
      exit 1
    fi
  '';

  # Scales property-based testing down for CI. CLCC_PBT_COUNT governs the
  # remaining home-grown cl-cc/pbt properties; CL_WEAVE_PROPERTY_TESTS governs
  # the ones migrated to cl-weave's native it-property, whose own default is
  # 100. Without the second knob the migrated properties silently ignore this
  # scale-down and run 100 cases each, which is ~33x the configured budget.
  pbtSanitize = ''
    case "''${CLCC_PBT_COUNT:-}" in
      ""|*[!0-9]*) CLCC_PBT_COUNT=3 ;;
    esac
    export CLCC_PBT_COUNT
    case "''${CL_WEAVE_PROPERTY_TESTS:-}" in
      ""|*[!0-9]*) CL_WEAVE_PROPERTY_TESTS="$CLCC_PBT_COUNT" ;;
    esac
    export CL_WEAVE_PROPERTY_TESTS
  '';

  faslCacheCleaner = ''
    rm -rf "$HOME/.cache/common-lisp"
    mkdir -p "$HOME/.cache/common-lisp"
    find . -name "*.fasl" -delete 2>/dev/null || true
  '';

  joinEvals = forms: lib.concatMapStringsSep " " (f: "--eval ${lib.escapeShellArg f}") forms;

  mkSbclScript =
    {
      name,
      description ? "cl-cc ${name} app (run via `nix run .#${name}`)",
      sbclVariant ? "production",
      sbclPkgOverride ? null,
      extraEnv ? "",
      lispPreLoadEvalForms ? [ ],
      lispPostLoadEvalForms ? [ ],
      loadAsdSystems ? [ ],
      # Lisp source files to `--load`, relative to the project root. Used to
      # keep a plan in a tracked .lisp file instead of inline Nix strings, so
      # the same file is runnable with a bare `sbcl --script`.
      loadFiles ? [ ],
      forceReload ? false,
      disableOutputTranslations ? false,
      loadProjectAsd ? true,
      needsRlwrap ? false,
      enableDispatchSemFix ? false,
      enablePbtSanitize ? false,
      enableFaslCacheCleaner ? false,
      enableCwdGuard ? true,
      forwardArgs ? false,
      extraTimeoutSeconds ? 120,
      extraSbclFlags ? [ ],
      trailingScript ? "",
    }:
    let
      sbclPkg =
        if sbclPkgOverride != null then
          sbclPkgOverride
        else if sbclVariant == "tests" then
          sbclWithTests
        else
          sbclWithCLCC;
      sbclBin = "${sbclPkg}/bin/sbcl";
      rlwrapPrefix = lib.optionalString needsRlwrap "${lib.getExe pkgs.rlwrap} ";
      forceFlag = lib.optionalString forceReload " :force t";
      loadSystemEvals = lib.concatMapStringsSep " " (
        sys: "--eval ${lib.escapeShellArg "(asdf:load-system ${sys}${forceFlag})"}"
      ) loadAsdSystems;
      loadFileFlags = lib.concatMapStringsSep " " (f: "--load ${lib.escapeShellArg f}") loadFiles;
      disableTranslationsFlag = lib.optionalString disableOutputTranslations "--eval '(asdf:disable-output-translations)'";
      forwardArgsFlag = lib.optionalString forwardArgs ''
        \
                 --end-toplevel-options "$@"'';
      dispatchExport = lib.optionalString (
        enableDispatchSemFix && pkgs.stdenv.isDarwin && dispatchSemFix != null
      ) ''export DYLD_INSERT_LIBRARIES="${dispatchSemFix}/lib/libdispatch_sem_fix.dylib"'';
      shellSrc = ''
        set -euo pipefail
        ${lib.optionalString enableCwdGuard cwdGuard}
        ${lib.optionalString enablePbtSanitize pbtSanitize}
        ${lib.optionalString enableFaslCacheCleaner faslCacheCleaner}
        ${dispatchExport}
        ${extraEnv}
        export CLCC_TEST_TIMEOUT="''${CLCC_TEST_TIMEOUT:-10}"
        export CLCC_SUITE_TIMEOUT="''${CLCC_SUITE_TIMEOUT:-600}"
        case "$CLCC_TEST_TIMEOUT" in ""|0|*[!0-9]*) CLCC_TEST_TIMEOUT=10 ;; esac
        case "$CLCC_SUITE_TIMEOUT" in ""|0|*[!0-9]*) CLCC_SUITE_TIMEOUT=600 ;; esac
        shell_timeout=$((CLCC_SUITE_TIMEOUT + ${toString extraTimeoutSeconds}))
        ${pkgs.coreutils}/bin/timeout --kill-after=30 "$shell_timeout" ${rlwrapPrefix}${sbclBin} ${sbclFlags} ${lib.concatStringsSep " " extraSbclFlags} \
          --non-interactive \
          ${joinEvals lispPreLoadEvalForms} \
          ${lib.removeSuffix "\n" sbclBootstrap} \
          ${disableTranslationsFlag} \
          ${lib.optionalString loadProjectAsd "--load cl-cc.asd"} \
          ${loadSystemEvals} \
          ${joinEvals lispPostLoadEvalForms} \
          ${loadFileFlags}${forwardArgsFlag}
        ${trailingScript}
      '';
    in
    {
      type = "app";
      meta.description = description;
      program = "${pkgs.writeShellScript "cl-cc-${name}" shellSrc}";
    };

  apps = rec {
    default = repl;

    # `test` runs the canonical fast unit plan via `cl-weave:run-all`
    # (packages/testing-framework's DEFTEST/etc. macros register into
    # cl-weave's suite tree; see framework-definitions.lisp).
    # `nix flake check` invokes this same program through `checks.default`.
    # Warm-cache reuse: the FASL cleaner is disabled by default so repeat
    # invocations stay fast. Manual cleanup: `rm -rf ~/.cache/common-lisp/`.
    #
    # The plan itself lives in the tracked ./run-tests.lisp rather than in Nix
    # strings here. That file is the org-standard Lisp-level entry point and
    # runs on its own with `sbcl --script run-tests.lisp`; keeping a single copy
    # means the fast (core-image) path and the plain path cannot drift apart.
    test = mkSbclScript {
      name = "test";
      description = "Run the canonical fast unit test plan";
      sbclVariant = "tests";
      loadProjectAsd = false;
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = false;
      forwardArgs = true;
      # Load the pre-compiled core image (save-lisp-and-die snapshot).
      # The core has :cl-cc, :cl-cc-cli, :cl-cc-testing-framework pre-loaded and
      # warm-stdlib-cache pre-initialized, so the heavy ASDF loading is skipped.
      # "cl-cc/test" is NOT in the core (test-file top-level forms must not bake
      # Nix sandbox paths into globals); run-tests.lisp loads it fresh after the
      # working-directory reset.
      # Cap worker count to 4: 8 or more workers trigger GC safepoint contention on
      # macOS 26 ARM64 (SBCL 2.6.1). Concurrent SBCL compiler calls (from compiler-macro
      # eval) are now serialised via the *macro-eval-fn* mutex, so 4 workers are safe.
      # Users may override upward via CL_CC_TEST_WORKERS=N nix run .#test.
      extraSbclFlags = [
        "--core"
        "${testImage}/cl-cc-test.core"
      ];
      extraEnv = ''export CL_CC_TEST_WORKERS="''${CL_CC_TEST_WORKERS:-4}"'';
      loadFiles = [ "run-tests.lisp" ];
    };

    coverage = mkSbclScript {
      name = "coverage";
      description = "Run the canonical test plan with sb-cover instrumentation";
      sbclVariant = "tests";
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = true;
      forwardArgs = true;
      forceReload = true;
      extraTimeoutSeconds = 600;
      lispPreLoadEvalForms = [
        "(require :sb-cover)"
        "(declaim (optimize sb-cover:store-coverage-data))"
        "(require :asdf)"
        "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))"
        ''(asdf:initialize-output-translations (quote (:output-translations (t (:home ".cache" "common-lisp" :implementation)) :ignore-inherited-configuration)))''
      ];
      lispPostLoadEvalForms = [
        ''(load (merge-pathnames "cl-cc.asd" *default-pathname-defaults*))''
        ''(format t "# reloading cl-cc/test under sb-cover instrumentation~%")''
        ''
          (handler-case
                        (asdf:load-system "cl-cc/test" :force t)
                      (error (e)
                        (format *error-output* "~&FATAL: ~A~%" e)
                        (uiop:quit 1)))''
        # cl-weave owns the coverage run/reset/report lifecycle (see
        # framework-definitions.lisp); this app no longer excludes the
        # integration/e2e suites from the coverage pass — cl-weave's
        # RUN-ALL always runs the full suite tree from its root.
        ''(format t "# starting coverage test plan (cl-weave + sb-cover)~%")''
        ''
          (let* ((args (uiop:command-line-arguments))
                           (filter (loop for rest on args
                                         when (and (string= (first rest) "--filter") (second rest))
                                           return (second rest))))
                      (handler-case
                          (uiop:quit (if (cl-weave:run-all :reporter :spec
                                                            :name-filter filter
                                                            :coverage t
                                                            :coverage-report-directory "coverage-report/")
                                         0 1))
                        (error (e)
                          (format t "~&not ok - coverage fatal error: ~A~%" e)
                          (format *error-output* "~&FATAL: ~A~%" e)
                          (uiop:quit 1))))''
      ];
    };

    coverage-js = mkSbclScript {
      name = "coverage-js";
      description = "Run the JavaScript frontend tests with sb-cover instrumentation";
      sbclPkgOverride = sbclWithJavascriptTests;
      enableDispatchSemFix = true;
      enablePbtSanitize = true;
      enableFaslCacheCleaner = true;
      forwardArgs = true;
      forceReload = true;
      extraTimeoutSeconds = 600;
      loadProjectAsd = false;
      lispPreLoadEvalForms = [
        "(require :sb-cover)"
        "(declaim (optimize sb-cover:store-coverage-data))"
        "(require :asdf)"
        "(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))"
        ''(asdf:initialize-output-translations (quote (:output-translations (t (:home ".cache" "common-lisp" :implementation)) :ignore-inherited-configuration)))''
      ];
      lispPostLoadEvalForms = [
        ''(format t "# enabling coverage before reloading :cl-cc and :cl-cc-javascript-test~%")''
        ''
          (handler-case
                        (asdf:load-system :cl-cc-type :force t)
                      (error (e)
                        (format *error-output* "~&FATAL: ~A~%" e)
                        (uiop:quit 1)))''
        ''
          (handler-case
                        (load "packages/testing-framework/src/package.lisp")
                      (error (e)
                        (format *error-output* "~&FATAL: ~A~%" e)
                        (uiop:quit 1)))''
        "(proclaim '(optimize (sb-cover:store-coverage-data 3)))"
        ''
          (handler-case
                        (asdf:load-system :cl-cc :force t)
                      (error (e)
                        (format *error-output* "~&FATAL: ~A~%" e)
                        (uiop:quit 1)))''
        ''
          (handler-case
                        (asdf:load-system :cl-cc-javascript-test :force t)
                      (error (e)
                        (format *error-output* "~&FATAL: ~A~%" e)
                        (uiop:quit 1)))''
        # :cl-cc-javascript-test depends only on :cl-cc/:cl-cc-testing-framework/
        # :cl-cc-javascript (NOT the "cl-cc/test" aggregate), so cl-weave's global
        # suite tree in this process already contains nothing but the JS tests —
        # no extra suite/location scoping is needed.
        ''(format t "# starting coverage test plan (cl-weave + sb-cover, javascript)~%")''
        ''
          (let* ((args (uiop:command-line-arguments))
                           (filter (loop for rest on args
                                         when (and (string= (first rest) "--filter") (second rest))
                                           return (second rest))))
                      (handler-case
                          (uiop:quit (if (cl-weave:run-all :reporter :spec
                                                            :name-filter filter
                                                            :coverage t
                                                            :coverage-report-directory "coverage-report-js/")
                                         0 1))
                        (error (e)
                          (format t "~&not ok - coverage fatal error: ~A~%" e)
                          (format *error-output* "~&FATAL: ~A~%" e)
                          (uiop:quit 1))))''
      ];
    };

    load = mkSbclScript {
      name = "load";
      sbclVariant = "production";
      disableOutputTranslations = true;
      loadAsdSystems = [ ":cl-cc" ];
    };

    # Singular `bench`, per PERFORMANCE_STANDARD.md. Diagnostic only: there is
    # deliberately no `checks.bench`, because `nix flake check` would then run
    # benchmarks on every pull request and a shared GitHub runner's wall-clock
    # variance would turn an unchanged commit red on re-run.
    bench = mkSbclScript {
      name = "bench";
      description = "Run all registered benchmarks and write JSON results to benchmark-results/";
      sbclVariant = "tests";
      enableDispatchSemFix = true;
      enableFaslCacheCleaner = true;
      extraSbclFlags = [
        "--core"
        "${testImage}/cl-cc-test.core"
      ];
      lispPostLoadEvalForms = [
        "(setf *default-pathname-defaults* (uiop:getcwd))"
        "(setf uiop:*temporary-directory* (uiop:temporary-directory))"
        ''(load (merge-pathnames "cl-cc.asd" *default-pathname-defaults*))''
        # No :force — the FASLs ship pre-compiled in sbclWithTests; forcing a
        # recompile writes into the read-only Nix store and fails on CI.
        "(asdf:load-system \"cl-cc/test\")"
        ''(format t "# running all benchmarks~%")''
        ''
          (handler-case
                       (cl-cc/test:run-all-benchmarks
                        :output-directory #p"benchmark-results/")
                     (error (e)
                       (format *error-output* "~&FATAL: ~A~%" e)
                       (uiop:quit 1)))''
      ];
    };

    repl = mkSbclScript {
      name = "repl";
      sbclVariant = "production";
      needsRlwrap = true;
      disableOutputTranslations = true;
      loadAsdSystems = [ ":cl-cc" ];
    };
  };
in
{
  inherit mkSbclScript apps;
}
