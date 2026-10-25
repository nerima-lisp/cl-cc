import json
import os
import re
import signal
import subprocess
import sys
import tempfile
import threading
import unittest
from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

if __package__:
    from . import run
else:
    import run


class PhptRunnerTests(unittest.TestCase):
    def test_parse_sections_preserves_payload_bytes(self):
        parsed = run.parse_phpt(
            b"--TEST--\nexample\n--FILE--\n<?php echo 42; ?>\n"
            b"--STDIN--\ninput bytes\n--EXPECT--\n42\n"
        )
        self.assertEqual(parsed["FILE"], b"<?php echo 42; ?>\n")
        self.assertEqual(parsed["STDIN"], b"input bytes\n")
        self.assertEqual(parsed["EXPECT"], b"42\n")

    def test_parse_rejects_duplicate_sections(self):
        with self.assertRaisesRegex(run.HarnessError, "duplicate section EXPECT"):
            run.parse_phpt(b"--TEST--\nx\n--EXPECT--\nx\n--EXPECT--\ny\n")

    def test_invalid_utf8_php_source_uses_scoped_reversible_byte_mapping(self):
        """Map each source octet to U+00xx, UTF-8 transport it, then reverse output bytes."""
        source = b'<?php echo "\xe5\xc4"; ?>\n'
        for case_id in run.BYTE_PRESERVING_CASES:
            with self.subTest(case_id=case_id):
                transported = run._transport_php_source(case_id, source)
                self.assertEqual(
                    transported.decode("utf-8"), '<?php echo "\u00e5\u00c4"; ?>\n'
                )
                self.assertEqual(
                    run._encode_driver_output("\u00e5\u00c4", byte_preserving=True),
                    b"\xe5\xc4",
                )
        self.assertEqual(
            run._transport_php_source(
                "ext/standard/tests/strings/htmlentities.phpt",
                '<?php echo "é"; ?>\n'.encode(),
            ),
            '<?php echo "é"; ?>\n'.encode(),
        )
        self.assertEqual(
            run._transport_php_source("Zend/example.phpt", source), source
        )

    def test_byte_mapped_output_serializes_non_latin1_as_utf8(self):
        output = "Latin-1: \u00e9; Unicode: \U0001f642"
        self.assertEqual(
            run._encode_driver_output(output, byte_preserving=True),
            b"Latin-1: \xe9; Unicode: \xf0\x9f\x99\x82",
        )
        self.assertEqual(
            run._encode_driver_output(output, byte_preserving=False),
            output.encode("utf-8"),
        )

    def test_phase_result_json_round_trips_byte_mapped_and_unicode_output(self):
        record = {
            "id": "ext/standard/tests/strings/utf8.phpt",
            "phase": "main",
            "status": "passed",
            "stdout": "Latin-1: \u00e9; Unicode: \U0001f642",
            "stderr": "diagnostic \U0001f642",
            "detail": "",
        }
        with tempfile.TemporaryDirectory() as directory:
            result_path = Path(directory) / "result.json"
            result_path.write_text(
                json.dumps(record, ensure_ascii=False), encoding="utf-8"
            )
            outcome = run._read_phase_result(
                result_path, record["id"], "main", byte_preserving_output=True
            )
        self.assertEqual(
            outcome.stdout, b"Latin-1: \xe9; Unicode: \xf0\x9f\x99\x82"
        )
        self.assertEqual(outcome.stderr, "diagnostic \U0001f642")

    def test_mail_ini_placeholder_expands_target_like_run_tests(self):
        case_path = Path("/php-src/ext/standard/tests/mail/mail_basic.phpt")
        cases = (
            (b"mail.out", "tee mail.out >/dev/null"),
            (
                b"{PWD}/mail.out",
                f"tee {case_path.parent}/mail.out >/dev/null",
            ),
        )
        for target, expected in cases:
            with self.subTest(target=target):
                case = run.PhptCase(
                    "ext/standard/tests/mail/mail_basic.phpt",
                    case_path,
                    {"INI": b"sendmail_path={MAIL:" + target + b"}\n"},
                    "mail test",
                )
                self.assertEqual(
                    run.parse_ini_settings(case, {})[0],
                    ("sendmail_path", expected),
                )

    def test_expectf_supports_php_placeholders_and_raw_regex(self):
        matcher = run.expectf_pattern("path=%e%s id=%d %r(?:ok|fine)%r")
        self.assertTrue(matcher.fullmatch("path=/tmp/x id=42 ok"))
        self.assertFalse(matcher.fullmatch("path= id=42 ok"))

    def test_manifest_requires_reason_and_exact_failure_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "expected-failures.sexp"
            manifest.write_text(
                '(("Zend/example.phpt" :classification :unimplemented '
                ':reason "missing iterator lowering" :match "iterator not supported" '
                ":stage :compile))\n",
                encoding="utf-8",
            )
            expected = run.load_expected_failures(manifest)
        self.assertTrue(
            run.apply_expected_failures(
                "Zend/example.phpt",
                "compile",
                "iterator not supported: foreach",
                expected,
                set(),
            )[0]
        )
        self.assertFalse(
            run.apply_expected_failures(
                "Zend/example.phpt",
                "compile",
                "different compiler error",
                expected,
                set(),
            )[0]
        )

    def test_manifest_cases_group_expands_to_individual_expected_failures(self):
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "expected-failures.sexp"
            manifest.write_text(
                '((:cases ("Zend/one.phpt" "Zend/two.phpt") '
                ':classification :unimplemented :reason "missing lowering" '
                ':match "unsupported iterator" :stage :compile))\n',
                encoding="utf-8",
            )
            expected = run.load_expected_failures(manifest)

        self.assertEqual(set(expected), {"Zend/one.phpt", "Zend/two.phpt"})
        self.assertEqual(expected["Zend/one.phpt"].case_id, "Zend/one.phpt")
        self.assertEqual(expected["Zend/two.phpt"].case_id, "Zend/two.phpt")
        consumed = set()
        for case_id in sorted(expected):
            accepted, _ = run.apply_expected_failures(
                case_id, "compile", "unsupported iterator opcode", expected, consumed
            )
            self.assertTrue(accepted)
        self.assertEqual(consumed, set(expected))

    def test_manifest_rejects_duplicate_case_ids_across_groups(self):
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "expected-failures.sexp"
            manifest.write_text(
                '((:cases ("Zend/one.phpt") :classification :bug '
                ':reason "reason" :match "error" :stage :runtime) '
                '("Zend/one.phpt" :classification :bug :reason "reason" '
                ':match "error" :stage :runtime))\n',
                encoding="utf-8",
            )
            with self.assertRaisesRegex(run.HarnessError, "duplicate expected-failure test"):
                run.load_expected_failures(manifest)

    def test_manifest_rejects_unprefixed_expected_failure_stage(self):
        with tempfile.TemporaryDirectory() as directory:
            manifest = Path(directory) / "expected-failures.sexp"
            manifest.write_text(
                '(("Zend/example.phpt" :classification :unimplemented '
                ':reason "missing iterator lowering" :match "iterator not supported" '
                ":stage xcompile))\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(run.HarnessError, "invalid stage"):
                run.load_expected_failures(manifest)

    def test_manifest_rejects_unknown_shard_roots(self):
        entry = run.ExpectedFailure(
            "Zend/example.phpt", "unimplemented", "reason", re.compile("error"), "compile"
        )
        with self.assertRaisesRegex(run.HarnessError, "unknown shard root"):
            run.validate_expected_failure_roots({"Zendish/example.phpt": entry})

    def test_scan_split_is_recursive_and_sorted(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "Zend" / "nested").mkdir(parents=True)
            (root / "Zend" / "z.phpt").touch()
            (root / "Zend" / "nested" / "a.phpt").touch()
            self.assertEqual(
                [path.relative_to(root).as_posix() for path in run.scan_split(root, "Zend")],
                ["Zend/nested/a.phpt", "Zend/z.phpt"],
            )

    def test_shard_syntax_ranges_and_partition_are_stable(self):
        items = ["a", "b", "c", "d", "e"]
        self.assertEqual(run.parse_shard("1/2"), (1, 2))
        self.assertEqual(run.select_shard(items, "1/2"), ["a", "c", "e"])
        self.assertEqual(run.select_shard(items, "2/2"), ["b", "d"])
        self.assertEqual(run.select_shard(items, "2/2"), ["b", "d"])
        self.assertEqual(run.select_shard(items, "3/8"), ["c"])
        with self.assertRaisesRegex(run.HarnessError, "selects no PHPT cases"):
            run.select_shard(["a", "b"], "3/3")
        for invalid in ("", "0/2", "1/0", "3/2", "1", "x/2", "1/2/3"):
            with self.subTest(invalid=invalid):
                with self.assertRaises(run.HarnessError):
                    run.parse_shard(invalid)

    def test_shard_selection_keeps_manifest_stale_validation_on_full_split(self):
        with tempfile.TemporaryDirectory() as directory:
            php_src = Path(directory)
            for name in ("a", "b"):
                case_path = php_src / "Zend" / f"{name}.phpt"
                case_path.parent.mkdir(parents=True, exist_ok=True)
                case_path.write_bytes(
                    f"--TEST--\n{name}\n--FILE--\n<?php echo 'ok'; ?>\n--EXPECT--\nok\n".encode()
                )
            expected = {
                "Zend/a.phpt": run.ExpectedFailure(
                    "Zend/a.phpt", "bug", "future failure", re.compile("error"), "runtime"
                ),
                "Zend/missing.phpt": run.ExpectedFailure(
                    "Zend/missing.phpt", "bug", "stale", re.compile("error"), "runtime"
                ),
            }
            stdout = StringIO()
            stderr = StringIO()
            with (
                mock.patch.object(run.shutil, "which", return_value="sbcl-test"),
                mock.patch.object(run, "load_expected_failures", return_value=expected),
                mock.patch.object(run, "prepare_core"),
                mock.patch.object(
                    run,
                    "run_case",
                    return_value={"main": run.Outcome("passed", stdout=b"ok\n")},
                ) as run_case,
                redirect_stdout(stdout),
                redirect_stderr(stderr),
            ):
                status = run.run_shard(php_src, "Zend", shard="2/2")

            self.assertEqual(status, 1)
            self.assertEqual(run_case.call_count, 1)
            self.assertEqual(run_case.call_args.args[0].case.case_id, "Zend/b.phpt")
            self.assertIn("total=1", stdout.getvalue())
            self.assertIn(
                "Zend/missing.phpt: stale expected-failure entry: test was not discovered",
                stderr.getvalue(),
            )
            self.assertNotIn("Zend/a.phpt: stale expected-failure", stderr.getvalue())

    def test_literal_sibling_inc_dependencies_are_staged_recursively_and_once(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "php-src" / "tests" / "basic"
            root.mkdir(parents=True)
            case_path = root / "include.phpt"
            first = root / "first.inc"
            second = root / "second.inc"
            nested_dir = root / "include_files"
            nested_dir.mkdir()
            nested = nested_dir / "function.inc"
            nested_sibling = nested_dir / "echo.inc"
            first.write_bytes(b"<?php include_once 'second.inc'; ?>\n")
            second.write_bytes(b"<?php require_once 'first.inc'; ?>\n")
            nested.write_bytes(b"<?php require_once 'echo.inc'; ?>\n")
            nested_sibling.write_bytes(b"<?php echo 'nested'; ?>\n")
            case = run.PhptCase(
                "tests/basic/include.phpt",
                case_path,
                {
                    "FILE": (
                        b"<?php include 'first.inc'; include_once(\"second.inc\"); ?>\n"
                        b"<?php include 'include_files/function.inc'; ?>\n"
                        b"// include 'commented-out.inc';\n"
                        b"$text = \"require 'inside-string.inc';\";\n"
                    ),
                    "EXPECT": b"ok\n",
                },
                "include dependencies",
            )
            workdir = Path(directory) / "case-work"
            workdir.mkdir()

            run.stage_literal_includes(case, workdir)

            self.assertEqual((workdir / "first.inc").read_bytes(), first.read_bytes())
            self.assertEqual((workdir / "second.inc").read_bytes(), second.read_bytes())
            self.assertEqual(
                (workdir / "include_files" / "function.inc").read_bytes(),
                nested.read_bytes(),
            )
            self.assertEqual(
                (workdir / "include_files" / "echo.inc").read_bytes(),
                nested_sibling.read_bytes(),
            )
            self.assertEqual(
                run.literal_sibling_includes(case.sections["FILE"]),
                ["first.inc", "second.inc", "include_files/function.inc"],
            )

    def test_literal_inc_staging_rejects_paths_outside_test_directory(self):
        with tempfile.TemporaryDirectory() as directory:
            parent = Path(directory) / "php-src" / "tests"
            root = parent / "basic"
            root.mkdir(parents=True)
            (parent / "outside.inc").write_bytes(b"<?php ?>")
            case = run.PhptCase(
                "tests/basic/include.phpt",
                root / "include.phpt",
                {"FILE": b"<?php require '../outside.inc'; ?>", "EXPECT": b""},
                "path containment",
            )
            workdir = Path(directory) / "work"
            workdir.mkdir()
            with self.assertRaisesRegex(run.HarnessError, "escapes or is missing"):
                run.stage_literal_includes(case, workdir)

    def test_cookie_only_case_skips_when_cgi_is_unavailable(self):
        with tempfile.TemporaryDirectory() as directory:
            php_src = Path(directory)
            case_path = php_src / "Zend" / "cookie.phpt"
            case_path.parent.mkdir(parents=True)
            case_path.write_bytes(
                b"--TEST--\nCookie needs CGI\n"
                b"--FILE--\n<?php echo 'should not run'; ?>\n"
                b"--COOKIE--\nsession=id\n"
                b"--EXPECT--\nshould not run\n"
            )
            observed = []
            real_outcome = run.Outcome

            def record_outcome(status, *args, **kwargs):
                outcome = real_outcome(status, *args, **kwargs)
                observed.append(outcome)
                return outcome

            with mock.patch.object(run.shutil, "which", return_value="sbcl-test"):
                with mock.patch.object(run, "load_expected_failures", return_value={}):
                    with mock.patch.object(run, "Outcome", side_effect=record_outcome):
                        with mock.patch.object(
                            run,
                            "prepare_core",
                            side_effect=AssertionError("CGI case reached core preparation"),
                        ) as prepare_core:
                            result = run.run_shard(php_src, "Zend")

            self.assertEqual(result, 0)
            self.assertEqual(
                [(outcome.status, outcome.detail) for outcome in observed],
                [("skipped", "CGI SAPI is unavailable")],
            )
            prepare_core.assert_not_called()

    def test_external_php_route_cases_skip_before_core_setup(self):
        fixtures = (
            (
                "bug67198.phpt",
                b"<?php php_cli_server_start(); ?>\n",
                "requires an external PHP CLI development server; the server route cannot be exercised by cl-cc",
            ),
            (
                "bug71273.phpt",
                b"<?php $cmd = getenv('TEST_PHP_EXECUTABLE_ESCAPED'); ?>\n",
                "uses TEST_PHP_EXECUTABLE_ESCAPED to invoke external PHP; that route does not test cl-cc",
            ),
        )
        with tempfile.TemporaryDirectory() as directory:
            php_src = Path(directory)
            for filename, source, _ in fixtures:
                path = php_src / "tests" / "basic" / filename
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(
                    b"--TEST--\nexternal PHP route\n"
                    b"--FILE--\n" + source
                    + b"--EXPECT--\nignored\n"
                )

            observed = []
            real_outcome = run.Outcome

            def record_outcome(status, *args, **kwargs):
                outcome = real_outcome(status, *args, **kwargs)
                observed.append(outcome)
                return outcome

            with (
                mock.patch.object(run.shutil, "which", return_value="sbcl-test"),
                mock.patch.object(run, "load_expected_failures", return_value={}),
                mock.patch.object(run, "Outcome", side_effect=record_outcome),
                mock.patch.object(
                    run,
                    "prepare_core",
                    side_effect=AssertionError("external PHP route reached core setup"),
                ) as prepare_core,
                redirect_stdout(StringIO()) as stdout,
                redirect_stderr(StringIO()) as stderr,
            ):
                status = run.run_shard(php_src, "tests")

            self.assertEqual(status, 0)
            self.assertIn("total=2", stdout.getvalue())
            self.assertIn("skipped=2", stdout.getvalue())
            self.assertEqual(
                sorted(outcome.detail for outcome in observed),
                sorted(reason for _, _, reason in fixtures),
            )
            for filename, _, reason in fixtures:
                case_id = f"tests/basic/{filename}"
                self.assertIn(f"{case_id}: skipped: {reason}", stderr.getvalue())
            prepare_core.assert_not_called()

    def test_cli_args_skip_and_existing_gate_precedence(self):
        fixtures = (
            ("args-only", b"--ARGS--\nvalue\n"),
            ("args-phpdbg", b"--ARGS--\nvalue\n--PHPDBG--\n"),
            ("args-cgi", b"--ARGS--\nvalue\n--GET--\nname=value\n"),
            ("args-extension", b"--ARGS--\nvalue\n--EXTENSIONS--\ngd\n"),
            (
                "args-redirect",
                b"--ARGS--\nvalue\n--REDIRECTTEST--\nreturn [];\n",
            ),
            (
                "args-redirect-extension",
                b"--ARGS--\nvalue\n--EXTENSIONS--\ngd\n"
                b"--REDIRECTTEST--\nreturn [];\n",
            ),
        )
        with tempfile.TemporaryDirectory() as directory:
            php_src = Path(directory)
            for name, extra in fixtures:
                case_path = php_src / "Zend" / f"{name}.phpt"
                case_path.parent.mkdir(parents=True, exist_ok=True)
                case_path.write_bytes(
                    b"--TEST--\n" + name.encode() + b"\n"
                    b"--FILE--\n<?php ?>\n"
                    + extra
                    + b"--EXPECT--\nok\n"
                )

            observed = []
            real_outcome = run.Outcome

            def record_outcome(status, *args, **kwargs):
                outcome = real_outcome(status, *args, **kwargs)
                observed.append(outcome)
                return outcome

            with (
                mock.patch.object(run.shutil, "which", return_value="sbcl-test"),
                mock.patch.object(run, "load_expected_failures", return_value={}),
                mock.patch.object(run, "Outcome", side_effect=record_outcome),
                mock.patch.object(
                    run,
                    "prepare_core",
                    side_effect=AssertionError("a gated CLI ARGS test reached core preparation"),
                ) as prepare_core,
                redirect_stdout(StringIO()) as stdout,
            ):
                status = run.run_shard(php_src, "Zend")

            self.assertEqual(status, 0)
            self.assertIn("total=6", stdout.getvalue())
            self.assertIn("skipped=6", stdout.getvalue())
            self.assertIn("args-skipped=2", stdout.getvalue())
            self.assertEqual(
                sorted((outcome.status, outcome.detail) for outcome in observed),
                sorted(
                    [
                        ("skipped", run.ARGS_SKIP_REASON),
                        ("skipped", "PHPDBG SAPI is unavailable"),
                        ("skipped", "CGI SAPI is unavailable"),
                        ("skipped", "required extension unavailable: gd"),
                        ("skipped", run.ARGS_SKIP_REASON),
                        ("skipped", "required extension unavailable: gd"),
                    ]
                ),
            )
            prepare_core.assert_not_called()

    def test_redirecttests_check_extensions_before_file_validation(self):
        fixtures = (
            ("pdo_dblib", ("pdo_dblib",), None),
            ("pdo_firebird", ("pdo_firebird",), b"<?php # vim:ft=php ?>\n"),
            ("pdo_mysql", ("pdo", "pdo_mysql"), None),
            (
                "pdo_odbc",
                ("pdo_odbc",),
                b"<?php if (true) { die('skip - no ODBC DSN'); } ?>\n",
            ),
            ("pdo_pgsql", ("pdo_pgsql",), None),
            ("pdo_sqlite", ("pdo_sqlite",), None),
        )
        with tempfile.TemporaryDirectory() as directory:
            php_src = Path(directory)
            for driver, extensions, skipif in fixtures:
                path = php_src / "ext" / driver / "tests" / "common.phpt"
                path.parent.mkdir(parents=True)
                sections = [
                    b"--TEST--\nPDO redirect " + driver.encode() + b"\n",
                    b"--EXTENSIONS--\n" + "\n".join(extensions).encode() + b"\n",
                ]
                if skipif is not None:
                    sections.append(b"--SKIPIF--\n" + skipif)
                sections.append(
                    b"--REDIRECTTEST--\nreturn ['TESTS' => 'ext/pdo/tests'];\n"
                )
                path.write_bytes(b"".join(sections))

            with (
                mock.patch.object(run.shutil, "which", return_value="sbcl-test"),
                mock.patch.object(run, "load_expected_failures", return_value={}),
                mock.patch.object(run, "prepare_core") as prepare_core,
            ):
                skipped_stdout = StringIO()
                with redirect_stdout(skipped_stdout):
                    skipped_status = run.run_shard(php_src, "ext")

                self.assertEqual(skipped_status, 0)
                self.assertIn("total=6", skipped_stdout.getvalue())
                self.assertIn("skipped=6", skipped_stdout.getvalue())

                available_extensions = run.AVAILABLE_EXTENSIONS | {
                    extension
                    for _, extensions, _ in fixtures
                    for extension in extensions
                }
                error_stdout = StringIO()
                error_stderr = StringIO()
                with (
                    mock.patch.object(run, "AVAILABLE_EXTENSIONS", available_extensions),
                    redirect_stdout(error_stdout),
                    redirect_stderr(error_stderr),
                ):
                    error_status = run.run_shard(php_src, "ext")

                self.assertEqual(error_status, 1)
                self.assertIn("harness-errors=6", error_stdout.getvalue())
                for driver, _, _ in fixtures:
                    case_id = f"ext/{driver}/tests/common.phpt"
                    self.assertIn(
                        f"{case_id}: harness error: unsupported PHPT sections: REDIRECTTEST",
                        error_stderr.getvalue(),
                    )
                prepare_core.assert_not_called()


    def _plan(
        self,
        directory,
        *,
        skipif=None,
        clean=None,
        stdin=None,
        capture="111",
        sections=None,
    ):
        case_path = directory / "php-src" / "Zend" / "example.phpt"
        case_path.parent.mkdir(parents=True, exist_ok=True)
        case = run.PhptCase(
            "Zend/example.phpt",
            case_path,
            sections
            or {
                "TEST": b"example",
                "FILE": b"<?php echo 'ok'; ?>",
                "EXPECT": b"ok",
            },
            "example",
        )
        source = directory / "source" / "main.php"
        source.parent.mkdir(parents=True, exist_ok=True)
        source.write_bytes(case.sections["FILE"])
        skipif_path = None
        if skipif is not None:
            skipif_path = directory / "source" / "skipif.php"
            skipif_path.write_bytes(skipif)
        clean_path = None
        if clean is not None:
            clean_path = directory / "source" / "clean.php"
            clean_path.write_bytes(clean)
        stdin_path = None
        if stdin is not None:
            stdin_path = directory / "source" / "stdin"
            stdin_path.write_bytes(stdin)
        workdir = directory / "work"
        workdir.mkdir(parents=True, exist_ok=True)
        return run.ExecutionPlan(
            case,
            source,
            workdir,
            directory / "driver" / "case.progress",
            skipif_path,
            clean_path,
            stdin_path,
            (("display_errors", "1"),),
            capture,
            {"PATH": "/bin", "PHPT_CASE_ENV": "present"},
        )

    def test_request_json_round_trips_supported_escapes_and_rejects_other_controls(self):
        value = 'quoted "text" \\path\nnext\rreturn\ttab'
        request = {"id": value, "phase": "main", "source_path": value}
        serialized = run._request_json_text(request)
        self.assertEqual(json.loads(serialized), request)
        with self.assertRaisesRegex(run.HarnessError, "control character"):
            run._request_json_text({"id": "bad" + chr(1)})

    def test_phase_artifacts_keep_phase_names_and_are_distinct(self):
        progress = Path("driver") / "case.progress"
        artifacts = {
            phase: run._phase_artifacts(progress, phase)
            for phase in ("skipif", "main", "clean")
        }
        self.assertEqual(
            [path.name for path in artifacts["skipif"]],
            ["case.skipif.progress", "case.skipif.request.json", "case.skipif.result.json"],
        )
        self.assertEqual(
            [path.name for path in artifacts["main"]],
            ["case.main.progress", "case.main.request.json", "case.main.result.json"],
        )
        self.assertEqual(
            [path.name for path in artifacts["clean"]],
            ["case.clean.progress", "case.clean.request.json", "case.clean.result.json"],
        )
        self.assertEqual(len({path for group in artifacts.values() for path in group}), 9)

    def test_prepare_core_loads_driver_json_parser_system(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            core = root / "cl-cc.core"

            def save_core(command, **kwargs):
                core.touch()
                return subprocess.CompletedProcess(command, 0, b"", b"")

            with mock.patch.object(run.subprocess, "run", side_effect=save_core) as call:
                run.prepare_core(root, core, "sbcl-test", root)

            bootstrap = (root / "prepare-core.lisp").read_text(encoding="utf-8")
            self.assertIn("(asdf:load-system :cl-cc-tools)", bootstrap)
            self.assertLess(
                bootstrap.index("(asdf:load-system :cl-cc)"),
                bootstrap.index("(asdf:load-system :cl-cc-tools)"),
            )
            self.assertEqual(call.call_args.args[0][0], "sbcl-test")

    def test_run_case_uses_a_distinct_deadline_for_each_phase(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(
                root,
                skipif=b"<?php echo ''; ?>",
                clean=b"<?php ?>",
                stdin=b"input\n",
            )
            plan.environment["TEST_TIMEOUT"] = "12.5"
            calls = []

            def run_phase(plan_arg, phase, source, stdin, capture, project_root,
                          core_path, sbcl, deadline):
                calls.append((phase, source, stdin, capture, deadline))
                return run.Outcome("passed", stdout=b"ok" if phase == "main" else b"")

            with (
                mock.patch.object(run, "_run_phase", side_effect=run_phase),
                mock.patch.object(run.time, "monotonic", side_effect=[100.0, 200.0, 300.0]),
            ):
                phases = run.run_case(plan, root, root / "cl-cc.core", "sbcl")

            self.assertEqual([call[0] for call in calls], ["skipif", "main", "clean"])
            self.assertEqual([call[4] for call in calls], [112.5, 212.5, 312.5])
            self.assertEqual(calls[0][3], "010")
            self.assertEqual(calls[1][2], plan.stdin)
            self.assertEqual(calls[1][3], "111")
            self.assertIsNone(calls[2][2])
            self.assertEqual(calls[2][3], "010")
            self.assertEqual(set(phases), {"skipif", "main", "clean"})

    def test_phase_timeout_uses_positive_test_timeout_and_safe_default(self):
        with tempfile.TemporaryDirectory() as directory:
            plan = self._plan(Path(directory))
            self.assertEqual(run._phase_timeout_seconds(plan), 60)

            plan.environment["TEST_TIMEOUT"] = "12.5"
            self.assertEqual(run._phase_timeout_seconds(plan), 12.5)

            for invalid_timeout in ("0", "-1", "nan", "inf", "not-a-number"):
                with self.subTest(invalid_timeout=invalid_timeout):
                    plan.environment["TEST_TIMEOUT"] = invalid_timeout
                    self.assertEqual(run._phase_timeout_seconds(plan), 60)

    def test_run_case_skipif_suppresses_main_and_clean(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root, skipif=b"<?php echo 'skip unavailable'; ?>", clean=b"<?php ?>")
            calls = []

            def run_phase(plan_arg, phase, source, stdin, capture, project_root,
                          core_path, sbcl, deadline):
                calls.append(phase)
                return run.Outcome("passed", stdout=b" \n skip unavailable \n")

            with mock.patch.object(run, "_run_phase", side_effect=run_phase):
                phases = run.run_case(plan, root, root / "cl-cc.core", "sbcl")

            self.assertEqual(calls, ["skipif"])
            self.assertEqual(set(phases), {"skipif"})
            self.assertEqual(
                run._case_outcome(plan.case, phases).status,
                "skipped",
            )

    def test_skipif_compile_runtime_failures_are_php_failures_only(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root, skipif=b"<?php ?>")
            for stage in ("compile", "runtime"):
                with self.subTest(stage=stage):
                    diagnostic = f"{stage} diagnostic"
                    outcome = run._case_outcome(
                        plan.case,
                        {
                            "skipif": run.Outcome(
                                stage,
                                detail=diagnostic,
                                stderr=f"stderr {diagnostic}",
                            )
                        },
                    )
                    self.assertEqual(outcome.status, stage)
                    self.assertIn(f"SKIPIF {stage} failure", outcome.detail)
                    self.assertIn(diagnostic, outcome.detail)
                    self.assertEqual(outcome.stderr, f"stderr {diagnostic}")

            for stage in ("source", "invalid", "setup"):
                with self.subTest(stage=stage):
                    outcome = run._case_outcome(
                        plan.case,
                        {"skipif": run.Outcome(stage, detail=f"{stage} diagnostic")},
                    )
                    self.assertEqual(outcome.status, "harness_error")
                    self.assertIn("SKIPIF", outcome.detail)
                    self.assertIn(f"{stage} diagnostic", outcome.detail)

    def test_run_case_runs_clean_even_after_main_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root, clean=b"<?php echo 'cleanup'; ?>")
            calls = []

            def run_phase(plan_arg, phase, source, stdin, capture, project_root,
                          core_path, sbcl, deadline):
                calls.append(phase)
                if phase == "main":
                    return run.Outcome("runtime", detail="main runtime failure")
                return run.Outcome("passed", stdout=b"cleanup")

            with mock.patch.object(run, "_run_phase", side_effect=run_phase):
                phases = run.run_case(plan, root, root / "cl-cc.core", "sbcl")

            self.assertEqual(calls, ["main", "clean"])
            self.assertEqual(phases["clean"].stdout, b"cleanup")
            self.assertEqual(run._case_outcome(plan.case, phases).status, "runtime")

    def test_phase_request_uses_json_contract_and_child_environment(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            sections = run.parse_phpt(
                b"--TEST--\nrequest stdin\n--FILE--\n<?php echo 'ok'; ?>\n"
                b"--STDIN--\nfeed me\n--EXPECT--\nok\n"
            )
            plan = self._plan(root, stdin=sections["STDIN"], sections=sections)
            observed = {}

            def run_child(command, cwd, environment, deadline):
                request_path = Path(command[-2])
                result_path = Path(command[-1])
                observed.update(
                    command=command,
                    cwd=cwd,
                    environment=environment,
                    deadline=deadline,
                    request=json.loads(request_path.read_text(encoding="utf-8")),
                )
                result_path.write_text(
                    json.dumps(
                        {
                            "id": plan.case.case_id,
                            "phase": "main",
                            "status": "passed",
                            "stdout": "ok",
                            "stderr": "",
                            "detail": "",
                        }
                    ),
                    encoding="utf-8",
                )
                return run.ChildResult(0, b"", b"", False)

            deadline = run.time.monotonic() + 10
            with mock.patch.object(run, "run_child", side_effect=run_child):
                outcome = run._run_phase(
                    plan,
                    "main",
                    plan.source,
                    plan.stdin,
                    plan.capture,
                    root,
                    root / "cl-cc.core",
                    "sbcl-test",
                    deadline,
                )

            self.assertEqual(outcome, run.Outcome("passed", stdout=b"ok"))
            self.assertEqual(
                observed["request"],
                {
                    "id": "Zend/example.phpt",
                    "phase": "main",
                    "source_path": str(plan.source.resolve()),
                    "working_directory": str(plan.workdir.resolve()),
                    "progress_path": str(run._phase_artifacts(plan.progress, "main")[0].resolve()),
                    "ini_settings": [["display_errors", "1"]],
                    "capture": "111",
                    "stdin_path": str(plan.stdin.resolve()),
                },
            )
            self.assertEqual(
                observed["command"],
                [
                    "sbcl-test",
                    "--core",
                    str((root / "cl-cc.core").resolve()),
                    "--dynamic-space-size",
                    "8192",
                    "--noinform",
                    "--disable-debugger",
                    "--script",
                    str(run.DRIVER.resolve()),
                    str(root.resolve()),
                    observed["command"][-2],
                    observed["command"][-1],
                ],
            )
            self.assertEqual(observed["cwd"], plan.workdir)
            self.assertEqual(observed["environment"]["PHPT_CASE_ENV"], "present")
            self.assertEqual(observed["deadline"], deadline)

    def test_phase_environment_cleans_request_vars_and_skipif_marker(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root, skipif=b"<?php ?>", clean=b"<?php ?>")
            with mock.patch.dict(
                os.environ,
                {"TEST_PHP_EVALUATING_SKIPIF": "from-parent"},
            ):
                environment = run.parse_environment(plan.case)
            self.assertNotIn("TEST_PHP_EVALUATING_SKIPIF", environment)
            environment["TEST_PHP_EVALUATING_SKIPIF"] = "from-parent"
            plan.environment.update(environment)
            observed = {}

            def run_child(command, cwd, child_environment, deadline):
                request_path = Path(command[-2])
                result_path = Path(command[-1])
                request = json.loads(request_path.read_text(encoding="utf-8"))
                phase = request["phase"]
                observed[phase] = child_environment.copy()
                result_path.write_text(
                    json.dumps(
                        {
                            "id": plan.case.case_id,
                            "phase": phase,
                            "status": "passed",
                            "stdout": "",
                            "stderr": "",
                            "detail": "",
                        }
                    ),
                    encoding="utf-8",
                )
                return run.ChildResult(0, b"", b"", False)

            phases = (
                ("skipif", plan.skipif, "010"),
                ("main", plan.source, plan.capture),
                ("clean", plan.clean, "010"),
            )
            with mock.patch.object(run, "run_child", side_effect=run_child):
                for phase, source, capture in phases:
                    outcome = run._run_phase(
                        plan,
                        phase,
                        source,
                        None,
                        capture,
                        root,
                        root / "cl-cc.core",
                        "sbcl-test",
                        run.time.monotonic() + 10,
                    )
                    self.assertEqual(outcome.status, "passed")

            self.assertEqual(
                observed["skipif"]["TEST_PHP_EVALUATING_SKIPIF"], "1"
            )
            self.assertNotIn("TEST_PHP_EVALUATING_SKIPIF", observed["main"])
            self.assertNotIn("TEST_PHP_EVALUATING_SKIPIF", observed["clean"])
            for name in run.CGI_REQUEST_ENVIRONMENT:
                self.assertNotIn(name, observed["skipif"])
                self.assertEqual(observed["main"][name], "")
                self.assertNotIn(name, observed["clean"])

    def test_child_timeout_kills_process_group_and_reaps_child(self):
        process = mock.Mock()
        process.pid = 4172
        process.returncode = -signal.SIGKILL
        process.communicate.side_effect = [
            subprocess.TimeoutExpired("sbcl", 2.0, output=b"", stderr=b""),
            (b"", b""),
        ]
        with (
            mock.patch.object(run.subprocess, "Popen", return_value=process) as popen,
            mock.patch.object(run.os, "killpg") as killpg,
            mock.patch.object(run.time, "monotonic", return_value=10.0),
        ):
            result = run.run_child(["sbcl"], Path("."), {}, deadline=12.0)

        popen.assert_called_once()
        self.assertTrue(popen.call_args.kwargs["start_new_session"])
        killpg.assert_called_once_with(4172, signal.SIGKILL)
        self.assertEqual(process.communicate.call_args_list, [mock.call(timeout=2.0), mock.call()])
        self.assertTrue(result.timed_out)
        self.assertEqual(result.returncode, -signal.SIGKILL)

    def test_real_timeout_terminates_and_reaps_a_subprocess(self):
        with tempfile.TemporaryDirectory() as directory:
            deadline = run.time.monotonic() + 0.25
            result = run.run_child(
                [sys.executable, "-c", "import time; time.sleep(30)"],
                Path(directory),
                os.environ.copy(),
                deadline,
            )
        self.assertTrue(result.timed_out)
        self.assertIsNotNone(result.returncode)
        self.assertEqual(result.returncode, -signal.SIGKILL)

    def test_launcher_error_is_harness_error_and_main_runtime_is_a_php_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root)

            with mock.patch.object(
                run,
                "run_child",
                return_value=run.ChildResult(127, b"", b"launcher failed", False),
            ):
                launcher = run._run_phase(
                    plan, "main", plan.source, None, plan.capture,
                    root, root / "cl-cc.core", "sbcl-test", run.time.monotonic() + 10
                )
            self.assertEqual(
                run._case_outcome(plan.case, {"main": launcher}).status,
                "harness_error",
            )

            def runtime_child(command, cwd, environment, deadline):
                Path(command[-1]).write_text(
                    json.dumps(
                        {
                            "id": plan.case.case_id,
                            "phase": "main",
                            "status": "runtime",
                            "stdout": "",
                            "stderr": "RuntimeException",
                            "detail": "runtime error: example",
                        }
                    ),
                    encoding="utf-8",
                )
                return run.ChildResult(0, b"", b"", False)

            with mock.patch.object(run, "run_child", side_effect=runtime_child):
                runtime = run._run_phase(
                    plan, "main", plan.source, None, plan.capture,
                    root, root / "cl-cc.core", "sbcl-test", run.time.monotonic() + 10
                )
            self.assertEqual(runtime.status, "runtime")
            self.assertEqual(
                run._case_outcome(plan.case, {"main": runtime}).status,
                "runtime",
            )

    def test_php_exit_without_result_json_uses_completed_phase_output(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root)

            for exit_code in (0, 7):
                with self.subTest(exit_code=exit_code):
                    def exited_child(command, cwd, environment, deadline):
                        request = json.loads(Path(command[-2]).read_text(encoding="utf-8"))
                        Path(request["progress_path"]).write_text("main-done\n", encoding="utf-8")
                        return run.ChildResult(exit_code, b"php output", b"diagnostic", False)

                    with mock.patch.object(run, "run_child", side_effect=exited_child):
                        outcome = run._run_phase(
                            plan, "main", plan.source, None, plan.capture,
                            root, root / "cl-cc.core", "sbcl-test", run.time.monotonic() + 10
                        )

                    self.assertEqual(outcome.status, "passed")
                    self.assertEqual(outcome.stdout, b"php output")
                    self.assertEqual(outcome.stderr, "diagnostic")

    def test_clean_uses_trimmed_php_truthiness_and_ignores_stderr(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            plan = self._plan(root, clean=b"<?php ?>")
            main = run.Outcome("passed", stdout=b"ok\n")

            for clean in (
                run.Outcome("passed", stdout=b" \r\n", stderr="diagnostic"),
                run.Outcome("runtime", stdout=b"0\n", stderr="cleanup exception"),
                run.Outcome("runtime", stdout=b"", stderr="cleanup exception"),
            ):
                with self.subTest(clean=clean):
                    self.assertEqual(
                        run._case_outcome(plan.case, {"main": main, "clean": clean}).status,
                        "passed",
                    )

            borked = run._case_outcome(
                plan.case,
                {
                    "main": main,
                    "clean": run.Outcome("passed", stdout=b" cleanup output \n"),
                },
            )
            self.assertEqual(borked.status, "harness_error")
            self.assertIn("BORKED", borked.detail)

            failed_main = run._case_outcome(
                plan.case,
                {
                    "main": run.Outcome("runtime", detail="main failed"),
                    "clean": run.Outcome("passed", stdout=b"cleanup output"),
                },
            )
            self.assertEqual(failed_main.status, "runtime")

    def test_clean_timeout_is_borked_and_not_an_expected_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            plan = self._plan(Path(directory), clean=b"<?php ?>")
            main = run.Outcome("passed", stdout=b"ok\n")

            for stage in ("compile", "runtime"):
                with self.subTest(stage=stage):
                    timeout = run.Outcome(
                        stage,
                        detail=f"clean child timed out after {run.TIMEOUT_SECONDS} seconds",
                    )
                    outcome = run._case_outcome(
                        plan.case,
                        {"main": main, "clean": timeout},
                    )
                    self.assertEqual(outcome.status, "harness_error")
                    self.assertIn("BORKED", outcome.detail)

                    expected = {
                        plan.case.case_id: run.ExpectedFailure(
                            plan.case.case_id,
                            "bug",
                            "CLEAN timed out",
                            re.compile("timed out"),
                            stage,
                        )
                    }
                    consumed = set()
                    accepted, _ = run.apply_expected_failures(
                        plan.case.case_id,
                        outcome.status,
                        outcome.detail,
                        expected,
                        consumed,
                    )
                    self.assertFalse(accepted)
                    self.assertEqual(consumed, set())

    def test_only_main_runtime_timeout_can_match_expected_failure_manifest(self):
        for phase in ("main", "skipif"):
            for stage in ("compile", "runtime"):
                with self.subTest(phase=phase, stage=stage):
                    with tempfile.TemporaryDirectory() as directory:
                        root = Path(directory)
                        plan = self._plan(root, skipif=b"<?php ?>")
                        source = plan.skipif if phase == "skipif" else plan.source

                        def timeout_child(command, cwd, environment, deadline):
                            request = json.loads(
                                Path(command[-2]).read_text(encoding="utf-8")
                            )
                            Path(request["progress_path"]).write_text(
                                f"{phase}-{stage}\n", encoding="utf-8"
                            )
                            return run.ChildResult(
                                None, b"", b"child timed out", True
                            )

                        with mock.patch.object(
                            run, "run_child", side_effect=timeout_child
                        ):
                            phase_outcome = run._run_phase(
                                plan,
                                phase,
                                source,
                                None,
                                "010" if phase == "skipif" else plan.capture,
                                root,
                                root / "cl-cc.core",
                                "sbcl-test",
                                run.time.monotonic() + 10,
                            )

                        should_be_runtime = phase == "main" and stage == "runtime"
                        self.assertEqual(
                            phase_outcome.status,
                            "runtime" if should_be_runtime else "invalid",
                        )
                        self.assertIn(f"{phase} child timed out during {stage} stage", phase_outcome.detail)
                        self.assertIn("after 60 seconds", phase_outcome.detail)
                        self.assertEqual(phase_outcome.stderr, "child timed out")

                        outcome = run._case_outcome(plan.case, {phase: phase_outcome})
                        self.assertEqual(
                            outcome.status,
                            "runtime" if should_be_runtime else "harness_error",
                        )
                        pattern = re.compile(
                            f"{phase} child timed out during {stage} stage"
                        )
                        self.assertRegex(outcome.detail, pattern)
                        expected = {
                            plan.case.case_id: run.ExpectedFailure(
                                plan.case.case_id,
                                "unimplemented",
                                "matching compiler/runtime failure",
                                pattern,
                                stage,
                            )
                        }
                        consumed = set()
                        accepted, _ = run.apply_expected_failures(
                            plan.case.case_id,
                            "runtime" if should_be_runtime else outcome.status,
                            outcome.detail,
                            expected,
                            consumed,
                        )
                        self.assertEqual(accepted, should_be_runtime)
                        self.assertEqual(
                            consumed,
                            {plan.case.case_id} if should_be_runtime else set(),
                        )

    def test_capture_stdio_maps_section_tokens_to_three_bits(self):
        def capture(section):
            case = run.PhptCase(
                "Zend/capture.phpt",
                Path("capture.phpt"),
                {"TEST": b"capture", "FILE": b"", "EXPECT": b"", "CAPTURE_STDIO": section},
                "capture",
            )
            return run._capture_mode(case)

        default = run.PhptCase(
            "Zend/capture.phpt",
            Path("capture.phpt"),
            {"TEST": b"capture", "FILE": b"", "EXPECT": b""},
            "capture",
        )
        self.assertEqual(run._capture_mode(default), "111")
        self.assertEqual(capture(b"STDIN"), "100")
        self.assertEqual(capture(b"STDOUT"), "010")
        self.assertEqual(capture(b"STDERR"), "001")
        self.assertEqual(capture(b"stdin + StDout + stderr"), "111")

    def test_capture_stdio_compares_only_the_selected_output_streams(self):
        fixtures = (
            (b"STDIN", "100", b"ignored", "ignored", b""),
            (b"STDOUT", "010", b"out", "error", b"out"),
            (b"STDERR", "001", b"ignored", "error", b"error"),
            (b"STDOUT STDERR", "011", b"out\nerror", "error", b"out\nerror"),
            (None, "111", b"out\nerror", "error", b"out\nerror"),
        )
        for section, capture, stdout, stderr, expected in fixtures:
            with self.subTest(section=section):
                sections = {"TEST": b"capture", "FILE": b"", "EXPECT": expected}
                if section is not None:
                    sections["CAPTURE_STDIO"] = section
                case = run.PhptCase(
                    "Zend/capture.phpt", Path("capture.phpt"), sections, "capture"
                )
                result = run.Outcome("passed", stdout=stdout, stderr=stderr)
                self.assertEqual(run._capture_mode(case), capture)
                actual = run._comparison_output(case, result)
                matches, detail = run.compare_output(case, actual)
                self.assertTrue(matches, detail)

    def test_xfail_static_and_skipif_markers_classify_failure_and_success(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            static = self._plan(
                root,
                sections={
                    "TEST": b"static xfail",
                    "FILE": b"<?php ?>",
                    "EXPECT": b"expected",
                    "XFAIL": b"known issue",
                },
            ).case
            dynamic = self._plan(
                root,
                sections={
                    "TEST": b"dynamic xfail",
                    "FILE": b"<?php ?>",
                    "EXPECT": b"expected",
                },
            ).case
            failing = {"main": run.Outcome("passed", stdout=b"actual")}
            passing = {"main": run.Outcome("passed", stdout=b"expected")}
            dynamic_failure = {
                **failing,
                "skipif": run.Outcome("passed", stdout=b" \n XFAIL dynamic reason \n"),
            }
            dynamic_success = {
                **passing,
                "skipif": run.Outcome("passed", stdout=b"xfail dynamic reason\n"),
            }

            self.assertEqual(run._case_outcome(static, failing).status, "xfailed")
            self.assertEqual(run._case_outcome(static, passing).status, "warning")
            self.assertEqual(run._case_outcome(dynamic, dynamic_failure).status, "xfailed")
            self.assertEqual(dynamic.sections["XFAIL"], b"dynamic reason")
            self.assertEqual(run._case_outcome(dynamic, dynamic_success).status, "warning")

    def _run_mocked_shard(self, root, sections, attempts):
        case_path = root / "Zend" / "marker.phpt"
        case_path.parent.mkdir(parents=True)
        payload = b"".join(
            b"--" + name.encode("ascii") + b"--\n" + value
            + (b"" if value.endswith(b"\n") else b"\n")
            for name, value in sections.items()
        )
        case_path.write_bytes(payload)
        output = StringIO()
        errors = StringIO()
        with (
            mock.patch.object(run.shutil, "which", return_value="sbcl-test"),
            mock.patch.object(run, "load_expected_failures", return_value={}),
            mock.patch.object(run, "prepare_core"),
            mock.patch.object(run, "run_case", side_effect=attempts) as run_case,
            redirect_stdout(output),
            redirect_stderr(errors),
        ):
            status = run.run_shard(root, "Zend")
        return status, output.getvalue(), errors.getvalue(), run_case

    def test_xfail_results_are_nonfailing_for_shard_exit(self):
        for dynamic in (False, True):
            for actual, expected_status in ((b"actual", "xfailed"), (b"expected", "warning")):
                with self.subTest(dynamic=dynamic, actual=actual):
                    with tempfile.TemporaryDirectory() as directory:
                        sections = {
                            "TEST": b"dynamic xfail" if dynamic else b"static xfail",
                            "FILE": b"<?php ?>",
                            "EXPECT": b"expected",
                        }
                        marker = b""
                        if dynamic:
                            sections["SKIPIF"] = b"<?php ?>"
                            marker = b"XFAIL dynamic reason"
                        else:
                            sections["XFAIL"] = b"static reason"
                        phases = {"main": run.Outcome("passed", stdout=actual)}
                        if dynamic:
                            phases["skipif"] = run.Outcome("passed", stdout=marker)
                        status, summary, errors, calls = self._run_mocked_shard(
                            Path(directory), sections, [phases]
                        )
                        self.assertEqual(status, 0)
                        count_name = "warnings" if expected_status == "warning" else expected_status
                        self.assertIn(f"{count_name}=1", summary)
                        self.assertEqual(calls.call_count, 1)
                        if expected_status == "xfailed":
                            self.assertIn("XFAILED", errors)
                        else:
                            self.assertIn("warning: XFAIL section but test passes", errors)

    def test_static_and_dynamic_flaky_retry_once_and_warn_on_success(self):
        def phases(actual, skipif=None):
            result = {"main": run.Outcome("passed", stdout=actual)}
            if skipif is not None:
                result["skipif"] = run.Outcome("passed", stdout=skipif)
            return result

        with tempfile.TemporaryDirectory() as directory:
            status, summary, errors, calls = self._run_mocked_shard(
                Path(directory),
                {
                    "TEST": b"static flaky",
                    "FILE": b"<?php ?>",
                    "FLAKY": b"",
                    "EXPECT": b"ok",
                },
                [phases(b"first"), phases(b"ok")],
            )
            self.assertEqual(status, 0)
            self.assertIn("warnings=1", summary)
            self.assertIn("warning: test passed on retry attempt", errors)
            self.assertEqual(calls.call_count, 2)

        with tempfile.TemporaryDirectory() as directory:
            status, summary, errors, calls = self._run_mocked_shard(
                Path(directory),
                {
                    "TEST": b"dynamic flaky",
                    "FILE": b"<?php ?>",
                    "SKIPIF": b"<?php ?>",
                    "EXPECT": b"ok",
                },
                [phases(b"first", b"flaky temporary\n"), phases(b"ok", b"")],
            )
            self.assertEqual(status, 0)
            self.assertIn("warnings=1", summary)
            self.assertIn("warning: test passed on retry attempt", errors)
            self.assertEqual(calls.call_count, 2)
            self.assertIn("FLAKY", calls.call_args_list[0].args[0].case.sections)

    def test_static_and_dynamic_flaky_failed_retry_remains_an_ordinary_failure(self):
        def phases(actual, skipif=None):
            result = {"main": run.Outcome("passed", stdout=actual)}
            if skipif is not None:
                result["skipif"] = run.Outcome("passed", stdout=skipif)
            return result

        with tempfile.TemporaryDirectory() as directory:
            status, summary, errors, calls = self._run_mocked_shard(
                Path(directory),
                {
                    "TEST": b"static flaky",
                    "FILE": b"<?php ?>",
                    "FLAKY": b"",
                    "EXPECT": b"ok",
                },
                [phases(b"first"), phases(b"still wrong")],
            )
            self.assertEqual(status, 1)
            self.assertIn("failures=1", summary)
            self.assertIn("unclassified output failure", errors)
            self.assertEqual(calls.call_count, 2)

        with tempfile.TemporaryDirectory() as directory:
            status, summary, errors, calls = self._run_mocked_shard(
                Path(directory),
                {
                    "TEST": b"dynamic flaky",
                    "FILE": b"<?php ?>",
                    "SKIPIF": b"<?php ?>",
                    "EXPECT": b"ok",
                },
                [phases(b"first", b"FLAKY"), phases(b"still wrong", b"")],
            )
            self.assertEqual(status, 1)
            self.assertIn("failures=1", summary)
            self.assertIn("unclassified output failure", errors)
            self.assertEqual(calls.call_count, 2)

    def test_retry_uses_perf_predicates_and_transient_output_messages(self):
        with tempfile.TemporaryDirectory() as directory:
            skip_perf = self._plan(
                Path(directory),
                sections={
                    "TEST": b"perf sensitive",
                    "FILE": b"<?php ?>",
                    "EXPECT": b"ok",
                    "SKIPIF": b"<?php // SKIP_PERF_SENSITIVE ?>",
                },
            )
            self.assertTrue(run._is_flaky(skip_perf.case))
            skip_perf.case.sections["SKIPIF"] = b"<?php // skip_perf_sensitive ?>"
            self.assertFalse(run._is_flaky(skip_perf.case))

            for function in (b"disk_free_space", b"hrtime", b"microtime", b"sleep", b"usleep"):
                with self.subTest(function=function):
                    file_case = self._plan(
                        Path(directory),
                        sections={
                            "TEST": b"perf source",
                            "FILE": b"<?php " + function.upper() + b"(1); ?>",
                            "EXPECT": b"ok",
                        },
                    )
                    self.assertTrue(run._is_flaky(file_case.case))
            prefixed = self._plan(
                Path(directory),
                sections={"TEST": b"not time function", "FILE": b"<?php mysleep(1); ?>", "EXPECT": b"ok"},
            )
            self.assertFalse(run._is_flaky(prefixed.case))

            plain = self._plan(
                Path(directory),
                sections={"TEST": b"plain", "FILE": b"<?php ?>", "EXPECT": b"ok"},
            )
            failed = {"main": run.Outcome("passed", stdout=b"Connection Refused")}
            mismatch = run._case_outcome(plain.case, failed)
            self.assertTrue(run._failure_may_be_retried(plain.case, failed, mismatch))
            for message in (
                b"404: page not found",
                b"address already in use",
                b"connection refused",
                b"deadlock",
                b"mailbox already exists",
                b"timed out",
            ):
                with self.subTest(message=message):
                    failed = {"main": run.Outcome("passed", stdout=message.upper())}
                    mismatch = run._case_outcome(plain.case, failed)
                    self.assertTrue(
                        run._failure_may_be_retried(plain.case, failed, mismatch)
                    )

            timeout = {
                "main": run.Outcome(
                    "runtime",
                    detail="main child timed out during runtime stage after 60 seconds",
                )
            }
            timeout_outcome = run._case_outcome(plain.case, timeout)
            self.assertTrue(
                run._failure_may_be_retried(plain.case, timeout, timeout_outcome)
            )
            launcher = {"main": run.Outcome("invalid", detail="cannot launch child: timed out")}
            launcher_outcome = run._case_outcome(plain.case, launcher)
            self.assertFalse(
                run._failure_may_be_retried(plain.case, launcher, launcher_outcome)
            )

    def test_retry_is_not_used_without_an_upstream_condition(self):
        with tempfile.TemporaryDirectory() as directory:
            phases = {"main": run.Outcome("passed", stdout=b"wrong")}
            status, summary, _, calls = self._run_mocked_shard(
                Path(directory),
                {
                    "TEST": b"ordinary mismatch",
                    "FILE": b"<?php echo 1; ?>",
                    "EXPECT": b"ok",
                },
                [phases],
            )
            self.assertEqual(status, 1)
            self.assertIn("failures=1", summary)
            self.assertEqual(calls.call_count, 1)

    def test_worker_pool_bounds_concurrency_orders_results_and_fails_closed(self):
        plans = [
            SimpleNamespace(case=SimpleNamespace(case_id=f"Zend/{index:02d}.phpt"))
            for index in range(5)
        ]
        first_pair = threading.Barrier(2)
        lock = threading.Lock()
        active = 0
        max_active = 0

        def execute(plan):
            nonlocal active, max_active
            with lock:
                active += 1
                max_active = max(max_active, active)
            try:
                if plan.case.case_id in {"Zend/00.phpt", "Zend/01.phpt"}:
                    first_pair.wait(timeout=5)
                if plan.case.case_id == "Zend/02.phpt":
                    raise RuntimeError("worker failure")
                return run.Outcome("passed", detail=plan.case.case_id)
            finally:
                with lock:
                    active -= 1

        results = run._execute_plans(plans, 2, execute)

        self.assertEqual(max_active, 2)
        self.assertEqual(
            [result.detail if result.status == "passed" else result.status for result in results],
            ["Zend/00.phpt", "Zend/01.phpt", "harness_error", "Zend/03.phpt", "Zend/04.phpt"],
        )
        self.assertIn("RuntimeError: worker failure", results[2].detail)
        with self.assertRaisesRegex(run.HarnessError, "between 1 and 4"):
            run._execute_plans(plans, 5, execute)

    def test_main_exposes_configurable_jobs_argument(self):
        with mock.patch.object(run, "run_shard", return_value=0) as run_shard:
            status = run.main(
                ["--php-src", "/tmp/php-src", "--split", "Zend", "--jobs", "2"]
            )
            self.assertEqual(status, 0)
            run_shard.assert_called_once_with(Path("/tmp/php-src"), "Zend", 2)

        with mock.patch.object(run, "run_shard", return_value=0) as run_shard:
            status = run.main(["--php-src", "/tmp/php-src", "--split", "Zend"])

        self.assertEqual(status, 0)
        run_shard.assert_called_once_with(Path("/tmp/php-src"), "Zend", 4)

        with mock.patch.object(run, "run_shard", return_value=0) as run_shard:
            status = run.main(
                [
                    "--php-src",
                    "/tmp/php-src",
                    "--split",
                    "Zend",
                    "--shard",
                    "2/3",
                ]
            )

        self.assertEqual(status, 0)
        run_shard.assert_called_once_with(Path("/tmp/php-src"), "Zend", 4, "2/3")


if __name__ == "__main__":
    unittest.main()
