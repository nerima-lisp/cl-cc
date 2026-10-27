#!/usr/bin/env python3
"""Run a recursive php-src PHPT shard through CL-CC's PHP frontend."""

from __future__ import annotations

import argparse
import concurrent.futures
import ctypes
import ctypes.util
import dataclasses
import json
import math
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any, Optional, Union


SPLITS = ("Zend", "tests", "ext", "sapi")
EXPECTED_FAILURES = Path(__file__).with_name("expected-failures.sexp")
DRIVER = Path(__file__).with_name("driver.lisp")
TIMEOUT_SECONDS = 60
INTERMITTENT_COMPILE_TIMEOUT = "main child timed out during compile stage after 60 seconds"
DEFAULT_WORKERS = 4
MAX_WORKERS = 4
ARGS_SKIP_REASON = "PHP CLI ARGS unavailable in driver"
BYTE_PRESERVING_CASES = frozenset(
    {
        "ext/standard/tests/serialize/006.phpt",
        "ext/standard/tests/strings/htmlentities.phpt",
        "ext/standard/tests/strings/utf8.phpt",
    }
)
CGI_REQUEST_ENVIRONMENT = (
    "REQUEST_METHOD",
    "QUERY_STRING",
    "PATH_TRANSLATED",
    "SCRIPT_FILENAME",
)
SECTION_HEADER = re.compile(rb"(?m)^--([_A-Z]+)--[^\r\n]*(?:\r?\n|$)")
SHARD_SPEC = re.compile(r"([1-9][0-9]*)/([1-9][0-9]*)\Z")
ALLOWED_CLASSIFICATIONS = {
    "unimplemented",
    "bug",
    "implementation-defined",
    "intentional-extension",
    "out-of-scope",
}
ALLOWED_SECTIONS = {
    "TEST",
    "FILE",
    "FILEEOF",
    "FILE_EXTERNAL",
    "SKIPIF",
    "CLEAN",
    "EXPECT",
    "EXPECTF",
    "EXPECTREGEX",
    "EXPECT_EXTERNAL",
    "EXPECTF_EXTERNAL",
    "EXPECTREGEX_EXTERNAL",
    "EXPECTHEADERS",
    "POST",
    "POST_RAW",
    "GZIP_POST",
    "DEFLATE_POST",
    "PUT",
    "GET",
    "COOKIE",
    "ARGS",
    "REDIRECTTEST",
    "CAPTURE_STDIO",
    "STDIN",
    "CGI",
    "PHPDBG",
    "INI",
    "ENV",
    "EXTENSIONS",
    "XFAIL",
    "XLEAK",
    "CREDITS",
    "DESCRIPTION",
    "CONFLICTS",
    "WHITESPACE_SENSITIVE",
    "FLAKY",
}
AVAILABLE_EXTENSIONS = frozenset({"standard", "date", "json", "pcre"})
PHP_TRIM_BYTES = b" \t\n\r\x00\x0b"
PCRE2_DOTALL = 0x00000020
FLAKY_FUNCTION = re.compile(
    rb"\b(?:disk_free_space|hrtime|microtime|sleep|usleep)\(", re.IGNORECASE
)
FLAKY_OUTPUT = re.compile(
    rb"\b(?:404: page not found|address already in use|connection refused|"
    rb"deadlock|mailbox already exists|timed out)\b",
    re.IGNORECASE,
)


class HarnessError(Exception):
    pass


@dataclasses.dataclass(frozen=True)
class PhptCase:
    case_id: str
    path: Path
    sections: dict[str, bytes]
    test_name: str


@dataclasses.dataclass(frozen=True)
class ExpectedFailureOutcome:
    classification: str
    reason: str
    pattern: re.Pattern[str]
    stage: str


@dataclasses.dataclass(frozen=True)
class ExpectedFailure:
    case_id: str
    classification: str
    reason: str
    pattern: re.Pattern[str]
    stage: str
    allow_pass: bool = False
    alternates: tuple[ExpectedFailureOutcome, ...] = ()


@dataclasses.dataclass
class Outcome:
    status: str
    detail: str = ""
    stdout: bytes = b""
    stderr: str = ""


@dataclasses.dataclass(frozen=True)
class ChildResult:
    returncode: Optional[int]
    stdout: bytes
    stderr: bytes
    timed_out: bool
    launch_error: str = ""


@dataclasses.dataclass(frozen=True)
class ExecutionPlan:
    case: PhptCase
    source: Path
    workdir: Path
    progress: Path
    skipif: Optional[Path]
    clean: Optional[Path]
    stdin: Optional[Path]
    ini: tuple[tuple[str, str], ...]
    capture: str
    environment: dict[str, str]


class Symbol(str):
    pass


def parse_phpt(payload: bytes) -> dict[str, bytes]:
    """Parse section boundaries without normalizing section contents."""
    lines = payload.splitlines(keepends=True)
    if not lines:
        raise HarnessError("empty PHPT file")
    first_header = SECTION_HEADER.match(lines[0])
    if first_header is None or first_header.group(1) != b"TEST":
        raise HarnessError("tests must start with --TEST--")

    sections: dict[str, bytes] = {"TEST": b""}
    current = "TEST"
    current_file_section = False
    discard_after_done = False
    for line in lines[1:]:
        match = SECTION_HEADER.match(line)
        if match is not None:
            name = match.group(1).decode("ascii")
            if name not in ALLOWED_SECTIONS:
                raise HarnessError(f'unknown PHPT section "{name}"')
            if name in sections and sections[name]:
                raise HarnessError(f"duplicate section {name}")
            sections[name] = b""
            current = name
            current_file_section = name in {"FILE", "FILEEOF", "FILE_EXTERNAL"}
            discard_after_done = False
            continue
        if not discard_after_done:
            sections[current] += line
        if current_file_section and re.fullmatch(rb"===DONE===\s*\r?\n?", line):
            discard_after_done = True
    return sections


def read_phpt_sections(path: Path, php_src: Path) -> tuple[str, dict[str, bytes]]:
    try:
        relative = path.relative_to(php_src).as_posix()
    except ValueError as error:
        raise HarnessError(f"PHPT path is outside php-src: {path}") from error
    try:
        payload = path.read_bytes()
    except OSError as error:
        raise HarnessError(f"cannot read PHPT: {error}") from error
    return relative, parse_phpt(payload)


def _case_from_sections(
    relative: str, path: Path, sections: dict[str, bytes]
) -> PhptCase:
    if "TEST" not in sections:
        raise HarnessError("missing required TEST section")
    files = [name for name in ("FILE", "FILEEOF", "FILE_EXTERNAL") if name in sections]
    if "PHPDBG" not in sections and len(files) != 1:
        raise HarnessError("requires exactly one of FILE, FILEEOF, FILE_EXTERNAL")
    expected = [
        name
        for name in (
            "EXPECT",
            "EXPECTF",
            "EXPECTREGEX",
            "EXPECT_EXTERNAL",
            "EXPECTF_EXTERNAL",
            "EXPECTREGEX_EXTERNAL",
        )
        if name in sections
    ]
    if len(expected) != 1:
        raise HarnessError("requires exactly one of EXPECT, EXPECTF, EXPECTREGEX")
    try:
        test_name = sections["TEST"].decode("utf-8").strip()
    except UnicodeDecodeError as error:
        raise HarnessError("TEST section is not UTF-8") from error
    return PhptCase(relative, path, sections, test_name)


def load_case(path: Path, php_src: Path) -> PhptCase:
    relative, sections = read_phpt_sections(path, php_src)
    return _case_from_sections(relative, path, sections)


def scan_split(php_src: Path, split: str) -> list[Path]:
    if split not in SPLITS:
        raise HarnessError(f"unknown split {split!r}; expected one of {', '.join(SPLITS)}")
    root = php_src / split
    if not root.is_dir():
        raise HarnessError(f"php-src split directory does not exist: {root}")
    files = sorted(path for path in root.rglob("*.phpt") if path.is_file())
    if not files:
        raise HarnessError(f"php-src split contains no .phpt files: {root}")
    return files


def parse_sexp(source: str) -> Any:
    """Read the small, data-only Common Lisp syntax used by the manifest."""
    tokens: list[Any] = []
    index = 0
    while index < len(source):
        character = source[index]
        if character.isspace():
            index += 1
        elif character == ";":
            newline = source.find("\n", index)
            index = len(source) if newline < 0 else newline + 1
        elif character in "()":
            tokens.append(character)
            index += 1
        elif character == '"':
            index += 1
            value: list[str] = []
            while index < len(source) and source[index] != '"':
                if source[index] == "\\":
                    index += 1
                    if index >= len(source):
                        raise HarnessError("unterminated escape in expected-failures.sexp")
                value.append(source[index])
                index += 1
            if index >= len(source):
                raise HarnessError("unterminated string in expected-failures.sexp")
            index += 1
            tokens.append("".join(value))
        else:
            end = index
            while end < len(source) and not source[end].isspace() and source[end] not in "();":
                end += 1
            tokens.append(Symbol(source[index:end]))
            index = end

    cursor = 0

    def read_form() -> Any:
        nonlocal cursor
        if cursor >= len(tokens):
            raise HarnessError("unexpected end of expected-failures.sexp")
        token = tokens[cursor]
        cursor += 1
        if token == "(":
            items = []
            while cursor < len(tokens) and tokens[cursor] != ")":
                items.append(read_form())
            if cursor >= len(tokens):
                raise HarnessError("unclosed list in expected-failures.sexp")
            cursor += 1
            return items
        if token == ")":
            raise HarnessError("unexpected ) in expected-failures.sexp")
        return token

    if not tokens:
        return []
    value = read_form()
    if cursor != len(tokens):
        raise HarnessError("multiple top-level forms in expected-failures.sexp")
    return value


def load_expected_failures(path: Path = EXPECTED_FAILURES) -> dict[str, ExpectedFailure]:
    try:
        raw = parse_sexp(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise HarnessError(f"cannot read expected-failures.sexp: {error}") from error
    if not isinstance(raw, list):
        raise HarnessError("expected-failures.sexp must contain a list")
    expected: dict[str, ExpectedFailure] = {}
    for entry in raw:
        if not isinstance(entry, list) or not entry:
            raise HarnessError(f"malformed expected-failure entry: {entry!r}")
        grouped = isinstance(entry[0], Symbol) and entry[0] == ":cases"
        if grouped:
            pair_items = entry
            case_ids: list[str] = []
        elif type(entry[0]) is str:
            case_ids = [entry[0]]
            pair_items = entry[1:]
        else:
            raise HarnessError(f"malformed expected-failure entry: {entry!r}")
        if len(pair_items) % 2:
            raise HarnessError(f"malformed expected-failure fields in {entry!r}")
        fields: dict[str, Any] = {}
        for key, value in zip(pair_items[::2], pair_items[1::2]):
            if not isinstance(key, Symbol) or not key.startswith(":"):
                raise HarnessError(f"invalid expected-failure key in {entry!r}")
            key_name = key[1:]
            if key_name in fields:
                raise HarnessError(f"duplicate {key} in expected-failure entry {entry!r}")
            fields[key_name] = value
        required_fields = {"classification", "reason", "match", "stage"}
        if grouped:
            if set(fields) != required_fields | {"cases"}:
                raise HarnessError(f"expected-failure fields are incomplete for {entry!r}")
            raw_case_ids = fields.pop("cases")
            if (
                not isinstance(raw_case_ids, list)
                or not raw_case_ids
                or not all(type(case_id) is str and case_id for case_id in raw_case_ids)
            ):
                raise HarnessError(f"invalid :cases list in expected-failure entry {entry!r}")
            if len(raw_case_ids) != len(set(raw_case_ids)):
                raise HarnessError(f"duplicate case id in expected-failure group {entry!r}")
            case_ids = raw_case_ids
        elif not required_fields <= set(fields) or set(fields) - required_fields - {
            "allow-pass",
            "alternates",
        }:
            raise HarnessError(f"expected-failure fields are incomplete for {case_ids[0]!r}")
        raw_alternates = fields.get("alternates", [])
        if not isinstance(raw_alternates, list):
            raise HarnessError(f"invalid :alternates value for {case_ids[0]!r}")
        if grouped and raw_alternates:
            raise HarnessError(f":alternates requires an individual entry for {case_ids[0]!r}")
        alternates: list[ExpectedFailureOutcome] = []
        for raw_alternate in raw_alternates:
            if not isinstance(raw_alternate, list) or len(raw_alternate) % 2:
                raise HarnessError(f"malformed alternate outcome for {case_ids[0]!r}")
            alternate_fields: dict[str, Any] = {}
            for key, value in zip(raw_alternate[::2], raw_alternate[1::2]):
                if not isinstance(key, Symbol) or not key.startswith(":"):
                    raise HarnessError(f"invalid alternate outcome key for {case_ids[0]!r}")
                key_name = key[1:]
                if key_name in alternate_fields:
                    raise HarnessError(f"duplicate {key} in alternate outcome for {case_ids[0]!r}")
                alternate_fields[key_name] = value
            if set(alternate_fields) != required_fields:
                raise HarnessError(f"alternate outcome fields are incomplete for {case_ids[0]!r}")
            alternate_classification = alternate_fields["classification"]
            alternate_reason = alternate_fields["reason"]
            alternate_match = alternate_fields["match"]
            alternate_stage = alternate_fields["stage"]
            if (
                not isinstance(alternate_classification, Symbol)
                or not alternate_classification.startswith(":")
                or alternate_classification[1:] not in ALLOWED_CLASSIFICATIONS
            ):
                raise HarnessError(f"invalid alternate classification for {case_ids[0]!r}")
            if (
                not isinstance(alternate_stage, Symbol)
                or not alternate_stage.startswith(":")
                or alternate_stage[1:] not in {"compile", "runtime", "output"}
            ):
                raise HarnessError(f"invalid alternate stage for {case_ids[0]!r}")
            if not isinstance(alternate_reason, str) or not alternate_reason.strip():
                raise HarnessError(f"empty alternate reason for {case_ids[0]!r}")
            run_match = re.search(r"\brun #?(\d{11})\b", alternate_reason)
            issue_refs = re.findall(r"#(\d+)\b", alternate_reason)
            if run_match is None or not any(
                issue_ref != run_match.group(1) for issue_ref in issue_refs
            ):
                raise HarnessError(
                    f"alternate reason requires an issue and observed run ID for {case_ids[0]!r}"
                )
            if not isinstance(alternate_match, str) or not alternate_match:
                raise HarnessError(f"empty alternate match expression for {case_ids[0]!r}")
            try:
                alternate_pattern = re.compile(alternate_match)
            except re.error as error:
                raise HarnessError(
                    f"invalid alternate match expression for {case_ids[0]!r}: {error}"
                ) from error
            alternates.append(
                ExpectedFailureOutcome(
                    alternate_classification[1:],
                    alternate_reason,
                    alternate_pattern,
                    alternate_stage[1:],
                )
            )
        raw_allow_pass = fields.get("allow-pass")
        allow_pass = raw_allow_pass is not None
        if allow_pass and (not isinstance(raw_allow_pass, Symbol) or raw_allow_pass != "t"):
            raise HarnessError(f"invalid :allow-pass value for {case_ids[0]!r}")
        classification = fields["classification"]
        stage = fields["stage"]
        reason = fields["reason"]
        pattern_text = fields["match"]
        if not isinstance(classification, Symbol) or not classification.startswith(":"):
            raise HarnessError(f"invalid classification for {case_ids[0]!r}")
        classification = classification[1:]
        if classification not in ALLOWED_CLASSIFICATIONS:
            raise HarnessError(f"unknown classification {classification!r} for {case_ids[0]!r}")
        if (
            not isinstance(stage, Symbol)
            or not stage.startswith(":")
            or stage[1:] not in {"compile", "runtime", "output"}
        ):
            raise HarnessError(f"invalid stage for {case_ids[0]!r}")
        if not isinstance(reason, str) or not reason.strip():
            raise HarnessError(f"empty reason for {case_ids[0]!r}")
        if not isinstance(pattern_text, str) or not pattern_text:
            raise HarnessError(f"empty match expression for {case_ids[0]!r}")
        if allow_pass and (
            grouped
            or classification != "bug"
            or stage != ":compile"
            or pattern_text != "^main child timed out during compile stage after 60 seconds$"
            or "intermittent 60s compile timeout" not in reason
            or not re.search(r"#\d+", reason)
            or not re.search(r"\b\d{11}\b", reason)
        ):
            raise HarnessError(
                f":allow-pass requires an individual :bug compile-timeout entry, issue, and run ID for {case_ids[0]!r}"
            )
        try:
            pattern = re.compile(pattern_text)
        except re.error as error:
            raise HarnessError(f"invalid match expression for {case_ids[0]!r}: {error}") from error
        for case_id in case_ids:
            if case_id in expected:
                raise HarnessError(f"duplicate expected-failure test {case_id!r}")
            expected[case_id] = ExpectedFailure(
                case_id,
                classification,
                reason,
                pattern,
                stage[1:],
                allow_pass,
                tuple(alternates),
            )
    return expected


def parse_shard(value: str) -> tuple[int, int]:
    match = SHARD_SPEC.fullmatch(value)
    if match is None:
        raise HarnessError("shard must use one-based INDEX/COUNT syntax")
    try:
        index, count = (int(part) for part in match.groups())
    except ValueError as error:
        raise HarnessError("shard index and count exceed supported integer size") from error
    if index > count:
        raise HarnessError(f"shard index {index} exceeds shard count {count}")
    return index, count


def select_shard(items: list[Any], shard: Optional[str]) -> list[Any]:
    if shard is None:
        return items
    index, count = parse_shard(shard)
    selected = [
        item for position, item in enumerate(items) if position % count == index - 1
    ]
    if not selected:
        raise HarnessError(f"shard {index}/{count} selects no PHPT cases")
    return selected


class PcrePattern:
    def __init__(self, pattern: bytes):
        self.pattern = pattern

    def fullmatch(self, subject: Union[str, bytes]) -> bool:
        raw = subject.encode("utf-8") if isinstance(subject, str) else subject
        return pcre2_match(b"^" + self.pattern + b"$", raw, PCRE2_DOTALL)


_PCRE2: Optional[ctypes.CDLL] = None


def _pcre2_library() -> ctypes.CDLL:
    global _PCRE2
    if _PCRE2 is not None:
        return _PCRE2
    library_name = os.environ.get("PCRE2_LIBRARY") or ctypes.util.find_library("pcre2-8")
    if not library_name:
        raise HarnessError("PCRE2 8-bit library is unavailable; set PCRE2_LIBRARY")
    try:
        library = ctypes.CDLL(library_name)
    except OSError as error:
        raise HarnessError(f"cannot load PCRE2 library {library_name!r}: {error}") from error
    library.pcre2_compile_8.argtypes = [
        ctypes.c_void_p,
        ctypes.c_size_t,
        ctypes.c_uint32,
        ctypes.POINTER(ctypes.c_int),
        ctypes.POINTER(ctypes.c_size_t),
        ctypes.c_void_p,
    ]
    library.pcre2_compile_8.restype = ctypes.c_void_p
    library.pcre2_match_data_create_from_pattern_8.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
    library.pcre2_match_data_create_from_pattern_8.restype = ctypes.c_void_p
    library.pcre2_match_8.argtypes = [
        ctypes.c_void_p,
        ctypes.c_void_p,
        ctypes.c_size_t,
        ctypes.c_size_t,
        ctypes.c_uint32,
        ctypes.c_void_p,
        ctypes.c_void_p,
    ]
    library.pcre2_match_8.restype = ctypes.c_int
    library.pcre2_match_data_free_8.argtypes = [ctypes.c_void_p]
    library.pcre2_code_free_8.argtypes = [ctypes.c_void_p]
    library.pcre2_get_error_message_8.argtypes = [ctypes.c_int, ctypes.c_void_p, ctypes.c_size_t]
    library.pcre2_get_error_message_8.restype = ctypes.c_int
    _PCRE2 = library
    return library


def pcre2_match(pattern: bytes, subject: bytes, options: int = 0) -> bool:
    library = _pcre2_library()
    pattern_buffer = ctypes.create_string_buffer(pattern)
    subject_buffer = ctypes.create_string_buffer(subject)
    error_code = ctypes.c_int()
    error_offset = ctypes.c_size_t()
    code = library.pcre2_compile_8(
        pattern_buffer,
        len(pattern),
        options,
        ctypes.byref(error_code),
        ctypes.byref(error_offset),
        None,
    )
    if not code:
        message = ctypes.create_string_buffer(256)
        length = library.pcre2_get_error_message_8(error_code.value, message, len(message))
        description = message.raw[:length].decode("ascii", errors="replace") if length >= 0 else str(error_code.value)
        raise HarnessError(f"invalid PCRE2 pattern at byte {error_offset.value}: {description}")
    match_data = library.pcre2_match_data_create_from_pattern_8(code, None)
    if not match_data:
        library.pcre2_code_free_8(code)
        raise HarnessError("PCRE2 could not allocate match data")
    try:
        result = library.pcre2_match_8(
            code,
            subject_buffer,
            len(subject),
            0,
            0,
            match_data,
            None,
        )
    finally:
        library.pcre2_match_data_free_8(match_data)
        library.pcre2_code_free_8(code)
    if result == -1:
        return False
    if result < 0:
        raise HarnessError(f"PCRE2 matching failed with error {result}")
    return True


def _pcre_quote_literal(value: bytes) -> bytes:
    special = b"\\.+*?[^]$(){}=!<>|:-#/"
    quoted = bytearray()
    for byte in value:
        if byte in special:
            quoted.append(ord("\\"))
        quoted.append(byte)
    return bytes(quoted)


def expectf_pattern(expected: Union[bytes, str]) -> PcrePattern:
    """Translate pinned php-src EXPECTF placeholders into a PCRE2 pattern."""
    raw = expected.encode("utf-8") if isinstance(expected, str) else expected
    raw = raw.replace(b"\r\n", b"\n")
    pieces: list[bytes] = []
    index = 0
    placeholders = {
        b"e": _pcre_quote_literal(os.sep.encode("utf-8")),
        b"s": b"[^\\r\\n]+",
        b"S": b"[^\\r\\n]*",
        b"a": b".+?",
        b"A": b".*?",
        b"w": b"\\s*",
        b"i": b"[+-]?\\d+",
        b"d": b"\\d+",
        b"x": b"[0-9a-fA-F]+",
        b"f": b"[+-]?(?:\\d+|(?=\\.\\d))(?:\\.\\d+)?(?:[Ee][+-]?\\d+)?",
        b"c": b".",
        b"0": b"\\x00",
    }
    while index < len(raw):
        if raw.startswith(b"%r", index):
            close = raw.find(b"%r", index + 2)
            if close >= 0:
                pieces.append(b"(" + raw[index + 2 : close] + b")")
                index = close + 2
                continue
            pieces.append(_pcre_quote_literal(raw[index:]))
            break
        elif raw[index : index + 1] == b"%" and raw[index + 1 : index + 2] in placeholders:
            pieces.append(placeholders[raw[index + 1 : index + 2]])
            index += 2
        else:
            start = index
            while index < len(raw) and not raw.startswith(b"%r", index) and not (
                raw[index : index + 1] == b"%" and raw[index + 1 : index + 2] in placeholders
            ):
                index += 1
            pieces.append(_pcre_quote_literal(raw[start:index]))
    return PcrePattern(b"".join(pieces))


def _php_trim(value: bytes) -> bytes:
    return value.strip(PHP_TRIM_BYTES).replace(b"\r\n", b"\n")


def compare_output(case: PhptCase, actual: bytes) -> tuple[bool, str]:
    expected_name = next(name for name in ("EXPECT", "EXPECTF", "EXPECTREGEX") if name in case.sections)
    expected_bytes = _php_trim(case.sections[expected_name])
    actual = _php_trim(actual)
    if expected_name == "EXPECT":
        if actual == expected_bytes:
            return True, ""
        mismatch = first_byte_mismatch(expected_bytes, actual)
        return False, mismatch
    if expected_name == "EXPECTF":
        matcher = expectf_pattern(expected_bytes)
        matched = matcher.fullmatch(actual)
    else:
        matched = pcre2_match(b"^" + expected_bytes + b"$", actual, PCRE2_DOTALL)
    if matched:
        return True, ""
    return False, f"{expected_name} did not match output (expected {len(expected_bytes)} bytes, got {len(actual)} bytes)"


def first_byte_mismatch(expected: bytes, actual: bytes) -> str:
    limit = min(len(expected), len(actual))
    mismatch = next((index for index in range(limit) if expected[index] != actual[index]), limit)
    expected_byte = "EOF" if mismatch >= len(expected) else f"0x{expected[mismatch]:02x}"
    actual_byte = "EOF" if mismatch >= len(actual) else f"0x{actual[mismatch]:02x}"
    return (
        f"EXPECT mismatch at byte {mismatch}: expected {expected_byte}, got {actual_byte} "
        f"(expected {len(expected)} bytes, got {len(actual)} bytes)"
    )


def unsupported_sections(case: PhptCase) -> list[str]:
    unsupported = []
    if "REDIRECTTEST" in case.sections:
        unsupported.append("REDIRECTTEST")
    return unsupported


def _has_cli_args(sections: dict[str, bytes]) -> bool:
    return bool(sections.get("ARGS", b"").strip())


def is_cgi_case(case: PhptCase) -> bool:
    return is_cgi_sections(case.sections)


def is_cgi_sections(sections: dict[str, bytes]) -> bool:
    return "CGI" in sections or any(
        sections.get(name, b"").strip()
        for name in (
            "GET",
            "POST",
            "POST_RAW",
            "GZIP_POST",
            "DEFLATE_POST",
            "PUT",
            "COOKIE",
            "EXPECTHEADERS",
        )
    )


def expand_external_sections(case: PhptCase) -> None:
    for external, inline in (
        ("FILE_EXTERNAL", "FILE"),
        ("EXPECT_EXTERNAL", "EXPECT"),
        ("EXPECTF_EXTERNAL", "EXPECTF"),
        ("EXPECTREGEX_EXTERNAL", "EXPECTREGEX"),
    ):
        if external not in case.sections:
            continue
        try:
            relative = _php_trim(case.sections[external]).decode("utf-8")
        except UnicodeDecodeError as error:
            raise HarnessError(f"{external} path is not UTF-8") from error
        relative = relative.replace("..", "")
        if not relative:
            raise HarnessError(f"{external} path is empty")
        base = case.path.parent.resolve()
        external_path = (base / relative).resolve()
        if not external_path.is_relative_to(base):
            raise HarnessError(f"{external} path escapes the PHPT directory")
        try:
            case.sections[inline] = external_path.read_bytes()
        except OSError as error:
            raise HarnessError(f"cannot read {external} file: {error}") from error
    if "FILEEOF" in case.sections:
        case.sections["FILE"] = case.sections.pop("FILEEOF").rstrip(b"\r\n")


class SkipCase(Exception):
    pass


def parse_environment(case: PhptCase) -> dict[str, str]:
    environment = os.environ.copy()
    environment["PWD"] = str(case.path.parent)
    for name in (
        "REDIRECT_STATUS",
        "QUERY_STRING",
        "PATH_TRANSLATED",
        "SCRIPT_FILENAME",
        "REQUEST_METHOD",
        "CONTENT_TYPE",
        "CONTENT_LENGTH",
        "TZ",
        "HTTP_COOKIE",
    ):
        environment[name] = ""
    section = case.sections.get("ENV", b"")
    try:
        text = section.decode("utf-8")
    except UnicodeDecodeError as error:
        raise HarnessError("ENV section is not UTF-8") from error
    text = text.replace("{PWD}", str(case.path.parent))
    for line in text.splitlines():
        assignment = line.strip()
        if "=" not in assignment:
            continue
        name, value = assignment.split("=", 1)
        if not name:
            continue
        if "\x00" in name or "\x00" in value:
            raise HarnessError("NUL in ENV section is unsupported")
        environment[name] = value
    environment.pop("TEST_PHP_EVALUATING_SKIPIF", None)
    return environment


def _phase_environment(environment: dict[str, str], phase: str) -> dict[str, str]:
    child_environment = environment.copy()
    if phase in {"skipif", "clean"}:
        for name in CGI_REQUEST_ENVIRONMENT:
            child_environment.pop(name, None)
    if phase == "skipif":
        child_environment["TEST_PHP_EVALUATING_SKIPIF"] = "1"
    else:
        child_environment.pop("TEST_PHP_EVALUATING_SKIPIF", None)
    return child_environment


def parse_ini_settings(case: PhptCase, environment: dict[str, str]) -> tuple[tuple[str, str], ...]:
    try:
        text = case.sections.get("INI", b"").decode("utf-8")
    except UnicodeDecodeError as error:
        raise HarnessError("INI section is not UTF-8") from error
    text = text.replace("{PWD}", str(case.path.parent))
    text = text.replace("{TMP}", tempfile.gettempdir())
    text = re.sub(
        r"\{MAIL:(\S+)\}",
        lambda match: f"tee {match.group(1)} >/dev/null",
        text,
    )

    missing_environment: list[str] = []

    def expand_environment(match: re.Match[str]) -> str:
        name = match.group(1)
        if name not in environment:
            missing_environment.append(name)
            return ""
        return environment[name]

    text = re.sub(r"\{ENV:(\S+?)\}", expand_environment, text)
    if missing_environment:
        raise SkipCase(f"Environment variable {missing_environment[0]} is not set")

    settings: dict[str, str] = {}
    for line in text.replace("\r\n", "\n").replace("\r", "\n").split("\n"):
        if "=" not in line:
            continue
        name, value = line.split("=", 1)
        name = name.strip()
        value = value.strip()
        if not name:
            continue
        if name.lower() in {"extension", "zend_extension"}:
            raise HarnessError(f"INI setting {name} requires dynamic extension loading")
        if "\x00" in name or "\x00" in value:
            raise HarnessError("NUL in INI section is unsupported")
        if len(value) >= 2 and value[0] in "\"'" and value[-1] == value[0]:
            quote = value[0]
            value = value[1:-1]
            if quote == '"':
                value = value.replace("\\\\", "\\").replace('\\"', '"')
        elif value.lower() in {"on", "true", "yes"}:
            value = "1"
        elif value.lower() in {"off", "false", "no", "none"}:
            value = ""
        settings[name] = value
    return tuple(settings.items())


def _kill_child_group(process: subprocess.Popen[bytes]) -> tuple[bytes, bytes]:
    try:
        os.killpg(process.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    except OSError:
        try:
            process.kill()
        except ProcessLookupError:
            pass
    return process.communicate()


def run_child(
    command: list[str],
    cwd: Path,
    environment: dict[str, str],
    deadline: float,
) -> ChildResult:
    if deadline - time.monotonic() <= 0:
        return ChildResult(None, b"", b"", True)

    try:
        process = subprocess.Popen(
            command,
            cwd=cwd,
            env=environment,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=True,
        )
    except OSError as error:
        return ChildResult(None, b"", str(error).encode("utf-8", errors="replace"), False, str(error))

    remaining = deadline - time.monotonic()
    if remaining <= 0:
        stdout, stderr = _kill_child_group(process)
        return ChildResult(process.returncode, stdout, stderr, True)
    try:
        stdout, stderr = process.communicate(timeout=remaining)
    except subprocess.TimeoutExpired:
        stdout, stderr = _kill_child_group(process)
        return ChildResult(process.returncode, stdout, stderr, True)
    return ChildResult(process.returncode, stdout, stderr, False)


def prepare_core(project_root: Path, core_path: Path, sbcl: str, temp_root: Path) -> None:
    bootstrap = temp_root / "prepare-core.lisp"
    bootstrap.write_text(
        "(require :asdf)\n"
        "(let* ((arguments (uiop:command-line-arguments))\n"
        "       (project-root (first arguments))\n"
        "       (core-path (second arguments)))\n"
        "  (asdf:load-asd (truename (merge-pathnames \"cl-cc.asd\"\n"
        "                                             (uiop:ensure-directory-pathname project-root))))\n"
        "  (asdf:load-system :cl-cc)\n"
        "  (asdf:load-system :cl-cc-tools)\n"
        "  (sb-ext:save-lisp-and-die core-path))\n",
        encoding="utf-8",
    )
    command = [
        sbcl,
        "--dynamic-space-size",
        "8192",
        "--noinform",
        "--disable-debugger",
        "--script",
        str(bootstrap),
        str(project_root.resolve()),
        str(core_path.resolve()),
    ]
    process = subprocess.run(
        command,
        cwd=project_root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if process.returncode != 0 or not core_path.is_file():
        stdout = process.stdout.decode("utf-8", errors="replace")[-4000:]
        stderr = process.stderr.decode("utf-8", errors="replace")[-4000:]
        raise HarnessError(
            f"SBCL core preparation failed with exit {process.returncode}; "
            f"core_created={core_path.is_file()}; stdout={stdout!r}; stderr={stderr!r}"
        )


def _request_json_text(request: dict[str, Any]) -> str:
    def validate(value: Any) -> None:
        if isinstance(value, str):
            if any(ord(character) < 32 and character not in "\n\r\t" for character in value):
                raise HarnessError("unsupported control character in PHPT request JSON")
        elif isinstance(value, list):
            for item in value:
                validate(item)
        elif isinstance(value, dict):
            for key, item in value.items():
                validate(key)
                validate(item)

    validate(request)
    try:
        return json.dumps(request, ensure_ascii=False, allow_nan=False)
    except (TypeError, ValueError) as error:
        raise HarnessError(f"cannot serialize PHPT request JSON: {error}") from error


def _phase_artifacts(progress_path: Path, phase: str) -> tuple[Path, Path, Path]:
    base = progress_path.with_suffix("")
    phase_base = base.with_name(f"{base.name}.{phase}")
    return (
        phase_base.with_name(f"{phase_base.name}.progress"),
        phase_base.with_name(f"{phase_base.name}.request.json"),
        phase_base.with_name(f"{phase_base.name}.result.json"),
    )


def _read_phase_result(
    result_path: Path,
    case_id: str,
    phase: str,
    byte_preserving_output: bool = False,
) -> Outcome:
    try:
        record = json.loads(result_path.read_text(encoding="utf-8"))
    except OSError as error:
        raise HarnessError(f"cannot read {phase} result JSON: {error}") from error
    except json.JSONDecodeError as error:
        raise HarnessError(f"invalid {phase} result JSON: {error}") from error
    if not isinstance(record, dict):
        raise HarnessError(f"{phase} result must be a JSON object")
    try:
        record_id = record["id"]
        record_phase = record["phase"]
        status = record["status"]
        stdout = record["stdout"]
        stderr = record["stderr"]
        detail = record["detail"]
    except KeyError as error:
        raise HarnessError(f"{phase} result is missing {error.args[0]!r}") from error
    if not isinstance(record_id, str) or record_id != case_id:
        raise HarnessError(f"SBCL driver returned unexpected case id {record_id!r}")
    if not isinstance(record_phase, str) or record_phase != phase:
        raise HarnessError(f"SBCL driver returned unexpected phase {record_phase!r}")
    if not isinstance(status, str) or status not in {
        "passed",
        "source",
        "compile",
        "runtime",
        "invalid",
    }:
        raise HarnessError(f"SBCL driver returned unknown status {status!r}")
    if not all(isinstance(value, str) for value in (stdout, stderr, detail)):
        raise HarnessError(f"{phase} result stdout, stderr, and detail must be strings")
    try:
        stdout_bytes = _encode_driver_output(stdout, byte_preserving_output)
        stderr.encode("utf-8")
    except UnicodeEncodeError as error:
        raise HarnessError(f"{phase} result contains invalid Unicode: {error}") from error
    return Outcome(status, detail=detail, stdout=stdout_bytes, stderr=stderr)


def _timeout_stage(progress_path: Path, phase: str) -> str:
    try:
        progress = progress_path.read_text(encoding="utf-8").strip()
    except OSError:
        return ""
    if "-" not in progress:
        return ""
    recorded_phase, stage = progress.split("-", 1)
    return stage if recorded_phase == phase else ""


def _run_phase(
    plan: ExecutionPlan,
    phase: str,
    source: Path,
    stdin: Optional[Path],
    capture: str,
    project_root: Path,
    core_path: Path,
    sbcl: str,
    deadline: float,
) -> Outcome:
    progress_path, request_path, result_path = _phase_artifacts(plan.progress, phase)
    if deadline - time.monotonic() <= 0:
        return Outcome("invalid", detail=f"case deadline expired before {phase} phase")
    try:
        progress_path.parent.mkdir(parents=True, exist_ok=True)
        progress_path.unlink(missing_ok=True)
        result_path.unlink(missing_ok=True)
        request = {
            "id": plan.case.case_id,
            "phase": phase,
            "source_path": str(source.resolve()),
            "working_directory": str(plan.workdir.resolve()),
            "progress_path": str(progress_path.resolve()),
            "ini_settings": [[name, value] for name, value in plan.ini],
            "capture": capture,
            "stdin_path": str(stdin.resolve()) if stdin else None,
        }
        request_path.write_text(_request_json_text(request), encoding="utf-8")
    except (HarnessError, OSError, UnicodeError) as error:
        return Outcome("invalid", detail=f"cannot prepare {phase} request: {error}")

    command = [
        sbcl,
        "--core",
        str(core_path.resolve()),
        "--dynamic-space-size",
        "8192",
        "--noinform",
        "--disable-debugger",
        "--script",
        str(DRIVER.resolve()),
        str(project_root.resolve()),
        str(request_path.resolve()),
        str(result_path.resolve()),
    ]
    child = run_child(command, plan.workdir, _phase_environment(plan.environment, phase), deadline)
    if child.launch_error:
        return Outcome("invalid", detail=f"cannot launch {phase} child: {child.launch_error}")
    if child.timed_out:
        stage = _timeout_stage(progress_path, phase)
        stage_detail = f" during {stage} stage" if stage else ""
        detail = (
            f"{phase} child timed out{stage_detail} "
            f"after {_phase_timeout_seconds(plan):g} seconds"
        )
        child_stderr = child.stderr.decode("utf-8", errors="replace")[-2000:]
        if child_stderr:
            detail += f": {child_stderr}"
        if phase == "main" and stage == "runtime":
            return Outcome("runtime", detail=detail, stderr=child_stderr)
        if stage in {"compile", "runtime"}:
            return Outcome("invalid", detail=detail, stderr=child_stderr)
        if stage == "source":
            return Outcome("source", detail=detail, stderr=child_stderr)
        return Outcome("invalid", detail=detail)

    if (
        not result_path.exists()
        and child.returncode is not None
        and child.returncode >= 0
        and _timeout_stage(progress_path, phase) == "done"
    ):
        # PHP exit terminates this phase's SBCL process before it can write JSON.
        return Outcome(
            "passed",
            stdout=child.stdout,
            stderr=child.stderr.decode("utf-8", errors="replace"),
        )

    if child.returncode != 0:
        child_stderr = child.stderr.decode("utf-8", errors="replace")[-2000:]
        detail = f"{phase} child exited with {child.returncode}"
        if child_stderr:
            detail += f": {child_stderr}"
        stage = _timeout_stage(progress_path, phase)
        if phase == "main" and stage in {"compile", "runtime"}:
            return Outcome(stage, detail=detail, stderr=child_stderr)
        return Outcome("invalid", detail=detail, stderr=child_stderr)
    try:
        return _read_phase_result(
            result_path,
            plan.case.case_id,
            phase,
            _uses_byte_preserving_transport(
                plan.case.case_id, plan.case.sections.get("FILE", b"")
            ),
        )
    except HarnessError as error:
        return Outcome("invalid", detail=str(error))


def _phase_timeout_seconds(plan: ExecutionPlan) -> float:
    raw_timeout = plan.environment.get("TEST_TIMEOUT")
    if raw_timeout is None:
        return TIMEOUT_SECONDS
    try:
        timeout = float(raw_timeout)
    except (OverflowError, TypeError, ValueError):
        return TIMEOUT_SECONDS
    return timeout if math.isfinite(timeout) and timeout > 0 else TIMEOUT_SECONDS


def run_case(
    plan: ExecutionPlan,
    project_root: Path,
    core_path: Path,
    sbcl: str,
) -> dict[str, Outcome]:
    outcomes: dict[str, Outcome] = {}

    if plan.skipif is not None:
        skipif = _run_phase(
            plan,
            "skipif",
            plan.skipif,
            None,
            "010",
            project_root,
            core_path,
            sbcl,
            time.monotonic() + _phase_timeout_seconds(plan),
        )
        outcomes["skipif"] = skipif
        if skipif.status != "passed":
            return outcomes
        skip_text = _php_trim(skipif.stdout).decode("utf-8", errors="replace").lower()
        if skip_text.startswith("skip"):
            return outcomes

    main = _run_phase(
        plan,
        "main",
        plan.source,
        plan.stdin,
        plan.capture,
        project_root,
        core_path,
        sbcl,
        time.monotonic() + _phase_timeout_seconds(plan),
    )
    outcomes["main"] = main

    if plan.clean is not None:
        outcomes["clean"] = _run_phase(
            plan,
            "clean",
            plan.clean,
            None,
            "010",
            project_root,
            core_path,
            sbcl,
            time.monotonic() + _phase_timeout_seconds(plan),
        )
    return outcomes


def _case_specific_skip_reason(case: PhptCase) -> Optional[str]:
    source = case.sections.get("FILE", b"")
    if case.case_id == "tests/basic/bug67198.phpt" and b"php_cli_server_start" in source:
        return (
            "requires an external PHP CLI development server; "
            "the server route cannot be exercised by cl-cc"
        )
    if (
        case.case_id == "tests/basic/bug71273.phpt"
        and b"TEST_PHP_EXECUTABLE_ESCAPED" in source
    ):
        return (
            "uses TEST_PHP_EXECUTABLE_ESCAPED to invoke external PHP; "
            "that route does not test cl-cc"
        )
    return None


def split_for_case(case_id: str) -> str:
    return case_id.split("/", 1)[0]


def validate_expected_failure_roots(expected: dict[str, ExpectedFailure]) -> None:
    invalid = sorted(case_id for case_id in expected if split_for_case(case_id) not in SPLITS)
    if invalid:
        raise HarnessError(
            "expected-failure path has an unknown shard root: " + ", ".join(invalid[:4])
        )


def apply_expected_failures(
    case_id: str,
    stage: str,
    detail: str,
    expected: dict[str, ExpectedFailure],
    consumed: set[str],
) -> tuple[bool, str]:
    entry = expected.get(case_id)
    if entry is None:
        return False, f"unclassified {stage} failure: {detail}"
    if entry.allow_pass and stage == "passed" and not detail:
        consumed.add(case_id)
        return True, "allowed pass for intermittent compile-timeout bug"
    if entry.stage != stage:
        if not any(
            alternate.stage == stage and alternate.pattern.search(detail)
            for alternate in entry.alternates
        ):
            return False, (
                f"expected-failure stage mismatch: manifest={entry.stage}, observed={stage}; "
                f"detail={detail!r}"
            )
    elif entry.pattern.search(detail):
        consumed.add(case_id)
        return True, f"expected {entry.classification}: {entry.reason}"
    matching_alternate = next(
        (
            alternate
            for alternate in entry.alternates
            if alternate.stage == stage and alternate.pattern.search(detail)
        ),
        None,
    )
    if matching_alternate is None:
        return False, f"expected-failure evidence did not match: {detail}"
    consumed.add(case_id)
    return True, f"expected {matching_alternate.classification}: {matching_alternate.reason}"


def _capture_mode(case: PhptCase) -> str:
    if "CAPTURE_STDIO" not in case.sections:
        return "111"
    try:
        declared = case.sections["CAPTURE_STDIO"].decode("ascii").upper()
    except UnicodeDecodeError as error:
        raise HarnessError("CAPTURE_STDIO section is not ASCII") from error
    return "".join(
        "1" if token in declared else "0"
        for token in ("STDIN", "STDOUT", "STDERR")
    )


def _missing_extension(case: PhptCase) -> Optional[str]:
    return _missing_extension_sections(case.sections)


def _missing_extension_sections(sections: dict[str, bytes]) -> Optional[str]:
    try:
        text = sections.get("EXTENSIONS", b"").decode("utf-8")
    except UnicodeDecodeError as error:
        raise HarnessError("EXTENSIONS section is not UTF-8") from error
    for name in text.splitlines():
        name = name.strip().lower()
        if name and name not in AVAILABLE_EXTENSIONS:
            return name
    return None


def _uses_byte_preserving_transport(case_id: str, source: bytes) -> bool:
    if case_id not in BYTE_PRESERVING_CASES:
        return False
    try:
        source.decode("utf-8")
    except UnicodeDecodeError:
        return True
    return False


def _transport_php_source(case_id: str, source: bytes) -> bytes:
    if not _uses_byte_preserving_transport(case_id, source):
        return source
    return source.decode("latin-1").encode("utf-8")


def _encode_driver_output(output: str, byte_preserving: bool) -> bytes:
    if not byte_preserving:
        return output.encode("utf-8")
    encoded = bytearray()
    for character in output:
        codepoint = ord(character)
        if codepoint <= 0xFF:
            encoded.append(codepoint)
        else:
            encoded.extend(character.encode("utf-8"))
    return bytes(encoded)


def _write_php_source(path: Path, source: bytes, case_id: Optional[str] = None) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    try:
        path.write_bytes(_transport_php_source(case_id or "", source))
    except OSError as error:
        raise HarnessError(f"cannot create temporary PHP source: {error}") from error


def _skip_php_trivia(source: bytes, index: int) -> int:
    while index < len(source):
        if source[index] in b" \t\r\n\v\f":
            index += 1
        elif source.startswith(b"/*", index):
            end = source.find(b"*/", index + 2)
            return len(source) if end < 0 else _skip_php_trivia(source, end + 2)
        elif source.startswith(b"//", index) or source[index:index + 1] == b"#":
            end = source.find(b"\n", index + 1)
            return len(source) if end < 0 else _skip_php_trivia(source, end + 1)
        else:
            return index
    return index


def _skip_php_heredoc(source: bytes, index: int) -> int:
    newline = source.find(b"\n", index + 3)
    if newline < 0:
        return len(source)
    header = source[index + 3:newline].strip()
    if len(header) >= 2 and header[:1] in {b"'", b'"'} and header[-1:] == header[:1]:
        label = header[1:-1]
    else:
        label = header
    if not label or re.fullmatch(rb"[A-Za-z_][A-Za-z0-9_]*", label) is None:
        return index + 3
    terminator = re.compile(rb"(?m)^[ \t]*" + re.escape(label) + rb";?[ \t]*(?:\r?\n|$)")
    match = terminator.search(source, newline + 1)
    return match.end() if match else len(source)


def literal_sibling_includes(source: bytes) -> list[str]:
    """Return literal relative .inc include paths while ignoring PHP comments and strings."""
    include_words = {b"include", b"include_once", b"require", b"require_once"}
    paths: list[str] = []
    index = 0
    has_php_tags = b"<?" in source
    in_php = not has_php_tags
    while index < len(source):
        if not in_php:
            if source.startswith(b"<?", index):
                index += 2
                if source[index:index + 3].lower() == b"php":
                    index += 3
                in_php = True
            else:
                index += 1
            continue
        if source.startswith(b"?>", index):
            index += 2
            in_php = False
            continue
        if source.startswith(b"/*", index):
            end = source.find(b"*/", index + 2)
            index = len(source) if end < 0 else end + 2
            continue
        if source.startswith(b"//", index) or source[index:index + 1] == b"#":
            end = source.find(b"\n", index + 1)
            index = len(source) if end < 0 else end + 1
            continue
        if source.startswith(b"<<<", index):
            index = _skip_php_heredoc(source, index)
            continue
        if source[index:index + 1] in {b"'", b'"', b"`"}:
            quote = source[index]
            index += 1
            while index < len(source):
                if source[index] == 0x5C:
                    index += 2
                elif source[index] == quote:
                    index += 1
                    break
                else:
                    index += 1
            continue
        if source[index:index + 1].isalpha() or source[index:index + 1] == b"_":
            end = index + 1
            while end < len(source) and (
                source[end:end + 1].isalnum() or source[end] == 0x5F
            ):
                end += 1
            word = source[index:end].lower()
            if word not in include_words:
                index = end
                continue
            argument = _skip_php_trivia(source, end)
            parenthesized = argument < len(source) and source[argument] == 0x28
            if parenthesized:
                argument = _skip_php_trivia(source, argument + 1)
            if argument >= len(source) or source[argument] not in {0x27, 0x22}:
                index = end
                continue
            quote = source[argument]
            path_start = argument + 1
            path_end = source.find(bytes([quote]), path_start)
            if path_end < 0:
                index = end
                continue
            raw_path = source[path_start:path_end]
            if b"\\" not in raw_path and b"$" not in raw_path and raw_path.endswith(b".inc"):
                try:
                    paths.append(raw_path.decode("utf-8"))
                except UnicodeDecodeError as error:
                    raise HarnessError("literal .inc include path is not UTF-8") from error
            index = path_end + 1
            continue
        index += 1
    return paths


def stage_literal_includes(case: PhptCase, workdir: Path) -> None:
    """Copy literal sibling .inc files and their dependencies into the case cwd."""
    source_root = case.path.parent.resolve()
    destination_root = workdir.resolve()
    pending = [
        (source_root, include)
        for section in ("FILE", "SKIPIF", "CLEAN")
        for include in literal_sibling_includes(case.sections.get(section, b""))
    ]
    visited: set[Path] = set()
    staged: set[Path] = set()
    while pending:
        include_root, include = pending.pop()
        relative = Path(include)
        if relative.is_absolute() or relative.suffix != ".inc":
            raise HarnessError(f"literal .inc include is not a sibling path: {include!r}")
        try:
            source_path = (include_root / relative).resolve(strict=True)
            staged_relative = source_path.relative_to(source_root)
        except (OSError, ValueError) as error:
            raise HarnessError(f"literal .inc include escapes or is missing: {include!r}") from error
        if not source_path.is_file():
            raise HarnessError(f"literal .inc include is not a file: {include!r}")
        destination = destination_root / staged_relative
        try:
            destination.resolve(strict=False).relative_to(destination_root)
            if destination not in staged:
                payload = source_path.read_bytes()
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes(payload)
                staged.add(destination)
        except (OSError, ValueError) as error:
            raise HarnessError(f"cannot stage literal .inc include {include!r}: {error}") from error
        if source_path not in visited:
            visited.add(source_path)
            pending.extend(
                (source_path.parent, nested_include)
                for nested_include in literal_sibling_includes(source_path.read_bytes())
            )


def _comparison_output(case: PhptCase, main: Outcome) -> bytes:
    capture = _capture_mode(case)
    capture_stdout = capture[1] == "1"
    capture_stderr = capture[2] == "1"
    if capture_stdout:
        # When both are enabled, the driver keeps their write order in stdout.
        return main.stdout
    if capture_stderr:
        return main.stderr.encode("utf-8")
    return b""


def _php_string_truthy(value: bytes) -> bool:
    trimmed = _php_trim(value)
    return trimmed not in {b"", b"0"}


def _has_xfail(case: PhptCase) -> bool:
    return "XFAIL" in case.sections


def _is_flaky(case: PhptCase) -> bool:
    if "FLAKY" in case.sections:
        return True
    if b"SKIP_PERF_SENSITIVE" in case.sections.get("SKIPIF", b""):
        return True
    return FLAKY_FUNCTION.search(case.sections.get("FILE", b"")) is not None


def _failure_may_be_retried(
    case: PhptCase, phases: dict[str, Outcome], outcome: Outcome
) -> bool:
    if outcome.status not in {"output", "compile", "runtime", "xfailed"}:
        return False
    if _is_flaky(case):
        return True
    main = phases.get("main")
    if main is None:
        return False
    if main.detail.lower().startswith("main child timed out during runtime stage after "):
        return True
    return FLAKY_OUTPUT.search(_comparison_output(case, main)) is not None


def _xfail_failure(case: PhptCase, outcome: Outcome) -> Outcome:
    if _has_xfail(case) and outcome.status in {"compile", "runtime", "output"}:
        return Outcome(
            "xfailed",
            detail=outcome.detail,
            stdout=outcome.stdout,
            stderr=outcome.stderr,
        )
    return outcome


def _case_outcome(case: PhptCase, phases: dict[str, Outcome]) -> Outcome:
    if "driver" in phases:
        return Outcome("harness_error", detail=phases["driver"].detail)
    skipif = phases.get("skipif")
    if skipif is not None:
        if skipif.status in {"compile", "runtime"}:
            return Outcome(
                skipif.status,
                detail=f"SKIPIF {skipif.status} failure: {skipif.detail or skipif.stderr}",
                stderr=skipif.stderr,
            )
        if skipif.status in {"source", "invalid"}:
            return Outcome("harness_error", detail=f"SKIPIF could not complete: {skipif.detail or skipif.stderr}")
        if skipif.status != "passed":
            return Outcome(
                "harness_error",
                detail=f"SKIPIF execution failed at {skipif.status}: {skipif.detail or skipif.stderr}",
            )
        message = _php_trim(skipif.stdout).decode("utf-8", errors="replace")
        lowered = message.lower()
        if lowered.startswith("skip"):
            return Outcome("skipped", detail=message.strip())
        if lowered.startswith("xfail"):
            case.sections["XFAIL"] = message[5:].lstrip().encode("utf-8")
        elif lowered.startswith("flaky"):
            case.sections["FLAKY"] = message[5:].lstrip().encode("utf-8")
        if message and not any(
            lowered.startswith(item)
            for item in ("info", "warn ", "xfail", "xleak", "flaky")
        ):
            return Outcome("harness_error", detail=f"invalid nonempty SKIPIF output: {message.strip()}")

    main = phases.get("main")
    if main is None:
        return Outcome("harness_error", detail="runner produced no main result")
    if main.status in {"source", "invalid"}:
        return Outcome("harness_error", detail=main.detail or main.stderr or "main PHPT child could not complete")
    if main.status not in {"passed", "compile", "runtime"}:
        return Outcome("harness_error", detail=f"unexpected main status {main.status!r}")
    if main.status != "passed":
        return _xfail_failure(case, main)

    try:
        actual_output = _comparison_output(case, main)
        matches, mismatch = compare_output(case, actual_output)
    except HarnessError as error:
        return Outcome("harness_error", detail=str(error))
    if not matches:
        return _xfail_failure(case, Outcome("output", detail=mismatch, stdout=actual_output))

    clean = phases.get("clean")
    if clean is not None:
        if "timed out" in clean.detail.lower():
            return Outcome(
                "harness_error",
                detail=f"test BORKED: CLEAN timed out: {clean.detail}",
                stderr=clean.stderr,
            )
        if clean.status in {"source", "invalid"}:
            return Outcome("harness_error", detail=f"CLEAN could not complete: {clean.detail or clean.stderr}")
        if _php_string_truthy(clean.stdout):
            return Outcome("harness_error", detail="test BORKED: CLEAN produced output")
    if _has_xfail(case):
        return Outcome(
            "warning",
            detail="XFAIL section but test passes",
            stdout=actual_output,
        )
    return Outcome("passed", stdout=actual_output)


def _execute_plans(
    plans: list[ExecutionPlan],
    jobs: int,
    execute_plan,
) -> list[Outcome]:
    if not 1 <= jobs <= MAX_WORKERS:
        raise HarnessError(f"PHPT worker count must be between 1 and {MAX_WORKERS}")
    if not plans:
        return []

    worker_count = min(jobs, len(plans))
    results: list[Optional[Outcome]] = [None] * len(plans)
    pending = {}
    next_index = 0
    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=worker_count) as executor:
            while next_index < len(plans) or pending:
                while next_index < len(plans) and len(pending) < worker_count:
                    plan = plans[next_index]
                    try:
                        future = executor.submit(execute_plan, plan)
                    except Exception as error:
                        raise HarnessError(
                            f"cannot schedule PHPT case {plan.case.case_id}: {error}"
                        ) from error
                    pending[future] = (next_index, plan)
                    next_index += 1

                completed, _ = concurrent.futures.wait(
                    pending,
                    return_when=concurrent.futures.FIRST_COMPLETED,
                )
                for future in completed:
                    index, plan = pending.pop(future)
                    try:
                        results[index] = future.result()
                    except Exception as error:
                        results[index] = Outcome(
                            "harness_error",
                            detail=f"worker raised {type(error).__name__}: {error}",
                        )
    except HarnessError:
        raise
    except Exception as error:
        raise HarnessError(
            f"PHPT worker pool failed: {type(error).__name__}: {error}"
        ) from error

    if any(result is None for result in results):
        raise HarnessError("PHPT worker pool returned an incomplete result set")
    return [result for result in results if result is not None]


def run_shard(
    php_src: Path,
    split: str,
    jobs: int = DEFAULT_WORKERS,
    shard: Optional[str] = None,
) -> int:
    started_at = time.monotonic()
    php_src = php_src.resolve()
    if not 1 <= jobs <= MAX_WORKERS:
        raise HarnessError(f"PHPT worker count must be between 1 and {MAX_WORKERS}")
    shard_index, shard_count = parse_shard(shard) if shard is not None else (1, 1)
    project_root = Path(__file__).resolve().parents[3]
    sbcl = os.environ.get("SBCL") or shutil.which("sbcl")
    if not sbcl:
        raise HarnessError("sbcl is unavailable on PATH")
    expected_all = load_expected_failures()
    validate_expected_failure_roots(expected_all)
    paths = scan_split(php_src, split)
    all_case_ids = [path.relative_to(php_src).as_posix() for path in paths]
    expected = {
        case_id: entry
        for case_id, entry in expected_all.items()
        if split_for_case(case_id) == split
    }
    all_discovered = set(all_case_ids)
    stale_manifest_ids = sorted(set(expected) - all_discovered)
    selected_paths = select_shard(paths, shard)
    selected_case_ids = {
        path.relative_to(php_src).as_posix() for path in selected_paths
    }
    case_records: dict[str, PhptCase] = {}
    outcomes: dict[str, Outcome] = {}
    consumed: set[str] = set()
    warnings: list[tuple[str, str]] = []
    xfailed: list[tuple[str, str]] = []
    skipped_cases: list[tuple[str, str]] = []
    counts = {
        "passed": 0,
        "expected": 0,
        "xfailed": 0,
        "warnings": 0,
        "skipped": 0,
        "args_skipped": 0,
        "failed": 0,
        "harness": 0,
    }
    details: list[tuple[str, str]] = []
    for stale_id in stale_manifest_ids:
        details.append((stale_id, "stale expected-failure entry: test was not discovered"))

    with tempfile.TemporaryDirectory(prefix=f"cl-cc-phpt-{split}-") as temp_name:
        temp_root = Path(temp_name)
        plans: list[ExecutionPlan] = []
        for index, path in enumerate(selected_paths):
            case_id = path.relative_to(php_src).as_posix()
            try:
                relative, sections = read_phpt_sections(path, php_src)
                if split_for_case(relative) != split:
                    raise HarnessError(f"case escaped its declared split: {case_id}")
                if "REDIRECTTEST" in sections:
                    if "PHPDBG" in sections:
                        outcomes[case_id] = Outcome(
                            "skipped", detail="PHPDBG SAPI is unavailable"
                        )
                        continue
                    if is_cgi_sections(sections):
                        outcomes[case_id] = Outcome(
                            "skipped", detail="CGI SAPI is unavailable"
                        )
                        continue
                    missing_extension = _missing_extension_sections(sections)
                    if missing_extension:
                        outcomes[case_id] = Outcome(
                            "skipped",
                            detail=f"required extension unavailable: {missing_extension}",
                        )
                        continue
                    if _has_cli_args(sections):
                        outcomes[case_id] = Outcome("skipped", detail=ARGS_SKIP_REASON)
                        continue
                    raise HarnessError("unsupported PHPT sections: REDIRECTTEST")

                case = _case_from_sections(relative, path, sections)
                case_records[case_id] = case
                expand_external_sections(case)
                if "PHPDBG" in case.sections:
                    outcomes[case_id] = Outcome("skipped", detail="PHPDBG SAPI is unavailable")
                    continue
                if is_cgi_case(case):
                    outcomes[case_id] = Outcome("skipped", detail="CGI SAPI is unavailable")
                    continue
                bad_sections = unsupported_sections(case)
                if bad_sections:
                    raise HarnessError("unsupported PHPT sections: " + ", ".join(bad_sections))
                missing_extension = _missing_extension(case)
                if missing_extension:
                    outcomes[case_id] = Outcome(
                        "skipped", detail=f"required extension unavailable: {missing_extension}"
                    )
                    continue
                if _has_cli_args(case.sections):
                    outcomes[case_id] = Outcome("skipped", detail=ARGS_SKIP_REASON)
                    continue
                case_skip_reason = _case_specific_skip_reason(case)
                if case_skip_reason:
                    outcomes[case_id] = Outcome("skipped", detail=case_skip_reason)
                    continue

                environment = parse_environment(case)
                ini = parse_ini_settings(case, environment)
                source_dir = temp_root / "source" / f"{index:06d}"
                workdir = temp_root / "work" / f"{index:06d}"
                workdir.mkdir(parents=True, exist_ok=True)
                stage_literal_includes(case, workdir)
                source = source_dir / "file.php"
                _write_php_source(source, case.sections["FILE"], case.case_id)
                skipif = None
                if case.sections.get("SKIPIF", b""):
                    skipif = source_dir / "skipif.php"
                    _write_php_source(skipif, case.sections["SKIPIF"], case.case_id)
                clean = None
                if case.sections.get("CLEAN", b""):
                    clean = source_dir / "clean.php"
                    _write_php_source(
                        clean, _php_trim(case.sections["CLEAN"]), case.case_id
                    )
                stdin = None
                if "STDIN" in case.sections:
                    stdin = source_dir / "stdin"
                    _write_php_source(stdin, case.sections["STDIN"])
                capture = _capture_mode(case)
                plans.append(
                    ExecutionPlan(
                        case,
                        source,
                        workdir,
                        temp_root / "driver" / f"{index:06d}.progress",
                        skipif,
                        clean,
                        stdin,
                        ini,
                        capture,
                        environment,
                    )
                )
            except SkipCase as skipped:
                outcomes[case_id] = Outcome("skipped", detail=str(skipped))
            except (HarnessError, OSError, UnicodeError, ValueError) as error:
                outcomes[case_id] = Outcome("harness_error", detail=str(error))

        if plans:
            core_path = temp_root / "cl-cc.core"
            prepare_core(project_root, core_path, sbcl, temp_root)
            def execute_plan(plan: ExecutionPlan) -> Outcome:
                phases = run_case(plan, project_root, core_path, sbcl)
                outcome = _case_outcome(plan.case, phases)
                if _failure_may_be_retried(plan.case, phases, outcome):
                    phases = run_case(plan, project_root, core_path, sbcl)
                    outcome = _case_outcome(plan.case, phases)
                    if outcome.status == "passed":
                        outcome = Outcome(
                            "warning",
                            detail="test passed on retry attempt",
                            stdout=outcome.stdout,
                        )
                entry = expected.get(plan.case.case_id)
                main = phases.get("main")
                if (
                    entry is not None
                    and entry.allow_pass
                    and main is not None
                    and main.status == "invalid"
                    and main.detail == INTERMITTENT_COMPILE_TIMEOUT
                ):
                    return Outcome("compile", detail=INTERMITTENT_COMPILE_TIMEOUT)
                return outcome

            for plan, outcome in zip(plans, _execute_plans(plans, jobs, execute_plan)):
                outcomes[plan.case.case_id] = outcome

    for case_id in sorted(selected_case_ids):
        outcome = outcomes.get(case_id)
        if outcome is None:
            outcome = Outcome("harness_error", detail="runner produced no outcome")
        entry = expected.get(case_id)
        if entry is not None and entry.allow_pass:
            if outcome.status == "passed":
                accepted, message = apply_expected_failures(
                    case_id, "passed", "", expected, consumed
                )
                if accepted:
                    counts["passed"] += 1
                    continue
                counts["failed"] += 1
                details.append((case_id, message))
                continue
            if outcome.status in {"warning", "xfailed", "skipped"}:
                counts["failed"] += 1
                details.append(
                    (
                        case_id,
                        f"intermittent compile-timeout entry accepts only pass or its exact timeout; observed {outcome.status}: {outcome.detail}",
                    )
                )
                continue
        if outcome.status in {"passed", "warning", "xfailed"}:
            if case_id in expected:
                if outcome.status == "xfailed":
                    details.append(
                        (case_id, "stale expected-failure entry: failure is covered by XFAIL")
                    )
                else:
                    details.append((case_id, "stale expected-failure entry: test now passes"))
            elif outcome.status == "passed":
                counts["passed"] += 1
            elif outcome.status == "warning":
                counts["warnings"] += 1
                warnings.append((case_id, outcome.detail))
            else:
                counts["xfailed"] += 1
                reason = case_records[case_id].sections.get("XFAIL", b"").decode(
                    "utf-8", errors="replace"
                ).strip()
                xfailed.append((case_id, reason))
            continue
        if outcome.status == "skipped":
            skipped_cases.append((case_id, outcome.detail))
            if outcome.detail == ARGS_SKIP_REASON:
                counts["args_skipped"] += 1
            if case_id in expected:
                details.append((case_id, "stale expected-failure entry: test is skipped"))
            else:
                counts["skipped"] += 1
            continue
        if outcome.status == "harness_error":
            counts["harness"] += 1
            details.append((case_id, f"harness error: {outcome.detail}"))
            continue
        stage = "output" if outcome.status == "output" else outcome.status
        evidence = outcome.detail or outcome.stderr.strip() or stage
        accepted, message = apply_expected_failures(case_id, stage, evidence, expected, consumed)
        if accepted:
            counts["expected"] += 1
        else:
            counts["failed"] += 1
            details.append((case_id, message))

    for case_id in sorted(set(expected) - consumed):
        if case_id in selected_case_ids and case_id not in {test_id for test_id, _ in details}:
            details.append((case_id, "stale expected-failure entry: no matching observed failure"))

    total = len(selected_paths)
    print(
        f"PHPT {split}: total={total} passed={counts['passed']} "
        f"expected-failures={counts['expected']} skipped={counts['skipped']} "
        f"args-skipped={counts['args_skipped']} "
        f"xfailed={counts['xfailed']} warnings={counts['warnings']} "
        f"failures={counts['failed']} harness-errors={counts['harness']}"
    )
    for case_id, reason in skipped_cases:
        print(f"{case_id}: skipped: {reason}", file=sys.stderr)
    for case_id, message in details:
        print(f"{case_id}: {message}", file=sys.stderr)
    for case_id, message in warnings:
        print(f"{case_id}: warning: {message}", file=sys.stderr)
    for case_id, reason in xfailed:
        suffix = f": {reason}" if reason else ""
        print(f"{case_id}: XFAILED{suffix}", file=sys.stderr)
    print(
        f"PHPT timing: split={split} shard={shard_index}/{shard_count} "
        f"elapsed_seconds={time.monotonic() - started_at:.3f}"
    )
    return int(bool(details) or counts["failed"] > 0 or counts["harness"] > 0)


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--php-src", type=Path, required=True)
    parser.add_argument("--split", choices=SPLITS, required=True)
    parser.add_argument(
        "--jobs",
        type=int,
        default=DEFAULT_WORKERS,
        metavar="N",
        help=f"parallel PHPT workers (1-{MAX_WORKERS}, default: {DEFAULT_WORKERS})",
    )
    parser.add_argument(
        "--shard",
        metavar="INDEX/COUNT",
        help="run one deterministic one-based partition of the selected split",
    )
    args = parser.parse_args(argv)
    try:
        if args.shard is None:
            return run_shard(args.php_src, args.split, args.jobs)
        return run_shard(args.php_src, args.split, args.jobs, args.shard)
    except HarnessError as error:
        print(f"PHPT harness error: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
