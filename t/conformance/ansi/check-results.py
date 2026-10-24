import argparse
from collections import Counter
import json
import re
import sys
from pathlib import Path


CHAPTERS = {
    "symbols",
    "eval-and-compile",
    "data-and-control-flow",
    "iteration",
    "objects",
    "conditions",
    "cons",
    "arrays",
    "hash-tables",
    "packages",
    "numbers",
    "sequences",
    "structures",
    "types-and-classes",
    "strings",
    "characters",
    "pathnames",
    "files",
    "streams",
    "printer",
    "reader",
    "system-construction",
    "environment",
    "misc",
}
CATEGORIES = {
    "未実装",
    "不具合",
    "implementation-defined",
    "意図的な拡張",
    "対象外",
}


def sexp_tokens(text, source_name):
    token_re = re.compile(r'\s*(;[^\n]*(?:\n|$)|\(|\)|"(?:\\.|[^"\\])*"|[^\s()]+)')
    position = 0
    while position < len(text):
        if text[position:].isspace():
            return
        match = token_re.match(text, position)
        if match is None:
            raise ValueError(f"invalid S-expression in {source_name} at byte {position}")
        token = match.group(1)
        position = match.end()
        if not token.startswith(";"):
            yield token


def parse_sexp(text, source_name):
    tokens = iter(sexp_tokens(text, source_name))

    def parse_one(token):
        if token == "(":
            values = []
            for token in tokens:
                if token == ")":
                    return values
                values.append(parse_one(token))
            raise ValueError(f"unclosed list in {source_name}")
        if token == ")":
            raise ValueError(f"unexpected close parenthesis in {source_name}")
        if token.startswith('"'):
            try:
                return json.loads(token)
            except json.JSONDecodeError as error:
                raise ValueError(f"invalid string in {source_name}") from error
        return token

    try:
        first = next(tokens)
    except StopIteration:
        raise ValueError(f"{source_name} is empty") from None
    result = parse_one(first)
    try:
        next(tokens)
    except StopIteration:
        return result
    raise ValueError(f"{source_name} has more than one top-level form")


def expected_cases(path):
    value = parse_sexp(path.read_text(encoding="utf-8"), path.name)
    if not isinstance(value, list):
        raise ValueError("expected-failures.sexp must contain one list")

    cases = []
    seen = set()
    required = {"chapter", "mode", "test", "category", "reason"}
    for index, row in enumerate(value, start=1):
        if not isinstance(row, list) or len(row) % 2:
            raise ValueError(f"expected-failures row {index} must be a property list")
        properties = {}
        for key, item in zip(row[::2], row[1::2]):
            if not isinstance(key, str) or not key.startswith(":"):
                raise ValueError(f"expected-failures row {index} has a non-keyword key")
            name = key[1:].lower()
            if name in properties:
                raise ValueError(f"expected-failures row {index} repeats :{name}")
            properties[name] = item
        if set(properties) != required:
            raise ValueError(
                f"expected-failures row {index} must have exactly {sorted(required)}"
            )
        chapter, mode, test, category, reason = (
            properties[name] for name in ("chapter", "mode", "test", "category", "reason")
        )
        if not all(isinstance(item, str) and item for item in (chapter, mode, test, category, reason)):
            raise ValueError(f"expected-failures row {index} values must be non-empty strings")
        if chapter not in CHAPTERS:
            raise ValueError(f"expected-failures row {index} has unknown chapter {chapter!r}")
        if mode not in {"eval", "compile"}:
            raise ValueError(f"expected-failures row {index} has unknown mode {mode!r}")
        if category not in CATEGORIES:
            raise ValueError(f"expected-failures row {index} has unknown category {category!r}")
        key = (chapter, mode, test.casefold())
        if key in seen:
            raise ValueError(f"expected-failures row {index} duplicates {key!r}")
        seen.add(key)
        cases.append((chapter, mode, test, category, reason))
    return cases


def expected_counts(path):
    value = parse_sexp(path.read_text(encoding="utf-8"), path.name)
    if not isinstance(value, list):
        raise ValueError("expected-counts.sexp must contain one list")

    counts = {}
    required = {"chapter", "mode", "loaded", "executed", "filtered", "inactive"}
    for index, row in enumerate(value, start=1):
        if not isinstance(row, list) or len(row) % 2:
            raise ValueError(f"expected-counts row {index} must be a property list")
        properties = {}
        for key, item in zip(row[::2], row[1::2]):
            if not isinstance(key, str) or not key.startswith(":"):
                raise ValueError(f"expected-counts row {index} has a non-keyword key")
            name = key[1:].lower()
            if name in properties:
                raise ValueError(f"expected-counts row {index} repeats :{name}")
            properties[name] = item
        if set(properties) != required:
            raise ValueError(
                f"expected-counts row {index} must have exactly {sorted(required)}"
            )
        chapter = properties["chapter"]
        mode = properties["mode"]
        if not isinstance(chapter, str) or chapter not in CHAPTERS:
            raise ValueError(f"expected-counts row {index} has an unknown chapter")
        if not isinstance(mode, str) or mode not in {"eval", "compile"}:
            raise ValueError(f"expected-counts row {index} has an unknown mode")
        raw_numbers = [
            properties[name] for name in ("loaded", "executed", "filtered", "inactive")
        ]
        if not all(isinstance(number, str) and number.isdecimal() for number in raw_numbers):
            raise ValueError(f"expected-counts row {index} counts must be non-negative integers")
        loaded, executed, filtered, inactive = map(int, raw_numbers)
        if loaded == 0 or executed == 0:
            raise ValueError(f"expected-counts row {index} must describe nonzero tests")
        if loaded != executed + filtered + inactive:
            raise ValueError(f"expected-counts row {index} has impossible counts")
        key = (chapter, mode)
        if key in counts:
            raise ValueError(f"expected-counts row {index} duplicates {key!r}")
        counts[key] = (loaded, executed, filtered, inactive)
    return counts


def normalized_names(pattern, text):
    return [match.casefold() for match in re.findall(pattern, text, re.MULTILINE)]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--chapter", required=True)
    parser.add_argument("--mode", choices=("eval", "compile"), required=True)
    parser.add_argument("--expected", type=Path, required=True)
    parser.add_argument("--counts", type=Path, required=True)
    parser.add_argument("--runner-log", type=Path, required=True)
    parser.add_argument("--runner-status", type=int, required=True)
    parser.add_argument("--summary", type=Path, required=True)
    args = parser.parse_args()

    summary = []
    errors = []
    try:
        if args.chapter not in CHAPTERS:
            raise ValueError(f"unknown chapter {args.chapter!r}")
        text = args.runner_log.read_text(encoding="utf-8", errors="replace")
        cases = expected_cases(args.expected)
        count_manifest = expected_counts(args.counts)
        expected = {
            test.casefold()
            for chapter, mode, test, _category, _reason in cases
            if chapter == args.chapter and mode == args.mode
        }
        selected_cases = [
            case for case in cases if case[0] == args.chapter and case[1] == args.mode
        ]
        results = re.findall(
            r"^ANSI-RESULT chapter=([^ ]+) mode=([^ ]+) loaded=(\d+) filtered=(\d+) "
            r"executed=(\d+) pass=(\d+) fail=(\d+) inactive=(\d+)$",
            text,
            re.MULTILINE,
        )
        if len(results) != 1:
            raise ValueError(f"expected one ANSI-RESULT marker, found {len(results)}")
        (
            chapter,
            mode,
            loaded_text,
            filtered_text,
            executed_text,
            passed_text,
            failed_text,
            inactive_text,
        ) = results[0]
        if chapter != args.chapter or mode != args.mode:
            raise ValueError(f"runner reported {chapter}/{mode}, expected {args.chapter}/{args.mode}")
        loaded_count, filtered_count, executed_count, passed_count, failed_count, inactive_count = map(
            int,
            (
                loaded_text,
                filtered_text,
                executed_text,
                passed_text,
                failed_text,
                inactive_text,
            ),
        )
        observed_counts = (loaded_count, executed_count, filtered_count, inactive_count)
        expected_count = count_manifest.get((args.chapter, args.mode))
        if expected_count is None:
            errors.append(
                "expected count tuple is missing; observed tuple: "
                f"loaded={loaded_count} executed={executed_count} "
                f"filtered={filtered_count} inactive={inactive_count}"
            )
        elif expected_count != observed_counts:
            errors.append(
                "expected count tuple "
                f"loaded={expected_count[0]} executed={expected_count[1]} "
                f"filtered={expected_count[2]} inactive={expected_count[3]}, "
                "observed tuple "
                f"loaded={loaded_count} executed={executed_count} "
                f"filtered={filtered_count} inactive={inactive_count}"
            )
        passed = normalized_names(r"^ANSI-PASSED-TEST:([^\r\n]+)$", text)
        failed = normalized_names(r"^ANSI-FAILED-TEST:([^\r\n]+)$", text)
        filtered = normalized_names(r"^ANSI-FILTERED-TEST:([^\r\n]+)$", text)
        if args.runner_status != 0:
            errors.append(f"cl-cc runner exited {args.runner_status}")
        if executed_count == 0:
            errors.append("executed zero tests")
        if loaded_count != filtered_count + executed_count + inactive_count:
            errors.append("loaded count differs from filtered, executed, and inactive counts")
        if executed_count != passed_count + failed_count:
            errors.append("executed count differs from pass plus fail counts")
        if len(passed) != passed_count or len(failed) != failed_count:
            errors.append("test name markers differ from the reported pass/fail counts")
        if len(filtered) != filtered_count:
            errors.append("filtered test name markers differ from the reported filtered count")
        if len(set(passed)) != len(passed) or len(set(failed)) != len(failed):
            errors.append("runner emitted duplicate test name markers")
        if len(set(filtered)) != len(filtered):
            errors.append("runner emitted duplicate filtered test markers")
        if set(passed) & set(failed):
            errors.append("runner marked a test as both passed and failed")
        if set(filtered) & (set(passed) | set(failed)):
            errors.append("runner marked a filtered test as executed")
        if not set(filtered) <= {"load-pathname.1", "load-truename.1"}:
            errors.append("runner filtered a test outside the two upstream driver exclusions")

        observed = set(passed) | set(failed)
        unexpected = sorted(set(failed) - expected)
        expected_passed = sorted(expected & set(passed))
        expected_missing = sorted(expected - observed)
        category_counts = Counter(case[3] for case in selected_cases)
        taxonomy_counts = " ".join(
            f"expected-category-{category}={category_counts[category]}"
            for category in sorted(CATEGORIES)
        )
        filtered_names = ",".join(filtered) if filtered else "none"
        summary.append(
            f"ANSI {args.chapter}/{args.mode}: loaded={loaded_count} "
            f"executed={executed_count} filtered={filtered_count} inactive={inactive_count} "
            f"expected-counts={expected_count if expected_count is not None else 'missing'} "
            f"filtered-tests={filtered_names} "
            f"passed={passed_count} failed={failed_count} expected-failures={len(expected)} "
            f"{taxonomy_counts}"
        )
        if unexpected:
            errors.append("unlisted failures: " + ", ".join(unexpected))
        if expected_passed:
            errors.append("expected failures passed: " + ", ".join(expected_passed))
        if expected_missing:
            errors.append("expected failures were not run: " + ", ".join(expected_missing))
    except (OSError, ValueError, json.JSONDecodeError) as error:
        errors.append(str(error))

    if errors:
        summary.extend(f"FAIL: {error}" for error in errors)
    else:
        summary.append("PASS: observed failures match the pinned expected-failures list")

    output = "\n".join(summary) + "\n"
    args.summary.write_text(output, encoding="utf-8")
    sys.stdout.write(output)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
