#!/usr/bin/env python3

# NOTE: This script is a python3 port of the test.dart script from
#       https://github.com/munificent/craftinginterpreters

import sys

MIN_PYTHON = (3, 11)
if sys.version_info < MIN_PYTHON:
    sys.exit("Python %s.%s or later is required.\n" % MIN_PYTHON)

from argparse import ArgumentParser, RawTextHelpFormatter
from collections.abc import Callable
from contextlib import chdir
from dataclasses import dataclass
from enum import Enum
from os.path import realpath, dirname
from pathlib import Path
from subprocess import DEVNULL, CalledProcessError, TimeoutExpired, run
from typing import Dict, Generator, List, Self, Tuple
import fnmatch
import re
import textwrap

DIR = Path(dirname(realpath(__file__)))

OUTPUT_EXPECT = re.compile(r"// expect: ?(.*)")
ERROR_EXPECT = re.compile(r"// (Error.*)")
ERROR_LINE_EXPECT = re.compile(r"// \[((java|c) )?line (\d+)\] (Error.*)")
RUNTIME_ERROR_EXPECT = re.compile(r"// expect runtime error: (.+)")

NONTEST_RE = re.compile(r"// nontest")

# [1:33] SyntaxError: expect '<literal>', got '+'
SYNTAX_ERROR_RE = re.compile(r"\[(\d+):(\d+)\] SyntaxError: expect '(.+)', got '(.+)'")

# [1:26] RuntimeError: Invalid binary operation '+' between '<bool>' and '<number>'
RUNTIME_ERROR_RE = re.compile(r"\[(\d+):(\d+)\] RuntimeError: (.+)")

# TODO: I haven't got this far :>
STACK_TRACE_RE = re.compile(r"\[line (\d+)\]")


class Variant(Enum):
    TREE_WALK = "tree_walk"
    BYTECODE = "bytecode"


@dataclass
class Binary:
    path: Path


@dataclass
class Command:
    args: List[str]


@dataclass
class Exe:
    name: str
    variant: Variant
    bin: Binary | Command


class Interpreter(Enum):
    LOXI = Exe("loxi", Variant.TREE_WALK, Binary(DIR / "target" / "release" / "loxi"))
    LOXII = Exe("loxii", Variant.BYTECODE, Binary(DIR / "target" / "release" / "loxii"))


@dataclass
class Filter:
    pat: re.Pattern
    invert: bool

    def match(self, path: Path) -> bool:
        match = self.pat.match(str(path)) is not None
        return not match if self.invert else match


@dataclass
class Result:
    passed: int = 0
    skipped: int = 0
    failed: int = 0

    def __add__(self, other):
        return Result(
            passed=self.passed + other.passed,
            skipped=self.skipped + other.skipped,
            failed=self.failed + other.failed,
        )

    def __str__(self):
        return (
            f"- {Cs.g('Passed ')}: {self.passed}"
            f"\n- {Cs.y('Skipped')}: {self.skipped}"
            f"\n- {Cs.r('Failed ')}: {self.failed}"
        )


@dataclass
class ChapterDetail:
    number: int
    name: str


class Chapter(Enum):
    SCANNING = ChapterDetail(4, "scanning")
    PARSING = ChapterDetail(6, "parsing")
    EVALUATING = ChapterDetail(7, "evaluating")
    STATEMENTS = ChapterDetail(8, "statements")
    CONTROL = ChapterDetail(9, "control")
    FUNCTIONS = ChapterDetail(10, "functions")
    RESOLVING = ChapterDetail(11, "resolving")
    CLASSES_I = ChapterDetail(12, "classes")
    INHERITANCE = ChapterDetail(13, "inheritance")

    COMPILING = ChapterDetail(17, "compiling")
    TYPE = ChapterDetail(18, "type")
    STRINGS = ChapterDetail(19, "strings")
    HASH = ChapterDetail(20, "hash")
    GLOBAL = ChapterDetail(21, "global")
    LOCAL = ChapterDetail(22, "local")
    JUMPING = ChapterDetail(23, "jumping")
    CALLS = ChapterDetail(24, "calls")
    CLOSURES = ChapterDetail(25, "closures")
    GARBAGE = ChapterDetail(26, "garbage")
    CLASSES_II = ChapterDetail(27, "classes")
    METHODS = ChapterDetail(28, "methods")
    SUPERCLASSES = ChapterDetail(29, "superclasses")
    OPTIMIZATION = ChapterDetail(30, "optimization")


class TestSuite:
    class Kind(Enum):
        PASS = "pass"
        SKIP = "skip"

    def __init__(self, chapter: Chapter, tests: Generator[Path, None, None]):
        self.chapter = chapter
        self.tests = tests

    @classmethod
    def from_passes(cls, chapter: Chapter, passes: List[str]) -> Self:
        return cls._from_files(chapter, passes, cls.Kind.PASS)

    @classmethod
    def from_skips(cls, chapter: Chapter, skips: List[str]) -> Self:
        return cls._from_files(chapter, skips, cls.Kind.SKIP)

    @classmethod
    def _from_files(cls, chapter: Chapter, files: List[str], kind: Kind) -> Self:
        paths = [DIR / file for file in files]
        pass_kind = kind == TestSuite.Kind.PASS

        def should_not_skip(path: Path) -> bool:
            for f in paths:
                if path.is_relative_to(f):
                    return pass_kind
            return not pass_kind

        tests = cls.walk(DIR / "test", should_not_skip)
        return cls(chapter, tests)

    @staticmethod
    def walk(path: Path, pred: Callable[[Path], bool]) -> Generator[Path, None, None]:
        for root, _, files in path.walk():
            for file in files:
                if pred(root / file):
                    path = (root / file).relative_to(DIR)
                    yield path


# populated by populate_tests() function
TESTS: Dict[Variant, List[TestSuite]] = {
    Variant.TREE_WALK: [],
    Variant.BYTECODE: [],
}


# colored strings
# ------------------------------
class Cs:
    @staticmethod
    def r(s: str) -> str:
        return f"\x1b[1;31m{s}\x1b[0m"

    @staticmethod
    def g(s: str) -> str:
        return f"\x1b[1;32m{s}\x1b[0m"

    @staticmethod
    def y(s: str) -> str:
        return f"\x1b[1;33m{s}\x1b[0m"


# cursor movement
# ---------------------
class Cm:
    @staticmethod
    def s() -> str:
        return "\x1b[s"

    @staticmethod
    def u() -> str:
        return "\x1b[u"


# pretty print stdoand stderr
def pprint(stdout: str, stderr: str):
    def strip(s: str):
        s = s.rstrip()
        return " " if len(s) == 0 else s

    width = 100
    print("┌─── [stdout]", "─" * (width - 13), sep="")
    print(textwrap.indent(strip(stdout), "│", lambda _: True))
    print("├─── [stderr]", "─" * (width - 13), sep="")
    print(textwrap.indent(strip(stderr), "│", lambda _: True))
    print("└", "─" * (width - 1), sep="")
    pass


class Test:
    @dataclass
    class Expect:
        count: int
        exit_code: int
        output: List[Tuple[int, str]]
        compile_error: List[Tuple[int, str]]
        runtime_error: Tuple[int, str] | None

    def __init__(self, interpreter: Interpreter, filter_path: Filter | None):
        self.interpreter = interpreter
        self.expectations: int = 0
        self.failures: Dict[Path, List[str]] = {}
        self.filter: Filter | None = filter_path
        pass

    def run_all(self) -> Result:
        tests = TESTS[self.interpreter.value.variant]
        print(f"\n>>> Running {self.interpreter.value.name} tests", flush=True)

        done_tests: Dict[Path, bool] = {}

        result = Result()
        for suite in tests:
            print(f"\n- Testing Chapter {suite.chapter.name}", flush=True)
            match suite.chapter:
                case Chapter.SCANNING | Chapter.PARSING | Chapter.EVALUATING:
                    print(f"\t- SKIPPED (can only be ran individually)")
                    result.skipped += 1
                    continue

            for test in suite.tests:
                if self.filter is not None and not self.filter.match(test):
                    result.skipped += 1
                    continue

                if (done := done_tests.get(test)) is not None:
                    if done:
                        print(f"\t> {Cs.g("PASSED")} {Cs.y("(cached)")}: {test}", flush=True)
                        result.passed += 1
                    else:
                        print(f"\t> {Cs.r("FAILED")} {Cs.y("(cached)")}: {test}", flush=True)
                        result.failed += 1
                    continue

                match self._run_test(test):
                    case True:
                        result.passed += 1
                        done_tests[test] = True
                    case False:
                        result.failed += 1
                        done_tests[test] = False
                    case None:
                        result.skipped += 1

        return result

    def run_chapter(self, chapter: Chapter) -> Result:
        print(f"\n>>> Running {self.interpreter.value.name} tests", flush=True)
        suite = self._suite_from_chapter(chapter)
        result = Result()

        if suite is None:
            print(f"Can't find suite for chapter {chapter}, probably wrong variant")
            result.skipped = 1
            return result

        print(f"\n- Testing Chapter {suite.chapter.name}", flush=True)
        for test in suite.tests:
            if self.filter is not None and not self.filter.match(test):
                result.skipped += 1
                continue

            match self._run_test(test):
                case True:
                    result.passed += 1
                case False:
                    result.failed += 1
                case None:
                    result.skipped += 1

        return result

    def _run_test(self, test: Path) -> bool | None:
        expect = self._parse_test(test)
        if expect is None:
            print(f"\t- 'test' is not a test file, skipping")
            return None

        def args(exe: Exe):
            match exe:
                case Exe(_, _, Binary(bin)):
                    return [str(bin)]
                case Exe(_, _, Command(cmd)):
                    return cmd
                case _:
                    raise TypeError("Not a Binary or Command, did you added new types?")

        print(f"\t> {Cm.s()}running '{test}'", end="", flush=True)
        exec = args(self.interpreter.value)

        try:
            proc = run(exec + [str(test)], capture_output=True, text=True, timeout=10)
        except TimeoutExpired:
            print(Cm.u() + Cs.r("TIMEOUT"))
            return False

        stdout = proc.stdout
        stderr = proc.stderr
        returncode = proc.returncode

        # panic!
        if returncode == 101:
            print(stderr)
            print("=" * 80)
            print("...")
            return False

        if self._validate(test, expect, stdout, stderr, returncode):
            print(f"{Cm.u()}{Cs.g("PASSED ")}")
            return True
        else:
            print(f"{Cm.u()}{Cs.r("FAILED ")}")
            [print(f"\t\t- {Cs.y(f)}") for f in self.failures[test]]
            pprint(stdout, stderr)
            return False

    def _validate(
        self, test: Path, expect: Expect, output: str, error: str, returncode: int
    ) -> bool:
        if expect.compile_error and expect.runtime_error:
            self._fail(test, "Test error: can't expect both compile and runtime errors")
            return False

        errlines = error.split("\n")

        # Validate that an expected runtime error occurred.
        if expect.runtime_error is not None:
            self._validate_runtime_error(errlines, expect.runtime_error)
        else:
            self._validate_compile_errors(errlines, expect.compile_error)

        if not self._validate_return_code(test, expect.exit_code, returncode):
            return False

        return self._validate_output(test, expect.output, output)

    def _validate_runtime_error(self, lines: List[str], error: Tuple[int, str]):
        # Skip any compile errors. This can happen if there is a compile error in
        # a module loaded by the module being tested.
        line = 0
        while SYNTAX_ERROR_RE.search(lines[line]):
            line += 1
        # TODO: implement

    def _validate_compile_errors(self, lines: List[str], errors: List[Tuple[int, str]]):
        # TODO: implement
        pass

    def _validate_output(
        self, test: Path, expect: List[Tuple[int, str]], real: str
    ) -> bool:
        success = True
        lines = real.splitlines()

        if (e := len(expect)) != (r := len(real)):
            self._fail(test, f"Got differing output length: expect '{e}', got '{r}'")

        for (_, e), r in zip(expect, lines):
            if e != r:
                success = False
                self._fail(test, f"Expected '{e}', got '{r}'")

        return success

    def _validate_return_code(self, test: Path, expect: int, real: int) -> bool:
        if expect != real:
            self._fail(test, f"Expected exit code '{expect}', got '{real}'")
            return False

        return True

    def _fail(self, test: Path, message: str):
        if test not in self.failures:
            self.failures[test] = [message]
        else:
            self.failures[test].append(message)

    def _parse_test(self, test: Path) -> Expect | None:
        exit_code: int = 0
        expectations: int = 0

        output: List[Tuple[int, str]] = []
        compile_error: List[Tuple[int, str]] = []
        runtime_error: Tuple[int, str] | None = None

        to_var = lambda lang: Variant.TREE_WALK if lang == "java" else Variant.BYTECODE

        with open(test, "r") as file:
            for i, line in enumerate(file, 1):
                if match := OUTPUT_EXPECT.search(line):
                    output.append((i, match.group(1)))
                    expectations += 1
                if match := ERROR_EXPECT.search(line):
                    compile_error.append((i, match.group(1)))
                    exit_code = 65
                    expectations += 1
                if match := ERROR_LINE_EXPECT.search(line):
                    # The two interpreters are slightly different in terms of which
                    # cascaded errors may appear after an initial compile error because
                    # their panic mode recovery is a little different. To handle that,
                    # the tests can indicate if an error line should only appear for a
                    # certain interpreter.
                    lang = match.group(2)
                    if not lang or to_var(lang) == self.interpreter.value.variant:
                        compile_error.append((int(match.group(3)), match.group(4)))
                        exit_code = 65
                        expectations += 1
                if match := RUNTIME_ERROR_EXPECT.search(line):
                    runtime_error = (i, match.group(1))
                    exit_code = 70
                    expectations += 1
                if match := NONTEST_RE.search(line):
                    return None

        return Test.Expect(
            expectations, exit_code, output, compile_error, runtime_error
        )

    def _suite_from_chapter(self, chapter: Chapter) -> TestSuite | None:
        tests = TESTS[self.interpreter.value.variant]
        for suite in tests:
            if suite.chapter == chapter:
                return suite
        return None


def main() -> int:
    interpreters = [e.value.name for e in Interpreter] + ["all"]

    parser = ArgumentParser(formatter_class=RawTextHelpFormatter)

    parser.add_argument(
        "interpreter",
        help=f"Choose which version of the interpreter to run:\n- "
        + "\n- ".join(interpreters),
        choices=interpreters,
        metavar="interpreter",
        default=None,
    )
    parser.add_argument(
        "-c",
        help="Run tests for a specific chapter:\n- "
        + "\n- ".join([c.value.name for c in Chapter]),
        nargs="?",
        metavar="chapter",
        default=None,
        choices=[c.name.lower() for c in Chapter],
    )

    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "-f",
        help="Filter test files by unix shell globbing pattern",
        nargs="?",
        metavar="glob",
        default=None,
    )
    group.add_argument(
        "-e",
        help="Exclude test files by unix shell globbing pattern",
        nargs="?",
        metavar="glob",
        default=None,
    )

    if len(sys.argv) == 1:
        parser.print_help(sys.stderr)
        return 1

    args = parser.parse_args()
    chapter = args.c

    filter = None
    match (args.f, args.e):
        case (None, None):
            pass
        case (f, None):
            filter = Filter(re.compile(fnmatch.translate(f)), False)
        case (None, e):
            filter = Filter(re.compile(fnmatch.translate(e)), True)

    # prepare the interpreters
    if not prepare_interpreters():
        return 2

    # populate the test cases, this is required
    populate_tests()

    def run(test: Test):
        if chapter is not None:
            return test.run_chapter(Chapter[chapter.upper()])
        else:
            return test.run_all()

    result = Result()
    if args.interpreter == "all":
        for interpreter in Interpreter:
            test = Test(interpreter, filter)
            result += run(test)
    else:
        interpreter = Interpreter[args.interpreter.upper()]
        test = Test(interpreter, filter)
        result = run(test)

    print("\n>>> Summary")
    print(result)

    return 1 if result.failed > 0 else 0


# -----------------------------------------------------------------------------
# test details
# -----------------------------------------------------------------------------


def prepare_interpreters() -> bool:
    """
    You can add anything here that is required to prepare the interpreters like building them.
    The function will be called before `populate_tests()` function.
    """

    print(Cs.y("Preparing interpreters..."), end=" ", flush=True)

    for interpreter in Interpreter:
        name = interpreter.value.name.lower()
        try:
            run(
                "cargo --color always build --release --bin".split() + [name],
                cwd=DIR,
                check=True,
                capture_output=True,
                text=True,
            )
            run(
                "cargo --color always test --release --package".split() + [name],
                cwd=DIR,
                check=True,
                capture_output=True,
                text=True,
            )
        except CalledProcessError as exc:
            print(Cs.r(f"FAILED [{exc.returncode}]"))
            pprint(exc.stdout, exc.stderr)

            return False

    print(Cs.g("DONE"))

    return True


def populate_tests():
    """
    Populate the tests for both interpreter variants.
    """

    tree_walk = [
        Chapter.SCANNING,
        Chapter.PARSING,
        Chapter.EVALUATING,
        Chapter.STATEMENTS,
        Chapter.CONTROL,
        Chapter.FUNCTIONS,
        Chapter.RESOLVING,
        Chapter.CLASSES_I,
        Chapter.INHERITANCE,
    ]

    bytecode = [
        Chapter.COMPILING,
        Chapter.TYPE,
        Chapter.STRINGS,
        Chapter.HASH,
        Chapter.GLOBAL,
        Chapter.LOCAL,
        Chapter.JUMPING,
        Chapter.CALLS,
        Chapter.CLOSURES,
        Chapter.GARBAGE,
        Chapter.CLASSES_II,
        Chapter.METHODS,
        Chapter.SUPERCLASSES,
        Chapter.OPTIMIZATION,
    ]

    for chapter in tree_walk:
        tests = TESTS[Variant.TREE_WALK]
        add_pass = lambda x: tests.append(TestSuite.from_passes(chapter, x))
        add_skip = lambda x: tests.append(TestSuite.from_skips(chapter, x))
        match chapter:
            case Chapter.SCANNING:
                add_pass(["test/scanning"])  # No interpreter yet.
            case Chapter.PARSING:
                add_pass(["test/expressions/parse.lox"])  # No real interpreter yet.
            case Chapter.EVALUATING:
                add_pass(["test/expressions/evaluate.lox"])  # No real interpreter yet.
            case Chapter.STATEMENTS:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox",
                        # No control flow.
                        "test/block/empty.lox",
                        "test/for",
                        "test/if",
                        "test/logical_operator",
                        "test/while",
                        "test/variable/unreached_undefined.lox",
                        # No functions.
                        "test/call",
                        "test/closure",
                        "test/function",
                        "test/operator/not.lox",
                        "test/regression/40.lox",
                        "test/return",
                        "test/unexpected_character.lox",
                        # Broken because we haven't fixed it yet by detecting the error.
                        "test/return/at_top_level.lox",
                        "test/variable/use_local_in_initializer.lox",
                        # No resolution.
                        "test/closure/assign_to_shadowed_later.lox",
                        "test/function/local_mutual_recursion.lox",
                        "test/variable/collide_with_parameter.lox",
                        "test/variable/duplicate_local.lox",
                        "test/variable/duplicate_parameter.lox",
                        "test/variable/early_bound.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/return/in_method.lox",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.CONTROL:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox",
                        # No functions.
                        "test/call",
                        "test/closure",
                        "test/for/closure_in_body.lox",
                        "test/for/return_closure.lox",
                        "test/for/return_inside.lox",
                        "test/for/syntax.lox",
                        "test/function",
                        "test/operator/not.lox",
                        "test/regression/40.lox",
                        "test/return",
                        "test/unexpected_character.lox",
                        "test/while/closure_in_body.lox",
                        "test/while/return_closure.lox",
                        "test/while/return_inside.lox",
                        # Broken because we haven't fixed it yet by detecting the error.
                        "test/return/at_top_level.lox",
                        "test/variable/use_local_in_initializer.lox",
                        # No resolution.
                        "test/closure/assign_to_shadowed_later.lox",
                        "test/function/local_mutual_recursion.lox",
                        "test/variable/collide_with_parameter.lox",
                        "test/variable/duplicate_local.lox",
                        "test/variable/duplicate_parameter.lox",
                        "test/variable/early_bound.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/return/in_method.lox",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.FUNCTIONS:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox",
                        # Broken because we haven't fixed it yet by detecting the error.
                        "test/return/at_top_level.lox",
                        "test/variable/use_local_in_initializer.lox",
                        # No resolution.
                        "test/closure/assign_to_shadowed_later.lox",
                        "test/function/local_mutual_recursion.lox",
                        "test/variable/collide_with_parameter.lox",
                        "test/variable/duplicate_local.lox",
                        "test/variable/duplicate_parameter.lox",
                        "test/variable/early_bound.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/return/in_method.lox",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.RESOLVING:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/return/in_method.lox",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.CLASSES_I:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox",
                        # No inheritance.
                        "test/class/local_inherit_self.lox",
                        "test/class/inherit_self.lox",
                        "test/class/inherited_method.lox",
                        "test/inheritance",
                        "test/super",
                    ]
                )
            case Chapter.INHERITANCE:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox",
                    ]
                )

    for chapter in bytecode:
        tests = TESTS[Variant.BYTECODE]
        add_skip = lambda x: tests.append(TestSuite.from_skips(chapter, x))
        add_pass = lambda x: tests.append(TestSuite.from_passes(chapter, x))
        match chapter:
            case Chapter.COMPILING:
                add_pass(["test/expressions/evaluate.lox"])  # No real interpreter yet.
            case Chapter.TYPE:
                add_pass(["test/expressions/evaluate.lox"])  # No real interpreter yet.
            case Chapter.STRINGS:
                add_pass(["test/expressions/evaluate.lox"])  # No real interpreter yet.
            case Chapter.HASH:
                add_pass(["test/expressions/evaluate.lox"])  # No real interpreter yet.
            case Chapter.GLOBAL:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No control flow.
                        "test/block/empty.lox",
                        "test/for",
                        "test/if",
                        "test/limit/loop_too_large.lox",
                        "test/logical_operator",
                        "test/variable/unreached_undefined.lox",
                        "test/while",
                        # No blocks.
                        "test/assignment/local.lox",
                        "test/variable/in_middle_of_block.lox",
                        "test/variable/in_nested_block.lox",
                        "test/variable/scope_reuse_in_different_blocks.lox",
                        "test/variable/shadow_and_local.lox",
                        "test/variable/undefined_local.lox",
                        # No local variables.
                        "test/block/scope.lox",
                        "test/variable/duplicate_local.lox",
                        "test/variable/shadow_global.lox",
                        "test/variable/shadow_local.lox",
                        "test/variable/use_local_in_initializer.lox",
                        # No functions.
                        "test/call",
                        "test/closure",
                        "test/function",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/stack_overflow.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        "test/regression/40.lox",
                        "test/return",
                        "test/unexpected_character.lox",
                        "test/variable/collide_with_parameter.lox",
                        "test/variable/duplicate_parameter.lox",
                        "test/variable/early_bound.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/class",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.LOCAL:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No control flow.
                        "test/block/empty.lox",
                        "test/for",
                        "test/if",
                        "test/limit/loop_too_large.lox",
                        "test/logical_operator",
                        "test/variable/unreached_undefined.lox",
                        "test/while",
                        # No functions.
                        "test/call",
                        "test/closure",
                        "test/function",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/stack_overflow.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        "test/regression/40.lox",
                        "test/return",
                        "test/unexpected_character.lox",
                        "test/variable/collide_with_parameter.lox",
                        "test/variable/duplicate_parameter.lox",
                        "test/variable/early_bound.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/class",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.JUMPING:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No functions.
                        "test/call",
                        "test/closure",
                        "test/for/closure_in_body.lox",
                        "test/for/return_closure.lox",
                        "test/for/return_inside.lox",
                        "test/for/syntax.lox",
                        "test/function",
                        "test/limit/no_reuse_constants.lox",
                        "test/limit/stack_overflow.lox",
                        "test/limit/too_many_constants.lox",
                        "test/limit/too_many_locals.lox",
                        "test/limit/too_many_upvalues.lox",
                        "test/regression/40.lox",
                        "test/return",
                        "test/unexpected_character.lox",
                        "test/variable/collide_with_parameter.lox",
                        "test/variable/duplicate_parameter.lox",
                        "test/variable/early_bound.lox",
                        "test/while/closure_in_body.lox",
                        "test/while/return_closure.lox",
                        "test/while/return_inside.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/class",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not.lox",
                        "test/operator/not_class.lox",
                        "test/super",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.CALLS:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No closures.
                        "test/closure",
                        "test/for/closure_in_body.lox",
                        "test/for/return_closure.lox",
                        "test/function/local_recursion.lox",
                        "test/limit/too_many_upvalues.lox",
                        "test/regression/40.lox",
                        "test/while/closure_in_body.lox",
                        "test/while/return_closure.lox",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not.lox",
                        "test/operator/not_class.lox",
                        "test/return/in_method.lox",
                        "test/super",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.CLOSURES:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not.lox",
                        "test/operator/not_class.lox",
                        "test/return/in_method.lox",
                        "test/super",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.GARBAGE:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No classes.
                        "test/assignment/to_this.lox",
                        "test/call/object.lox",
                        "test/class",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field",
                        "test/inheritance",
                        "test/method",
                        "test/number/decimal_point_at_eof.lox",
                        "test/number/trailing_dot.lox",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/operator/not.lox",
                        "test/operator/not_class.lox",
                        "test/return/in_method.lox",
                        "test/super",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.CLASSES_II:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No inheritance.
                        "test/class/local_inherit_self.lox",
                        "test/class/inherit_self.lox",
                        "test/class/inherited_method.lox",
                        "test/inheritance",
                        "test/super",
                        # No methods.
                        "test/assignment/to_this.lox",
                        "test/class/local_reference_self.lox",
                        "test/class/reference_self.lox",
                        "test/closure/close_over_method_parameter.lox",
                        "test/constructor",
                        "test/field/get_and_set_method.lox",
                        "test/field/method.lox",
                        "test/field/method_binds_this.lox",
                        "test/method",
                        "test/operator/equals_class.lox",
                        "test/operator/equals_method.lox",
                        "test/return/in_method.lox",
                        "test/this",
                        "test/variable/local_from_method.lox",
                    ]
                )
            case Chapter.METHODS:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                        # No inheritance.
                        "test/class/local_inherit_self.lox",
                        "test/class/inherit_self.lox",
                        "test/class/inherited_method.lox",
                        "test/inheritance",
                        "test/super",
                    ]
                )
            case Chapter.SUPERCLASSES:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                    ]
                )
            case Chapter.OPTIMIZATION:
                add_skip(
                    [
                        # These are just for earlier chapters.
                        "test/scanning",
                        "test/expressions",
                    ]
                )


if __name__ == "__main__":
    with chdir(DIR):
        ret = main()
        exit(ret)
