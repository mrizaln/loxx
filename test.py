#!/usr/bin/env python3

# NOTE: This script is a python3 port of the test.dart script from
#       https://github.com/munificent/craftinginterpreters

from argparse import ArgumentParser, RawTextHelpFormatter
from contextlib import chdir
from dataclasses import dataclass
from enum import Enum
from os.path import realpath, dirname
from pathlib import Path, PurePath
import sys
from typing import Dict, List, Tuple
from subprocess import run
import re

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
    LOXI = Exe("loxi", Variant.TREE_WALK, Binary(DIR / "target" / "debug" / "loxi"))
    LOXII = Exe("loxii", Variant.BYTECODE, Binary(DIR / "target" / "debug" / "loxii"))


class Result(Enum):
    PASS = "pass"
    FAIL = "fail"
    SKIP = "skip"


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
    chapter: Chapter
    tests: List[Path]

    def __init__(self, chapter: Chapter, tests: Dict[str, str]):
        self.chapter = chapter

        candidates = []
        skips = []

        def add(test: Path, result: str):
            if result == "pass":
                candidates.append(test)
            else:
                skips.append(test)

        for test, result in tests.items():
            if Path(test).is_file():
                add(Path(test), result)
                continue

            for root, _, files in (DIR / test).walk():
                for file in files:
                    add(root / file, result)

        self.tests = [test for test in candidates if test not in skips]


# populated by populate_tests() function
TESTS: Dict[Variant, List[TestSuite]] = {
    Variant.TREE_WALK: [],
    Variant.BYTECODE: [],
}


class Test:
    @dataclass
    class Expect:
        count: int
        exit_code: int
        output: List[Tuple[int, str]]
        compile_error: List[Tuple[int, str]]
        runtime_error: Tuple[int, str] | None

    def __init__(self, interpreter: Interpreter, filter_path: PurePath | None):
        self.interpreter = interpreter
        self.failed: int = 0
        self.passed: int = 0
        self.skipped: int = 0
        self.expectations: int = 0
        self.failures: Dict[Path, List[str]] = {}
        self.filter_path: PurePath | None = filter_path
        pass

    def run(self) -> Result:
        tests = TESTS[self.interpreter.value.variant]
        print(f"\n>>> Running {self.interpreter.value.name} tests")
        for suite in tests:
            if (
                self.filter_path is not None
                and suite.chapter.name != self.filter_path.name
            ):
                continue

            print(f"\n- Chapter {suite.chapter.name}")
            for test in suite.tests:
                if self.filter_path is not None and test != self.filter_path:
                    continue

                if self._run_test(test):
                    self.passed += 1
                else:
                    self.failed += 1

        return Result.PASS

    def run_chapter(self, chapter: Chapter) -> Result:
        suite = self._suite_from_chapter(chapter)
        if suite is None:
            print(
                f"Could not find suite for chapter {chapter.name}, probably wrong variant"
            )
            return Result.SKIP

        print(f"\n- Chapter {suite.chapter.name}")
        for test in suite.tests:
            if self.filter_path is not None and test != self.filter_path:
                continue

            if self._run_test(test):
                self.passed += 1
            else:
                self.failed += 1

        return Result.PASS

    def _run_test(self, test: Path):
        expect = self._parse_test(test)
        if expect is None:
            print(f"\t- 'test' is not a test file, skipping")
            return

        def args(exe: Exe):
            match exe:
                case Exe(_, _, Binary(bin)):
                    return [str(bin)]
                case Exe(_, _, Command(cmd)):
                    return cmd
                case _:
                    raise TypeError("Not a Binary or Command, did you added new types?")

        exec = args(self.interpreter.value)
        proc = run(exec + [str(test)], capture_output=True, text=True)

        stdout = proc.stdout
        stderr = proc.stderr
        returncode = proc.returncode

        # print(f"expectations: {expect}")
        print(stdout)
        print("-" * 80)
        # print(f"stderr: {stderr}")

        return

        self._validate(test, expect, output, error)

    def _validate(self, test: Path, expect: Expect, output: str, error: str):
        if expect.compile_error and expect.runtime_error:
            self._fail(test, "Test error: can't expect both compile and runtime errors")
            return

        errlines = error.split("\n")

        # Validate that an expected runtime error occurred.
        if expect.runtime_error is not None:
            self._validate_runtime_error(errlines, expect.runtime_error)
        else:
            self._validate_compile_errors(errlines, expect.compile_error)
        pass

    def _validate_runtime_error(self, lines: List[str], error: Tuple[int, str]):
        # Skip any compile errors. This can happen if there is a compile error in
        # a module loaded by the module being tested.
        line = 0
        while SYNTAX_ERROR_RE.search(lines[line]):
            line += 1

        # TODO: implement

        pass

    def _validate_compile_errors(self, lines: List[str], errors: List[Tuple[int, str]]):
        pass

    def _fail(self, test: Path, message: str):
        self.failures[test].append(message)

    def _parse_test(self, test: Path) -> Expect | None:
        exit_code: int = 0
        expectations: int = 0

        output: List[Tuple[int, str]] = []
        compile_error: List[Tuple[int, str]] = []
        runtime_error: Tuple[int, str] | None = None

        to_var = lambda lang: Variant.TREE_WALK if lang == "java" else Variant.BYTECODE

        print(f"\trunning '{test}'")
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
        help=f"Choose which version of the interpreter to run: {interpreters}",
        choices=interpreters,
        metavar="interpreter",
        default=None,
    )
    parser.add_argument("-f", help="Filter the tests", nargs="?")
    parser.add_argument(
        "-c",
        help="Run tests for a specific chapter:\n- "
        + "\n- ".join([c.value.name for c in Chapter]),
        nargs="?",
        metavar="chapter",
        default=None,
        choices=[c.name.lower() for c in Chapter],
    )

    if len(sys.argv) == 1:
        parser.print_help(sys.stderr)
        return 1

    args = parser.parse_args()
    filter = None if args.f is None else Path(args.f)
    chapter = args.c

    # populate the test cases, this is required
    populate_tests()

    def run(test: Test):
        if chapter is not None:
            return test.run_chapter(Chapter[chapter.upper()])
        else:
            return test.run()

    if args.interpreter == "all":
        failed = 0
        for interpreter in Interpreter:
            test = Test(interpreter, filter)
            if not run(test):
                failed += 1
        return 1 if failed > 0 else 0
    else:
        interpreter = Interpreter[args.interpreter.upper()]
        test = Test(interpreter, filter)
        run(test)
        return 1 if test.failed > 0 else 0


# -----------------------------------------------------------------------------
# test details
# -----------------------------------------------------------------------------


def populate_tests():
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
        add = lambda x: TESTS[Variant.TREE_WALK].append(TestSuite(chapter, x))
        match chapter:
            case Chapter.SCANNING:
                add(
                    {
                        # No interpreter yet.
                        "test": "skip",
                        "test/scanning": "pass",
                    }
                )
            case Chapter.PARSING:
                add(
                    {
                        # No real interpreter yet.
                        "test": "skip",
                        "test/expressions/parse.lox": "pass",
                    },
                )
            case Chapter.EVALUATING:
                add(
                    {
                        # No real interpreter yet.
                        "test": "skip",
                        "test/expressions/evaluate.lox": "pass",
                    }
                )
            case Chapter.STATEMENTS:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox": "skip",
                        # No control flow.
                        "test/block/empty.lox": "skip",
                        "test/for": "skip",
                        "test/if": "skip",
                        "test/logical_operator": "skip",
                        "test/while": "skip",
                        "test/variable/unreached_undefined.lox": "skip",
                        # No functions.
                        "test/call": "skip",
                        "test/closure": "skip",
                        "test/function": "skip",
                        "test/operator/not.lox": "skip",
                        "test/regression/40.lox": "skip",
                        "test/return": "skip",
                        "test/unexpected_character.lox": "skip",
                        # Broken because we haven't fixed it yet by detecting the error.
                        "test/return/at_top_level.lox": "skip",
                        "test/variable/use_local_in_initializer.lox": "skip",
                        # No resolution.
                        "test/closure/assign_to_shadowed_later.lox": "skip",
                        "test/function/local_mutual_recursion.lox": "skip",
                        "test/variable/collide_with_parameter.lox": "skip",
                        "test/variable/duplicate_local.lox": "skip",
                        "test/variable/duplicate_parameter.lox": "skip",
                        "test/variable/early_bound.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.CONTROL:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox": "skip",
                        # No functions.
                        "test/call": "skip",
                        "test/closure": "skip",
                        "test/for/closure_in_body.lox": "skip",
                        "test/for/return_closure.lox": "skip",
                        "test/for/return_inside.lox": "skip",
                        "test/for/syntax.lox": "skip",
                        "test/function": "skip",
                        "test/operator/not.lox": "skip",
                        "test/regression/40.lox": "skip",
                        "test/return": "skip",
                        "test/unexpected_character.lox": "skip",
                        "test/while/closure_in_body.lox": "skip",
                        "test/while/return_closure.lox": "skip",
                        "test/while/return_inside.lox": "skip",
                        # Broken because we haven't fixed it yet by detecting the error.
                        "test/return/at_top_level.lox": "skip",
                        "test/variable/use_local_in_initializer.lox": "skip",
                        # No resolution.
                        "test/closure/assign_to_shadowed_later.lox": "skip",
                        "test/function/local_mutual_recursion.lox": "skip",
                        "test/variable/collide_with_parameter.lox": "skip",
                        "test/variable/duplicate_local.lox": "skip",
                        "test/variable/duplicate_parameter.lox": "skip",
                        "test/variable/early_bound.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.FUNCTIONS:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox": "skip",
                        # Broken because we haven't fixed it yet by detecting the error.
                        "test/return/at_top_level.lox": "skip",
                        "test/variable/use_local_in_initializer.lox": "skip",
                        # No resolution.
                        "test/closure/assign_to_shadowed_later.lox": "skip",
                        "test/function/local_mutual_recursion.lox": "skip",
                        "test/variable/collide_with_parameter.lox": "skip",
                        "test/variable/duplicate_local.lox": "skip",
                        "test/variable/duplicate_parameter.lox": "skip",
                        "test/variable/early_bound.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.RESOLVING:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.CLASSES_I:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox": "skip",
                        # No inheritance.
                        "test/class/local_inherit_self.lox": "skip",
                        "test/class/inherit_self.lox": "skip",
                        "test/class/inherited_method.lox": "skip",
                        "test/inheritance": "skip",
                        "test/super": "skip",
                    }
                )
            case Chapter.INHERITANCE:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No hardcoded limits in jlox.
                        "test/limit/loop_too_large.lox": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        # Rely on JVM for stack overflow checking.
                        "test/limit/stack_overflow.lox": "skip",
                    }
                )

    for chapter in bytecode:
        add = lambda x: TESTS[Variant.BYTECODE].append(TestSuite(chapter, x))
        match chapter:
            case Chapter.COMPILING:
                add(
                    {
                        # No real interpreter yet.
                        "test": "skip",
                        "test/expressions/evaluate.lox": "pass",
                    }
                )
            case Chapter.TYPE:
                add(
                    {
                        # No real interpreter yet.
                        "test": "skip",
                        "test/expressions/evaluate.lox": "pass",
                    }
                )
            case Chapter.STRINGS:
                add(
                    {
                        # No real interpreter yet.
                        "test": "skip",
                        "test/expressions/evaluate.lox": "pass",
                    }
                )
            case Chapter.HASH:
                add(
                    {
                        # No real interpreter yet.
                        "test": "skip",
                        "test/expressions/evaluate.lox": "pass",
                    }
                )
            case Chapter.GLOBAL:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No control flow.
                        "test/block/empty.lox": "skip",
                        "test/for": "skip",
                        "test/if": "skip",
                        "test/limit/loop_too_large.lox": "skip",
                        "test/logical_operator": "skip",
                        "test/variable/unreached_undefined.lox": "skip",
                        "test/while": "skip",
                        # No blocks.
                        "test/assignment/local.lox": "skip",
                        "test/variable/in_middle_of_block.lox": "skip",
                        "test/variable/in_nested_block.lox": "skip",
                        "test/variable/scope_reuse_in_different_blocks.lox": "skip",
                        "test/variable/shadow_and_local.lox": "skip",
                        "test/variable/undefined_local.lox": "skip",
                        # No local variables.
                        "test/block/scope.lox": "skip",
                        "test/variable/duplicate_local.lox": "skip",
                        "test/variable/shadow_global.lox": "skip",
                        "test/variable/shadow_local.lox": "skip",
                        "test/variable/use_local_in_initializer.lox": "skip",
                        # No functions.
                        "test/call": "skip",
                        "test/closure": "skip",
                        "test/function": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/stack_overflow.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        "test/regression/40.lox": "skip",
                        "test/return": "skip",
                        "test/unexpected_character.lox": "skip",
                        "test/variable/collide_with_parameter.lox": "skip",
                        "test/variable/duplicate_parameter.lox": "skip",
                        "test/variable/early_bound.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/class": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.LOCAL:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No control flow.
                        "test/block/empty.lox": "skip",
                        "test/for": "skip",
                        "test/if": "skip",
                        "test/limit/loop_too_large.lox": "skip",
                        "test/logical_operator": "skip",
                        "test/variable/unreached_undefined.lox": "skip",
                        "test/while": "skip",
                        # No functions.
                        "test/call": "skip",
                        "test/closure": "skip",
                        "test/function": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/stack_overflow.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        "test/regression/40.lox": "skip",
                        "test/return": "skip",
                        "test/unexpected_character.lox": "skip",
                        "test/variable/collide_with_parameter.lox": "skip",
                        "test/variable/duplicate_parameter.lox": "skip",
                        "test/variable/early_bound.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/class": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.JUMPING:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No functions.
                        "test/call": "skip",
                        "test/closure": "skip",
                        "test/for/closure_in_body.lox": "skip",
                        "test/for/return_closure.lox": "skip",
                        "test/for/return_inside.lox": "skip",
                        "test/for/syntax.lox": "skip",
                        "test/function": "skip",
                        "test/limit/no_reuse_constants.lox": "skip",
                        "test/limit/stack_overflow.lox": "skip",
                        "test/limit/too_many_constants.lox": "skip",
                        "test/limit/too_many_locals.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        "test/regression/40.lox": "skip",
                        "test/return": "skip",
                        "test/unexpected_character.lox": "skip",
                        "test/variable/collide_with_parameter.lox": "skip",
                        "test/variable/duplicate_parameter.lox": "skip",
                        "test/variable/early_bound.lox": "skip",
                        "test/while/closure_in_body.lox": "skip",
                        "test/while/return_closure.lox": "skip",
                        "test/while/return_inside.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/class": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.CALLS:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No closures.
                        "test/closure": "skip",
                        "test/for/closure_in_body.lox": "skip",
                        "test/for/return_closure.lox": "skip",
                        "test/function/local_recursion.lox": "skip",
                        "test/limit/too_many_upvalues.lox": "skip",
                        "test/regression/40.lox": "skip",
                        "test/while/closure_in_body.lox": "skip",
                        "test/while/return_closure.lox": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.CLOSURES:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.GARBAGE:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No classes.
                        "test/assignment/to_this.lox": "skip",
                        "test/call/object.lox": "skip",
                        "test/class": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field": "skip",
                        "test/inheritance": "skip",
                        "test/method": "skip",
                        "test/number/decimal_point_at_eof.lox": "skip",
                        "test/number/trailing_dot.lox": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/operator/not.lox": "skip",
                        "test/operator/not_class.lox": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/super": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.CLASSES_II:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No inheritance.
                        "test/class/local_inherit_self.lox": "skip",
                        "test/class/inherit_self.lox": "skip",
                        "test/class/inherited_method.lox": "skip",
                        "test/inheritance": "skip",
                        "test/super": "skip",
                        # No methods.
                        "test/assignment/to_this.lox": "skip",
                        "test/class/local_reference_self.lox": "skip",
                        "test/class/reference_self.lox": "skip",
                        "test/closure/close_over_method_parameter.lox": "skip",
                        "test/constructor": "skip",
                        "test/field/get_and_set_method.lox": "skip",
                        "test/field/method.lox": "skip",
                        "test/field/method_binds_this.lox": "skip",
                        "test/method": "skip",
                        "test/operator/equals_class.lox": "skip",
                        "test/operator/equals_method.lox": "skip",
                        "test/return/in_method.lox": "skip",
                        "test/this": "skip",
                        "test/variable/local_from_method.lox": "skip",
                    }
                )
            case Chapter.METHODS:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                        # No inheritance.
                        "test/class/local_inherit_self.lox": "skip",
                        "test/class/inherit_self.lox": "skip",
                        "test/class/inherited_method.lox": "skip",
                        "test/inheritance": "skip",
                        "test/super": "skip",
                    }
                )
            case Chapter.SUPERCLASSES:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                    }
                )
            case Chapter.OPTIMIZATION:
                add(
                    {
                        "test": "pass",
                        # These are just for earlier chapters.
                        "test/scanning": "skip",
                        "test/expressions": "skip",
                    }
                )


if __name__ == "__main__":
    with chdir(DIR):
        ret = main()
        exit(ret)
