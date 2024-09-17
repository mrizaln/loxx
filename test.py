#!/usr/bin/env python3

# NOTE: This script is a python3 port of the test.dart script from
#       https://github.com/munificent/craftinginterpreters

from argparse import ArgumentParser, RawTextHelpFormatter
from contextlib import chdir
from dataclasses import dataclass
from enum import Enum
from os.path import realpath, dirname
from pathlib import Path, PurePath
from typing import Dict, List
import re

DIR = Path(dirname(realpath(__file__)))

OUTPUT_EXPECT = re.compile(r"// expect: ?(.*)")
ERROR_EXPECT = re.compile(r"// (Error.*)")
ERROR_LINE_EXPECT = re.compile(r"// \[((java|c) )?line (\d+)\] (Error.*)")
RUNTIME_ERROR_EXPECT = re.compile(r"// expect runtime error: (.+)")
SYNTAX_ERROR_RE = re.compile(r"\[.*line (\d+)\] (Error.+)")
STACK_TRACE_RE = re.compile(r"\[line (\d+)\]")
NONTEST_RE = re.compile(r"// nontest")


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
        self.tests = []

        for test, result in tests.items():
            if result == "skip":
                continue

            if Path(test).is_file():
                self.tests.append(Path(test))
                continue

            for root, _, files in (DIR / test).walk():
                for file in files:
                    self.tests.append(root / file)


# populated by populate_tests() function
TESTS: Dict[Variant, List[TestSuite]] = {
    Variant.TREE_WALK: [],
    Variant.BYTECODE: [],
}


class Test:
    def __init__(self, interpreter: Interpreter, filter_path: PurePath | None):
        self.interpreter = interpreter
        self.failed: int = 0
        self.passed: int = 0
        self.skipped: int = 0
        self.expectations: int = 0
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

    def _run_test(self, test: Path) -> bool:
        print(f"\trunning '{test}'")
        return True

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
