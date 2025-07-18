#!/usr/bin/env python3

import sys
from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser, Namespace
from contextlib import chdir
from dataclasses import dataclass
from enum import Enum
from os import system
from os.path import dirname, realpath
from pathlib import Path, PurePath
from subprocess import CalledProcessError, run
from textwrap import indent
from typing import Any, Generator, List

from tabulate import tabulate

DIR = Path(dirname(realpath(__file__)))


@dataclass
class Command:
    args: List[str]


class Interpreter(Enum):
    LOXI = Command(["./target/release/loxi"])
    # LOXII = Command(["./target/release/loxii"])
    JLOX = Command(["./reference/crafting-interpreters-code/jlox"])
    CLOX = Command(["./reference/crafting-interpreters-code/clox"])
    RLOX = Command(["./reference/rlox-interpreter/target/release/rlox-interpreter"])


class Result:
    def __init__(self):
        self.__inner: dict[PurePath, float] = dict()

    def get(self, key: PurePath) -> float | None:
        return self.__inner.get(key)

    def set(self, key: PurePath, value: float):
        self.__inner[key] = value


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


# print wrapper function that always flushes the output
def printfl(*args, **kwargs):
    print(*args, **kwargs, flush=True)


# pretty print stdout and stderr
def pprint(stdout: str, stderr: str):
    def strip(s: str):
        s = s.rstrip()
        return " " if len(s) == 0 else s

    width = 100
    printfl("┌─── [stdout]", "─" * (width - 13), sep="")
    printfl(indent(strip(stdout), "│", lambda _: True))
    printfl("├─── [stderr]", "─" * (width - 13), sep="")
    printfl(indent(strip(stderr), "│", lambda _: True))
    printfl("└", "─" * (width - 1), sep="")


def benchmarks_file_generator() -> Generator[Path, None, None]:
    dir = DIR / "benchmark"
    for root, _, files in dir.walk():
        for file in files:
            yield root / file


def benchmark_file(interpreter: Interpreter, file: PurePath) -> float | None:
    printfl(f"\t{Cs.y('> Running')} {interpreter.name.lower()} {file}")
    args = interpreter.value.args + [file]

    try:
        proc = run(args, capture_output=True, text=True, check=True)
    except KeyboardInterrupt:
        return None
    except CalledProcessError as e:
        printfl(f"non zero exit code: {e.returncode}")
        pprint(e.stdout, e.stderr)
        return None

    stdout = proc.stdout
    stderr = proc.stderr

    if stderr:
        printfl(f"\n{Cs.r('Error:')} {file}")
        pprint(stdout, stderr)
        return None

    lines = stdout.splitlines()
    index = 0
    for i, line in enumerate(lines):
        if line.startswith("elapsed:"):
            index = i
            break

    time = float(lines[index + 1])
    return time


def run_benchmark(interpreter: Interpreter, repeat: int) -> Result:
    if repeat <= 0:
        return Result()

    printfl(f"\n{Cs.y('>>> Benchmarking')} {interpreter.name.lower()}")
    files = benchmarks_file_generator()

    result = Result()
    for file in files:
        time_acc = 0.0
        for _ in range(repeat):
            match benchmark_file(interpreter, file):
                case None:
                    return result
                case time:
                    time_acc += time
        result.set(file, time_acc / repeat)
    return result


def clear_screen():
    match sys.platform:
        case "win32":
            system("cls")
        case "linux" | "darwin":
            system("clear")


def print_args(args: Namespace):
    printfl(">>> Arguments")
    for key, value in vars(args).items():
        printfl(f"{key:<11}: {value}")


def prepare_interpreters() -> bool:
    printfl(Cs.y("\n>>> Preparing interpreters..."), end=" ")

    for name in ["loxi", "loxii"]:
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
            printfl(Cs.r(f"FAILED [{exc.returncode}]"))
            pprint(exc.stdout, exc.stderr)

            return False

    printfl(Cs.g("DONE"))

    return True


def report(results: dict[Interpreter, Result]):
    interpreters = results.keys()
    headers = [""]
    headers.extend((interp.name.lower() for interp in interpreters))

    table = []
    for file in benchmarks_file_generator():
        row: List[Any] = [file.name]
        for interp in interpreters:
            result = results[interp]
            row.append(result.get(file) or "-")
        table.append(row)

    printfl("\n>>> Results")
    printfl(tabulate(table, headers=headers, tablefmt="github"))


def main() -> int:
    parser = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument(
        "interpreter",
        type=str,
        choices=[i.name.lower() for i in Interpreter] + ["all"],
        help="The interpreter to benchmark",
    )

    parser.add_argument(
        "repeat",
        type=int,
        help="The number of times to repeat the benchmark",
        default=1,
        nargs="?",
    )

    parser.add_argument(
        "--warm-up",
        help="Do warm-up before benchmarking",
        action="store_true",
    )

    if len(sys.argv) == 1:
        parser.print_help(sys.stderr)
        return 1

    args = parser.parse_args()

    # clear_screen()
    print_args(args)
    prepare_interpreters()

    # warm-up
    if args.warm_up:
        printfl(f"\n{Cs.y('>-- Warm-up start')} ")
        _ = run_benchmark(Interpreter.JLOX, 1)
        printfl(f"\n{Cs.y('>-- Warm-up end')} ")

    def interpreters():
        if args.interpreter == "all":
            for interpreter in Interpreter:
                yield interpreter
        else:
            yield Interpreter[args.interpreter.upper()]

    results: dict[Interpreter, Result] = {}
    for interpreter in interpreters():
        result = run_benchmark(interpreter, args.repeat)
        results[interpreter] = result

    report(results)

    return 0


if __name__ == "__main__":
    with chdir(DIR):
        ret = main()
        exit(ret)
