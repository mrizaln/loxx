#!/usr/bin/env python3

import sys
from argparse import ArgumentDefaultsHelpFormatter, ArgumentParser, Namespace
from contextlib import chdir
from dataclasses import dataclass
from enum import Enum
from os import system
from os.path import dirname, realpath
from pathlib import Path
from subprocess import CalledProcessError, run
from textwrap import indent
from typing import Generator, List

DIR = Path(dirname(realpath(__file__)))


@dataclass
class Command:
    args: List[str]


class Interpreter(Enum):
    LOXI = Command(["./target/release/loxi"])
    # LOXII = Command(["./target/release/loxii"])


@dataclass
class Result:
    interpreter: Interpreter
    file: Path
    time: float

    def __add__(self, other: "Result") -> "Result":
        if self.interpreter != other.interpreter:
            raise ValueError("Cannot add results from different interpreters")
        return Result(self.interpreter, self.file, self.time + other.time)

    def __truediv__(self, divisor: float) -> "Result":
        if divisor == 0:
            raise ValueError("Cannot divide by zero")
        return Result(self.interpreter, self.file, self.time / divisor)

    def __str__(self) -> str:
        name = Cs.y(f"{self.interpreter.name.lower():>10}")
        time = Cs.g(f"{self.time:>10.6f}s")
        file = self.file.name
        return f"{name}: [{time}] {file}"


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


def benchmark_file(interpreter: Interpreter, file: Path) -> Result | None:
    printfl(f"\t{Cs.y('> Running')} {interpreter.name.lower()} {file}")
    args = interpreter.value.args + [file]

    try:
        proc = run(args, capture_output=True, text=True, check=True)
    except KeyboardInterrupt:
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
    return Result(interpreter, file, time)


def run_benchmark(interpreter: Interpreter, repeat: int) -> List[Result]:
    printfl(f"\n{Cs.y('>>> Benchmarking')} {interpreter.name.lower()}")
    files = benchmarks_file_generator()
    results = []
    for file in files:
        result_acc = Result(interpreter, file, 0)
        for _ in range(repeat):
            match benchmark_file(interpreter, file):
                case None:
                    return results
                case result:
                    result_acc += result
        results.append(result_acc / repeat)

    return results


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

    if len(sys.argv) == 1:
        parser.print_help(sys.stderr)
        return 1

    args = parser.parse_args()

    clear_screen()
    print_args(args)
    prepare_interpreters()

    def report(results: List[Result]):
        for result in results:
            printfl(result)

    if args.interpreter == "all":
        resultss: List[List[Result]] = []
        for interpreter in Interpreter:
            results = run_benchmark(interpreter, args.repeat)
            resultss.append(results)
        printfl("\n>>> Results")
        for results in resultss:
            report(results)
            printfl()
    else:
        results = run_benchmark(Interpreter[args.interpreter.upper()], args.repeat)
        printfl("\n>>> Results")
        report(results)

    return 0


if __name__ == "__main__":
    with chdir(DIR):
        ret = main()
        exit(ret)
