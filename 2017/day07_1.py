# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Program = tuple[str, list[str]]


def solution(content: list[str]) -> Any:
    programs = [parse(line) for line in content]
    names = [name for name, _ in programs]
    top_names = set(name for _, progs in programs for name in progs)
    return [name for name in names if name not in top_names][0]


def parse(line: str) -> Program:
    match = re.match(r"(\w+) \(\d+\)( -> ([\w\s,]+))?", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    group3 = match.group(3)
    return (match.group(1), group3.strip().split(", ") if group3 else [])


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
