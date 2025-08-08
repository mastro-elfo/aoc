# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Limits = tuple[int, int]
type Pair = tuple[Limits, Limits]


def solution(content: list[str]) -> Any:
    return len([True for line in content if is_contained(parse(line))])


def is_contained(pair: Pair) -> bool:
    first, second = pair
    fs, fe = first
    ss, se = second
    return (ss <= fs <= se and ss <= fe <= se) or (fs <= ss <= fe and fs <= se <= fe)


def parse(line: str) -> Pair:
    match = re.match(r"(\d+)-(\d+),(\d+)-(\d+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (
        (int(match.group(1)), int(match.group(2))),
        (int(match.group(3)), int(match.group(4))),
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
