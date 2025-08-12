# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Scratchcard = tuple[list[int], list[int]]


def solution(content: list[str]) -> Any:
    return sum(
        2 ** (n - 1) for n in (winning(parse(line)) for line in content) if n > 0
    )


def winning(card: Scratchcard):
    nums, wnums = card
    return len([True for n in nums if n in wnums])


def parse(line: str) -> Scratchcard:
    match = re.match(r"Card\s*\d+:\s*([\d\s]+)\s*\|\s*([\d\s]+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (
        [int(x) for x in re.sub(r"\s+", " ", match.group(1)).strip().split(" ")],
        [int(x) for x in re.sub(r"\s+", " ", match.group(2)).strip().split(" ")],
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
