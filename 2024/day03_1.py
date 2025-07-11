# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any


def solution(content: str) -> Any:
    return sum(int(a) * int(b) for a, b in re.findall(r"mul\((\d+),(\d+)\)", content))


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
