# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any


def solution(content: list[str]) -> Any:
    return len(
        [
            True
            for line in content
            if is_triangle(*(int(x) for x in re.findall(r"\d+", line)))
        ]
    )


def is_triangle(a: int, b: int, c: int):
    return (a + b > c) and (a + c > b) and (b + c > a)


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
