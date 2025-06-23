# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any


def solution(content: list[str]) -> Any:
    lines = [int(x) for line in content for x in re.findall(r"\d+", line)]
    fst = lines[::3]
    snd = lines[1::3]
    trd = lines[2::3]
    return search(fst) + search(snd) + search(trd)


def search(lst: list[int]):
    return len(
        [
            True
            for a, b, c in zip(lst[::3], lst[1::3], lst[2::3])
            if is_triangle(a, b, c)
        ]
    )


def is_triangle(a: int, b: int, c: int):
    return (a + b > c) and (a + c > b) and (b + c > a)


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
