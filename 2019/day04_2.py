# pylint: disable=missing-module-docstring, missing-function-docstring


from itertools import groupby
from typing import Any


def solution(content: str) -> Any:
    lo, hi = [int(x) for x in content.split("-")]
    return len(
        [
            number
            for number in (str(x) for x in range(lo, hi))
            if all(a <= b for a, b in zip(number, number[1:]))
            and any(len(list(g)) == 2 for _, g in groupby(number))
        ]
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
