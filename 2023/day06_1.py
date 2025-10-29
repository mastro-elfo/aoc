# pylint: disable=missing-module-docstring, missing-function-docstring


import math
from typing import Any


def solution(content: str) -> Any:
    return math.prod(winning(pair) for pair in parse(content))


def winning(pair: tuple[int, int]) -> int:
    t, d = pair
    delta = (t**2 - 4 * d) ** 0.5 / 2
    half_t = t / 2
    intersect = half_t + delta
    return (
        math.floor(half_t + delta)
        - math.ceil(half_t - delta)
        + 1
        - (2 if int(intersect) == intersect else 0)
    )


def parse(content: str) -> list[tuple[int, int]]:
    times, dists = content.split("\n")
    return [
        (int(t), int(d))
        for t, d in list(
            zip((x for x in times.split(" ") if x), (x for x in dists.split(" ") if x))
        )[1:]
    ]


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
