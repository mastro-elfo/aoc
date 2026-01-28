# pylint: disable=missing-module-docstring, missing-function-docstring


import functools
from typing import Any


def solution(content: str) -> Any:
    stones = (int(x) for x in content.split(" "))
    return sum(count(75, stone) for stone in stones)


@functools.cache
def count(blnk: int, stone: int) -> int:
    if blnk == 0:
        return 1
    if stone == 0:
        return count(blnk - 1, 1)
    label = str(stone)
    if len(label) % 2 == 0:
        half = len(label) // 2
        return count(blnk - 1, int(label[:half])) + count(blnk - 1, int(label[half:]))
    return count(blnk - 1, stone * 2024)


def main() -> None:
    with open("day11.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
