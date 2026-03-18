# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(to_int(extract(12, parse(line))) for line in content)


def to_int(lst: list[int]) -> int:
    total = 0
    for item in lst:
        total = total * 10 + item
    return total


def extract(n: int, joltage: list[int]) -> list[int]:
    if n == 0:
        return [0]
    if n == 1:
        return [max(joltage)]
    first = max(joltage[: -n + 1])
    return [first, *extract(n - 1, joltage[joltage.index(first) + 1 :])]


def parse(line: str) -> list[int]:
    return [int(x) for x in line.strip()]


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
