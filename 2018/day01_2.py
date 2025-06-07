# pylint: disable=missing-module-docstring, missing-function-docstring

from typing import Sequence


def solution(content: Sequence[str]) -> int:
    deltas = [int(f) for f in content]
    current = 0
    frequencies = []
    for delta in repeat(deltas):
        if current in frequencies:
            return current
        frequencies.append(current)
        current += delta
    return 0


def repeat(deltas: list[int]):
    index = 0
    while True:
        yield deltas[index % len(deltas)]
        index += 1


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
