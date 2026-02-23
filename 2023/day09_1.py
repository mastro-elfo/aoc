# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Sequence


def solution(content: list[str]) -> Any:
    return sum(
        following(sequence) for sequence in (parse_data(line) for line in content)
    )


def following(sequence: Sequence[int]) -> int:
    if all(value == 0 for value in sequence):
        return 0
    deltas = [a - b for a, b in zip(sequence, sequence[1:])]
    first, *_ = sequence
    return first + following(deltas)


def parse_data(line: str) -> list[int]:
    return [int(x) for x in line.split(" ")][::-1]


def main() -> None:
    with open("day09.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
