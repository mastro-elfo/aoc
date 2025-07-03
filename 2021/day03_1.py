# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    parsed = [parse(line) for line in content if line]
    acc = count_all(parsed)
    return to_int(most_common_value(acc)) * to_int(least_common_value(acc))


def to_int(binary: list[int]):
    return sum(bit * 2**index for index, bit in enumerate(reversed(binary)))


def most_common_value(count: list[tuple[int, int]]):
    return [0 if c0 > c1 else 1 for c0, c1 in count]


def least_common_value(count: list[tuple[int, int]]):
    return [0 if c0 < c1 else 1 for c0, c1 in count]


def init(length: int):
    return [(0, 0)] * length


def count_all(content: list[list[int]]):
    acc = init(len(content[0]))
    for line in content:
        acc = count_line(acc, line)
    return acc


def count_line(acc: list[tuple[int, int]], line: list[int]):
    return [
        (c0 + (1 if bit == 0 else 0), c1 + (1 if bit == 1 else 0))
        for ((c0, c1), bit) in zip(acc, line)
    ]


def parse(line: str):
    return [int(x) for x in line.strip()]


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
