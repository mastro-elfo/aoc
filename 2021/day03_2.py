# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    parsed = [parse(line) for line in content if line]

    return to_int(filter_most_common(parsed, 0)) * to_int(
        filter_least_common(parsed, 0)
    )


def filter_most_common(content: list[list[int]], at: int):
    if len(content) == 1:
        return content[0]
    count = count_all(content)
    (c0, c1) = count[at]
    return filter_most_common(
        [line for line in content if line[at] == (0 if c0 > c1 else 1)],
        at + 1,
    )


def filter_least_common(content: list[list[int]], at: int):
    if len(content) == 1:
        return content[0]
    count = count_all(content)
    (c0, c1) = count[at]
    return filter_least_common(
        [line for line in content if line[at] == (0 if c0 <= c1 else 1)],
        at + 1,
    )


def to_int(binary: list[int]):
    return sum(bit * 2**index for index, bit in enumerate(reversed(binary)))


def parse(line: str):
    return [int(x) for x in line.strip()]


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


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
