# pylint: disable=missing-module-docstring, missing-function-docstring


from functools import reduce
from typing import Any, Literal, Sequence

type Operation = Literal["+"] | Literal["*"]


def solution(content: list[str]) -> Any:
    numbers = parse_numbers(content[:-1])
    operations = parse_operations(content[-1])
    return sum(apply(operation, nums) for operation, nums in zip(operations, numbers))


def apply(operation: Operation, lst: list[int]) -> int:
    if operation == "*":
        return reduce(lambda acc, cur: acc * cur, lst, 1)
    if operation == "+":
        return sum(lst)


def transpose[T](rows: Sequence[Sequence[T]]) -> list[list[T]]:
    height = len(rows)
    width = len(rows[0])
    return [[rows[i][j] for i in range(height)] for j in range(width)]


def parse_operations(line: str) -> list[Operation]:
    return [parse_operation(item) for item in line.strip().split(" ") if item]


def parse_operation(item: str) -> Operation:
    if item == "+":
        return "+"
    if item == "*":
        return "*"
    raise ValueError(f"Invalid operation: {item}")


def parse_numbers(content: list[str]) -> list[list[int]]:
    return [
        [parse_number(item) for item in group]
        for group in reduce(
            lambda acc, cur: (append_or_extend(acc, cur, all(c == " " for c in cur))),
            transpose([line[:-1] for line in content]),
            [],
        )
    ]


def append_or_extend[T](
    acc: list[list[T]], cur: T, should_extend: bool
) -> list[list[T]]:
    if should_extend:
        return [*acc, []]
    return [*acc[:-1], [*(acc[-1] if acc else []), cur]]


def parse_number(column: list[str]):
    return reduce(
        lambda acc, cur: 10 * acc + int(cur), (x for x in column if x != " "), 0
    )


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
