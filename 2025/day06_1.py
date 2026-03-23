# pylint: disable=missing-module-docstring, missing-function-docstring


from functools import reduce
from typing import Any, Literal

type Operation = Literal["+"] | Literal["*"]


def solution(content: list[str]) -> Any:
    numbers = transpose([parse_numbers(line) for line in content[:-1]])
    operations = parse_operations(content[-1])
    return sum(apply(operation, nums) for operation, nums in zip(operations, numbers))


def apply(operation: Operation, lst: list[int]) -> int:
    if operation == "*":
        return reduce(lambda acc, cur: acc * cur, lst, 1)
    if operation == "+":
        return sum(lst)


def transpose[T](rows: list[list[T]]) -> list[list[T]]:
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


def parse_numbers(line: str) -> list[int]:
    return [int(x) for x in line.strip().split(" ") if x]


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
