# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from functools import reduce
from typing import Any


def solution(content: str) -> Any:
    _, value = reduce(
        filter_operation,
        re.findall(r"mul\((\d+),(\d+)\)|(do\(\))|(don't())", content),
        (True, 0),
    )
    return value


def filter_operation(acc, cur):
    active, value = acc
    a, b, is_do, is_dont, _ = cur
    if is_do:
        return (True, value)
    if is_dont:
        return (False, value)
    if active:
        return (True, value + int(a) * int(b))
    return (False, value)


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
