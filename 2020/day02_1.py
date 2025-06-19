# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Entity = tuple[int, int, str, str]


def solution(content: list[str]) -> Any:
    return len([True for line in content if is_valid(parse(line))])


def is_valid(entity: Entity):
    minimum, maximum, char, password = entity
    return minimum <= len([True for ch in password if ch == char]) <= maximum


def parse(line: str) -> Entity:
    match = re.match(r"(\d+)-(\d+) ([a-z]): ([a-z]+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (int(match.group(1)), int(match.group(2)), match.group(3), match.group(4))


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
