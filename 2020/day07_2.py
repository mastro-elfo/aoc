# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Rule = tuple[str, list[tuple[str, int]]]


def solution(content: list[str]) -> Any:
    rules = [parse(line) for line in content]
    return count(rules, "shiny gold")


table: dict[str, int] = {}


def count(rules: list[Rule], name: str):
    if name in table:
        return table[name]
    contains = next(contains for rule, contains in rules if rule == name)
    value = sum(num * (1 + count(rules, sub)) for sub, num in contains)
    table[name] = value
    return value


def parse(line: str) -> Rule:
    match = re.match(r"(.+) bags contain (.+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    name, contains = match.groups()
    return (
        name,
        parse_contains(contains),
    )


def parse_contains(line: str) -> list[tuple[str, int]]:
    if "no other bags" in line:
        return []
    return [
        parse_part(
            part.replace(".", "").replace(" bags", "").replace(" bag", "").strip()
        )
        for part in line.split(",")
    ]


def parse_part(line: str) -> tuple[str, int]:
    match = re.match(r"(\d+) (.+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (match.group(2), int(match.group(1)))


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
