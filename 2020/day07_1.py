# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Rule = tuple[str, list[str]]


def solution(content: list[str]) -> Any:
    rules = [parse(line) for line in content]
    return len([(rule) for rule in rules if is_valid(rules, rule)])


table: dict[str, bool] = {}


def is_valid(rules: list[Rule], rule: Rule) -> bool:
    name, contains = rule
    if name in table:
        return table.get(name, False)
    result = "shiny gold" in contains or any(
        is_valid(rules, (sub_name, sub_contains))
        for sub_name, sub_contains in rules
        if sub_name in contains
    )
    table[name] = result
    return result


def parse(line: str) -> Rule:
    match = re.match(r"(.+) bags contain (.+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    name, contains = match.groups()
    if "no other bags" in contains:
        return (name, [])
    return (
        name,
        [
            rule.strip()
            for rule in re.sub(
                r"\d",
                "",
                contains.replace(".", "").replace(" bags", "").replace(" bag", ""),
            ).split(",")
        ],
    )


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
