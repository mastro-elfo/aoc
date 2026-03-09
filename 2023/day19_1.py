# pylint: disable=missing-module-docstring, missing-function-docstring

import re
from typing import Any, Literal, cast

type Cop = Literal[">", "<"]
type Rule = tuple[str, Cop, int, str]
type Workflow = tuple[list[Rule], str]
type Workflows = dict[str, Workflow]
type Rating = tuple[int, int, int, int]


def solution(content: str) -> Any:
    workflow_block, ratings_block, *_ = content.split("\n\n")
    workflows = parse_workflows(workflow_block)
    ratings = [parse_rating(line) for line in ratings_block.split("\n")]
    return sum(
        total_rating(rating) for rating in ratings if is_accepted(workflows, rating)
    )


def total_rating(rating: Rating) -> int:
    x, m, a, s = rating
    return x + m + a + s


def get_argument(param: str, rating: Rating) -> int:
    x, m, a, s = rating
    if param == "x":
        return x
    if param == "m":
        return m
    if param == "a":
        return a
    if param == "s":
        return s
    raise ValueError(f"Invalid param: {param}")


def compare(cop: Cop, argument: int, value: int) -> bool:
    if cop == "<":
        return argument < value
    return argument > value


def rule_match(rule: Rule, rating: Rating) -> bool:
    param, cop, value, _ = rule
    return compare(cop, get_argument(param, rating), value)


def is_accepted(workflows: Workflows, rating: Rating) -> bool:
    current = "in"
    while True:
        rules, current = workflows[current]
        for rule in rules:
            _, _, _, next_rule = rule
            if rule_match(rule, rating):
                current = next_rule
                break
        if current == "A":
            return True
        if current == "R":
            return False


def parse_rating(line: str) -> Rating:
    match = re.match(r"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}", line)
    if match is None:
        raise ValueError(f"Invalid rating: {line}")
    return (int(match[1]), int(match[2]), int(match[3]), int(match[4]))


def parse_rule(part: str) -> Rule:
    match = re.match(r"(\w+)([><])(\d+):(\w+)", part)
    if match is None:
        raise ValueError(f"Invalid rule: {part}")
    param, cop, value, next_rule = match[1], match[2], match[3], match[4]
    return (param, cast(Cop, cop), int(value), next_rule)


def parse_workflow(line: str) -> tuple[str, Workflow]:
    match = re.match(r"(\w+)\{([\w><:,]+)\}", line)
    if match is None:
        raise ValueError(f"Invalid workflow: {line}")
    workflow, rules = match[1], match[2]
    rules = rules.split(",")
    default_rule = rules[-1]
    rules = [parse_rule(rule) for rule in rules[:-1]]
    return (workflow, (rules, default_rule))


def parse_workflows(block: str) -> Workflows:
    workflows = {}
    for line in block.split("\n"):
        label, workflow = parse_workflow(line)
        workflows[label] = workflow
    return workflows


def main() -> None:
    with open("day19.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
