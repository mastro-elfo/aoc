# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any, Literal

type Instruction = Literal["L", "R"]
type Tree = dict[str, tuple[str, str]]


def solution(content: str) -> Any:
    instructions = parse_instructions(content.split("\n")[0])
    tree = parse_tree(content.split("\n\n")[1])
    current = "AAA"
    count = 0
    for instruction in generate_instructions(instructions):
        if current == "ZZZ":
            return count
        left, right = tree[current]
        current = left if instruction == "L" else right
        count += 1

    return count


def generate_instructions(instructions: list[Instruction]):
    while True:
        for instruction in instructions:
            yield instruction


def parse_tree(block: str) -> Tree:
    tree = {}
    for line in block.split("\n"):
        match = re.match(r"(\w+) = \((\w+), (\w+)\)", line)
        if match is None:
            raise ValueError(f"Invalid line: {line}")
        tree[match[1]] = (match[2], match[3])
    return tree


def parse_instructions(line: str) -> list[Instruction]:
    return ["L" if char == "L" else "R" for char in line]


def main() -> None:
    with open("day08.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
