# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Instruction = tuple[int, int, int]
type Stacks = list[list[str]]


def solution(content: str) -> Any:
    stacks, instructions = parse(content)
    for instruction in instructions:
        stacks = act(instruction, stacks)
    return tops(stacks)


def tops(stacks: Stacks):
    return "".join(stack[0] for stack in stacks)


def act(instruction: Instruction, stacks: Stacks) -> Stacks:
    qty, frm, to = instruction
    copy = stacks.copy()
    while qty > 0:
        fst, *rest = copy[frm]
        copy[frm] = rest
        copy[to].insert(0, fst)
        qty -= 1
    return copy


def parse(content: str):
    part_1, part_2 = content.split("\n\n")
    return (parse_stacks(part_1), parse_instructions(part_2))


def parse_stacks(content: str) -> Stacks:
    lines = content.split("\n")
    size = (len(lines[0]) + 1) // 4
    stacks: list[str] = ["" for _ in range(size)]
    for line in lines[:-1]:
        for idx in range(size):
            index = idx * 4 + 1
            crate = line[index].strip()
            if crate:
                stacks[idx] += crate
    return [list(stack) for stack in stacks]


def parse_instructions(content: str):
    return [parse_procedure(line) for line in content.split("\n") if line]


def parse_procedure(line: str) -> Instruction:
    match = re.match(r"move (\d+) from (\d+) to (\d+)", line)
    if match is None:
        raise ValueError(f"Invalid line {line}")
    return (int(match[1]), int(match[2]) - 1, int(match[3]) - 1)


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read()))


if __name__ == "__main__":
    main()
