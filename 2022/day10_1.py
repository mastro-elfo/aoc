# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type InstructionType = Literal["addx", "noop"]
type Instruction = tuple[InstructionType, int]


def solution(content: list[str]) -> Any:
    program = [parse(line) for line in content]
    x = 1
    strength = 0
    cycle = 0
    index = 0
    while cycle <= 220:
        cycle += 1
        if cycle in (20, 60, 100, 140, 180, 220):
            strength += x * cycle

        instruction, amount = program[index % len(program)]

        if instruction == "addx":
            cycle += 1
            if cycle in (20, 60, 100, 140, 180, 220):
                strength += x * cycle
            x += amount

        index += 1

    return strength


def parse(line: str) -> Instruction:
    if line.startswith("noop"):
        return ("noop", 0)
    return ("addx", int(line.split(" ")[-1]))


def main() -> None:
    with open("day10.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
