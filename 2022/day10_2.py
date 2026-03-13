# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type InstructionType = Literal["addx", "noop"]
type Instruction = tuple[InstructionType, int]


def solution(content: list[str]) -> Any:
    pretty(draw([parse(line) for line in content]))


def draw(program: list[Instruction]) -> list[bool]:
    screen: list[bool] = []
    x = 1
    index = 0

    while len(screen) < 240:
        last = len(screen) % 40
        screen.append(last - 1 <= x <= last + 1)

        instruction, amount = program[index % len(program)]

        if instruction == "addx":
            last = len(screen) % 40
            screen.append(last - 1 <= x <= last + 1)
            x += amount

        index += 1

    return screen


def pretty(screen: list[bool]):
    for index, pixel in enumerate(screen, 1):
        print("#" if pixel else " ", end="")
        if index % 40 == 0:
            print()


def parse(line: str) -> Instruction:
    if line.startswith("noop"):
        return ("noop", 0)
    return ("addx", int(line.split(" ")[-1]))


def main() -> None:
    with open("day10.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
