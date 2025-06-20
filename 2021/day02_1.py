# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Position = tuple[int, int]
type Instruction = tuple[str, int]


def solution(content: list[str]) -> Any:
    instructions = [parse(line) for line in content]
    position = (0, 0)
    for instruction in instructions:
        position = move(position, instruction)
    return position[0] * position[1]


def move(position: Position, instruction: Instruction):
    direction, amount = instruction
    forward, depth = position
    if direction == "forward":
        return (forward + amount, depth)
    if direction == "up":
        return (forward, depth - amount)
    if direction == "down":
        return (forward, depth + amount)
    return position


def parse(line: str) -> Instruction:
    return (line.split(" ").pop(0), int(line.split(" ").pop()))


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
