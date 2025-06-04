# pylint: disable=missing-module-docstring, missing-function-docstring

from typing import Literal

type Direction = Literal["N", "S", "W", "E"]
type Position = tuple[int, int, Direction]
type Turn = Literal["L", "R"]
type Instruction = tuple[Turn, int]


def solution(content: str) -> None:
    instructions: list[Instruction] = [
        parse(instruction) for instruction in content.split(", ")
    ]
    current: Position = (0, 0, "N")
    for instruction in instructions:
        current = move(current, instruction)

    print(abs(current[0]) + abs(current[1]))


def turn(position: Position, instruction: Instruction) -> Position:
    if position[2] == "E":
        return (position[0], position[1], "N" if instruction[0] == "L" else "S")
    if position[2] == "W":
        return (position[0], position[1], "S" if instruction[0] == "L" else "N")
    if position[2] == "N":
        return (position[0], position[1], "W" if instruction[0] == "L" else "E")
    if position[2] == "S":
        return (position[0], position[1], "E" if instruction[0] == "L" else "W")
    raise ValueError()


def walk(position: Position, instruction: Instruction) -> Position:
    if position[2] == "N":
        return (position[0], position[1] + instruction[1], position[2])
    if position[2] == "S":
        return (position[0], position[1] - instruction[1], position[2])
    if position[2] == "E":
        return (position[0] + instruction[1], position[1], position[2])
    if position[2] == "W":
        return (position[0] - instruction[1], position[1], position[2])
    raise ValueError()


def move(position: Position, instruction: Instruction) -> Position:
    return walk(turn(position, instruction), instruction)


def parse(element: str) -> Instruction:
    return ("L" if element[0] == "L" else "R", int(element[1:]))


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        solution(file.read())


if __name__ == "__main__":
    main()
