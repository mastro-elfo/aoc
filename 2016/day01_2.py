# pylint: disable=missing-module-docstring, missing-function-docstring

from typing import Literal

type Direction = Literal["N", "S", "W", "E"]
type Position = tuple[int, int, Direction]
type Turn = Literal["L", "R"]
type Instruction = tuple[Turn, int]
type Coordinate = tuple[int, int]


def solution(content: str) -> None:
    instructions: list[Instruction] = [
        parse(instruction) for instruction in content.split(", ")
    ]
    current: Position = (0, 0, "N")
    visited: list[Coordinate] = [to_coordinate(current)]
    for instruction in instructions:
        new_visited = move(current, instruction)
        current = new_visited[-1]
        visited_twice = already_visited(visited, new_visited)
        if visited_twice:
            print(abs(visited_twice[0]) + abs(visited_twice[1]))
            break
        visited += [to_coordinate(nw) for nw in new_visited]


def to_coordinate(position: Position) -> Coordinate:
    return (position[0], position[1])


def already_visited(visited: list[Coordinate], new_visited: list[Position]):
    list_visited_twice = [
        v for v in visited if v in [to_coordinate(nw) for nw in new_visited]
    ]
    if list_visited_twice:
        return list_visited_twice[0]
    return None


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


def walk(position: Position, instruction: Instruction) -> list[Position]:
    if position[2] == "N":
        return [
            (position[0], position[1] + x + 1, position[2])
            for x in range(instruction[1])
        ]
    if position[2] == "S":
        return [
            (position[0], position[1] - x - 1, position[2])
            for x in range(instruction[1])
        ]
    if position[2] == "E":
        return [
            (position[0] + x + 1, position[1], position[2])
            for x in range(instruction[1])
        ]
    if position[2] == "W":
        return [
            (position[0] - x - 1, position[1], position[2])
            for x in range(instruction[1])
        ]
    raise ValueError()


def move(position: Position, instruction: Instruction) -> list[Position]:
    return walk(turn(position, instruction), instruction)


def parse(element: str) -> Instruction:
    return ("L" if element[0] == "L" else "R", int(element[1:]))


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        solution(file.read())


if __name__ == "__main__":
    main()
