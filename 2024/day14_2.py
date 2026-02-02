# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Vector = tuple[int, int]
type Robot = tuple[Vector, Vector]


def solution(content: list[str]) -> Any:
    width = 101
    height = 103
    robots = [parse(line) for line in content]
    step = 0
    while True:
        step += 1
        robots = [move((width, height), robot, 1) for robot in robots]
        row_density = [
            True
            for row in range(height)
            if len([y for (_, y), _ in robots if y == row]) > 30
        ]
        col_density = [
            True
            for col in range(width)
            if len([x for (x, _), _ in robots if x == col]) > 30
        ]
        if len(row_density) > 1 and len(col_density) > 1:
            break
    return step


def move(room: Vector, robot: Robot, time: int) -> Robot:
    width, height = room
    (px, py), (vx, vy) = robot
    return (((px + vx * time) % (width), (py + vy * time) % (height)), (vx, vy))


def parse(line: str) -> Robot:
    match = re.match(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)", line)
    if not match:
        raise ValueError(f"Invalid line: {line}")
    return ((int(match[1]), int(match[2])), (int(match[3]), int(match[4])))


def main() -> None:
    with open("day14.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
