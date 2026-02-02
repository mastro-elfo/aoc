# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Vector = tuple[int, int]
type Robot = tuple[Vector, Vector]


def solution(content: list[str]) -> Any:
    width = 101
    height = 103
    robots = [move((width, height), parse(line), 100) for line in content]
    tl = len([True for (x, y), _ in robots if x < width // 2 and y < height // 2])
    tr = len([True for (x, y), _ in robots if x > width // 2 and y < height // 2])
    bl = len([True for (x, y), _ in robots if x < width // 2 and y > height // 2])
    br = len([True for (x, y), _ in robots if x > width // 2 and y > height // 2])
    return tl * tr * bl * br


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
