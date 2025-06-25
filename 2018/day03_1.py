# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Rectangle = tuple[int, int, int, int, int]
type Coords = tuple[int, int]
type Fabric = dict[Coords, int]


def solution(content: list[str]) -> Any:
    rects = [parse(line) for line in content]
    fabric: Fabric = {}
    for rect in rects:
        draw(rect, fabric)
    return len([v for v in fabric.values() if v == 0])


def draw(rect: Rectangle, fabric: Fabric):
    for x in range(rect[1], rect[3]):
        for y in range(rect[2], rect[4]):
            draw_at(rect[0], x, y, fabric)


def draw_at(rid: int, x: int, y: int, fabric: Fabric):
    coords = (x, y)
    if fabric.get(coords) is None:
        fabric[coords] = rid
    else:
        fabric[coords] = 0


def parse(line: str) -> Rectangle:
    match = re.match(r"#(\d+)\s*@\s*(\d+),(\d+):\s*(\d+)x(\d+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    x = int(match[2])
    y = int(match[3])
    return (
        int(match[1]),
        int(match[2]),
        int(match[3]),
        x + int(match[4]),
        y + int(match[5]),
    )


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
