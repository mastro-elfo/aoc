# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Rectangle = tuple[int, int, int, int, int]


def solution(content: list[str]) -> Any:
    rects = [parse(line) for line in content]
    lonelys = [
        rect
        for rect in rects
        if all(not overlap(rect, r2) for r2 in rects if rect != r2)
    ]
    return lonelys[0][0]


def overlap(r1: Rectangle, r2: Rectangle):
    return (
        is_inside(r1, r2)
        or is_inside(r2, r1)
        or is_outside(r1, r2)
        or is_outside(r2, r1)  # pylint: disable=arguments-out-of-order
    )


def is_outside(r1: Rectangle, r2: Rectangle):
    _, x1, y1, _, ey1 = r1
    _, x2, y2, ex2, _ = r2

    return (x2 <= x1 < ex2) and y1 <= y2 < ey1


def is_inside(r1: Rectangle, rect: Rectangle):
    _, x1, y1, ex1, ey1 = r1
    return (
        is_internal(x1, y1, rect)
        or is_internal(x1, ey1, rect)
        or is_internal(ex1, y1, rect)
        or is_internal(ex1, ey1, rect)
    )


def is_internal(x: int, y: int, rect: Rectangle):
    _, rx, ry, ex, ey = rect
    return rx <= x < ex and ry <= y < ey


def parse(line: str) -> Rectangle:
    match = re.match(r"#(\d+)\s*@\s*(\d+),(\d+):\s*(\d+)x(\d+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    x = int(match[2])
    y = int(match[3])
    return (
        int(match[1]),
        x,
        y,
        x + int(match[4]),
        y + int(match[5]),
    )


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
