# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any, Literal

type Instruction = tuple[
    Literal["on"] | Literal["off"] | Literal["toggle"], int, int, int, int
]
type Lights = list[list[bool]]


def solution(content: list[str]) -> Any:
    lights = [[False for _ in range(1000)] for _ in range(1000)]
    instructions = [parse(line) for line in content]
    for instruction in instructions:
        lights = act(instruction, lights)
    return len([True for row in lights for light in row if light])


def act(instruction: Instruction, lights: Lights) -> Lights:
    action, sx, sy, ex, ey = instruction
    copy = lights.copy()
    for x in range(sx, ex + 1):
        for y in range(sy, ey + 1):
            if action == "off":
                copy[x][y] = False
            elif action == "on":
                copy[x][y] = True
            else:
                copy[x][y] = not copy[x][y]
    return copy


def parse(line: str) -> Instruction:
    match = re.match(r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", line)
    if match is None:
        raise ValueError(f"Invalid line {line}")

    return (
        (
            "on"
            if match[1] == "turn on"
            else "off" if match[1] == "turn off" else "toggle"
        ),
        int(match[2]),
        int(match[3]),
        int(match[4]),
        int(match[5]),
    )


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
