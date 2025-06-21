# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Colors = tuple[int, int, int]
type Game = tuple[int, list[Colors]]


def solution(content: list[str]) -> Any:
    return sum(
        game[0] for game in [parse(line) for line in content] if is_possible(game)
    )


def is_possible(game: Game):
    return all(is_valid(colors) for colors in game[1])


def is_valid(colors: Colors):
    return colors[0] <= 12 and colors[1] <= 13 and colors[2] <= 14


def parse(line: str) -> Game:
    match = re.match(r"Game (\d+): (.+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (int(match[1]), [parse_cubes(cubes) for cubes in match[2].split(";")])


def parse_cubes(cubes: str):
    red = re.search(r"(\d+) red", cubes)
    green = re.search(r"(\d+) green", cubes)
    blue = re.search(r"(\d+) blue", cubes)
    return (
        int(red[1]) if red is not None else 0,
        int(green[1]) if green is not None else 0,
        int(blue[1]) if blue is not None else 0,
    )


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
