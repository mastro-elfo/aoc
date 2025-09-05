# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Coords = tuple[int, int]
type Vector = tuple[Coords, Coords]


def solution(content: list[str]) -> Any:
    straight = [vec for vec in [parse(line) for line in content] if is_straight(vec)]
    max_x, max_y = get_limit(straight)
    return len([coord for coord in each(max_x, max_y) if find_two(coord, straight)])


def find_one(coords: Coords, vecs: list[Vector]):
    for vec in vecs:
        if is_in_line(coords, vec):
            return True
    return False


def find_two(coords: Coords, vecs: list[Vector]):
    for index, vec in enumerate(vecs):
        if is_in_line(coords, vec):
            return find_one(coords, vecs[index + 1 :])
    return False


def is_in_line(coords: Coords, vec: Vector):
    x, y = coords
    ((sx, sy), (ex, ey)) = vec
    if x == sx == ex:
        return sy <= y <= ey or ey <= y <= sy
    if y == sy == ey:
        return sx <= x <= ex or ex <= x <= sx
    return False


def each(mx: int, my: int):
    for x in range(mx + 1):
        for y in range(my + 1):
            yield (x, y)


def get_limit(vecs: list[Vector]) -> Coords:
    x, y = 0, 0
    for (sx, sy), (ex, ey) in vecs:
        x = max(x, sx, ex)
        y = max(y, sy, ey)
    return (x, y)


def is_straight(vec: Vector):
    start, end = vec
    sx, sy = start
    ex, ey = end
    return sx == ex or sy == ey


def parse(line: str) -> Vector:
    match = re.match(r"(\d+),(\d+) -> (\d+),(\d+)", line)
    if match is None:
        raise ValueError(f"Invalid line {line}")
    return ((int(match[1]), int(match[2])), (int(match[3]), int(match[4])))


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
