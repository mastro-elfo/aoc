# pylint: disable=missing-module-docstring, missing-function-docstring


from multiprocessing import Pool
from typing import Any

type Coord = tuple[int, int]
type Guard = tuple[int, int, str]


def solution(content: str) -> Any:
    obstacles = parse_obstacles(content)
    start = parse_start_position(content)
    limits = parse_limits(content)
    positions = list(set(to_coord(g) for g in track(obstacles, limits, start)))

    with Pool(4) as p:
        return sum(p.map(process, [(obstacles, limits, start, p) for p in positions]))


def process(data: tuple[list[Coord], Coord, Guard, Coord]):
    obstacles, limits, start, obstacle = data
    print(obstacle)
    return play([*obstacles, obstacle], limits, start)


def play(obstacles: list[Coord], limits: Coord, start: Guard):
    current = start
    positions: list[Guard] = [start]
    while True:
        new_current = move(current)
        if is_outside(limits, new_current):
            return False
        if new_current in positions:
            return True

        if is_obstacle(obstacles, new_current):
            current = rotate(current)
        else:
            current = new_current
            positions.append(current)


def track(obstacles: list[Coord], limits: Coord, start: Guard):
    current = start
    positions: list[Guard] = [start]
    while True:
        new_current = move(current)
        if is_outside(limits, new_current):
            return positions

        if is_obstacle(obstacles, new_current):
            current = rotate(current)
        else:
            current = new_current
            positions.append(current)


def rotate(current: Guard) -> Guard:
    x, y, d = current
    if d == "^":
        return (x, y, ">")
    if d == "v":
        return (x, y, "<")
    if d == "<":
        return (x, y, "^")
    if d == ">":
        return (x, y, "v")
    return (x, y, d)


def move(current: Guard) -> Guard:
    x, y, d = current
    if d == "^":
        return (x, y - 1, d)
    if d == "v":
        return (x, y + 1, d)
    if d == "<":
        return (x - 1, y, d)
    if d == ">":
        return (x + 1, y, d)
    return (x, y, d)


def to_coord(guard: Guard) -> Coord:
    x, y, _ = guard
    return (x, y)


def is_obstacle(obstacles: list[Coord], current: Guard):
    x, y, _ = current
    return (x, y) in obstacles


def is_outside(limits: Coord, current: Guard):
    bottom, right = limits
    x, y, _ = current
    return x < 0 or y < 0 or x >= right or y >= bottom


def parse_limits(content: str) -> Coord:
    return (len(content.split("\n")), len(content.split("\n")[0]))


def parse_obstacles(content: str) -> list[Coord]:
    obstacles: list[Coord] = []
    for row_index, row in enumerate(content.split("\n")):
        for col_index, col in enumerate(row):
            if col == "#":
                obstacles.append((col_index, row_index))
    return obstacles


def parse_start_position(content: str) -> Guard:
    for row_index, row in enumerate(content.split("\n")):
        for col_index, col in enumerate(row):
            if col in "^v<>":
                return (col_index, row_index, col)
    raise ValueError("Start position not found")


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
