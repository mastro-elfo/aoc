# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Map = dict[Coords, int]


def solution(content: str) -> Any:
    mp = parse(content)
    return sum([len(find_trails(mp, trailhead)) for trailhead in find_trailheads(mp)])


def find_trails(mp: Map, head: Coords) -> list[Coords]:
    level = [head]
    for current in range(1, 10):
        level = find_trails_for(mp, current, level)
    return level


def find_trails_for(mp: Map, level: int, previous: list[Coords]) -> list[Coords]:
    return [
        coords
        for coords, height in mp.items()
        for other in previous
        if height == level and is_neigbor(other, coords)
    ]


def find_trailheads(mp: Map) -> list[Coords]:
    return [coords for coords, height in mp.items() if height == 0]


def is_neigbor(one: Coords, two: Coords) -> bool:
    x1, y1 = one
    x2, y2 = two
    return (y1 == y2 and abs(x1 - x2) == 1) or (x1 == x2 and abs(y1 - y2) == 1)


def parse(content: str) -> Map:
    return {
        (int(row_index), int(col_index)): int(height)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, height in enumerate(row)
    }


def main() -> None:
    with open("day10.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
