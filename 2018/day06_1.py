# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]


def solution(content: list[str]) -> Any:
    pivot = [parse(line) for line in content]
    hlimit = max(col for col, _ in pivot)
    vlimit = max(row for _, row in pivot)
    the_map = [
        nearest_location((x, y), pivot)
        for x in range(hlimit + 1)
        for y in range(vlimit + 1)
    ]
    borderlines = extract_borderlines(the_map, vlimit)
    no_border_map = [c for c in the_map if c is not None and c not in borderlines]
    return max(
        [len([c for c in no_border_map if c == f]) for f in frozenset(no_border_map)]
    )


def extract_borderlines(coords: list[Coords | None], vlimit: int):
    return frozenset(
        c
        for c in (
            coords[0 : vlimit + 1]
            + coords[-vlimit - 1 :]
            + coords[:: vlimit + 1]
            + coords[vlimit :: vlimit + 1]
        )
        if c is not None
    )


def nearest_location(c: Coords, pivot: list[Coords]):
    distances = [manhattan(c, p) for p in pivot]
    min_distance = min(distances)
    if (len([d for d in distances if d == min_distance])) > 1:
        return None
    return pivot[distances.index(min_distance)]


def manhattan(c1: Coords, c2: Coords):
    x1, y1 = c1
    x2, y2 = c2
    return abs(x1 - x2) + abs(y1 - y2)


def parse(line: str) -> Coords:
    [x, y] = line.split(", ")
    return (int(x), int(y))


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
