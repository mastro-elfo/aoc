# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]


def solution(content: list[str]) -> Any:
    pivots = [parse(line) for line in content]
    hlimit = max(col for col, _ in pivots)
    vlimit = max(row for _, row in pivots)
    the_map = [
        sum(manhattan((x, y), pivot) for pivot in pivots)
        for x in range(hlimit + 1)
        for y in range(vlimit + 1)
    ]
    return len([c for c in the_map if c < 10_000])


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
