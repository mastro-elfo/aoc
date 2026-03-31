# pylint: disable=missing-module-docstring, missing-function-docstring


import math
from typing import Any

type Coords = tuple[int, int, int]
type Distance = tuple[Coords, Coords, float]
type Circuit = list[Coords]


def solution(content: list[str]) -> Any:
    return math.prod(
        sorted(
            [
                len(x)
                for x in connect(
                    [
                        [a, b]
                        for a, b, _ in sorted(
                            to_distances([parse(line) for line in content]),
                            key=get_value,
                        )[:1000]
                    ]
                )
            ],
            reverse=True,
        )[:3]
    )


def connect(circuits: list[Circuit]) -> list[Circuit]:
    index = 0
    connected = circuits.copy()
    while index < len(connected):
        current = connected[index]
        rest = connected[index + 1 :]
        other = [os for os in rest if any(o in current for o in os)]
        if other:
            other = other[0]
            connected[index] = list(set(current + other))
            tail = []
            for idx in range(index + 1, len(connected)):
                if any(x != y for x, y in zip(other, connected[idx])):
                    tail.append(connected[idx])
            connected = connected[: index + 1] + tail
        else:
            # index += 1
            return [current] + connect(rest)
    return connected


def get_value(dist: Distance) -> float:
    _, _, v = dist
    return v


def to_distances(coords: list[Coords]) -> list[Distance]:
    return [
        (x, y, euclide(x, y))
        for i, x in enumerate(coords)
        for j, y in enumerate(coords)
        if j > i
    ]


def euclide(c1: Coords, c2: Coords) -> float:
    x1, y1, z1 = c1
    x2, y2, z2 = c2
    return math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2)


def parse(line: str) -> Coords:
    x, y, z = line.split(",")
    return (int(x), int(y), int(z))


def main() -> None:
    with open("day08.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
