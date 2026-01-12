# pylint: disable=missing-module-docstring, missing-function-docstring


from math import prod
from typing import Any

type Coord = tuple[int, int]
type Component = tuple[int, list[Coord]]


def solution(content: str) -> Any:
    components: list[Component] = parse_components(content)
    stars: list[Coord] = parse_stars(content)
    return sum(
        prod(v for v, c in components if g in surroundings(c))
        for g in [
            star
            for star in stars
            if len([True for _, coords in components if star in surroundings(coords)])
            == 2
        ]
    )


def surroundings(coords: list[Coord]):
    return {
        c
        for x, y in coords
        for c in [
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x - 1, y),
            (x + 1, y),
            (x - 1, y + 1),
            (x, y + 1),
            (x + 1, y + 1),
        ]
    }


def parse_stars(content: str):
    return [
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, char in enumerate(row)
        if char == "*"
    ]


def parse_components(content: str):
    component: None | Component = None
    output = []
    for row_index, row in enumerate(content.split("\n")):
        for col_index, char in enumerate(row):
            if char in "1234567890":
                if component is None:
                    component = (int(char), [(row_index, col_index)])
                else:
                    value, coords = component
                    component = (
                        value * 10 + int(char),
                        [*coords, (row_index, col_index)],
                    )
            elif component:
                output.append(component)
                component = None
        if component:
            output.append(component)
            component = None
    return output


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
