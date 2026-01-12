# pylint: disable=missing-module-docstring, missing-function-docstring

from typing import Any

type Coord = tuple[int, int]
type Component = tuple[int, list[Coord]]


def solution(content: str) -> Any:
    components: list[Component] = parse_components(content)
    symbols: list[Coord] = parse_symbols(content)
    return sum(
        val
        for val, coords in components
        if [c for c in surroundings(coords) if c in symbols]
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


def parse_symbols(content: str):
    return [
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, char in enumerate(row)
        if char not in ".1234567890"
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
