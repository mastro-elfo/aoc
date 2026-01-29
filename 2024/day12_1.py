# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Field = dict[Coords, str]


def solution(content: str) -> Any:
    field = parse(content)
    accumulator = 0
    while field:
        tile, plant = field.popitem()
        region = pop_region(field, tile, plant)
        accumulator += area(region) * perimeter(region)
    return accumulator


def area(region: set[Coords]) -> int:
    return len(region)


def perimeter(region: set[Coords]) -> int:
    return sum(
        4
        - len(
            [
                cs
                for cs in (
                    (row - 1, col),
                    (row + 1, col),
                    (row, col - 1),
                    (row, col + 1),
                )
                if cs in region
            ]
        )
        for row, col in region
    )


def pop_region(field: Field, start: Coords, plant: str) -> set[Coords]:
    region = {start}
    explore = neighbors(field, start, plant)
    while explore:
        new_explore = set()
        for tile in explore:
            region.add(tile)
            del field[tile]
            new_explore = new_explore.union(neighbors(field, tile, plant))
        explore = new_explore
    return region


def neighbors(field: Field, tile: Coords, plant: str) -> set[Coords]:
    row, col = tile
    return {
        coords
        for coords in [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
        if field.get(coords) == plant
    }


def parse(content: str) -> Field:
    return {
        (int(row_index), int(col_index)): col
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
    }


def main() -> None:
    with open("day12.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
