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
    corners = 0
    for tile in region:
        ns8 = {n for n in neighbors8(tile) if n in region}
        if is_top_left(ns8, tile):
            corners += 1
        if is_top_right(ns8, tile):
            corners += 1
        if is_bottom_left(ns8, tile):
            corners += 1
        if is_bottom_right(ns8, tile):
            corners += 1
        if inv_top_left(ns8, tile):
            corners += 1
        if inv_top_right(ns8, tile):
            corners += 1
        if inv_bottom_left(ns8, tile):
            corners += 1
        if inv_bottom_right(ns8, tile):
            corners += 1
    return corners


def is_top_left(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (row - 1, col) not in ns and (row, col - 1) not in ns


def is_top_right(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (row - 1, col) not in ns and (row, col + 1) not in ns


def is_bottom_left(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (row + 1, col) not in ns and (row, col - 1) not in ns


def is_bottom_right(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (row + 1, col) not in ns and (row, col + 1) not in ns


def inv_top_left(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (
        (row, col + 1) in ns and (row + 1, col) in ns and (row + 1, col + 1) not in ns
    )


def inv_top_right(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (
        (row, col - 1) in ns and (row + 1, col) in ns and (row + 1, col - 1) not in ns
    )


def inv_bottom_left(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (
        (row, col + 1) in ns and (row - 1, col) in ns and (row - 1, col + 1) not in ns
    )


def inv_bottom_right(ns: set[Coords], tile: Coords) -> bool:
    row, col = tile
    return (
        (row, col - 1) in ns and (row - 1, col) in ns and (row - 1, col - 1) not in ns
    )


def pop_region(field: Field, start: Coords, plant: str) -> set[Coords]:
    region = {start}
    explore = region_neighbors(field, start, plant)
    while explore:
        new_explore = set()
        for tile in explore:
            region.add(tile)
            del field[tile]
            new_explore = new_explore.union(region_neighbors(field, tile, plant))
        explore = new_explore
    return region


def region_neighbors(field: Field, tile: Coords, plant: str) -> set[Coords]:
    return {coords for coords in neighbors(tile) if field.get(coords) == plant}


def neighbors(tile: Coords):
    row, col = tile
    return (
        (row - 1, col),
        (row + 1, col),
        (row, col - 1),
        (row, col + 1),
    )


def neighbors8(tile: Coords):
    row, col = tile
    return (
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1),
    )


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
