# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]


def solution(content: str) -> Any:
    size = parse_size(content)
    rocks = parse_rocks(content)
    start = parse_start(content)
    return len(get_plots_for_even_steps(rocks, size, start, 64))


def get_plots_for_even_steps(
    rocks: set[Coords], size: Coords, start: Coords, steps: int
) -> set[Coords]:
    frontier = {start}
    plots: set[Coords] = set()
    for _ in range(steps // 2):
        frontier = {
            item
            for coords in frontier
            for item in get_neighbors(coords)
            if item not in rocks and is_within(size, item)
        }
        frontier = {
            item
            for coords in frontier
            for item in get_neighbors(coords)
            if item not in rocks and is_within(size, item) and item not in plots
        }
        plots |= frontier
    return plots


def is_within(size: Coords, coords: Coords) -> bool:
    height, width = size
    row, col = coords
    return 0 <= row < height and 0 <= col < width


def get_neighbors(coords: Coords) -> set[Coords]:
    row, col = coords
    return {(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)}


def parse_rocks(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "#"
    }


def parse_size(content: str) -> Coords:
    return (len(content.split("\n")), len(content.split("\n")[0]))


def parse_start(content: str) -> Coords:
    return next(
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "S"
    )


def main() -> None:
    with open("day21.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
