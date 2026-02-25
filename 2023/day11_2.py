# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]

EXPANSION = 1_000_000


def solution(content: str) -> Any:
    galaxies = parse_galaxies(content)
    max_col = max(col for _, col in galaxies)
    max_row = max(row for row, _ in galaxies)
    vertical_empty = {
        col for col in range(max_col) if not {r for r, c in galaxies if c == col}
    }
    horizontal_empty = {
        row for row in range(max_row) if not {c for r, c in galaxies if r == row}
    }
    galaxies = [
        (row, col + len({c for c in vertical_empty if c < col}) * (EXPANSION - 1))
        for row, col in galaxies
    ]
    galaxies = [
        (row + len({r for r in horizontal_empty if r < row}) * (EXPANSION - 1), col)
        for row, col in galaxies
    ]
    return sum(
        manhattan(coords1, coords2)
        for index, coords1 in enumerate(galaxies)
        for coords2 in galaxies[index + 1 :]
    )


def manhattan(coords1: Coords, coords2: Coords) -> int:
    row1, col1 = coords1
    row2, col2 = coords2
    return abs(row1 - row2) + abs(col1 - col2)


def parse_galaxies(content: str) -> list[Coords]:
    return [
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "#"
    ]


def main() -> None:
    with open("day11.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
