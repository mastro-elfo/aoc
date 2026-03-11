# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Forest = dict[Coords, int]


def solution(content: str) -> Any:
    forest = parse(content)
    return max(get_scenic_score(forest, coords) for coords in forest)


def get_scenic_score(forest: Forest, coords: Coords) -> int:
    row, col = coords
    height = forest[(row, col)]

    up = 0
    current_row = row
    while True:
        current_row -= 1
        if (current_row, col) not in forest:
            break
        up += 1
        if forest[(current_row, col)] >= height:
            break

    down = 0
    current_row = row
    while True:
        current_row += 1
        if (current_row, col) not in forest:
            break
        down += 1
        if forest[(current_row, col)] >= height:
            break

    left = 0
    current_col = col
    while True:
        current_col -= 1
        if (row, current_col) not in forest:
            break
        left += 1
        if forest[(row, current_col)] >= height:
            break

    right = 0
    current_col = col
    while True:
        current_col += 1
        if (row, current_col) not in forest:
            break
        right += 1
        if forest[(row, current_col)] >= height:
            break

    return up * down * left * right


def parse(content: str) -> Forest:
    return {
        (row_index, col_index): int(height)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, height in enumerate(row)
    }


def main() -> None:
    with open("day08.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
