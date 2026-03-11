# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Forest = dict[Coords, int]


def solution(content: str) -> Any:
    forest = parse(content)
    size = parse_size(content)
    return len(visible_each_row(forest, size) | visible_each_col(forest, size))


def visible_each_row(forest: Forest, size: Coords) -> set[Coords]:
    rows, _ = size
    output: set[Coords] = set()
    for row_index in range(rows):
        output |= visible_by_row(forest, size, row_index)
    return output


def visible_each_col(forest: Forest, size: Coords) -> set[Coords]:
    _, cols = size
    output: set[Coords] = set()
    for col_index in range(cols):
        output |= visible_by_col(forest, size, col_index)
    return output


def visible_by_row(forest: Forest, size: Coords, row_index: int) -> set[Coords]:
    _, cols = size
    output: set[Coords] = set()

    output.add((row_index, 0))
    height = forest[(row_index, 0)]
    for col_index in range(1, cols):
        if forest[(row_index, col_index)] > height:
            output.add((row_index, col_index))
            height = forest[(row_index, col_index)]

    output.add((row_index, cols - 1))
    height = forest[(row_index, cols - 1)]
    for col_index in range(cols - 2, 0, -1):
        if forest[(row_index, col_index)] > height:
            output.add((row_index, col_index))
            height = forest[(row_index, col_index)]

    return output


def visible_by_col(forest: Forest, size: Coords, col_index: int) -> set[Coords]:
    rows, _ = size
    output: set[Coords] = set()

    output.add((0, col_index))
    height = forest[(0, col_index)]
    for row_index in range(1, rows):
        if forest[(row_index, col_index)] > height:
            output.add((row_index, col_index))
            height = forest[(row_index, col_index)]

    output.add((rows - 1, col_index))
    height = forest[(rows - 1, col_index)]
    for row_index in range(rows - 2, 0, -1):
        if forest[(row_index, col_index)] > height:
            output.add((row_index, col_index))
            height = forest[(row_index, col_index)]

    return output


def parse_size(content: str) -> Coords:
    return (len(content.split("\n")), len(content.split("\n")[0]))


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
