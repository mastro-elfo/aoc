# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Direction = Literal["^", "v", "<", ">"]
type Coords = tuple[int, int]
type Position = tuple[Coords, Direction]


def solution(content: str) -> Any:
    tlbr_mirrors = parse_tlbr_mirrors(content)
    trbl_mirrors = parse_trbl_mirrors(content)
    vertical_splitters = parse_vertical_splitters(content)
    horizontal_splitters = parse_horizontal_splitters(content)
    height, width = parse_sizes(content)
    positions = propagate(
        tlbr_mirrors,
        trbl_mirrors,
        vertical_splitters,
        horizontal_splitters,
        height,
        width,
        ((0, 0), ">"),
    )
    return len({coords for coords, _ in positions})


def propagate(
    tlbr_mirrors: set[Coords],
    trbl_mirrors: set[Coords],
    vertical_splitters: set[Coords],
    horizontal_splitters: set[Coords],
    height: int,
    width: int,
    start: Position,
) -> set[Position]:
    currents = {start}
    positions: set[Position] = currents.copy()
    while currents:
        currents = move_all(
            tlbr_mirrors,
            trbl_mirrors,
            vertical_splitters,
            horizontal_splitters,
            height,
            width,
            currents,
        )
        currents = currents.difference(positions)
        positions = positions | currents
    return positions


def move_all(
    tlbr_mirrors: set[Coords],
    trbl_mirrors: set[Coords],
    vertical_splitters: set[Coords],
    horizontal_splitters: set[Coords],
    height: int,
    width: int,
    currents: set[Position],
) -> set[Position]:
    output: set[Position] = set()
    for current in currents:
        coords, direction = current
        row, col = coords
        if coords in tlbr_mirrors:
            if direction == "<":
                output.add(((row - 1, col), "^"))
            if direction == ">":
                output.add(((row + 1, col), "v"))
            if direction == "^":
                output.add(((row, col - 1), "<"))
            if direction == "v":
                output.add(((row, col + 1), ">"))
        elif coords in trbl_mirrors:
            if direction == "<":
                output.add(((row + 1, col), "v"))
            if direction == ">":
                output.add(((row - 1, col), "^"))
            if direction == "^":
                output.add(((row, col + 1), ">"))
            if direction == "v":
                output.add(((row, col - 1), "<"))
        elif coords in vertical_splitters:
            if direction in "<>":
                output.add(((row - 1, col), "^"))
                output.add(((row + 1, col), "v"))
            if direction == "^":
                output.add(((row - 1, col), "^"))
            if direction == "v":
                output.add(((row + 1, col), "v"))
        elif coords in horizontal_splitters:
            if direction in "^v":
                output.add(((row, col - 1), "<"))
                output.add(((row, col + 1), ">"))
            if direction == "<":
                output.add(((row, col - 1), "<"))
            if direction == ">":
                output.add(((row, col + 1), ">"))
        else:
            if direction == "^":
                output.add(((row - 1, col), "^"))
            if direction == "v":
                output.add(((row + 1, col), "v"))
            if direction == "<":
                output.add(((row, col - 1), "<"))
            if direction == ">":
                output.add(((row, col + 1), ">"))

    return {
        (coords, direction)
        for coords, direction in output
        if within(height, width, coords)
    }


def within(height: int, width: int, coords: Coords) -> bool:
    row, col = coords
    return 0 <= row < height and 0 <= col < width


def parse_sizes(content: str) -> tuple[int, int]:
    return (len(content.split("\n")), len(content.split("\n")[0]))


def parse_tlbr_mirrors(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "\\"
    }


def parse_trbl_mirrors(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "/"
    }


def parse_vertical_splitters(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "|"
    }


def parse_horizontal_splitters(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "-"
    }


def main() -> None:
    with open("day16.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
