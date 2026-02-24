# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type PipeType = str
type Pipes = dict[Coords, PipeType]


def solution(content: str) -> Any:
    pipes = parse_pipes(content)
    start = parse_start(content)
    return follow(pipes, start)


def follow(pipes: Pipes, start: Coords) -> int:
    left, right = get_neighbors(pipes, True, start)
    count = 1
    previous_left = previous_right = start
    while left != right:
        new_left = next(
            coords
            for coords in get_neighbors(pipes, True, left)
            if coords != previous_left
        )
        new_right = next(
            coords
            for coords in get_neighbors(pipes, True, right)
            if coords != previous_right
        )
        previous_left, previous_right = left, right
        left, right = new_left, new_right
        count += 1
    return count


def get_neighbors(pipes: Pipes, is_start: bool, current: Coords) -> set[Coords]:
    row, col = current
    crn = pipes.get((row, col), "S" if is_start else "None")
    top = (
        (row - 1, col)
        if pipes.get((row - 1, col), "None") in ("|7F" if crn in "|JLS" else "")
        else None
    )
    bot = (
        (row + 1, col)
        if pipes.get((row + 1, col), "None") in ("|JL" if crn in "|7FS" else "")
        else None
    )
    lef = (
        (row, col - 1)
        if pipes.get((row, col - 1), "None") in ("-FL" if crn in "-7JS" else "")
        else None
    )
    rig = (
        (row, col + 1)
        if pipes.get((row, col + 1), "None") in ("-7J" if crn in "-FLS" else "")
        else None
    )
    return {coords for coords in (top, bot, lef, rig) if coords}


def parse_start(content: str) -> Coords:
    return next(
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, char in enumerate(row)
        if char == "S"
    )


def parse_pipes(content: str) -> Pipes:
    return {
        (row_index, col_index): char
        for row_index, row in enumerate(content.split("\n"))
        for col_index, char in enumerate(row)
        if char in "|-7JLF"
    }


def main() -> None:
    with open("day10.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
