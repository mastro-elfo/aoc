# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]


def solution(content: str) -> Any:
    rounded = parse_rock(content, "O")
    cube = parse_rock(content, "#")
    height = len(content.split("\n"))
    tilted = tilt_north(rounded, cube, height)
    return sum(height - row for row, _ in tilted)


def tilt_north(rounded: set[Coords], cube: set[Coords], height: int) -> set[Coords]:
    output = set()
    for current in range(height + 1):
        for rock in rounded:
            row, col = rock
            if row != current:
                continue
            row -= 1
            while (row, col) not in output and (row, col) not in cube and row >= 0:
                row -= 1
            output.add((row + 1, col))
    return output


def parse_rock(content: str, rock: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == rock
    }


def main() -> None:
    with open("day14.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
