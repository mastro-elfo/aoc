# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type CacheKey = tuple[Coords, ...]

CYCLES = 1_000_000_000

cache: dict[CacheKey, int] = {}


def solution(content: str) -> Any:
    rounded = parse_rock(content, "O")
    cube = parse_rock(content, "#")
    height = len(content.split("\n"))
    width = len(content.split("\n")[0])
    tilted = rounded.copy()
    for index in range(CYCLES):
        tilted = cycle(tilted, cube, height, width)
        key = tuple(sorted(tilted))
        if key in cache:
            previous = cache[key]
            diff = index - previous
            reminder = (CYCLES - index - 1) % diff
            for _ in range(reminder):
                tilted = cycle(tilted, cube, height, width)
            return load(tilted, height)
        cache[key] = index


def load(stones: set[Coords], height: int) -> int:
    return sum(height - row for row, _ in stones)


def cycle(
    rounded: set[Coords], cube: set[Coords], height: int, width: int
) -> set[Coords]:
    tilted = tilt_north(rounded, cube, height)
    tilted = tilt_west(tilted, cube, width)
    tilted = tilt_south(tilted, cube, height)
    tilted = tilt_east(tilted, cube, width)
    return tilted


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


def tilt_south(rounded: set[Coords], cube: set[Coords], height: int) -> set[Coords]:
    output = set()
    for current in range(height, -1, -1):
        for rock in rounded:
            row, col = rock
            if row != current:
                continue
            row += 1
            while (row, col) not in output and (row, col) not in cube and row < height:
                row += 1
            output.add((row - 1, col))
    return output


def tilt_west(rounded: set[Coords], cube: set[Coords], width: int) -> set[Coords]:
    output = set()
    for current in range(width + 1):
        for rock in rounded:
            row, col = rock
            if col != current:
                continue
            col -= 1
            while (row, col) not in output and (row, col) not in cube and col >= 0:
                col -= 1
            output.add((row, col + 1))
    return output


def tilt_east(rounded: set[Coords], cube: set[Coords], width: int) -> set[Coords]:
    output = set()
    for current in range(width, -1, -1):
        for rock in rounded:
            row, col = rock
            if col != current:
                continue
            col += 1
            while (row, col) not in output and (row, col) not in cube and col < width:
                col += 1
            output.add((row, col - 1))
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
