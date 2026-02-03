# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Storage = dict[Coords, str]

DELTA = {
    "^": (-1, 0),
    "v": (1, 0),
    "<": (0, -1),
    ">": (0, 1),
}


def solution(content: str) -> Any:
    parts = content.split("\n\n")
    storage = parse_storage(parts[0])
    instructions = parse_instructions(parts[1])
    robot = parse_robot(parts[0])

    for instruction in instructions:
        storage, robot = move(storage, robot, instruction)

    return sum(to_gps(coords) for coords, value in storage.items() if value == "O")


def move(storage: Storage, robot: Coords, instruction: str) -> tuple[Storage, Coords]:
    free = next_free(storage, robot, instruction)
    if free is None:
        return (storage, robot)
    neighbor = translate(robot, DELTA[instruction])
    copy = storage.copy()
    if neighbor in copy:
        copy[free] = copy[neighbor]
        del copy[neighbor]
    return (copy, neighbor)


def next_free(storage: Storage, robot: Coords, instruction: str):
    current = robot
    while True:
        current = translate(current, DELTA[instruction])
        if current not in storage:
            return current
        if storage[current] == "#":
            return None


def translate(v1: Coords, v2: Coords) -> Coords:
    v1x, v1y = v1
    v2x, v2y = v2
    return (v1x + v2x, v1y + v2y)


def to_gps(coords: Coords) -> int:
    row, col = coords
    return 100 * row + col


def parse_robot(block: str) -> Coords:
    return next(
        (row_index, col_index)
        for row_index, row in enumerate(block.split("\n"))
        for col_index, col in enumerate(row)
        if col == "@"
    )


def parse_storage(block: str) -> Storage:
    return {
        (row_index, col_index): col
        for row_index, row in enumerate(block.split("\n"))
        for col_index, col in enumerate(row)
        if col == "#" or col == "O"
    }


def parse_instructions(block: str) -> list[str]:
    return [char for char in block if char in "<>^v"]


def main() -> None:
    with open("day15.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
