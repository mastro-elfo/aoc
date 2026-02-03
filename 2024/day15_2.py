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
    storage_part = (
        parts[0]
        .replace("#", "##")
        .replace("O", "[]")
        .replace(".", "..")
        .replace("@", "@.")
    )
    storage = parse_storage(storage_part)
    instructions = parse_instructions(parts[1])
    robot = parse_robot(storage_part)

    for instruction in instructions:
        storage, robot = move(storage, robot, instruction)

    return sum(to_gps(coords) for coords, value in storage.items() if value == "[")


def move(storage: Storage, robot: Coords, instruction: str) -> tuple[Storage, Coords]:
    neighbor = translate(robot, DELTA[instruction])
    if neighbor not in storage:
        return (storage, neighbor)
    if storage[neighbor] == "#":
        return (storage, robot)
    if can_move(storage, robot, instruction):
        return (do_move(storage, robot, instruction), neighbor)
    return (storage, robot)


def do_move(storage: Storage, current: Coords, instruction: str) -> Storage:
    neighbor = translate(current, DELTA[instruction])
    if neighbor not in storage:
        copy = storage.copy()
        copy[neighbor] = copy[current]
        del copy[current]
        return copy
    if storage[neighbor] == "#":
        return storage
    if instruction in "<>":
        new_storage = do_move(storage, neighbor, instruction)
        if current in new_storage:
            new_storage[neighbor] = new_storage[current]
            del new_storage[current]
        return new_storage
    if storage[neighbor] == "[":
        new_storage = do_move(storage, neighbor, instruction)
        new_storage = do_move(new_storage, translate(neighbor, DELTA[">"]), instruction)
        if current in new_storage:
            new_storage[neighbor] = new_storage[current]
            del new_storage[current]
        return new_storage
    if storage[neighbor] == "]":
        new_storage = do_move(storage, neighbor, instruction)
        new_storage = do_move(new_storage, translate(neighbor, DELTA["<"]), instruction)
        if current in new_storage:
            new_storage[neighbor] = new_storage[current]
            del new_storage[current]
        return new_storage
    return storage


def can_move(storage: Storage, current: Coords, instruction: str) -> bool:
    neighbor = translate(current, DELTA[instruction])
    if neighbor not in storage:
        return True
    if storage[neighbor] == "#":
        return False
    if instruction in "<>":
        return can_move(storage, neighbor, instruction)
    if storage[neighbor] == "[":
        return can_move(storage, neighbor, instruction) and can_move(
            storage, translate(neighbor, DELTA[">"]), instruction
        )
    if storage[neighbor] == "]":
        return can_move(storage, neighbor, instruction) and can_move(
            storage, translate(neighbor, DELTA["<"]), instruction
        )
    return False


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
        if col == "#" or col == "[" or col == "]"
    }


def parse_instructions(block: str) -> list[str]:
    return [char for char in block if char in "<>^v"]


def main() -> None:
    with open("day15.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
