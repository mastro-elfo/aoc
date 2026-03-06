# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal, cast

type Direction = Literal["U", "D", "L", "R"]
type Instruction = tuple[Direction, int]
type Coords = tuple[int, int]


def solution(content: str) -> Any:
    perimeter = unwrap([parse(line) for line in content.split("\n")])
    return measure_inner_volume(perimeter, find_inner_square(perimeter)) + len(
        perimeter
    )


def measure_inner_volume(perimeter: set[Coords], inner: Coords) -> int:
    visited = set()
    frontier = {inner}
    while frontier:
        first, *rest = frontier
        frontier = set(rest)
        visited.add(first)
        neighbors = get_neighbors(first)
        neighbors = {
            current
            for current in neighbors
            if current not in visited
            and current not in frontier
            and current not in perimeter
        }
        frontier |= neighbors
    return len(visited)


def get_neighbors(current: Coords) -> set[Coords]:
    x, y = current
    return {(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)}


def find_inner_square(perimeter: set[Coords]) -> Coords:
    top = min(y for _, y in perimeter)
    top_tiles = ((x, y) for x, y in perimeter if y == top)
    for x, y in top_tiles:
        if (x, y + 1) not in perimeter:
            return (x, y + 1)
    raise ValueError("Inner tile not found")


def unwrap(instructions: list[Instruction]) -> set[Coords]:
    output = set()
    current = (0, 0)
    for direction, amount in instructions:
        for _ in range(amount):
            x, y = current
            if direction == "U":
                current = (x, y - 1)
            if direction == "D":
                current = (x, y + 1)
            if direction == "L":
                current = (x - 1, y)
            if direction == "R":
                current = (x + 1, y)
            output.add(current)
    return output


def pretty_print(instructions: list[Instruction]):
    coords = []
    current = (0, 0)
    for direction, amount in instructions:
        for _ in range(amount):
            x, y = current
            if direction == "U":
                current = (x, y - 1)
            if direction == "D":
                current = (x, y + 1)
            if direction == "L":
                current = (x - 1, y)
            if direction == "R":
                current = (x + 1, y)
            coords.append(current)
    top = min(y for _, y in coords)
    bottom = max(y for _, y in coords)
    left = min(x for x, _ in coords)
    right = max(x for x, _ in coords)
    for y in range(top, bottom + 1):
        for x in range(left, right + 1):
            print("#" if (x, y) in coords else ".", end="")
        print()


def is_clockwise(instructions: list[Instruction]) -> bool:
    angle = 0
    for (fdir, _), (sdir, _) in zip(instructions, instructions[1:]):
        rotation = fdir, sdir
        if rotation in [("U", "R"), ("R", "D"), ("D", "L"), ("L", "U")]:
            angle += 1
        if rotation in [("U", "L"), ("L", "D"), ("D", "R"), ("R", "U")]:
            angle -= 1
    return angle > 0


def parse(line: str) -> Instruction:
    direction, amount, *_ = line.split(" ")
    return (cast(Direction, direction), int(amount))


def main() -> None:
    with open("day18.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
