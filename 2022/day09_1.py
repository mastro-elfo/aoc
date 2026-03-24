# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal, cast

type Direction = Literal["U", "D", "L", "R"]
type Instruction = tuple[Direction, int]
type Coords = tuple[int, int]


def solution(content: list[str]) -> Any:
    instructions = [parse(line) for line in content]
    head: Coords = (0, 0)
    tail: Coords = (0, 0)
    positions: set[Coords] = {tail}
    while instructions:
        (direction, amount), *rest = instructions
        if amount == 0:
            instructions = rest
            continue
        head = move_head(direction, head)
        tail = move_tail(direction, head, tail)
        positions.add(tail)
        instructions = [(direction, amount - 1), *rest]
    return len(positions)


def move_head(direction: Direction, head: Coords) -> Coords:
    x, y = head
    if direction == "D":
        return (x, y + 1)
    if direction == "L":
        return (x - 1, y)
    if direction == "R":
        return (x + 1, y)
    if direction == "U":
        return (x, y - 1)


def move_tail(direction: Direction, head: Coords, tail: Coords) -> Coords:
    head_x, head_y = head
    tail_x, tail_y = tail
    if abs(head_x - tail_x) > 1 or abs(head_y - tail_y) > 1:
        if head_x == tail_x or head_y == tail_y:
            return move_head(direction, tail)
        if abs(head_x - tail_x) > 1:
            return move_head(direction, (tail_x, head_y))
        if abs(head_y - tail_y) > 1:
            return move_head(direction, (head_x, tail_y))
        raise ValueError(f"Should not be here: {direction} {head} {tail}")
    return tail


def parse(line: str) -> Instruction:
    direction, _, amount = line.partition(" ")
    return (cast(Direction, direction), int(amount))


def main() -> None:
    with open("day09.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
