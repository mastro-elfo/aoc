# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal, cast

type Direction = Literal["U", "D", "L", "R"]
type Instruction = tuple[Direction, int]
type Coords = tuple[int, int]


def solution(content: list[str]) -> Any:
    instructions = [parse(line) for line in content]
    head: Coords = (0, 0)
    tail1: Coords = (0, 0)
    tail2: Coords = (0, 0)
    tail3: Coords = (0, 0)
    tail4: Coords = (0, 0)
    tail5: Coords = (0, 0)
    tail6: Coords = (0, 0)
    tail7: Coords = (0, 0)
    tail8: Coords = (0, 0)
    tail9: Coords = (0, 0)
    positions: set[Coords] = {tail9}
    while instructions:
        (direction, amount), *rest = instructions
        if amount == 0:
            instructions = rest
            continue
        head = move_head(direction, head)
        tail1 = move_tail(direction, head, tail1)
        tail2 = move_tail(direction, tail1, tail2)
        tail3 = move_tail(direction, tail2, tail3)
        tail4 = move_tail(direction, tail3, tail4)
        tail5 = move_tail(direction, tail4, tail5)
        tail6 = move_tail(direction, tail5, tail6)
        tail7 = move_tail(direction, tail6, tail7)
        tail8 = move_tail(direction, tail7, tail8)
        tail9 = move_tail(direction, tail8, tail9)
        positions.add(tail9)
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
        if head_x == tail_x:
            return move_head("D" if head_y > tail_y else "U", tail)
        if head_y == tail_y:
            return move_head("R" if head_x > tail_x else "L", tail)
        if abs(head_x - tail_x) > 1 and abs(head_y - tail_y) > 1:
            return (
                (head_x + tail_x) // 2,
                (head_y + tail_y) // 2,
            )
        if abs(head_x - tail_x) > 1:
            return move_head("R" if head_x > tail_x else "L", (tail_x, head_y))
        if abs(head_y - tail_y) > 1:
            return move_head("D" if head_y > tail_y else "U", (head_x, tail_y))
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
