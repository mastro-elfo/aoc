# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Coords = tuple[int, int]
type Direction = Literal["^"] | Literal["v"] | Literal["<"] | Literal[">"]
type Position = tuple[Coords, Direction]


def solution(content: str) -> Any:
    start = parse_start(content)
    end = parse_end(content)
    walls = parse_walls(content)
    return solve(
        walls,
        end,
        (start, ">"),
    )


def solve(
    walls: set[Coords],
    end: Coords,
    start: Position,
) -> int | float:
    frontier: set[Position] = {start}
    visited: dict[Position, int] = {start: 0}

    while True:
        current = sorted(
            ((p, v) for p, v in visited.items() if p in frontier),
            key=get_score,
        )[0]
        position, score = current
        coords, direction = position

        if coords == end:
            return score

        frontier.remove(position)
        for neighbor in get_neighbors(position):
            if hit_wall(walls, neighbor):
                continue
            if neighbor in visited and visited[neighbor] < score:
                continue
            _, n_direction = neighbor
            n_score = score + (1 if direction == n_direction else 1001)
            visited[neighbor] = min(visited.get(neighbor, n_score), n_score)
            frontier.add(neighbor)


def get_score(item: tuple[Position, int]) -> int:
    _, score = item
    return score


def hit_wall(walls: set[Coords], position: Position) -> bool:
    coords, _ = position
    return coords in walls


def get_neighbors(current: Position) -> list[Position]:
    ((row, col), direction) = current
    if direction == "<":
        return [
            ((row, col - 1), "<"),
            ((row - 1, col), "^"),
            ((row + 1, col), "v"),
        ]
    if direction == ">":
        return [
            ((row, col + 1), ">"),
            ((row - 1, col), "^"),
            ((row + 1, col), "v"),
        ]
    if direction == "^":
        return [
            ((row - 1, col), "^"),
            ((row, col - 1), "<"),
            ((row, col + 1), ">"),
        ]
    if direction == "v":
        return [
            ((row + 1, col), "v"),
            ((row, col - 1), "<"),
            ((row, col + 1), ">"),
        ]
    raise ValueError(f"Invalid direction: {direction}")


def pretty_print(walls: set[Coords], end: Coords, start: Coords):
    height, width = max(row for row, _ in walls), max(col for _, col in walls)
    for row in range(height + 1):
        for col in range(width + 1):
            if (row, col) in walls:
                print("#", end="")
            elif (row, col) == start:
                print("S", end="")
            elif (row, col) == end:
                print("E", end="")
            else:
                print(".", end="")
        print("")


def parse_walls(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "#"
    }


def parse_end(content: str) -> Coords:
    return next(
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "E"
    )


def parse_start(content: str) -> Coords:
    return next(
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "S"
    )


def main() -> None:
    with open("day16.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
