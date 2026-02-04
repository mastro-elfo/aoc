# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Coords = tuple[int, int]
type Direction = Literal["^"] | Literal["v"] | Literal["<"] | Literal[">"]
type Position = tuple[Coords, Direction]
type CacheValue = tuple[int, set[Position]]
type Cache = dict[Position, CacheValue]


def solution(content: str) -> Any:
    start = parse_start(content)
    end = parse_end(content)
    walls = parse_walls(content)
    return len(
        rewind(
            solve(walls, (start, ">")),
            end,
        )
    )


def solve(walls: set[Coords], start: Position) -> Cache:
    frontier: set[Position] = {start}
    visited: Cache = {start: (0, set())}

    while True:
        current = sorted(
            ((p, v) for p, v in visited.items() if p in frontier),
            key=get_score,
        )

        if not current:
            return visited
        current = current[0]

        position, (score, _) = current
        coords, direction = position

        frontier.remove(position)
        for neighbor in get_neighbors((coords, direction)):
            if hit_wall(walls, neighbor):
                continue

            _, n_direction = neighbor
            n_score = score + (1 if direction == n_direction else 1001)

            if neighbor not in visited:
                visited[neighbor] = (n_score, {position})
                frontier.add(neighbor)
                continue

            v_score, v_origin = visited[neighbor]

            if v_score < n_score:
                continue
            if v_score == n_score:
                visited[neighbor] = (v_score, v_origin | {position})
            if v_score > n_score:
                visited[neighbor] = (n_score, {position})
                frontier.add(neighbor)


def rewind(visited: Cache, end: Coords) -> set[Coords]:
    coords = set()
    min_score = min(
        score for (coords, _), (score, _) in visited.items() if coords == end
    )
    current = [
        (coords, direction)
        for (coords, direction), (score, _) in visited.items()
        if coords == end and score == min_score
    ]
    origins = [
        position
        for (coords, _), (score, os) in visited.items()
        for position in os
        if coords == end and score == min_score
    ]
    while current:
        for crd, _ in current:
            coords.add(crd)
        current = [position for position in visited if position in origins]
        origins = [
            position
            for ps, (_, os) in visited.items()
            for position in os
            if ps in origins
        ]
        # print(coords)
        # print(current)
        # print(origins)
        # input()
    return coords


def get_score(item: tuple[Any, CacheValue]) -> int:
    _, (score, _) = item
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
