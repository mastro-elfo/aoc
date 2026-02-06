# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Cache = dict[Coords, tuple[int, Coords | None]]
SIZE = 70


def solution(content: list[str]) -> Any:
    falling = [parse(line) for line in content]
    corrupted = []
    current_path = shortest(corrupted, (0, 0), (SIZE, SIZE), [])
    while current_path:
        first, *falling = falling
        corrupted.append(first)
        if first in current_path:
            index = current_path.index(first)
            second_part = shortest(
                corrupted,
                current_path[index - 1],
                (SIZE, SIZE),
                current_path[(index + 1) :],
            )
            if not second_part:
                return ",".join(str(x) for x in first)
            current_path = current_path[: (index - 1)] + second_part


def shortest(
    corrupted: list[Coords], start: Coords, end: Coords, given: list[Coords]
) -> list[Coords]:
    visited: Cache = {start: (0, None)}
    frontier: set[Coords] = {start}

    while True:
        if not frontier:
            return []

        current = next(
            coords
            for coords, _ in sorted(
                ((key, val) for key, val in visited.items() if key in frontier),
                key=second,
            )
        )

        if current == end:
            return rewind(visited, end)
        if current in given:
            index = given.index(current)
            return rewind(visited, current) + given[(index + 1) :]

        score, _ = visited[current]
        frontier.remove(current)
        for neighbor in get_neighbors(current, SIZE):
            if neighbor in corrupted:
                continue
            if neighbor not in visited:
                visited[neighbor] = (1 + score, current)
                frontier.add(neighbor)
            else:
                n_score, _ = visited[neighbor]
                if 1 + score < n_score:
                    visited[neighbor] = (1 + score, current)


def rewind(visited: Cache, out: Coords) -> list[Coords]:
    output: list[Coords] = []
    current = out
    while current:
        output.append(current)
        _, current = visited[current]
    return output[::-1]


def second[T](x: tuple[Any, T]) -> T:
    _, s = x
    return s


def get_neighbors(coords: Coords, size: int) -> set[Coords]:
    x, y = coords
    return {
        (a, b)
        for a, b in (
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1),
        )
        if 0 <= a <= size and 0 <= b <= size
    }


def parse(line: str) -> Coords:
    x, y = line.split(",")
    return (int(x), int(y))


def main() -> None:
    with open("day18.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
