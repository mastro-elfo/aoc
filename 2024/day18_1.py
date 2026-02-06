# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]

SIZE = 70
N = 1024


def solution(content: list[str]) -> Any:
    falling = [parse(line) for line in content]
    corrupted = falling[:N]
    return shortest(corrupted, (SIZE, SIZE))


def shortest(corrupted: list[Coords], out: Coords):
    visited: dict[Coords, int] = {(0, 0): 0}
    frontier: set[Coords] = {(0, 0)}

    while True:
        current = next(
            coords
            for coords, _ in sorted(
                ((key, val) for key, val in visited.items() if key in frontier),
                key=second,
            )
        )

        if current == out:
            return visited[current]

        frontier.remove(current)
        for neighbor in get_neighbors(current, SIZE):
            if neighbor in corrupted:
                continue
            if neighbor not in visited or visited[neighbor] > 1 + visited[current]:
                visited[neighbor] = 1 + visited[current]
                frontier.add(neighbor)


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
