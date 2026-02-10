# pylint: disable=missing-module-docstring, missing-function-docstring

from typing import Any

type Coords = tuple[int, int]
type Labyrinth = dict[Coords, int]


def solution(content: str) -> Any:
    start = parse_position(content, "S")
    end = parse_position(content, "E")
    walls = parse_walls(content)
    labyrinth = get_labyrinth(walls, start, end)
    return len(get_cheat_saving(labyrinth, walls))


def get_cheat_saving(
    labyrinth: Labyrinth, cheats: set[Coords]
) -> set[tuple[Coords, Coords]]:
    saves: set[tuple[Coords, Coords]] = set()
    for cheat in cheats:
        neighbors = get_neighbors(cheat)
        neighbors = {neighbor for neighbor in neighbors if neighbor in labyrinth}
        for neighbor in neighbors:
            scores = (
                (coords, score, manhattan(neighbor, coords))
                for coords, score in labyrinth.items()
            )
            for coords, score, distance in scores:
                if distance > 20:
                    continue
                saving = score - distance - labyrinth[neighbor]
                if saving < 100:
                    continue
                saves.add((neighbor, coords))
    return saves


def manhattan(c1: Coords, c2: Coords) -> int:
    row1, col1 = c1
    row2, col2 = c2
    return abs(row1 - row2) + abs(col1 - col2)


def get_labyrinth(walls: set[Coords], start: Coords, end: Coords) -> Labyrinth:
    labyrinth: Labyrinth = {start: 0}
    current = start
    while current != end:
        score = labyrinth[current]
        neighbors = get_neighbors(current)
        neighbors = {
            neighbor
            for neighbor in neighbors
            if neighbor not in walls and neighbor not in labyrinth
        }
        if len(neighbors) != 1:
            raise ValueError(f"Too many ways for {current}")
        current = neighbors.pop()
        labyrinth[current] = score + 1
    return labyrinth


def get_neighbors(position: Coords) -> set[Coords]:
    row, col = position
    return {(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)}


def parse_position(content: str, label: str) -> Coords:
    for row_index, row in enumerate(content.split("\n")):
        for col_index, col in enumerate(row):
            if col == label:
                return row_index, col_index
    raise IndexError(f"Label not found {label}")


def parse_walls(content: str) -> set[Coords]:
    return {
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "#"
    }


def main() -> None:
    with open("day20.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
