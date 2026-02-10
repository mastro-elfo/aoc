# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Labyrinth = dict[Coords, int]


def solution(content: str) -> Any:
    start = parse_position(content, "S")
    end = parse_position(content, "E")
    walls = parse_walls(content)
    labyrinth = get_labyrinth(walls, start, end)
    cheats = get_cheats(walls, labyrinth)
    return len(
        [saving for saving in get_cheat_saving(labyrinth, cheats) if saving >= 100]
    )


def get_cheat_saving(labyrinth: Labyrinth, cheats: set[Coords]) -> list[int]:
    saves: list[int] = []
    for cheat in cheats:
        neighbors = get_neighbors(cheat)
        neighbors = {neighbor for neighbor in neighbors if neighbor in labyrinth}
        saves.append(
            max(labyrinth[neighbor] for neighbor in neighbors)
            - min(labyrinth[neighbor] for neighbor in neighbors)
            - 2
        )
    return saves


def get_cheats(walls: set[Coords], labyrinth: Labyrinth) -> set[Coords]:
    cheats: set[Coords] = set()
    for wall in walls:
        neighbors = get_neighbors(wall)
        neighbors = {neighbor for neighbor in neighbors if neighbor in labyrinth}
        if len(neighbors) > 1:
            cheats.add(wall)
    return cheats


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
