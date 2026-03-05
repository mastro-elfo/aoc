# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Coords = tuple[int, int]
type Map = dict[Coords, int]
type Direction = Literal["^", "v", "<", ">", "."]
type Position = tuple[Coords, Direction, int]
type Visited = dict[Position, int]


def solution(content: str) -> Any:
    print("It's slow but it works")
    heatmap = parse_heatmap(content)
    the_exit = parse_exit(content)
    return find_best_path(heatmap, the_exit, (0, 0))


def find_best_path(heatmap: Map, the_exit: Coords, start: Coords) -> int:
    frontier: set[Position] = {(start, ".", 0)}
    visited: Visited = {(start, ".", 0): 0}
    while frontier:
        best = sorted((position for position in frontier), key=lambda p: visited[p])[0]
        frontier = {position for position in frontier if position != best}
        coords, direction, repeat = best
        previous = visited[best]
        top, bottom, left, right = get_neighbors(coords)

        if coords == the_exit:
            return previous

        if top in heatmap and direction != "v" and (direction != "^" or repeat < 3):
            key = (top, "^", 1 if direction != "^" else (repeat + 1))
            value = previous + heatmap[top]
            current = visited.get(key, float("Inf"))
            if key not in visited or current > value:
                frontier.add(key)
                visited[key] = value

        if bottom in heatmap and direction != "^" and (direction != "v" or repeat < 3):
            key = (bottom, "v", 1 if direction != "v" else (repeat + 1))
            value = previous + heatmap[bottom]
            current = visited.get(key, float("Inf"))
            if key not in visited or current > value:
                frontier.add(key)
                visited[key] = value

        if left in heatmap and direction != ">" and (direction != "<" or repeat < 3):
            key = (left, "<", 1 if direction != "<" else (repeat + 1))
            value = previous + heatmap[left]
            current = visited.get(key, float("Inf"))
            if key not in visited or current > value:
                frontier.add(key)
                visited[key] = value

        if right in heatmap and direction != "<" and (direction != ">" or repeat < 3):
            key = (right, ">", 1 if direction != ">" else (repeat + 1))
            value = previous + heatmap[right]
            current = visited.get(key, float("Inf"))
            if key not in visited or current > value:
                frontier.add(key)
                visited[key] = value

    return -1


def get_neighbors(coords: Coords) -> list[Coords]:
    row, col = coords
    return [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]


def parse_exit(content: str) -> Coords:
    return (len(content.split("\n")) - 1, len(content.split("\n")[0]) - 1)


def parse_heatmap(content: str) -> Map:
    return {
        (row_index, col_index): int(col)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
    }


def main() -> None:
    with open("day17.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
