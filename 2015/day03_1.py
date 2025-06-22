# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Position = tuple[int, int]


def solution(content: str) -> Any:
    current: Position = (0, 0)
    visited: list[Position] = [current]
    for direction in content:
        current = move(current, direction)
        visited.append(current)
    return len(
        [True for index, item in enumerate(visited) if item not in visited[:index]]
    )


def move(current: Position, direction: str) -> Position:
    if direction == "^":
        return (current[0], current[1] + 1)
    if direction == "v":
        return (current[0], current[1] - 1)
    if direction == "<":
        return (current[0] - 1, current[1])
    if direction == ">":
        return (current[0] + 1, current[1])
    raise ValueError(f"Invalid direction: {direction}")


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
