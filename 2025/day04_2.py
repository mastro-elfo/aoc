# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]


def solution(content: str) -> Any:
    return remove(parse(content))


def remove(rolls: frozenset[Coords]) -> int:
    to_remove = {roll for roll in rolls if is_accessible(rolls, roll)}
    if not to_remove:
        return 0
    return len(to_remove) + remove(
        frozenset(roll for roll in rolls if roll not in to_remove)
    )


def is_accessible(rolls: frozenset[Coords], roll: Coords) -> bool:
    row, col = roll
    return (
        len(
            [
                r
                for r in (
                    (row - 1, col - 1),
                    (row - 1, col),
                    (row - 1, col + 1),
                    (row, col - 1),
                    (row, col + 1),
                    (row + 1, col - 1),
                    (row + 1, col),
                    (row + 1, col + 1),
                )
                if r in rolls
            ]
        )
        < 4
    )


def parse(content: str) -> frozenset[Coords]:
    return frozenset(
        (row_index, col_index)
        for row_index, row in enumerate(content.split("\n"))
        for col_index, col in enumerate(row)
        if col == "@"
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
