# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    matrix = [row.strip() for row in content[::]]
    return len(
        [
            True
            for row in range(len(matrix))
            for col in range(len(matrix[0]))
            if matrix[row][col] == "A" and check(matrix, row, col)
        ]
    )


def check(matrix: list[str], row: int, col: int):
    if row == 0 or row == len(matrix) - 1 or col == 0 or col == len(matrix[0]) - 1:
        return False
    return (
        (
            matrix[row - 1][col - 1] == "M"
            and matrix[row + 1][col + 1] == "S"
            and matrix[row + 1][col - 1] == "M"
            and matrix[row - 1][col + 1] == "S"
        )
        or (
            matrix[row - 1][col - 1] == "M"
            and matrix[row + 1][col + 1] == "S"
            and matrix[row - 1][col + 1] == "M"
            and matrix[row + 1][col - 1] == "S"
        )
        or (
            matrix[row + 1][col + 1] == "M"
            and matrix[row - 1][col - 1] == "S"
            and matrix[row - 1][col + 1] == "M"
            and matrix[row + 1][col - 1] == "S"
        )
        or (
            matrix[row + 1][col + 1] == "M"
            and matrix[row - 1][col - 1] == "S"
            and matrix[row + 1][col - 1] == "M"
            and matrix[row - 1][col + 1] == "S"
        )
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
