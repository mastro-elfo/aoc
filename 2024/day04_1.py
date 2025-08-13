# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    matrix = [row.strip() for row in content[::]]
    return sum(
        count(matrix, row, col)
        for row in range(len(matrix))
        for col in range(len(matrix[0]))
        if matrix[row][col] == "X"
    )


def count(matrix: list[str], row: int, col: int):
    return len(
        [
            check
            for check in [
                # Horizontal Forward
                (
                    col < len(matrix[0]) - 3
                    and matrix[row][col + 1] == "M"
                    and matrix[row][col + 2] == "A"
                    and matrix[row][col + 3] == "S"
                ),
                # Horizontal Backward
                (
                    col > 2
                    and matrix[row][col - 1] == "M"
                    and matrix[row][col - 2] == "A"
                    and matrix[row][col - 3] == "S"
                ),
                # Vertical Downward
                (
                    row < len(matrix) - 3
                    and matrix[row + 1][col] == "M"
                    and matrix[row + 2][col] == "A"
                    and matrix[row + 3][col] == "S"
                ),
                # Vertical Upward
                (
                    row > 2
                    and matrix[row - 1][col] == "M"
                    and matrix[row - 2][col] == "A"
                    and matrix[row - 3][col] == "S"
                ),
                # Diagonal TL-BR
                (
                    row < len(matrix) - 3
                    and col < len(matrix[0]) - 3
                    and matrix[row + 1][col + 1] == "M"
                    and matrix[row + 2][col + 2] == "A"
                    and matrix[row + 3][col + 3] == "S"
                ),
                # Diagonal BR-TL
                (
                    row > 2
                    and col > 2
                    and matrix[row - 1][col - 1] == "M"
                    and matrix[row - 2][col - 2] == "A"
                    and matrix[row - 3][col - 3] == "S"
                ),
                # Diagonal BL-TR
                (
                    row > 2
                    and col < len(matrix[0]) - 3
                    and matrix[row - 1][col + 1] == "M"
                    and matrix[row - 2][col + 2] == "A"
                    and matrix[row - 3][col + 3] == "S"
                ),
                # Diagonal TR-BL
                (
                    row < len(matrix) - 3
                    and col > 2
                    and matrix[row + 1][col - 1] == "M"
                    and matrix[row + 2][col - 2] == "A"
                    and matrix[row + 3][col - 3] == "S"
                ),
            ]
            if check
        ]
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
