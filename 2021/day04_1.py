# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Cell = tuple[int, bool]
type Row = list[Cell]
type Board = list[Row]


def solution(content: list[str]) -> Any:
    winning_board, winning_number = play(
        parse_boards("\n".join(content[2:]).split("\n\n")),
        [int(x) for x in content[0].split(",")],
    )
    return (
        sum(
            num
            for num, check in [cell for row in winning_board for cell in row]
            if not check
        )
        * winning_number
    )


def is_winning_line(line: list[Cell]):
    return all(check for _, check in line)


def transpose(board: Board):
    return [[row[index] for row in board] for index in range(len(board))]


def is_winner(board: Board):
    return any(is_winning_line(row) for row in board) or any(
        is_winning_line(col) for col in transpose(board)
    )


def winner(boards: list[Board]):
    for board in boards:
        if is_winner(board):
            return board
    return None


def play(boards: list[Board], numbers: list[int]):
    current = boards.copy()
    for number in numbers:
        current = [
            [[(num, check or num == number) for num, check in row] for row in board]
            for board in current
        ]

        if winning_board := winner(current):
            return (winning_board, number)

    return ([], 0)


def parse_boards(lines: list[str]):
    boards: list[Board] = [[]]
    for line in lines:
        if line:
            if not boards[-1]:
                boards[-1] = []
            boards[-1].append([(int(x), False) for x in line.split(" ") if x])
        else:
            boards.append([])
    return boards


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
