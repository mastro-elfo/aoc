# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(play(parse(line)) for line in content)


def play(hands: tuple[str, str]):
    opponent, player = hands
    return (1 if player == "R" else 2 if player == "P" else 3) + (
        0
        if (opponent, player) in [("R", "S"), ("S", "P"), ("P", "R")]
        else 3 if opponent == player else 6
    )


def parse(line: str):
    opponent, player = line.strip().split(" ")
    return (
        "R" if opponent == "A" else "P" if opponent == "B" else "S",
        "R" if player == "X" else "P" if player == "Y" else "S",
    )


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
