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
        (
            "R"
            if is_rock(opponent, player)
            else "P" if is_paper(opponent, player) else "S"
        ),
    )


def is_rock(opponent: str, player):
    return (
        (opponent == "B" and player == "X")
        or (opponent == "A" and player == "Y")
        or (opponent == "C" and player == "Z")
    )


def is_paper(opponent: str, player):
    return (
        (opponent == "C" and player == "X")
        or (opponent == "B" and player == "Y")
        or (opponent == "A" and player == "Z")
    )


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
