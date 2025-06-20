# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(play(parse(line)) for line in content)


def play(hands: tuple[str, str]):
    opponent, player = hands
    return (0 if player == "X" else 3 if player == "Y" else 6) + (
        1
        if (opponent, player) in [("P", "X"), ("R", "Y"), ("S", "Z")]
        else 2 if (opponent, player) in [("S", "X"), ("P", "Y"), ("R", "Z")] else 3
    )


def parse(line: str):
    opponent, player = line.strip().split(" ")
    return ("R" if opponent == "A" else "P" if opponent == "B" else "S", player)


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
