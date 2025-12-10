# pylint: disable=missing-module-docstring, missing-function-docstring


import itertools
from typing import Any

type Hand = tuple[str, str, int]


def solution(content: list[str]) -> Any:
    return sum(
        index * bid
        for index, (_, _, bid) in enumerate(
            sorted(
                sorted((parse(line) for line in content), key=sort_by_card),
                key=sort_by_type,
            ),
            start=1,
        )
    )


def sort_by_card(hand: Hand):
    card, _, _ = hand
    return ["23456789TJQKA".index(c) for c in card]


def sort_by_type(hand: Hand):
    _, tp, _ = hand
    if tp == "FiveOfKind":
        return 6
    if tp == "FourOfKind":
        return 5
    if tp == "FullHouse":
        return 4
    if tp == "ThreeOfKind":
        return 3
    if tp == "TwoPair":
        return 2
    if tp == "OnePair":
        return 1
    return 0


def parse_type(cards: str):
    signature = tuple(sorted(len(list(g)) for _, g in itertools.groupby(sorted(cards))))
    if (5,) == signature:
        return "FiveOfKind"
    if (1, 4) == signature:
        return "FourOfKind"
    if (2, 3) == signature:
        return "FullHouse"
    if (1, 1, 3) == signature:
        return "ThreeOfKind"
    if (1, 2, 2) == signature:
        return "TwoPair"
    if (1, 1, 1, 2) == signature:
        return "OnePair"
    return "HighCard"


def parse(line: str) -> Hand:
    cards, bid = line.split(" ")
    return (cards, parse_type(cards), int(bid))


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
