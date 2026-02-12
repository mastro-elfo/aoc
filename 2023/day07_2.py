# pylint: disable=missing-module-docstring, missing-function-docstring


import itertools
from typing import Any, Literal

type HandType = Literal[
    "FiveOfKind",
    "FourOfKind",
    "FullHouse",
    "ThreeOfKind",
    "TwoPair",
    "OnePair",
    "HighCard",
]
type Hand = tuple[str, HandType, int]


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
    return ["J23456789TQKA".index(c) for c in card]


def sort_by_type(hand: Hand):
    _, tp, _ = hand
    return [
        "HighCard",
        "OnePair",
        "TwoPair",
        "ThreeOfKind",
        "FullHouse",
        "FourOfKind",
        "FiveOfKind",
    ].index(tp)


def parse_type(cards: str) -> HandType:
    signature = tuple(sorted(len(list(g)) for _, g in itertools.groupby(sorted(cards))))
    jacks = len([card for card in cards if card == "J"])
    if (5,) == signature:
        return "FiveOfKind"
    if (1, 4) == signature and jacks:
        return "FiveOfKind"
    if (1, 4) == signature:
        return "FourOfKind"
    if (2, 3) == signature and jacks:
        return "FiveOfKind"
    if (2, 3) == signature:
        return "FullHouse"
    if (1, 1, 3) == signature and jacks:
        return "FourOfKind"
    if (1, 1, 3) == signature:
        return "ThreeOfKind"
    if (1, 2, 2) == signature and jacks == 1:
        return "FullHouse"
    if (1, 2, 2) == signature and jacks:
        return "FourOfKind"
    if (1, 2, 2) == signature:
        return "TwoPair"
    if (1, 1, 1, 2) == signature and jacks:
        return "ThreeOfKind"
    if (1, 1, 1, 2) == signature:
        return "OnePair"
    if jacks:
        return "OnePair"
    return "HighCard"


def parse(line: str) -> Hand:
    cards, bid = line.split(" ")
    htype = parse_type(cards)
    return (cards, htype, int(bid))


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
