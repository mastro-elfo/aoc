# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Scratchcard = tuple[str, list[int], list[int], int]


def solution(content: list[str]) -> Any:
    return sum(a[3] for a in play([parse(line) for line in content]))


def play(cards: list[Scratchcard]):
    output = cards[::]
    for index, card in enumerate(output):
        wins = winning(card)
        for i in range(index + 1, index + 1 + wins):
            output[i] = (
                output[i][0],
                output[i][1],
                output[i][2],
                output[i][3] + card[3],
            )
    return output


def winning(card: Scratchcard):
    _, nums, wnums, _ = card
    return len([True for n in nums if n in wnums])


def parse(line: str) -> Scratchcard:
    match = re.match(r"Card\s*(\d+):\s*([\d\s]+)\s*\|\s*([\d\s]+)", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (
        match.group(1),
        [int(x) for x in re.sub(r"\s+", " ", match.group(2)).strip().split(" ")],
        [int(x) for x in re.sub(r"\s+", " ", match.group(3)).strip().split(" ")],
        1,
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
