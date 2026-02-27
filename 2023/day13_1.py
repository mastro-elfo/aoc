# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Hashes = tuple[list[int], list[int]]


def solution(content: str) -> Any:
    return sum(find_mirror(parse(block)) for block in content.split("\n\n"))


def summarize(column: int, row: int) -> int:
    return column + 100 * row


def find_mirror(hashes: Hashes) -> int:
    by_row, by_col = hashes
    return summarize(get_nearby_twins(by_col), get_nearby_twins(by_row))


def get_nearby_twins(lst: list[int]) -> int:
    for index, (a, b) in enumerate(zip(lst, lst[1:])):
        if a != b:
            continue
        if is_palindrome(lst, index):
            return index + 1
    return 0


def is_palindrome(lst: list[int], index: int) -> bool:
    i = 1
    while index - i >= 0 and index + i + 1 < len(lst):
        if lst[index - i] != lst[index + i + 1]:
            return False
        i += 1
    return True


def parse(block: str) -> Hashes:
    by_row = []
    for row in block.split("\n"):
        by_row.append(sum(2**index for index, char in enumerate(row) if char == "#"))

    by_col = [0] * len(block.split("\n")[0])
    for index, row in enumerate(block.split("\n")):
        by_col = [
            by_col[idx] + (2**index * (1 if char == "#" else 0))
            for idx, char in enumerate(row)
        ]

    return by_row, by_col


def main() -> None:
    with open("day13.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
