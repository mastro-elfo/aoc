# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Range = tuple[int, int]


def solution(content: str) -> Any:
    range_block, ingredient_block = content.split("\n\n")
    ranges = [parse_range(line) for line in range_block.split("\n")]
    ingredients = [int(x) for x in ingredient_block.split("\n")]
    return len(
        [ingredient for ingredient in ingredients if is_fresh(ranges, ingredient)]
    )


def is_fresh(ranges: list[Range], ingredient: int) -> bool:
    return any(start <= ingredient <= end for start, end in ranges)


def parse_range(line: str) -> Range:
    start, _, end = line.partition("-")
    return (int(start), int(end))


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
