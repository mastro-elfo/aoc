# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return count_trees(content.split("\n"), (0, 0), (3, 1))


def count_trees(treemap: list[str], start: tuple[int, int], delta: tuple[int, int]):
    width = len(treemap[0])
    height = len(treemap)
    current = start
    trees = 0
    while current[1] < height:
        if treemap[current[1]][current[0]] == "#":
            trees += 1
        current = ((current[0] + delta[0]) % width, current[1] + delta[1])
    return trees


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
