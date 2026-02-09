# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    pattern_block, design_block = content.split("\n\n")
    patterns = sorted(pattern_block.split(", "), key=len, reverse=True)
    designs = design_block.split("\n")
    return sum(counter(patterns, design) for design in designs)


cache: dict[str, int] = {}


def counter(patterns: list[str], design: str) -> int:
    if design in cache:
        return cache[design]
    if not design:
        return 1
    count = 0
    for pattern in patterns:
        if design.startswith(pattern):
            count += counter(patterns, design[len(pattern) :])
    cache[design] = count
    return count


def main() -> None:
    with open("day19.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
