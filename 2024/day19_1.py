# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    pattern_block, design_block = content.split("\n\n")
    patterns = sorted(pattern_block.split(", "), key=len, reverse=True)
    designs = design_block.split("\n")
    return len([design for design in designs if is_valid(patterns, design)])


cache: dict[str, bool] = {}


def is_valid(patterns: list[str], design: str) -> bool:
    if design in cache:
        return cache[design]
    if not design:
        return True
    for pattern in patterns:
        if design.startswith(pattern):
            result = is_valid(patterns, design[len(pattern) :])
            if result:
                cache[design] = True
                return True
    cache[design] = False
    return False


def main() -> None:
    with open("day19.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
