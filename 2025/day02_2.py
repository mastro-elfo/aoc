# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return sum(
        item
        for rng in (parse(block) for block in content.split(","))
        for item in rng
        if is_invalid(item)
    )


def is_invalid(item: int) -> bool:
    half = len(str(item)) // 2
    for n in range(half, 0, -1):
        start = str(item)[:n]
        if is_repeating(start, str(item)):
            return True
    return False


def is_repeating(start: str, rest: str) -> bool:
    if start == rest:
        return True
    if rest.startswith(start):
        return is_repeating(start, rest[len(start) :])
    return False


def parse(block: str) -> range:
    start, end = block.split("-")
    return range(int(start), int(end) + 1)


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
