# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return len(
        [
            True
            for level in ([int(x) for x in line.split(" ")] for line in content)
            if is_safe(level) or is_safeable(level)
        ]
    )


def is_safeable(level: list[int]):
    return any(is_safe(removed) for removed in tolerate(level))


def tolerate(level: list[int]):
    for index in range(len(level)):
        yield level[:index] + level[index + 1 :]


def is_safe(level: list[int]):
    return is_within_range(level) and (is_increasing(level) or is_decreasing(level))


def is_increasing(level: list[int]):
    return all(b > a for a, b in zip(level, level[1:]))


def is_decreasing(level: list[int]):
    return all(b < a for a, b in zip(level, level[1:]))


def is_within_range(level: list[int]):
    return all(1 <= abs(a - b) <= 3 for a, b in zip(level, level[1:]))


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
