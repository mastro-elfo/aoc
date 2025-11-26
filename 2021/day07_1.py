# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    positions = [int(x) for x in content.split(",")]
    min_position = min(positions)
    max_position = max(positions)
    return min(
        fuel_tot(positions, end) for end in range(min_position, max_position + 1)
    )


def fuel_tot(starts: list[int], end: int) -> int:
    return sum(fuel_to(start, end) for start in starts)


def fuel_to(start: int, end: int) -> int:
    return abs(start - end)


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
