# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(
        row_exact_division(row)
        for row in [[int(x) for x in line.split()] for line in content]
    )


def row_exact_division(line: list[int]):
    for index, current in enumerate(line):
        division = value_exact_division(current, line[:index] + line[index + 1 :])
        if division is not None:
            return division
    return 0


def value_exact_division(value: int, line: list[int]):
    for current in line:
        if value % current == 0:
            return value // current
    return None


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
