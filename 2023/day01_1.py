# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(first_digit(line) * 10 + first_digit(line[::-1]) for line in content)


def first_digit(line: str) -> int:
    return int([ch for ch in line if ch in "1234567890"][0])


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
