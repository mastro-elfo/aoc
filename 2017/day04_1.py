# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return len([line for line in content if is_valid(line)])


def is_valid(line: str):
    words = line.strip().split(" ")
    return len(words) == len(set(words))


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
