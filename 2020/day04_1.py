# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return len([doc for doc in content.split("\n\n") if is_valid(doc)])


def is_valid(line: str):
    return all(
        key in line
        for key in [
            "byr:",
            "iyr:",
            "eyr:",
            "hgt:",
            "hcl:",
            "ecl:",
            "pid:",
        ]
    )


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
