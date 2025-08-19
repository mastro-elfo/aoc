# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return len([line for line in content if is_nice(line)])


def is_nice(line: str):
    return all(
        f(line)
        for f in [has_doubled_letter, has_more_than_three_vowels, has_no_naugthy]
    )


def has_doubled_letter(line: str):
    return any(x == y for x, y in zip(line, line[1:]))


def has_more_than_three_vowels(line: str):
    return len([ch for ch in line if ch in "aeiou"]) >= 3


def has_no_naugthy(line: str):
    return not any(naugthy in line for naugthy in ["ab", "cd", "pq", "xy"])


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
