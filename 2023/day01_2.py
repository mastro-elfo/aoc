# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(first_digit(line) * 10 + first_digit(line, True) for line in content)


def first_digit(line: str, backward=False) -> int:
    return int([ch for ch in replace(line, backward) if ch in "1234567890"][0])


def replace(line: str, backward=False) -> str:
    if backward:
        return (
            replace(
                line.replace("twone", "1")
                .replace("zerone", "1")
                .replace("eightwo", "2")
                .replace("eighthree", "3")
                .replace("oneight", "8")
                .replace("nineight", "8"),
                False,
            )
        )[::-1]
    return (
        line.replace("twone", "2")
        .replace("zerone", "0")
        .replace("eightwo", "8")
        .replace("eighthree", "8")
        .replace("oneight", "1")
        .replace("nineight", "9")
        .replace("one", "1")
        .replace("two", "2")
        .replace("three", "3")
        .replace("four", "4")
        .replace("five", "5")
        .replace("six", "6")
        .replace("seven", "7")
        .replace("eight", "8")
        .replace("nine", "9")
        .replace("zero", "0")
    )


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
