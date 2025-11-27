# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return 4 + next(
        index
        for index, values in enumerate(
            zip(content, content[1:], content[2:], content[3:])
        )
        if not repeating(values)
    )


def repeating(values: tuple[str, str, str, str]) -> bool:
    a, b, c, d = values
    return a == b or a == c or a == d or b == c or b == d or c == d


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
