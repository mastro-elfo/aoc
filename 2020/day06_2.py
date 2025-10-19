# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return sum(len(every_yes(group.split("\n"))) for group in content.split("\n\n"))


def every_yes(group: list[str]):
    return [
        answer
        for answer in set("".join(group))
        if all(answer in person for person in group)
    ]


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
