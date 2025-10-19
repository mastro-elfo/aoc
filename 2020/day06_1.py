# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return sum(len(set(group.replace("\n", ""))) for group in content.split("\n\n"))


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
