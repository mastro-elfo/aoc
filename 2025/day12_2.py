# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(_: str) -> Any:
    return "Nothing to do"


def main() -> None:
    with open("day12.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
