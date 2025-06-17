# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    count = [(has_2x(line), has_3x(line)) for line in content]
    return len([x for x, _ in count if x]) * len([x for _, x in count if x])


def has_2x(line: str):
    return any(z == 2 for z in [len([x for x in line if x == y]) for y in line])


def has_3x(line: str):
    return any(z == 3 for z in [len([x for x in line if x == y]) for y in line])


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
