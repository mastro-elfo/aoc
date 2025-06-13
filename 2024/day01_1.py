# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    split = [line.split() for line in content]
    left = sorted(int(a) for a, *_ in split)
    right = sorted(int(a) for *_, a in split)
    return sum(abs(a - b) for a, b in zip(left, right))


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
