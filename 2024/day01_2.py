# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    split = [line.split() for line in content]
    right = [a for *_, a in split]
    return sum(int(a) * len([r for r in right if r == a]) for a, *_ in split)


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
