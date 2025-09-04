# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    codes = sorted(list(parse(line) for line in content))
    for a, b in zip(codes, codes[1:]):
        if a == b - 2:
            return a + 1


def parse(line: str):
    row = int(line[:7].replace("B", "1").replace("F", "0"), 2)
    col = int(line[7:].replace("R", "1").replace("L", "0"), 2)
    return row * 8 + col


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
