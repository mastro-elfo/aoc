# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Position = tuple[int, int]


def solution(content: list[str]) -> Any:
    current = (0, 21)
    code = ""
    for line in content:
        for char in line:
            current = follow(char, current)
        code += to_code(current)
    return code


def to_code(position: Position):
    if position == (2, 0):
        return "1"
    if position == (1, 1):
        return "2"
    if position == (2, 1):
        return "3"
    if position == (3, 1):
        return "4"
    if position == (0, 2):
        return "5"
    if position == (1, 2):
        return "6"
    if position == (2, 2):
        return "7"
    if position == (3, 2):
        return "8"
    if position == (4, 2):
        return "9"
    if position == (1, 3):
        return "A"
    if position == (2, 3):
        return "B"
    if position == (3, 3):
        return "C"
    if position == (2, 4):
        return "D"
    return ""


def follow(instruction: str, current: Position):
    if instruction == "U" and current not in [(0, 2), (1, 1), (2, 0), (3, 1), (4, 2)]:
        return (current[0], current[1] - 1)
    if instruction == "D" and current not in [(0, 2), (1, 3), (2, 4), (3, 3), (4, 2)]:
        return (current[0], current[1] + 1)
    if instruction == "L" and current not in [(2, 0), (1, 1), (0, 2), (1, 3), (2, 4)]:
        return (current[0] - 1, current[1])
    if instruction == "R" and current not in [(2, 0), (3, 1), (4, 2), (3, 3), (2, 4)]:
        return (current[0] + 1, current[1])
    return current


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
