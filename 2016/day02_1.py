# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Position = tuple[int, int]


def solution(content: list[str]) -> Any:
    current = (1, 1)
    code = ""
    for line in content:
        for char in line:
            current = follow(char, current, 2)
        code += to_code(current)
    return code


def to_code(position: Position):
    if position == (0, 0):
        return "1"
    if position == (1, 0):
        return "2"
    if position == (2, 0):
        return "3"
    if position == (0, 1):
        return "4"
    if position == (1, 1):
        return "5"
    if position == (2, 1):
        return "6"
    if position == (0, 2):
        return "7"
    if position == (1, 2):
        return "8"
    if position == (2, 2):
        return "9"
    return ""


def follow(instruction: str, current: Position, size: int):
    if instruction == "U":
        return (current[0], max(0, current[1] - 1))
    if instruction == "D":
        return (current[0], min(size, current[1] + 1))
    if instruction == "L":
        return (max(0, current[0] - 1), current[1])
    if instruction == "R":
        return (min(size, current[0] + 1), current[1])
    return current


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
