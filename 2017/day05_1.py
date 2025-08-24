# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return play([int(j) for j in content])


def play(jumps: list[int]):
    step = 0
    index = 0
    copy = jumps[::]
    while 0 <= index < len(copy):
        offset = copy[index]
        copy[index] += 1
        index += offset
        step += 1
    return step


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
