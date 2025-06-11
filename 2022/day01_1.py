# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    elves: list[list[int]] = [[]]
    for line in content:
        if line.strip() == "":
            elves += [[]]
        else:
            elves[-1] += [int(line)]
    return sorted([sum(elf) for elf in elves], reverse=True)[0]


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
