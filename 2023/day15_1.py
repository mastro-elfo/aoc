# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return sum(hashfn(rts.strip()) for rts in content.split(","))


def hashfn(rts: str) -> int:
    current = 0
    for char in rts:
        current = (17 * (current + ord(char))) % 256
    return current


def main() -> None:
    with open("day15.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
