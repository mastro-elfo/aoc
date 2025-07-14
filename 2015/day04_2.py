# pylint: disable=missing-module-docstring, missing-function-docstring


import hashlib
from typing import Any


def solution(content: str) -> Any:
    index = 1
    while True:
        hashed = hashlib.md5(bytes(content + str(index), "utf8")).hexdigest()
        if hashed.startswith("000000"):
            return index
        index += 1


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
