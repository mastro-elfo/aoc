# pylint: disable=missing-module-docstring, missing-function-docstring


import hashlib
from typing import Any, Generator


def solution(content: str) -> Any:
    return "".join(p[5] for p in take(8, hash_gen(content)))


def hash_gen(prefix: str):
    counter = 0
    while True:
        hashed = hashlib.md5(bytes(prefix + str(counter), "utf8")).hexdigest()
        if hashed.startswith("0" * 5):
            print(counter, hashed)
            yield hashed
        counter += 1


def take(n: int, g: Generator):
    for _ in range(n):
        yield next(g)


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
