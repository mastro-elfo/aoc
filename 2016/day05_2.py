# pylint: disable=missing-module-docstring, missing-function-docstring


import hashlib
from typing import Any


def solution(content: str) -> Any:
    password = [""] * 8
    generator = hash_gen(content)
    while len([p for p in password if p]) < 8:
        hashed = next(generator)
        index = int(hashed[5], 16)
        if index > 7:
            continue
        if password[index]:
            continue
        password[index] = hashed[6]
    return "".join(password)


def hash_gen(prefix: str):
    counter = 0
    while True:
        hashed = hashlib.md5(bytes(prefix + str(counter), "utf8")).hexdigest()
        if hashed.startswith("0" * 5):
            yield hashed
        counter += 1


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
