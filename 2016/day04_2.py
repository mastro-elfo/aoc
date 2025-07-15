# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Room = tuple[str, int, str]


def solution(content: list[str]) -> Any:
    return "\n".join(
        str(num)
        for name, num, check in (parse(line) for line in content)
        if check == checksum(name) and "north" in decrypt(name, num)
    )


def decrypt(name: str, key: int):
    return "".join(rotate(char, key) for char in name)


def rotate(char: str, key: int):
    if char == "-":
        return " "
    return chr(ord("a") + ((ord(char) - ord("a") + key) % 26))


def checksum(name: str):
    def helper(count: dict[str, int], line: list[str]):
        if not line:
            return "".join(
                key
                for key, _ in sorted(
                    sorted(count.items(), key=lambda item: item[0]),
                    key=lambda item: item[1],
                    reverse=True,
                )
            )[:5]
        char, *rest = line
        count[char] = 1 + count.get(char, 0)
        return helper(count, rest)

    return helper({}, list(name.replace("-", "")))


def parse(line: str) -> Room:
    match = re.match(r"([a-z\-]+)(\d+)\[([a-z]+)\]", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    return (match.group(1), int(match.group(2)), match.group(3))


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
