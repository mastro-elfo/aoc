# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(priority(repeated(line)) for line in content)


def repeated(line: str):
    fst = line[: len(line) // 2]
    snd = line[len(line) // 2 :]
    return next(ch for ch in fst if ch in snd)


def priority(ch: str):
    if "a" <= ch[0] <= "z":
        return ord(ch) - ord("a") + 1
    if "A" <= ch[0] <= "Z":
        return ord(ch) - ord("A") + 27
    return 0


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
