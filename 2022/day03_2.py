# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(priority(badge(item)) for item in group_by(content))


def badge(group: tuple[str, str, str]):
    fst, snd, trd = group
    return next(ch for ch in fst if ch in snd and ch in trd)


def group_by(lines: list[str]):
    rest = lines
    output = []
    while len(rest) > 0:
        fst, snd, trd, *rest = rest
        output.append((fst, snd, trd))
    return output


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
