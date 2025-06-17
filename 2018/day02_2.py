# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    fst, snd = [
        (fst, snd) for fst in content for snd in content if distance(fst, snd) == 1
    ][0]
    return "".join(a for a, b in zip(fst, snd) if a == b)


def distance(fst: str, snd: str):
    return sum(a != b for a, b in zip(fst, snd))


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
