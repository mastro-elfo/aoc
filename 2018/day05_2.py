# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return min(
        len(react(remove(unit, content))) for unit in {ch.lower() for ch in content}
    )


def remove(unit: str, polymer: str):
    return "".join(ch for ch in polymer if ch != unit and ch.lower() != unit)


def react(polymer: str):
    copy = polymer[:]
    is_on = True
    while is_on:
        is_on = False
        for index, (fst, snd) in enumerate(zip(copy, copy[1:])):
            if should_react(fst, snd):
                is_on = True
                copy = copy[:index] + copy[index + 2 :]
                break
    return copy


def should_react(a: str, b: str):
    return a.lower() == b.lower() and a != b


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
