# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return len([line for line in content if is_valid(line)])


def is_valid(line: str):
    words = line.strip().split(" ")
    return len(words) == len(set(words)) and all(
        not is_anagram(word, [w for w in words if w != word]) for word in words
    )


def is_anagram(word: str, others: list[str]):
    srtd = sorted(word)
    return any(srtd == sorted(w) for w in others)


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
