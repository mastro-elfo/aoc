# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return propagate(
        find_start(content[0]),
        [line for line in content[1:] if any(ch != "." for ch in line)],
    )


def propagate(start: int, content: list[str]) -> int:
    count = 0
    beams = set([start])
    for line in content:
        splitters = find_splitters(line)
        count += len([beam for beam in beams if beam in splitters])
        beams = split(beams, splitters)
    return count


def find_splitters(line: str) -> list[int]:
    return [index for index, char in enumerate(line) if char == "^"]


def split(beams: set[int], splitters: list[int]) -> set[int]:
    return set(
        [beam for beam in beams if beam not in splitters]
        + [splitter - 1 for splitter in splitters]
        + [splitter + 1 for splitter in splitters]
    )


def find_start(line: str) -> int:
    return line.index("S")


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
