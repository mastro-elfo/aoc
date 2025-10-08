# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    hashes = []
    memory = parse(content)
    last = str(memory)
    while last not in hashes:
        hashes.append(last)
        memory = reallocate(memory)
        last = str(memory)
    return len(hashes) - hashes.index(last)


def reallocate(memory: list[int]) -> list[int]:
    max_blocks = max(memory)
    index = [index for index, value in enumerate(memory) if value == max_blocks][0]
    copy = memory.copy()
    copy[index] = 0
    idx = 0
    while idx < max_blocks:
        copy[(index + idx + 1) % len(copy)] += 1
        idx += 1
    return copy


def parse(content: str) -> list[int]:
    return [int(x) for x in content.split()]


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
