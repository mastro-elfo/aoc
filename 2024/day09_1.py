# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Block = tuple[int, bool]


def solution(content: str) -> Any:
    return checksum(defrag(parse(content)))


def checksum(disk: list[Block]) -> int:
    return sum(
        index * file_id(block) for index, block in enumerate(disk) if is_file(block)
    )


def defrag(disk: list[Block]) -> list[Block]:
    copy = disk[::]
    start = next_empty(copy, 0)
    end = prev_file(copy, len(copy) - 1)
    while end > start:
        copy[start] = copy[end]
        copy[end] = (0, False)
        start = next_empty(copy, start)
        end = prev_file(copy, end)
    return copy


def next_empty(disk: list[Block], start: int) -> int:
    while is_file(disk[start]):
        start += 1
    return start


def prev_file(disk: list[Block], end: int) -> int:
    while not is_file(disk[end]):
        end -= 1
    return end


def is_file(block: Block) -> bool:
    _, file = block
    return file


def file_id(block: Block) -> int:
    fid, _ = block
    return fid


def parse(content: str) -> list[Block]:
    fid = 0
    output = []
    for index, char in enumerate(content):
        output += [(fid, index % 2 == 0)] * int(char)
        if index % 2 == 0:
            fid += 1
    return output


def main() -> None:
    with open("day09.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
