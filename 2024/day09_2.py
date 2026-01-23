# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Block = tuple[int, int, bool, bool]


def solution(content: str) -> Any:
    return checksum(unzip(defrag(parse(content))))


def pretty(disk: list[Block]):
    return "".join(str(file_id(b)) if is_file(b) else "." for b in disk)


def checksum(disk: list[Block]) -> int:
    return sum(
        index * file_id(block) for index, block in enumerate(disk) if is_file(block)
    )


def unzip(disk: list[Block]) -> list[Block]:
    return [b for block in disk for b in [block] * file_size(block)]


def defrag(disk: list[Block]) -> list[Block]:
    copy = disk[::]
    end = prev_file(copy, len(copy) - 1)
    while end >= 0:
        fid, fsize, _, _ = copy[end]
        start = next_empty(copy, 0, end, fsize)
        _, esize, _, _ = copy[start]
        if start != -1:
            copy = (
                copy[:start]
                + [(fid, fsize, True, True), (0, max(0, esize - fsize), False, False)]
                + copy[(start + 1) : end]
                + [(0, fsize, False, False)]
                + copy[(end + 1) :]
            )
            end = prev_file(copy, end)
        else:
            end = prev_file(copy, end - 1)
    return copy


def next_empty(disk: list[Block], start: int, rindex: int, rsize: int) -> int:
    while start < rindex:
        if not is_file(disk[start]):
            _, size, _, _ = disk[start]
            if size >= rsize:
                return start
        start += 1
    return -1


def prev_file(disk: list[Block], end: int) -> int:
    while not is_file(disk[end]) or did_move(disk[end]):
        end -= 1
    return end


def did_move(block: Block) -> bool:
    _, _, _, move = block
    return move


def is_file(block: Block) -> bool:
    _, _, file, _ = block
    return file


def file_id(block: Block) -> int:
    fid, _, _, _ = block
    return fid


def file_size(block: Block) -> int:
    _, size, _, _ = block
    return size


def parse(content: str) -> list[Block]:
    return [
        (index // 2, int(char), index % 2 == 0, False)
        for index, char in enumerate(content)
    ]


def main() -> None:
    with open("day09.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
