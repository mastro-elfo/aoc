# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Histogram = list[int]


def solution(content: str) -> Any:
    items = [parse(block) for block in content.split("\n\n")]
    height = len(content.split("\n\n")[0].split("\n"))
    locks = [histo for tp, histo in items if tp == "Lock"]
    keys = [histo for tp, histo in items if tp == "Key"]

    return sum(
        1 if all(l + k <= height for l, k in zip(lock, key)) else 0
        for lock in locks
        for key in keys
    )


def parse(block: str) -> tuple[Literal["Key", "Lock"], Histogram]:
    lines = block.split("\n")
    first, *_ = lines
    histo = [0] * len(first)

    for line in lines:
        for index, pin in enumerate(line.strip()):
            histo[index] += 1 if pin == "#" else 0

    return ("Key" if first.startswith(".") else "Lock", histo)


def main() -> None:
    with open("day25.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
