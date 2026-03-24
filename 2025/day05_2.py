# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Range = tuple[int, int]


def solution(content: str) -> Any:
    range_block, _ = content.split("\n\n")
    return sum(
        end - start + 1
        for start, end in merge([parse_range(line) for line in range_block.split("\n")])
    )


def merge(ranges: list[Range]) -> list[Range]:
    if not ranges:
        return []
    first, *rest = ranges
    for rng in rest:
        if overlap(first, rng):
            s1, e1 = first
            s2, e2 = rng
            return merge([(min(s1, s2), max(e1, e2)), *[r for r in rest if r != rng]])
    return [first, *merge(rest)]


def overlap(rng1: Range, rng2: Range) -> bool:
    # pylint: disable=arguments-out-of-order
    return overlap_helper(rng1, rng2) or overlap_helper(rng2, rng1)


def overlap_helper(rng1: Range, rng2: Range) -> bool:
    s1, e1 = rng1
    s2, e2 = rng2
    return s1 <= s2 <= e1 or s1 <= e2 <= e1


def parse_range(line: str) -> Range:
    start, _, end = line.partition("-")
    return (int(start), int(end))


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
