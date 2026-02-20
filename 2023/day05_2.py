# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Range = tuple[int, int, int]
type Interval = tuple[int, int]


def solution(content: str) -> Any:
    intvs = parse_seeds(content.split("\n")[0])
    maps = parse_ranges("\n".join(content.split("\n")[2:]))

    for mp in maps:
        intvs = apply_all(mp, intvs)

    return min(start for start, _ in intvs)


def apply_all(ranges: list[Range], intvs: list[Interval]) -> list[Interval]:
    return [x for intv in intvs for x in apply_any(ranges, intv)]


def apply_any(ranges: list[Range], seeds: Interval):
    for rng in ranges:
        if overlap(rng, seeds):
            same, transformed = apply_one(rng, seeds)
            return transformed + apply_all(ranges, same)
    return [seeds]


def apply_one(rng: Range, seeds: Interval) -> tuple[list[Interval], list[Interval]]:
    start, end = seeds
    dest, source, size = rng
    if start < source <= end < source + size:
        return [(start, source - 1)], [(dest, dest + end - source)]
    if start < source and source + size <= end:
        return [(start, source - 1), (source + size, end)], [(dest, dest + size - 1)]
    if source <= start and end < source + size:
        return ([]), [(dest + start - source, dest + end - source)]
    if source <= start < source + size <= end:
        return [(source + size, end)], [
            (dest + start - source, dest + size - 1),
        ]
    return [seeds], []


def overlap(rng: Range, seeds: Interval) -> bool:
    start, end = seeds
    _, source, size = rng
    if end < source:
        return False
    if source + size <= start:
        return False
    return True


def parse_seeds(line: str) -> list[Interval]:
    parts = [int(x) for x in line[7:].strip().split(" ")]
    return [(start, start + size - 1) for start, size in zip(parts[::2], parts[1::2])]


def parse_ranges(content: str) -> list[list[Range]]:
    groups = content.split("\n\n")
    maps: list[list[Range]] = []
    for group in groups:
        range_group: list[Range] = []
        for line in group.split("\n")[1:]:
            parts = [int(x) for x in line.split(" ")]
            range_group.append((parts[0], parts[1], parts[2]))
        maps.append(range_group)
    return maps


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
